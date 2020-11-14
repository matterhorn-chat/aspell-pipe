{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A pipe-based interface to Aspell.
--
-- This interface is beneficial when dynamic linking against the Aspell
-- library would be undesirable, e.g., for binary portability reasons.
--
-- This implementation is based on the description of the Aspell pipe
-- protocol at
--
-- http://aspell.net/man-html/Through-A-Pipe.html
module Text.Aspell
  ( Aspell
  , AspellResponse(..)
  , Mistake(..)
  , AspellOption(..)
  , startAspell
  , stopAspell
  , askAspell
  , aspellIdentification
  , aspellDictionaries
  )
where

import qualified Control.Exception as E
import Control.Monad (forM, when, void)
import qualified Control.Concurrent.Async as A
import Data.Monoid ((<>))
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import System.IO (Handle, hFlush)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified System.Process as P

-- | A handle to a running Aspell instance.
data Aspell =
    Aspell { aspellProcessHandle  :: P.ProcessHandle
           , aspellStdin          :: Handle
           , aspellStdout         :: Handle
           , aspellIdentification :: T.Text -- ^ startup-reported version string
           }

instance Show Aspell where
    show as = mconcat [ "Aspell<"
                      , T.unpack (aspellIdentification as)
                      , ">"
                      ]

-- | The kind of responses we can get from Aspell.
data AspellResponse =
    AllCorrect
    -- ^ The input had no spelling mistakes.
    | Mistakes [Mistake]
    -- ^ The input had the specified mistakes.
    deriving (Eq, Show)

-- | A spelling mistake.
data Mistake =
    Mistake { mistakeWord         :: T.Text
            -- ^ The original word in misspelled form.
            , mistakeNearMisses   :: Int
            -- ^ The number of alternative correct spellings that were
            -- counted.
            , mistakeOffset       :: Int
            -- ^ The offset, starting at zero, in the original input
            -- where this misspelling occurred.
            , mistakeAlternatives :: [T.Text]
            -- ^ The correct spelling alternatives.
            }
            deriving (Show, Eq)

-- | An Aspell option.
data AspellOption =
    UseDictionary T.Text
    -- ^ Use the specified dictionary (see aspell -d).
    | RawArg T.Text
    -- ^ Provide a command-line argument directly to aspell.
    deriving (Show, Eq)

-- | Start Aspell with the specified options. Returns either an error
-- message on failure or an Aspell handle on success.
--
-- Any 'RawArg's provided in the option list are provided to @aspell@ as
-- command-line arguments in the order provided.
startAspell :: [AspellOption] -> IO (Either String Aspell)
startAspell options = do
    optResult <- checkOptions options
    case optResult of
        Just e -> return $ Left e
        Nothing -> tryConvert $ do
            let proc = (P.proc aspellCommand ("-a" : (concat $ optionToArgs <$> options)))
                       { P.std_in = P.CreatePipe
                       , P.std_out = P.CreatePipe
                       , P.std_err = P.CreatePipe
                       }

            (Just inH, Just outH, Just errH, ph) <- P.createProcess proc

            errorAsync <- A.async (T.hGetLine errH)

            -- If startup is unsuccessful, stdout will close without output.
            result <- E.try (T.hGetLine outH) :: IO (Either E.SomeException T.Text)

            case result of
                Left{} -> do
                    e <- A.wait errorAsync
                    fail ("Error starting aspell: " <> T.unpack e)

                Right ident -> do
                    A.cancel errorAsync
                    -- Now that aspell has started and we got an
                    -- identification string, we need to make sure it
                    -- looks legitimate before we proceed.
                    case validIdent ident of
                        False -> fail ("Unexpected identification string: " <> T.unpack ident)
                        True -> do
                            let as = Aspell { aspellProcessHandle  = ph
                                            , aspellStdin          = inH
                                            , aspellStdout         = outH
                                            , aspellIdentification = ident
                                            }

                            -- Enable terse mode with aspell to improve performance.
                            T.hPutStrLn inH "!"

                            return as

validIdent :: T.Text -> Bool
validIdent s =
    "@(#) International Ispell Version" `T.isPrefixOf` s &&
    "but really Aspell" `T.isInfixOf` s

checkOptions :: [AspellOption] -> IO (Maybe String)
checkOptions [] = return Nothing
checkOptions (o:os) = do
    result <- checkOption o
    case result of
        Nothing -> checkOptions os
        Just msg -> return $ Just msg

aspellCommand :: String
aspellCommand = "aspell"

checkOption :: AspellOption -> IO (Maybe String)
checkOption (RawArg {}) = return Nothing
checkOption (UseDictionary d) = do
    -- Get the list of installed dictionaries and check whether the
    -- desired dictionary is included.
    dictListResult <- aspellDictionaries
    case dictListResult of
        Left msg -> return $ Just msg
        Right dictList ->
            case d `elem` dictList of
                True -> return Nothing
                False -> return $ Just $ "Requested dictionary " <> show d <> " is not installed"

-- | Obtain the list of installed Aspell dictionaries.
aspellDictionaries :: IO (Either String [T.Text])
aspellDictionaries =
    tryConvert $
    (T.pack <$>) <$> lines <$> P.readProcess aspellCommand ["dicts"] ""

optionToArgs :: AspellOption -> [String]
optionToArgs (UseDictionary d) = ["-d", T.unpack d]
optionToArgs (RawArg val) = [T.unpack val]

-- | Stop a running Aspell instance.
stopAspell :: Aspell -> IO ()
stopAspell = P.terminateProcess . aspellProcessHandle

-- | Submit user input to Aspell for spell-checking. Returns an
-- AspellResponse for each line of user input.
askAspell :: Aspell -> T.Text -> IO [AspellResponse]
askAspell as t = do
    -- Send the user's input. Prefix with "^" to ensure that the line is
    -- checked even if it contains metacharacters.
    forM (T.lines t) $ \theLine -> do
        T.hPutStrLn (aspellStdin as) ("^" <> theLine)
        hFlush (aspellStdin as)

        -- Read lines until we get an empty one, which indicates that aspell
        -- is done with the request.
        resultLines <- readLinesUntil (aspellStdout as) T.null

        case resultLines of
            [] -> return AllCorrect
            _ -> return $ Mistakes $ parseMistake <$> resultLines

parseMistake :: T.Text -> Mistake
parseMistake t
    | "&" `T.isPrefixOf` t = parseWithAlternatives t
    | "#" `T.isPrefixOf` t = parseWithoutAlternatives t

parseWithAlternatives :: T.Text -> Mistake
parseWithAlternatives t =
    let (header, altsWithColon) = T.breakOn ": " t
        altsStr = T.drop 2 altsWithColon
        ["&", orig, nearMissesStr, offsetStr] = T.words header
        alts = T.splitOn ", " altsStr
        offset = fromJust $ readMaybe $ T.unpack offsetStr
        nearMisses = fromJust $ readMaybe $ T.unpack nearMissesStr
    in Mistake { mistakeWord = orig
               , mistakeNearMisses = nearMisses
               -- Aspell's offset starts at 1 here because of the "^"
               -- we included in the input. Here we adjust the offset
               -- so that it's relative to the beginning of the user's
               -- input, not our protocol input.
               , mistakeOffset = offset - 1
               , mistakeAlternatives = alts
               }

parseWithoutAlternatives :: T.Text -> Mistake
parseWithoutAlternatives t =
    let ["#", orig, offsetStr] = T.words t
        offset = fromJust $ readMaybe $ T.unpack offsetStr
    in Mistake { mistakeWord = orig
               , mistakeNearMisses = 0
               , mistakeOffset = offset
               , mistakeAlternatives = []
               }

readLinesUntil :: Handle -> (T.Text -> Bool) -> IO [T.Text]
readLinesUntil h f = do
    line <- T.hGetLine h
    case f line of
        True -> return []
        False -> do
            rest <- readLinesUntil h f
            return $ line : rest

tryConvert :: IO a -> IO (Either String a)
tryConvert act = do
    result <- E.try act
    return $ either (Left . showException) Right result

showException :: E.SomeException -> String
showException = show
