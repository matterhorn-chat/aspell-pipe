{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A pipe-based interface to Aspell.
--
-- This interface is helpful when dynamic linking against the Aspell
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
  , startAspell
  , stopAspell
  , askAspell
  , aspellIdentification
  )
where

import qualified Control.Exception as E
import Data.Monoid ((<>))
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import System.IO (Handle, hFlush)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified System.Process as P

data Aspell =
    Aspell { aspellProcessHandle  :: P.ProcessHandle
           , aspellStdin          :: Handle
           , aspellStdout         :: Handle
           , aspellIdentification :: T.Text
           }

instance Show Aspell where
    show as = mconcat [ "Aspell<"
                      , T.unpack (aspellIdentification as)
                      , ">"
                      ]

data AspellResponse =
    AllCorrect
    | Mistakes [Mistake]
    deriving (Eq, Show)

data Mistake =
    Mistake { mistakeWord         :: T.Text
            , mistakeNearMisses   :: Int
            , mistakeOffset       :: Int
            , mistakeAlternatives :: [T.Text]
            }
            deriving (Show, Eq)

startAspell :: IO (Either String Aspell)
startAspell = do
    let proc = (P.proc "aspell" ["-a"])
               { P.std_in = P.CreatePipe
               , P.std_out = P.CreatePipe
               , P.std_err = P.NoStream
               }

    result <- E.try $ P.createProcess proc
    case result of
        Left (e::E.SomeException) -> return $ Left $ show e
        Right (Just inH, Just outH, Nothing, ph) -> do
            ident <- T.hGetLine outH
            let as = Aspell { aspellProcessHandle  = ph
                            , aspellStdin          = inH
                            , aspellStdout         = outH
                            , aspellIdentification = ident
                            }

            -- Enable terse mode with aspell to improve performance.
            T.hPutStrLn inH "!"

            return $ Right as

stopAspell :: Aspell -> IO ()
stopAspell = P.terminateProcess . aspellProcessHandle

askAspell :: Aspell -> T.Text -> IO (Either String AspellResponse)
askAspell as t = do
    -- Send the user's input. Prefix with "^" to ensure that the line is
    -- checked even if it contains metacharacters.
    T.hPutStrLn (aspellStdin as) ("^" <> t)
    hFlush (aspellStdin as)

    -- Read lines until we get an empty one, which indicates that aspell
    -- is done with the request.
    resultLines <- readLinesUntil (aspellStdout as) T.null

    case resultLines of
        [] -> return $ Right AllCorrect
        _ -> return $ Right $ Mistakes $ parseMistake <$> resultLines

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
               -- Aspell's offset starts at 1, but to make it
               -- compatible with all other APIs we subtract one to
               -- make it start at 0.
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
