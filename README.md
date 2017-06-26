
aspell-pipe
-----------

This package provides a simple interface to the `aspell` spell checker
tool. In particular, this library communicates with `aspell` using
its pipe interface rather than linking directly to the Aspell shared
library. This is because in some cases it is desirable to avoid a shared
library dependency and to deal gracefully with an absent spell checker.

To get started, see the Haddock documentation for `Text.Aspell`.
