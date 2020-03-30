
0.4
---

Misc changes:
 * The package now uses and depends on `async` instead of using raw
   `forkIO`.

0.3
---

Package changes:
 * Relaxed process lower bound to version 1.3.

API changes:
 * Added 'aspellDictionaries' to obtain the list of installed
   dictionaries.

Other changes:
 * startAspell now gracefully handles Aspell startup failures by
   reporting Aspell's standard error output in the event of a failed
   startup. It also sanity-checks the Aspell pipe mode identification
   string. (Fixes #2, #3.)


0.2
---

* Relaxed base upper bound to < 5.

0.1
---

Initial release.
