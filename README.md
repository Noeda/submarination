Submarination
=============

This is a roguelike themed around diving with a submarine.

This used to be `ghcjs` + native program. In 2025-07-17, I grabbed the code
that had not been touched in 8 years, and slapped WASM backend to it, using
GHC 9.12's experimental wasm backend.

This is not a complete game at all. But what's there is functional.

How to compile and run
----------------------

To run it natively, get yourself a Haskell setup, and do `cabal run`.

I've tested this with GHC 9.12, but it likely runs on much older GHC's as well.

WASM (browser backend)
----------------------

If you want to build for wasm, do `cabal build --flag browser`. This only
builds the `.wasm` file, so you may want to check `build_wasm.sh` for more
information. At the time of writing this (2025-07-17), the wasm building is
fragile as it relies on experimental GHC features and I would expect it to
break as the wasm backend matures and the hacks in `build_wasm.sh` become less
necessary.

