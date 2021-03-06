Submarination
=============

[![Build Status](https://travis-ci.org/Noeda/submarination.svg?branch=master)](https://travis-ci.org/Noeda/submarination)

This is a roguelike themed around diving with a submarine.

Compilation
-----------

Compile with Haskell `stack` tool. Execute `submarination` executable to play.
You may want to zoom into your terminal if it's much larger than 80x24.

If you want to poke around with the browser version, invoke `stack` with
`--stack-yaml stack-browser.yaml` and then navigate to `web/` directory. You
may have to relink `bundle.js` to point to `all.js` in stack build files.

There is also a Shake file in `Build.hs` at the root of this repository. You'll
need it to build so-called baked levels that are included and compiled within
source code. It speeds up loading considerably but isn't essential otherwise.

