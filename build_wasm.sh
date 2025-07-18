#!/usr/bin/env bash

# 2025-07-17: build script for wasm. Today I tried running Build.hs from 8
# years ago and it did not even run. So I made this shell script just to be
# able to compile and test the .wasm file.

# You need to source wasm32-wasi-* tools for this script to work. See:
# https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta README.
#
# Or possibly if you are reading this in like 2030 maybe GHC's wasm backend is
# much more mature and a lot of this script is unnecessary ;)
#

set -euxo pipefail

# Set working directory to where this script is
cd "$(dirname "$0")"

# Build level files (does not check if they already are built because we are
# lazy coders here)
./build_level_files.sh

wasm32-wasi-cabal build --flag browser --flag use-baked-levels
SUBMARINATION_WASM="$(wasm32-wasi-cabal list-bin exe:submarination)"

# work directory for building
mkdir wasm-build-tmp || true

# in ormolu, the command was like this:
#"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs --input "$SUBMARINATION_WASM" --output wasm-build-tmp/ghc_wasm_jsffi.js
# but submarination that produced a file that esbuild would not accept into a
# bundle. So I made a copy of post-link.mjs for our purposes.

./wasm_hackery/post-link.mjs --input "$SUBMARINATION_WASM" --output wasm-build-tmp/ghc_wasm_jsffi.js
cp "$SUBMARINATION_WASM" wasm-build-tmp/submarination-pre-wizer.wasm

wizer --allow-wasi --wasm-bulk-memory true wasm-build-tmp/submarination-pre-wizer.wasm -o wasm-build-tmp/submarination-post-wizer.wasm
wasm-opt --strip wasm-build-tmp/submarination-post-wizer.wasm -o wasm-build-tmp/submarination-post-wasm-opt.wasm -Oz
SUBMARINATION_FINAL_WASM=wasm-build-tmp/submarination-post-wasm-opt.wasm

# optionally could run wasm-strip here to further reduce size

cp wasm-build-tmp/ghc_wasm_jsffi.js ./web_wasm/ghc_wasm_jsffi.js
cp $SUBMARINATION_FINAL_WASM web_wasm/submarination.wasm

esbuild --format=iife --bundle --platform=browser web_wasm/submarination_entry.js --outfile=web_wasm/submarination.js

