import { WASI, File, OpenFile, ConsoleStdout } from './vendored_browser_wasi_shim/index.ts';
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

async function runWasm() {
    const instance_exports = {};

    // let fds here was made following 'browser_wasi_shim' example code
    // When I added wasm backend (2025-07-17), I initially was lazy and added
    // no fds at all ([] passed to new WASI), but that made all the putStrLn in
    // the game not output anything. I added the support so I could see debug
    // messages from the game.
    const fds = [
        new OpenFile(new File([])), // stdin
        ConsoleStdout.lineBuffered(msg => console.log(`[WASI stdout] ${msg}`)),
        ConsoleStdout.lineBuffered(msg => console.log(`[WASI stderr] ${msg}`)),
    ];

    const wasi_mod = new WASI(['submarination'], [], fds);
    const { instance } = await WebAssembly.instantiateStreaming(fetch("submarination.wasm"), {
        "wasi_snapshot_preview1": wasi_mod.wasiImport,
        "ghc_wasm_jsffi": ghc_wasm_jsffi(instance_exports)
    });
    Object.assign(instance_exports, instance.exports);

    wasi_mod.initialize(instance);
    await instance_exports.submarination_reactor_init();
}
runWasm();

