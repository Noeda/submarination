// A version of prelude.mjs from ghc's WASM backend for submarination.
// The vanilla one would not play nice with esbuild. I'm not sure why (I even
// followed existing project example). It was easier to just write it myself
// than try figure it out. -- 2025-07-17 Mikko Juola

export function JSValManager() {
    this.lastk = 0;
    this.kv = new Map();

    this.newJSVal = function(v) {
        const k = ++this.lastk;
        this.kv.set(k, v);
        return k;
    }

    this.getJSVal = function(k) {
        if (!this.kv.has(k)) {
            throw new WebAssembly.RuntimeError(`getJSVal(${k}`);
        }
        return this.kv.get(k);
    }

    this.freeJSVal = function(k) {
        if (!this.kv.delete(k)) {
            throw new WebAssembly.RuntimeError(`freeJSVal(${k})`);
        }
    }
}
