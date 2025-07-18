/* Only compiled for Wasm. Initializes the program, meant to be utilized by
 * wizer tool.
 *
 * The code here (roughly) follows the ghc-wasm guide from:
 * https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta (2025-07-17)
 * */

// If you are here and have no idea what 'wizer' is:
//
// Wizer will, at compile time, run your .wasm module for some
// initializations, which you then repackage into another .wasm, with
// initialization already done. For that purpose, this wasm_init.c file
// will attempt to do Haskell initialization things, and clear out memory
// to minimize its footprint and to start up faster.
//
// https://github.com/bytecodealliance/wizer <-- refers to this.

#include <Rts.h>
#include <string.h>

STATIC_INLINE void hs_init_with_rtsopts_(char *argv[]) {
  int argc;
  for (argc = 0; argv[argc] != NULL; ++argc) {
  }
  hs_init_with_rtsopts(&argc, &argv);
}

void malloc_inspect_all(void (*handler)(void *start, void *end,
                                        size_t used_bytes, void *callback_arg),
                        void *arg);

static void malloc_inspect_all_handler(void *start, void *end,
                                       size_t used_bytes, void *callback_arg) {
  if (used_bytes == 0) {
    memset(start, 0, (size_t)end - (size_t)start);
  }
}

extern char __stack_low;
extern char __stack_high;

__attribute__((export_name("wizer.initialize"))) void __wizer_initialize(void) {
  char *argv[] = {"submarination.wasm", "+RTS", "-A64m", "-RTS", NULL};

  hs_init_with_rtsopts_(argv);

  // Run gc multiple times, because finalizers might only be found, not run
  // on runs.
  hs_perform_gc();
  hs_perform_gc();

  rts_clearMemory();

  // Zeroes out bunch of memory (helps making .wasm smaller).
  malloc_inspect_all(malloc_inspect_all_handler, NULL);
  memset(&__stack_low, 0, &__stack_high - &__stack_low);
}
