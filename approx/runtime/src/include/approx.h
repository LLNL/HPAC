#include <stdbool.h>

extern "C" {
void __approx_exec_call(void (*accurate)(void *), void (*perforate)(void *),
                        void *arg, bool cond, void *perfoArgs, void *deps,
                        int num_deps, int memo_type);
}
