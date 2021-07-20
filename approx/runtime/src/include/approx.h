#include <stdbool.h>

extern "C" {
void __approx_exec_call(void (*accurate)(void *), void (*perforate)(void *),
                        void *arg, bool cond, const char *region_name, void *perfoArgs, int memo_type,
                        void *inputs, int num_inputs, void *outputs,
                        int num_outputs);
}
