#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif
bool __approx_skip_iteration(unsigned int i, float pr);
void __approx_exec_call(void (*accurate)(void *), void (*perforate)(void *),
                        void *arg, bool cond, const char *region_name, void *perfoArgs, int memo_type,
                        void *inputs, int num_inputs, void *outputs,
                        int num_outputs);
const float approx_rt_get_percentage();
const int approx_rt_get_step();

extern float __approx_perfo_rate__;
extern int __approx_perfo_step__;

#ifdef __cplusplus
}
#endif
