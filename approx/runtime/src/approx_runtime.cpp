#include <stdint.h>
#include <stdio.h>
#include <string>

#include <approx.h>
#include <approx_data_util.h>
#include <approx_internal.h>

using namespace std;


void _printdeps(approx_var_info_t *vars, int num_deps) {
  for (int i = 0; i < num_deps; i++) {
    printf("%p, NE:%ld, SE:%ld, DT:%s, DIR:%d\n", vars[i].ptr,
           vars[i].num_elem, vars[i].sz_elem,
           getTypeName((ApproxType) vars[i].data_type), vars[i].dir);
  }
}

void __approx_exec_call(void (*accurate)(void *), void (*perforate)(void *),
                        void *arg, bool cond, void *perfoArgs, int memo_type,
                        void *inputs, int num_inputs, void *outputs,
                        int num_outputs) {
  approx_perfo_info_t *perfo = (approx_perfo_info_t *)perfoArgs;
  approx_var_info_t *input_vars = (approx_var_info_t *)inputs;
  approx_var_info_t *output_vars = (approx_var_info_t *)outputs;

  if (cond) {
   memoize_out(accurate, arg, output_vars, num_outputs);
  } else {
   accurate(arg);
  }
}
