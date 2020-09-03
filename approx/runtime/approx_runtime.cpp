#include "approx.h"
#include <stdio.h>

typedef struct approx_perfo_info_t {
  int type;
  int region;
  int step;
  float rate;
} approx_perfo_info_t;

void __approx_exec_call(void (*accFn)(void *), void (*perfFn)(void *),
                        void *arg, bool cond, void *perfoArgs) {
  printf("I received condition %d ptr\n", cond);
  approx_perfo_info_t *perfo = (approx_perfo_info_t *)perfoArgs;

  printf("Debug Perfo Args : (%d,%d,%d,%f)\n", perfo->type, perfo->region,
         perfo->step, perfo->rate);
  if (cond) {
    if (perfFn) {
      perfFn(arg);
    } else {
      printf("Perfo Is not implemented\n");
      accFn(arg);
    }
  } else {
    accFn(arg);
  }
}
