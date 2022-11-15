 //===--- approx.h - Approx public API ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file defines the public accessible API to the approximate runtime system.
///
//===----------------------------------------------------------------------===//
#ifndef APPROX_HH_INCLUDED
#define APPROX_HH_INCLUDED


#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif
enum TableReplacementPolicy : unsigned char{
  DEFAULT,
  CLOCK,
  ROUND_ROBIN
};
bool __approx_skip_iteration(unsigned int i, float pr);
void __approx_exec_call(void (*accurate)(void *), void (*perforate)(void *),
                        void *arg, bool cond, const char *region_name, void *perfoArgs, int memo_type,
                        void *inputs, int num_inputs, void *outputs,
                        int num_outputs);
#pragma omp begin declare target device_type(nohost)
__attribute__((always_inline))
void __approx_device_memo(void (*accurateFN)(void *), void *arg, int memo_type, const void *region_info_in, const void *ipt_access, const void **inputs, const int nInputs, const void *region_info_out, const void *opt_access, void **outputs, const int nOutputs, const char init_done);
#pragma omp end declare target
#pragma omp begin declare target
  void __approx_check_init(char init_done);

#pragma omp end declare target
const float approx_rt_get_percentage();
const int approx_rt_get_step();

extern float __approx_perfo_rate__;
extern int __approx_perfo_step__;

#ifdef __cplusplus
}
#endif

#endif //APPROX_HH_INCLUDED
