//===--- approx_dev_runtime.cpp - driver of approximate runtime system for on-device approximation----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This files is the driver of the approximate runtime for GPU-based approximations
///
//===----------------------------------------------------------------------===//
//
#include "approx.h"
#include "approx_internal.h"
#include <omp.h>
#include <stdio.h>
#include <cstdlib>
#include <iostream>

// #pragma omp declare target
// class ApproxRuntimeDevDataEnv{

// public:
//   int tableSize;
//   void *table;
//   ApproxRuntimeDevDataEnv() {

// #pragma omp target enter data map(to:this[:1], tableSize)
//     }
// };

// ApproxRuntimeDevDataEnv RTEnvd = ApproxRuntimeDevDataEnv();
// #pragma omp end declare target




// class RTEnvDevDataWrapper{

//   public:
//   int talbe = 0;
//   RTEnvDevDataWrapper() {
//     const char *env_p = std::getenv("TABLE_SIZE");
//     printf( "HELLO WORLD\n");
//     if(env_p){
//       RTEnvd.tableSize = atoi(env_p);
//     } else {
//       std::cerr << "ERROR: No tablesize provided via the 'TABLE_SIZE' environment variable.\n";
//       std::abort();
//     }


//     #pragma omp target update to(RTEnvd)
//   }
// };

// RTEnvDevDataWrapper RTEnvDDW;

// void envddw()
// {  printf("hey %d\n", RTEnvDDW.talbe);}

// #pragma omp declare target

// void __approx_device_memo(void (*accurateFN)(void *), void *arg, int memo_type, void *in_data, int nInputs, void *out_data, int nOutputs)
// {
//   approx_var_info_t *in_vars = (approx_var_info_t*) in_data;
//   printf("Table size: %d\n", RTEnvd.tableSize);
//   accurateFN(arg);
// }
// #pragma omp end declare target

