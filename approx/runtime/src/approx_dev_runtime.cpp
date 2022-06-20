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
#include <omp.h>
#include <stdio.h>

#pragma omp declare target
void __approx_device_memo(int memo_type, void *in_data, int nInputs, void *out_data, int nOutputs)
{
  printf("Approximated %d inputs, %d outputs, value: %d\n", nInputs, nOutputs, ((int*)in_data)[0]);
}
#pragma omp end declare target

