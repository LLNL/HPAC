//===--- approx_data_move.cpp - runtime host <-> device data movement for HPAC  ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file provides routines for data movement between different OpenMP devices
///
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <cassert>
#include "approx_internal.h"


int copy_vars_to_device(approx_var_info_t *vals, int numVals, int destDev, int srcDev)
{
  int total_transferred = 0;
  for(int i = 0; i < numVals; i++)
    {
      total_transferred += copy_var_to_device(vals+i, destDev, srcDev);
    }

  return total_transferred;
}

int copy_var_to_device(approx_var_info_t *val, int destDev, int srcDev)
{
  void *host_ptr = val->ptr;
  int src_size = val->num_elem*val->sz_elem;
  bool buf_on_dev = omp_target_is_present(host_ptr, 0);

  if(buf_on_dev) {
    void *src_ptr = nullptr;
    void *dest_ptr = nullptr;

    // host to device
    if(omp_get_initial_device() == srcDev) {
      src_ptr = host_ptr;
      dest_ptr = omp_get_mapped_ptr(host_ptr, destDev);
    } else {
      // device to host (assume no device to device transfer for now)
      src_ptr = omp_get_mapped_ptr(host_ptr, srcDev);
      dest_ptr = host_ptr;
    }

    omp_target_memcpy(dest_ptr,
                      src_ptr,
                      src_size,
                      /*Dst offset*/ 0, /*src offset */0,
                      destDev, srcDev
                      );
    return src_size;
  }

  return 0;
}
