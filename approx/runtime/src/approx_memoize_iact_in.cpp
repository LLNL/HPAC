//===--- approx_memoize_iact_in.cpp - runtime implementation of iACT  ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This files is the driver of the iACT approximate memoization 
///
//===----------------------------------------------------------------------===//

#include "approx_internal.h"
#include "approx_memoize_iact_in.h"

ThreadMemoryPool<MemoizeInput> inputMemo;
void memoize_in(void (*accurate)(void *), void *arg, approx_var_info_t *inputs,
        int num_inputs, approx_var_info_t *outputs, int num_outputs, bool ExecBoth, int tSize, real_t threshold) {
    static thread_local int threadId = -1;
    static thread_local MemoizeInput *curr;

    if (threadId == -1){
        if (omp_in_parallel())
            threadId = omp_get_thread_num();
        else
            threadId = 0;
    }

    if (curr && curr->Addr != (unsigned long) accurate)
        curr = inputMemo.findMemo(threadId, (unsigned long)accurate);

    if (!curr){ 
        curr = inputMemo.addNew(threadId, new MemoizeInput(accurate, num_inputs, num_outputs, inputs, outputs, getTableSize(), getThreshold()));
    }

    curr->execute(arg, inputs, outputs);
}
