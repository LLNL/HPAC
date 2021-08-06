//===--- metrics.cpp - Measure execution time ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file measures execution time 
///
//===----------------------------------------------------------------------===//


#ifndef __EXECUTION_TIME_H__
#define __EXECUTION_TIME_H__
#ifdef __cplusplus 
extern "C" {
#endif
    void stopMeasure();
    void startMeasure();
#ifdef __cplusplus 
}
#endif

#endif
