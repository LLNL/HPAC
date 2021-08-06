//===--- metrics.hpp - computes error metrics  ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file computes error metrics 
///
//===----------------------------------------------------------------------===//
//
#ifndef __METRIC__
#define __METRIC__
    double computeQuality(double* , double *, size_t , char *);
    double computeQuality(int* , int *, size_t , char *);
    void printSupportedMetrics();
    int checkMetrics(const char *metric);
#endif
