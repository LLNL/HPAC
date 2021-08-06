//===--- metrics.cpp - compute fabs ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file computes fabs 
///
//===----------------------------------------------------------------------===//

#include <math.h>

double MP_fabs(double x){
    return fabs(x);
}

float MP_fabs(float x){
    return fabsf(x);
}


