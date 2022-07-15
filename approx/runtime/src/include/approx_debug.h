 //===--- approx_debug.h - Approx debugging API ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file defines the accessible API for development/debugging of the approx runtime system
///
//===----------------------------------------------------------------------===//


#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif
void resetDeviceTable(int newSize = -1, float threshold = -1.0, size_t newiSize = -1, size_t newoSize = -1);
float getDeviceThreshold();
#ifdef __cplusplus
}
#endif
