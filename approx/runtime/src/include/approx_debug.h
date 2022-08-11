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
#include <iostream>

#ifdef __cplusplus
extern "C" {
#endif
  void resetDeviceTable(float threshold = -1.0, size_t newiSize = -1, size_t newoSize = -1, int numTabEntries = -1, int numThreads = -1);
  void setHostThreshold(float newThresh);
float getDeviceThreshold();
  int getNThreadsPerWarp();
  int getNTablesPerWarp();
  int calcBlockTableSizeInBytes(int threadsPerBlock, int entriesPerTable, int itemSize, int totalInputValuesPerInvocation);
#ifdef APPROX_DEV_STATS
void writeDeviceThreadStatistics(std::ostream& file);
#endif
#ifdef __cplusplus
}
#endif
