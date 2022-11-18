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
#ifndef APPROX_DEBUG_HH_INCLUDED
#define APPROX_DEBUG_HH_INCLUDED

#include <stdbool.h>
#include <iostream>
#include <omp.h>
#include <utility>
#include "approx.h"

void resetDeviceTable(float thresh = -1, int threads_per_block = -1, int num_blocks = -1, int num_input_items_per_entry = -1, int num_output_items_per_entry = -1, int num_tab_entries = -1, int num_threads = -1, TableReplacementPolicy RP = DEFAULT);
void resetDeviceOutputTable(float thresh, int num_output_items_per_entry, int pSize, int history_size, int window_size, int num_blocks, int num_threads);
  void setHostThreshold(float newThresh);
float getDeviceThreshold();
  int getNThreadsPerWarp();
  int getNTablesPerWarp();
  int calcBlockTableSizeInBytes(int threadsPerBlock, int entriesPerTable, int itemSize, int totalInputValuesPerInvocation);
  bool areThreadStatisticsCaptured();
  float getApproxRatioForThread(int threadNum);
std::pair<int*, int*> getApproxRatioInformation();
#ifdef APPROX_DEV_STATS
void writeDeviceThreadStatistics(std::ostream& file, bool iact = true);
#endif
#endif // APPROX_DEBUG_HH_INCLUDED
