//===--- approx_runtime.cpp - driver of approximate runtime system----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This files is the driver of the approximate runtime 
///
//===----------------------------------------------------------------------===//
//


#include <stdint.h>
#include <string>
#include <cstring>
#include <cstdlib>
#include <chrono>
#include <unordered_map>
#include <algorithm>
#include <random>
#include <omp.h>
#include <iostream>

#include "approx.h"
#include "approx_debug.h"
#include "approx_data_util.h"
#include "approx_internal.h"
#include "approx_device_memo_table.h"
#include "device_intrinsics.h"

    // TODO: this should go to cmake
#define NTHREADS_IN_WARP 32
#define FULL_MASK 0xFFFFFFFF



using namespace std;

#define MEMO_IN 1
#define MEMO_OUT 2

#define RAND_SIZE  10000

float __approx_perfo_rate__;
int __approx_perfo_step__;

enum ExecuteMode: uint8_t{
  EXECUTE
};

struct ApproxRuntimeDevMetadata
{
  // don't transfer these to the device: they are used on the host
  int total_input_size;
  int total_output_size;
  int total_num_tables;
  int replacement_data_size;
  int num_blocks;
  int num_tab_entries;
  int num_threads;
};

#pragma omp declare target
class ApproxRuntimeDevDataEnv{

public:
  char *inputIdx = nullptr;
  unsigned char *ReplacementData = nullptr;
  int *ClockIndexes = nullptr;
  int *tabNumEntries = nullptr;
  float *threshold = nullptr;
  real_t *iTable = nullptr;
  real_t *oTable = nullptr;
#ifdef APPROX_DEV_STATS
  int *accurate_count = nullptr;
  int *approx_count = nullptr;
  int *nthreads = nullptr;
#endif //APPROX_DEV_STATS

  ApproxRuntimeDevDataEnv() = default;

#pragma omp end declare target
  void resetTable(float _threshold, int total_input_size, int total_output_size, int total_num_tables, int replacement_data_size, int num_blocks, int _numTableEntries, int _nthreads, ApproxRuntimeDevMetadata& RTMeta){
    // we want to have at least one 8-bit integer per table
    destruct();
    RTMeta.total_input_size = total_input_size;
    RTMeta.total_output_size = total_output_size;
    RTMeta.total_num_tables = total_num_tables;
    RTMeta.replacement_data_size = replacement_data_size;
    RTMeta.num_blocks = num_blocks;
    RTMeta.num_tab_entries = _numTableEntries;
    RTMeta.num_threads = _nthreads;
    threshold = new float[1];
    tabNumEntries = new int[1];
    tabNumEntries[0] = _numTableEntries;
    threshold[0] = _threshold;
    inputIdx = new char[total_num_tables];
    // we want one bit for each entyr in every table
    ReplacementData = new unsigned char[replacement_data_size];
    ClockIndexes = new int[total_num_tables];
    iTable = new real_t[total_input_size];
    oTable = new real_t[total_output_size];

    #ifdef APPROX_DEV_STATS
    nthreads = new int[1];
    *nthreads = _nthreads;
    accurate_count = new int[*nthreads];
    approx_count = new int[*nthreads];

    std::fill(accurate_count, accurate_count+*nthreads, 0);
    std::fill(approx_count, approx_count+*nthreads, 0);
    #endif // APPROX_DEV_STATS

    std::fill(iTable, iTable + total_input_size, 0);
    std::fill(inputIdx, inputIdx+total_num_tables, 0);
    std::fill(ClockIndexes, ClockIndexes+total_num_tables, 0);
    std::fill(ReplacementData, ReplacementData+replacement_data_size, 0);
  }

  // allow explicit destruction.
  void destruct(){
    delete[] threshold;
    delete[] tabNumEntries;
    delete[] inputIdx;
    delete[] ReplacementData;
    delete[] ClockIndexes;
    delete[] iTable;
    delete[] oTable;
    #ifdef APPROX_DEV_STATS
    delete[] nthreads;
    delete[] accurate_count;
    delete[] approx_count;
    #endif // APPROX_DEV_STATS
  }
#pragma omp declare target
};

ApproxRuntimeDevDataEnv RTEnvd = ApproxRuntimeDevDataEnv();
ApproxRuntimeDevMetadata RTMeta = ApproxRuntimeDevMetadata();
#pragma omp end declare target

void resetDeviceTable(float thresh, int threads_per_block, int num_blocks, int num_input_items_per_entry, int num_output_items_per_entry, int num_tab_entries, int num_threads){
  float threshold = thresh == -1.0 ? *RTEnvd.threshold : thresh;
  int numTabEntries = num_tab_entries == -1 ? *RTEnvd.tabNumEntries : num_tab_entries;
  if(num_threads == -1)
    {
  #ifdef APPROX_DEV_STATS
    num_threads = *RTEnvd.nthreads;
  #endif
    }

  #ifndef APPROX_DEV_STATS
  if(num_threads != -1)
    printf("WARNING: A number of threads has been give, but HPAC is not built to collect per-thread memoization statistics. "
           "The argument will be ignored\n");
  #endif

  if(omp_target_is_present(RTEnvd.threshold, 0))
    {
      int oldNumEntries = *RTEnvd.tabNumEntries;

      #ifdef APPROX_DEV_STATS
#pragma omp target exit data map(delete:RTEnvd, RTEnvd.threshold[0:1], RTEnvd.tabNumEntries[0:1], RTEnvd.inputIdx[0:RTMeta.total_num_tables], RTEnvd.ReplacementData[0:RTMeta.replacement_data_size], RTEnvd.ClockIndexes[0:RTMeta.total_num_tables], RTEnvd.iTable[0:RTMeta.total_input_size], RTEnvd.oTable[0:RTMeta.total_output_size], RTEnvd.accurate_count[0:RTMeta.num_threads], RTEnvd.approx_count[0:RTMeta.num_threads])
      #else
#pragma omp target exit data map(delete:RTEnvd, RTEnvd.threshold[0:1], RTEnvd.tabNumEntries[0:1], RTEnvd.inputIdx[0:RTMeta.total_num_tables], RTEnvd.ReplacementData[0:RTMeta.replacement_data_size], RTEnvd.ClockIndexes[0:RTMeta.total_num_tables], RTEnvd.iTable[0:RTMeta.total_input_size], RTEnvd.oTable[0:RTMeta.total_output_size])
      #endif //APPROX_DEV_STATS
    }
  int total_num_tables = num_blocks * ((threads_per_block/NTHREADS_PER_WARP)*TABLES_PER_WARP);
  int total_num_table_entries = total_num_tables * num_tab_entries;
  int total_input_size = total_num_table_entries * num_input_items_per_entry;
  int total_output_size = total_num_table_entries * num_output_items_per_entry;
  int replacement_data_size = total_num_tables * (1+std::max(1, num_tab_entries/8));
  int clock_data_size = total_num_tables;
  RTEnvd.resetTable(threshold, total_input_size, total_output_size, total_num_tables, replacement_data_size, num_blocks, num_tab_entries, num_threads, RTMeta);
  #ifdef APPROX_DEV_STATS
#pragma omp target enter data map(to:RTEnvd, RTEnvd.threshold[0:1], RTEnvd.tabNumEntries[0:1], RTEnvd.inputIdx[0:total_num_tables], RTEnvd.ReplacementData[0:replacement_data_size], RTEnvd.ClockIndexes[0:clock_data_size], RTEnvd.iTable[0:total_input_size], RTEnvd.oTable[0:total_output_size], RTEnvd.accurate_count[0:num_threads], RTEnvd.approx_count[0:num_threads])
  #else
#pragma omp target enter data map(to:RTEnvd, RTEnvd.threshold[0:1], RTEnvd.tabNumEntries[0:1], RTEnvd.inputIdx[0:total_num_tables], RTEnvd.ReplacementData[0:replacement_data_size], RTEnvd.ClockIndexes[0:clock_data_size], RTEnvd.iTable[0:total_input_size], RTEnvd.oTable[0:total_output_size])
  #endif // APPROX_DEV_STATS
}

  int getNThreadsPerWarp()
  {
    return NTHREADS_PER_WARP;
  }
  int getNTablesPerWarp()
  {
    return TABLES_PER_WARP;
  }


  int calcBlockTableSizeInBytes(int threadsPerBlock, int entriesPerTable, int itemSize, int totalInputValuesPerInvocation)
  {
    int warpsPerBlock = threadsPerBlock / NTHREADS_PER_WARP;
    int tablesPerBlock = warpsPerBlock * TABLES_PER_WARP;
    int entriesPerBlock = tablesPerBlock * entriesPerTable;
    int entriesPerBlockAllInputs = entriesPerBlock * totalInputValuesPerInvocation;
    int totalTableBytes = entriesPerBlockAllInputs * itemSize;
    return totalTableBytes;
  }

bool areThreadStatisticsCaptured()
{
  #ifdef APPROX_DEV_STATS
  return true;
  #else
  return false;
  #endif
}


float getApproxRatioForThread(int threadNum)
{
  #ifdef APPROX_DEV_STATS
#pragma omp target update from(RTEnvd.accurate_count[threadNum])
#pragma omp target update from(RTEnvd.approx_count[threadNum])

  int accur = RTEnvd.accurate_count[threadNum];
  int appro = RTEnvd.approx_count[threadNum];
  return (float) appro / (accur+appro);
  #else
  return 0;
  #endif
}


std::pair<int*, int*> getApproxRatioInformation()
{
  #ifdef APPROX_DEV_STATS
#pragma omp target update from(RTEnvd.accurate_count[0:RTMeta.num_threads])
#pragma omp target update from(RTEnvd.approx_count[0:RTMeta.num_threads])
  return std::make_pair(RTEnvd.accurate_count, RTEnvd.approx_count);
  #else
  return std::make_pair(nullptr, nullptr);
  #endif
}

void writeDeviceThreadStatistics(std::ostream& file){
  #ifdef APPROX_DEV_STATS
  file << "THREAD,ACCURATE,APPROX,RATIO\n";
  int nthreads = *RTEnvd.nthreads;
#pragma omp target update from(RTEnvd.accurate_count[0:nthreads], RTEnvd.approx_count[0:nthreads])
  for(int tnum = 0; tnum < nthreads; tnum++)
    {
      int accur = RTEnvd.accurate_count[tnum];
      int appro = RTEnvd.approx_count[tnum];
      file << tnum << ","
           << accur << ","
           << appro << ","
           << (float) appro / (accur + appro)
           << "\n";
    }
  #endif
}


float getDeviceThreshold(){
  if(RTEnvd.threshold)
    return *RTEnvd.threshold;
  return -1.0;
}

class ApproxRuntimeConfiguration{
  ExecuteMode Mode;
public:
  bool ExecuteBoth;
  int tableSize;
  int inputNumEntries;
  int outputNumEntries;
  int threadsPerBlock;
  int numBlocks;
  int inputSize;
  int outputSize;
  float threshold;
  int historySize;
  int predictionSize;
  int perfoStep;
  float perfoRate;
  float *randomNumbers;
  int count;

  ApproxRuntimeConfiguration() {
      ExecuteBoth = false;
      count = 0;

    const char *env_p = std::getenv("EXECUTE_BOTH");
    if (env_p){
      ExecuteBoth = true;
    }

    env_p = std::getenv("EXECUTE_MODE");
    if (!env_p) {
      Mode = EXECUTE;
    } else{
        Mode = EXECUTE;
    }

    env_p = std::getenv("THRESHOLD");
    if (env_p) {
      threshold = atof(env_p);
    }

    tableSize = 0;
    env_p = std::getenv("TABLE_SIZE");
    if (env_p){
      tableSize = atoi(env_p);
      inputNumEntries = tableSize;
      outputNumEntries = tableSize;
    }

    env_p = std::getenv("THREADS_PER_BLOCK");
      if(env_p) {
        threadsPerBlock = atoi(env_p);
      }

    env_p = std::getenv("NUM_BLOCKS");
    if(env_p) {
      numBlocks = atoi(env_p);
    }

    env_p = std::getenv("INPUT_ENTRY_SIZE");
    if(env_p) {
      inputSize = atoi(env_p);
    }

    env_p = std::getenv("OUTPUT_ENTRY_SIZE");
    if(env_p) {
      outputSize = atoi(env_p);
    }

    env_p = std::getenv("PREDICTION_SIZE");
    if (env_p) {
      predictionSize = atoi(env_p);
    }

    env_p = std::getenv("HISTORY_SIZE");
    if (env_p) {
      historySize = atoi(env_p);
    }

    env_p = std::getenv("THRESHOLD");
    if (env_p) {
      threshold = atof(env_p);
    }

    env_p = std::getenv("PERFO_STEP");
    if (env_p) {
      perfoStep = atoi(env_p);
      __approx_perfo_step__ = perfoStep;
    }

    env_p = std::getenv("PERFO_RATE");
    if (env_p) {
      perfoRate = atof(env_p);
      __approx_perfo_rate__ = perfoRate;
    }

 // This is not the optimal way. Since, we will 
 // always use the same random numbers.
    int numThreads = 32; //omp_get_max_threads();
    randomNumbers = new float[RAND_SIZE*numThreads];
    static std::default_random_engine generator;
    static std::uniform_real_distribution<float> distribution(0.0f, 1.0f);

    for (int i = 0 ; i < RAND_SIZE*numThreads; i++){
     randomNumbers[i] = distribution(generator);
    }


    #ifdef APPROX_DEV_STATS
    int nThreads = numBlocks*threadsPerBlock;
    printf("WARNING: HPAC will collect per-thread approximation statistics for %d threads, affecting runtime. For performance benchmarking, disable this feature\n", numThreads);
    resetDeviceTable(threshold, threadsPerBlock, numBlocks, inputSize, outputSize, tableSize, numThreads);
    #else
    resetDeviceTable(threshold, threadsPerBlock, numBlocks, inputSize, outputSize, tableSize, numBlocks*threadsPerBlock);
    #endif  //APPROX_DEV_STATS

  }

  ~ApproxRuntimeConfiguration(){
    delete [] randomNumbers;
  }

   ExecuteMode getMode(){return Mode;}

  bool getExecuteBoth(){ return ExecuteBoth; }

};


ApproxRuntimeConfiguration RTEnv;

int getPredictionSize() { return RTEnv.predictionSize;}
int getHistorySize() { return RTEnv.historySize; }
int getTableSize() { return RTEnv.tableSize; }
float getThreshold(){ return RTEnv.threshold;}
void setHostThreshold(float newThreshold){
  RTEnv.threshold = newThreshold;
}


bool __approx_skip_iteration(unsigned int i, float pr) {
    static thread_local int index = 0;
    static thread_local int threadId = -1;
    if ( threadId == -1 ){
        threadId = 0;
        if (omp_in_parallel()){
            threadId = omp_get_thread_num();
        }
    }

    if (RTEnv.randomNumbers[threadId*RAND_SIZE + index++] <= pr) {
        return true;
    }
    index = (index+1)%RAND_SIZE;
    return false;
}

void __approx_exec_call(void (*accurateFN)(void *), void (*perfoFN)(void *),
                        void *arg, bool cond, const char *region_name,
                        void *perfoArgs, int memo_type, void *inputs,
                        int num_inputs, void *outputs, int num_outputs) {
  approx_perfo_info_t *perfo = (approx_perfo_info_t *)perfoArgs;
  approx_var_info_t *input_vars = (approx_var_info_t *)inputs;
  approx_var_info_t *output_vars = (approx_var_info_t *)outputs;

  if ( perfoFN ){
      perforate(accurateFN, perfoFN, arg, input_vars, num_inputs, output_vars, num_outputs, RTEnv.getExecuteBoth());
  } else if (memo_type == MEMO_IN) {
    memoize_in(accurateFN, arg, input_vars, num_inputs, output_vars,
               num_outputs, RTEnv.getExecuteBoth(), RTEnv.tableSize, RTEnv.threshold );
  } else if (memo_type == MEMO_OUT) {
    memoize_out(accurateFN, arg, output_vars, num_outputs);
  } else {
    accurateFN(arg);
  }
}

const float approx_rt_get_percentage(){
  return RTEnv.perfoRate;
}

const int approx_rt_get_step(){
  return RTEnv.perfoStep;
}

#pragma omp begin declare target device_type(nohost)

unsigned int get_table_mask()
{

  int tid_in_block = omp_get_thread_num();
  int tid_in_warp = tid_in_block % NTHREADS_PER_WARP;
  int warp_in_block = tid_in_block / NTHREADS_PER_WARP;
  int table_in_warp = tid_in_warp / (NTHREADS_PER_WARP/TABLES_PER_WARP);
  int table_number = warp_in_block * TABLES_PER_WARP + table_in_warp;
  unsigned int threads_per_table = (NTHREADS_PER_WARP/TABLES_PER_WARP);
  unsigned int thread_in_table = tid_in_warp % threads_per_table;

  if(threads_per_table == NTHREADS_PER_WARP)
    return FULL_MASK;
  return ((1 << threads_per_table) - 1) << (threads_per_table*table_in_warp);
}

// return the global thread number of the thread in my table
// that has the maximum distance to other values seen
unsigned int tnum_in_table_with_max_dist(float max_dist)
{

  if(NTHREADS_PER_WARP == TABLES_PER_WARP)
    {
      return omp_get_num_threads() * omp_get_team_num() + omp_get_thread_num();
    }

  int tid_in_block = omp_get_thread_num();
  int tid_in_warp = tid_in_block % NTHREADS_PER_WARP;
  int warp_in_block = tid_in_block / NTHREADS_PER_WARP;
  int table_in_warp = tid_in_warp / (NTHREADS_PER_WARP/TABLES_PER_WARP);
  int table_number = warp_in_block * TABLES_PER_WARP + table_in_warp;
  unsigned int threads_per_table = (NTHREADS_PER_WARP/TABLES_PER_WARP);
  unsigned int thread_in_table = tid_in_warp % threads_per_table;

  unsigned int my_mask = 0;
  unsigned int firstThreadWithMax = 0;

  if(threads_per_table == NTHREADS_PER_WARP)
    my_mask = FULL_MASK;
  else
      my_mask = ((1 << threads_per_table) - 1) << (threads_per_table*table_in_warp);

  unsigned int shift = threads_per_table * table_in_warp;
  if(threads_per_table == NTHREADS_PER_WARP)
    shift = 0;
  float max_dist_warp = reduceMaxImpl(my_mask, max_dist, shift);
  unsigned int hasMax = warpBallot(my_mask, max_dist == max_dist_warp) >> shift;
  //-1 because ffs(0) = 0, ffs(1) = 1. hasMax should never be zero
  firstThreadWithMax = ffs(hasMax) - 1;

  return (firstThreadWithMax + table_number * threads_per_table) + (omp_get_num_threads() * omp_get_team_num());
}

void __approx_device_memo(void (*accurateFN)(void *), void *arg, int memo_type, void *in_data, int nInputs, void *out_data, int nOutputs)
{
  approx_var_info_t *in_vars = (approx_var_info_t*) in_data;
  approx_var_info_t *out_vars = (approx_var_info_t*) out_data;
  int tid_global = omp_get_thread_num() + omp_get_team_num() * omp_get_num_threads();
  real_t n_input_values = 0.0;
  int entry_index = -1;
  size_t i_tab_offset = 0;
  int n_output_values = 0;

  // FIXME: assume inputs are the same size
  for(int i = 0; i < nInputs; i++)
    {
      i_tab_offset += in_vars[i].num_elem;
    }

  for(int i = 0; i < nOutputs; i++)
    {
      n_output_values += out_vars[i].num_elem;
    }

  n_input_values = i_tab_offset;

  int tables_per_block = (omp_get_num_threads()/NTHREADS_PER_WARP) * TABLES_PER_WARP;
  int s_tab_size = ((int)(n_input_values)*tables_per_block*(*RTEnvd.tabNumEntries));
  int gmem_start = s_tab_size * omp_get_team_num();

  real_t ipt_table[SM_SZ_IN_BYTES/4];
  #pragma omp allocate(ipt_table) allocator(omp_pteam_mem_alloc)
  int n_sm_vals = SM_SZ_IN_BYTES/4;

  MemoTable<real_t, SM_SZ_IN_BYTES, NTHREADS_PER_WARP, TABLES_PER_WARP> _ipt_table{*RTEnvd.tabNumEntries, ipt_table,
                                                                                   n_input_values, RTEnvd.inputIdx, RTEnvd.ClockIndexes,
                                                                                   RTEnvd.ReplacementData
  };

  syncThreadsAligned();
  _ipt_table.copy_from(RTEnvd.iTable + gmem_start);
  syncThreadsAligned();

  int offset = 0;
  real_t dist_total = 0;
  real_t my_max_dist = 0.0f;

  for(int k = 0; k < _ipt_table.getSize(); k++)
    {
      dist_total = 0;
      offset = 0;
      for(int j = 0; j < nInputs; j++)
        {
          dist_total += _ipt_table.calc_distance(in_vars[j], k, offset);
          offset += in_vars[j].num_elem;
        }

      dist_total /= n_input_values;
      my_max_dist = max(my_max_dist, dist_total);
      // TODO: is divergence an issue?
      if(dist_total < *RTEnvd.threshold)
        {
          entry_index = k;
          break;
        }
    }

  // every thread needs to participate because we use butterfly reduction
  // that requires power of 2 participation
  bool have_max_dist = tnum_in_table_with_max_dist(my_max_dist) == tid_global;

  // table management
  {
  // one thread will register an access with the ReplacementData table
  // we have to synchronize afterward to remove race condition
  // between updating ReplacementData and adding any new entry
  if(entry_index != -1 && dist_total < *RTEnvd.threshold)
    {
      _ipt_table.registerAccess(entry_index);
    }

    unsigned int my_mask = get_table_mask();
    syncWarp(my_mask);

  }

  if(entry_index != -1 && dist_total < *RTEnvd.threshold)
    {
      offset = 0;
      for(int j = 0; j < nOutputs; j++)
        {
          for(int i = 0; i < out_vars[j].num_elem; i++)
            {
              int tid_in_block = omp_get_thread_num();
              int tid_in_warp = tid_in_block % NTHREADS_PER_WARP;
              int warp_in_block = tid_in_block / NTHREADS_PER_WARP;
              int table_in_warp = tid_in_warp / (NTHREADS_PER_WARP/TABLES_PER_WARP);
              int table_number = warp_in_block * TABLES_PER_WARP + table_in_warp;


              int row_number = entry_index * n_output_values + offset + i;
              int access_idx = (row_number * tables_per_block) + table_number;
              int o_tab_size = (n_output_values*tables_per_block*(*RTEnvd.tabNumEntries));
              int gmem_start_o = o_tab_size * omp_get_team_num();

              convertFromSingleWithOffset(out_vars[j].ptr,
                                          RTEnvd.oTable,
                                          i, access_idx+gmem_start_o,
                                          (ApproxType) out_vars[j].data_type
                                          );
            }
          offset += out_vars[j].num_elem;
        }

      #ifdef APPROX_DEV_STATS
      RTEnvd.approx_count[tid_global] += 1;
      #endif // APPROX_DEV_STATS
    }
  else
    {

      // NOTE: for correctness of inout, we have to copy the input before calling accurateFN
      if(have_max_dist)
        {
          _ipt_table.add_entry(in_vars, nInputs);
        }

      accurateFN(arg);

      #ifdef APPROX_DEV_STATS
      RTEnvd.accurate_count[tid_global] += 1;
      #endif // APPROX_DEV_STATS

      offset = 0;
      // I certainly wrote here above, we just use entry index as the row number
      // We want to subtract 1 to get entry index before it was incremented above,
      // ensuring it is always above 0
      // TODO: this should be size_t

      if(have_max_dist)
        {
      for(int j = 0; j < nOutputs; j++)
        {
          for(size_t i = 0; i < out_vars[j].num_elem; i++)
            {
              int tid_in_block = omp_get_thread_num();
              int tid_in_warp = tid_in_block % NTHREADS_PER_WARP;
              int warp_in_block = tid_in_block / NTHREADS_PER_WARP;
              int table_in_warp = tid_in_warp / (NTHREADS_PER_WARP/TABLES_PER_WARP);
              int table_number = warp_in_block * TABLES_PER_WARP + table_in_warp;

              entry_index = max(RTEnvd.inputIdx[tables_per_block * omp_get_team_num() + table_number]-1, 0);
              int row_number = entry_index * n_output_values + offset + i;
              int access_idx = (row_number * tables_per_block) + table_number;


              int o_tab_size = (n_output_values*tables_per_block*(*RTEnvd.tabNumEntries));
              int gmem_start_o = o_tab_size * omp_get_team_num();
              convertToSingleWithOffset(RTEnvd.oTable, out_vars[j].ptr, access_idx+gmem_start_o, i,
                                        (ApproxType) out_vars[j].data_type);

            }
          offset += out_vars[j].num_elem;
        }
      }
    }

  // race condition between writer thread that has max dist and other threads
  syncThreadsAligned();
  _ipt_table.copy_to(RTEnvd.iTable+gmem_start);
}
#pragma omp end declare target

