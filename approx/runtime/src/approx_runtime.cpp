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

    // TODO: this should go to cmake
#define NTHREADS_IN_WARP 32
#pragma omp begin declare target device_type(nohost)
void syncThreadsAligned(){};
unsigned int warpReduceMax(unsigned mask, unsigned int value) {return 0;}
unsigned int warpBallot(unsigned int mask, bool pred) { return 0;}
unsigned int ffs(unsigned int val) { return __builtin_ffs(val);}
unsigned int popc(unsigned int val) { return __builtin_popcount(val);}
void syncWarp(unsigned int mask) {}
unsigned int activeMask() {return 0;}
float reduceMaxImpl(unsigned int mask, float value, unsigned int shift) {return 0.0f;}
float warpShuffleXOR(unsigned int mask, float value, int lanemask){return 0.0f;}
#pragma omp end declare target
#pragma omp begin declare variant match(                                       \
    device = {arch(nvptx, nvptx64)}, implementation = {extension(match_any)})
void syncThreadsAligned() { __syncthreads(); }
unsigned int warpReduceMax(unsigned int mask, unsigned int value) { return __nvvm_redux_sync_umax(value, mask);}
unsigned int warpBallot(unsigned int mask, bool pred) { return __nvvm_vote_ballot_sync(mask, pred);}
void syncWarp(unsigned int mask) { return __nvvm_bar_warp_sync(mask);}
float warpShuffleXOR(unsigned int mask, float value, int lanemask){ return __nvvm_shfl_sync_bfly_f32(mask, value, lanemask, 0);}

unsigned int activeMask() {
  unsigned int Mask;
  asm("activemask.b32 %0;" : "=r"(Mask));
  return Mask;
}

// max reduction among threads named in the mask.
// the threads named in 'mask' must be a power of 2
float reduceMaxImpl(unsigned int mask, float value, unsigned int shift)
{
  unsigned int num_participants = popc(mask);
  for(int i = 1; i < num_participants; i *= 2)
    {
      value = fmax(warpShuffleXOR(mask, value, i<<shift), value);
    }
  return value;
}

#pragma omp end declare variant
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

#pragma omp declare target
class ApproxRuntimeDevDataEnv{

public:
  char *inputIdx = nullptr;
  size_t *iSize = nullptr;
  size_t *oSize = nullptr;
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
  void resetTable(float _threshold, size_t _iput_size, size_t _oput_size, int _numTableEntries, int _nthreads){
    destruct();
    threshold = new float[1];
    iSize = new size_t[1];
    oSize = new size_t[1];
    tabNumEntries = new int[1];
    tabNumEntries[0] = _numTableEntries;
    threshold[0] = _threshold;
    iSize[0] = _iput_size;
    oSize[0] = _oput_size;
    inputIdx = new char[_iput_size];
    iTable = new real_t[_iput_size*_numTableEntries];
    oTable = new real_t[_oput_size*_numTableEntries];
    #ifdef APPROX_DEV_STATS
    nthreads = new int[1];
    *nthreads = _nthreads;
    accurate_count = new int[*nthreads];
    approx_count = new int[*nthreads];

    std::fill(accurate_count, accurate_count+*nthreads, 0);
    std::fill(approx_count, approx_count+*nthreads, 0);
    std::fill(iTable, iTable + _iput_size*_numTableEntries, 0);
    #endif // APPROX_DEV_STATS


    std::fill(inputIdx, inputIdx+_iput_size, 0);
  }

  // allow explicit destruction.
  void destruct(){
    delete[] threshold;
    delete[] iSize;
    delete[] oSize;
    delete[] tabNumEntries;
    delete[] inputIdx;
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
#pragma omp end declare target

void resetDeviceTable(float newThresh, size_t newiSize, size_t newoSize, int newNumTabEntries, int nThreads){
  float threshold = newThresh == -1.0 ? *RTEnvd.threshold : newThresh;
  size_t iSize = newiSize == -1 ? *RTEnvd.iSize : newiSize;
  size_t oSize = newoSize == -1 ? *RTEnvd.oSize : newoSize;
  int numTabEntries = newNumTabEntries == -1 ? *RTEnvd.tabNumEntries : newNumTabEntries;
  if(nThreads == -1)
    {
  #ifdef APPROX_DEV_STATS
    nThreads = *RTEnvd.nthreads;
  #endif
    }

  #ifndef APPROX_DEV_STATS
  if(nThreads != -1)
    printf("WARNING: A number of threads has been give, but HPAC is not built to collect per-thread memoization statistics. "
           "The argument will be ignored\n");
  #endif

  if(omp_target_is_present(RTEnvd.iSize, 0))
    {
      size_t oldInSize = *RTEnvd.iSize;
      size_t oldOutSize = *RTEnvd.oSize;
      int oldNumEntries = *RTEnvd.tabNumEntries;

      #ifdef APPROX_DEV_STATS
      #pragma omp target exit data map(delete:RTEnvd, RTEnvd.threshold[0:1], RTEnvd.tabNumEntries[0:1], RTEnvd.iSize[0:1], RTEnvd.oSize[0:1], RTEnvd.inputIdx[0:oldInSize], RTEnvd.iTable[0:oldInSize*oldNumEntries], RTEnvd.oTable[0:oldOutSize*oldNumEntries], RTEnvd.accurate_count[0:nThreads], RTEnvd.approx_count[0:nThreads])
      #else
      #pragma omp target exit data map(delete:RTEnvd, RTEnvd.threshold[0:1], RTEnvd.tabNumEntries[0:1], RTEnvd.iSize[0:1], RTEnvd.oSize[0:1], RTEnvd.inputIdx[0:oldInSize], RTEnvd.iTable[0:oldInSize*oldNumEntries], RTEnvd.oTable[0:oldOutSize*oldNumEntries])
      #endif //APPROX_DEV_STATS
    }
  RTEnvd.resetTable(threshold, iSize, oSize, numTabEntries, nThreads);
  #ifdef APPROX_DEV_STATS
  #pragma omp target enter data map(to:RTEnvd, RTEnvd.threshold[0:1], RTEnvd.tabNumEntries[0:1], RTEnvd.iSize[0:1], RTEnvd.oSize[0:1], RTEnvd.inputIdx[0:iSize], RTEnvd.iTable[0:iSize*numTabEntries], RTEnvd.oTable[0:oSize*numTabEntries], RTEnvd.accurate_count[0:nThreads], RTEnvd.approx_count[0:nThreads])
  #else
  #pragma omp target enter data map(to:RTEnvd, RTEnvd.threshold[0:1], RTEnvd.tabNumEntries[0:1], RTEnvd.iSize[0:1], RTEnvd.oSize[0:1], RTEnvd.inputIdx[0:iSize], RTEnvd.iTable[0:iSize*numTabEntries], RTEnvd.oTable[0:oSize*numTabEntries])
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
  int offloadTableISize;
  int offloadTableOSize;
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
    }

    offloadTableISize = 0;
    env_p = std::getenv("OFFLOAD_TABLE_IN_SIZE");
    if (env_p){
      offloadTableISize = atoi(env_p);
    }

    offloadTableOSize = 0;
    env_p = std::getenv("OFFLOAD_TABLE_OUT_SIZE");
    if (env_p){
      offloadTableOSize = atoi(env_p);
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
    char *n_threads = std::getenv("NUM_THREADS");
    if(!n_threads)
      {
        printf("HPAC was built to gather per-thread approx statistics, but 'NUM_THREADS' not provided in the environment. Exiting...\n");
        std::abort();
      }
    printf("WARNING: HPAC will collect per-thread approximation statistics, affecting runtime. For performance benchmarking, disable this feature\n");
    int nThreads = std::atoi(n_threads);
    resetDeviceTable(threshold, offloadTableISize, offloadTableOSize, tableSize, nThreads);
    #else
    resetDeviceTable(threshold, offloadTableISize, offloadTableOSize, tableSize, 0);
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

  real_t ipt_table[SM_SZ_IN_BYTES/4];
  #pragma omp allocate(ipt_table) allocator(omp_pteam_mem_alloc)
  int n_sm_vals = SM_SZ_IN_BYTES/4;

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

  // copy the input table in a block-stride loop
  // this works fine as long as the input table is written correctly in the first place
  for(int i = omp_get_thread_num(); i < s_tab_size; i += omp_get_num_threads())
    {
      ipt_table[i] = RTEnvd.iTable[i + gmem_start];
    }

  syncThreadsAligned();

  int offset = 0;
  real_t dist_total = 0;
  real_t my_max_dist = 0.0f;

  {
    int tid_in_block = omp_get_thread_num();
    int tid_in_warp = tid_in_block % NTHREADS_PER_WARP;
    int warp_in_block = tid_in_block / NTHREADS_PER_WARP;
    int table_in_warp = tid_in_warp / (NTHREADS_PER_WARP/TABLES_PER_WARP);
    int table_number = warp_in_block * TABLES_PER_WARP + table_in_warp;

  for(int k = 0; k < RTEnvd.inputIdx[tables_per_block * omp_get_team_num() + table_number]; k++)
    {
      dist_total = 0;
      offset = 0;
      for(int j = 0; j < nInputs; j++)
        {
          for(int i = 0; i < in_vars[j].num_elem; i++)
            {
              int row_number = k * n_input_values + offset + i;
              int column_number = table_number;
              int access_idx = (row_number * tables_per_block) + table_number;
              real_t in_val_conv = 0;
              convertToSingleWithOffsetIptRead(&in_val_conv, in_vars[j].ptr, 0, i, (ApproxType) in_vars[j].data_type);
              real_t dist = fabs(ipt_table[access_idx] - in_val_conv);
              dist_total += dist;
            }

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
  }

  // every thread needs to participate because we use butterfly reduction
  // that requires power of 2 participation
  bool have_max_dist = tnum_in_table_with_max_dist(my_max_dist) == tid_global;

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
              int o_tab_size = (n_input_values*tables_per_block*(*RTEnvd.tabNumEntries));
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

      offset = 0;
      // NOTE: for correctness of inout, we have to copy the input before calling accurateFN

      if(have_max_dist)
        {
          int idx_offset = 0;
          for(int j = 0; j < nInputs; j++)
            {
              for(int i = 0; i < in_vars[j].num_elem; i++)
                {
                  int tid_in_block = omp_get_thread_num();
                  int tid_in_warp = tid_in_block % NTHREADS_PER_WARP;
                  int warp_in_block = tid_in_block / NTHREADS_PER_WARP;
                  int table_in_warp = tid_in_warp / (NTHREADS_PER_WARP/TABLES_PER_WARP);
                  int table_number = warp_in_block * TABLES_PER_WARP + table_in_warp;

                  int row_number = offset + i;
                  int column_number = table_number;
                  int access_idx = (row_number * tables_per_block) + table_number;

                  entry_index = RTEnvd.inputIdx[tables_per_block * omp_get_team_num() + table_number];

                  row_number = entry_index * n_input_values + offset + i;
                  access_idx = (row_number * tables_per_block) + table_number;

                  convertToSingleWithOffsetIptCopy(ipt_table, in_vars[j].ptr, access_idx, i,
                                            (ApproxType) in_vars[j].data_type);

                  idx_offset = tables_per_block * omp_get_team_num() + table_number;
                }
              offset += in_vars[j].num_elem;
            }

          RTEnvd.inputIdx[idx_offset] = min(*RTEnvd.tabNumEntries, RTEnvd.inputIdx[idx_offset]+1);
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
  for(int i = omp_get_thread_num(); i < s_tab_size; i += omp_get_num_threads())
    {
      RTEnvd.iTable[i+gmem_start] = ipt_table[i];
    }

}
#pragma omp end declare target

