//===--t approx_runtime.cpp - driver of approximate runtime system----------------------===//
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
#include <cassert>

#include "approx.h"
#include "approx_debug.h"
#include "approx_data_util.h"
#include "approx_internal.h"
#include "approx_device_memo_table.h"
#include "device_intrinsics.h"

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
  TableReplacementPolicy RP;
};

#pragma omp declare target
#define ACCURATE 0
#define APPROX 1

class ApproxRuntimeDevDataOutputEnv
{
public:
  char *states = nullptr;
  char *predicted_values = nullptr;
  int *pSize = nullptr;
  real_t *oTable = nullptr;
  real_t *threshold = nullptr;
  int *history_size = nullptr;
  int *window_size = nullptr;
  char *active_values = nullptr;
  char *cur_index = nullptr;
#ifdef APPROX_DEV_STATS
  int *accurate_count = nullptr;
  int *approx_count = nullptr;
  int *nthreads = nullptr;
#endif //APPROX_DEV_STATS


#pragma omp end declare target

  void resetTable(float _threshold, int _total_output_size, int _pSize, int _history_size, int _num_blocks, int _nthreads)
  {
    destruct();
    int total_nthreads = _num_blocks * _nthreads;
    states = new char[total_nthreads];
    predicted_values = new char[total_nthreads];
    pSize = new int[1];
    oTable = new real_t[total_nthreads * _total_output_size * _history_size];
    threshold = new real_t[1];
    history_size = new int[1];
    window_size = new int[1];
    active_values = new char[total_nthreads];
    cur_index = new char[total_nthreads];

    pSize[0] = _pSize;
    history_size[0] = _history_size;
    threshold[0] = _threshold;

    std::fill(states, states+total_nthreads, 0);
    std::fill(predicted_values, predicted_values+total_nthreads, 0);
    std::fill(oTable, oTable+(total_nthreads * _total_output_size * _history_size), 0);
    std::fill(active_values, active_values+total_nthreads, 0);
    std::fill(cur_index, cur_index+total_nthreads, 0);


    #ifdef APPROX_DEV_STATS
    accurate_count = new int[total_nthreads];
    approx_count = new int[total_nthreads];
    nthreads = new int[1];
    *nthreads = total_nthreads;
    std::fill(accurate_count, accurate_count + total_nthreads, 0);
    std::fill(approx_count, approx_count + total_nthreads, 0);
    #endif

  }

  void destruct()
  {
    delete[] states;
    delete[] predicted_values;
    delete[] pSize;
    delete[] oTable;
    delete[] threshold;
    delete[] history_size;
    delete[] window_size;
    delete[] active_values;
    delete[] cur_index;
    #ifdef APPROX_DEV_STATS
    delete[] accurate_count;
    delete[] approx_count;
    delete[] nthreads;
    #endif
  }

#pragma omp begin declare target
};


class ApproxRuntimeDevDataEnv{

public:
  char *inputIdx = nullptr;
  unsigned char *ReplacementData = nullptr;
  int *ClockIndexes = nullptr;
  int *tabNumEntries = nullptr;
  float *threshold = nullptr;
  real_t *iTable = nullptr;
  real_t *oTable = nullptr;
  TableReplacementPolicy *TableRP = nullptr;
#ifdef APPROX_DEV_STATS
  int *accurate_count = nullptr;
  int *approx_count = nullptr;
  int *nthreads = nullptr;
#endif //APPROX_DEV_STATS

  ApproxRuntimeDevDataEnv() = default;

#pragma omp end declare target

  void resetTable(float _threshold, int total_input_size, int total_output_size, int total_num_tables, int replacement_data_size, int num_blocks, int _numTableEntries, int _nthreads, ApproxRuntimeDevMetadata& RTMeta, TableReplacementPolicy RP){
    // we want to have at least one 8-bit integer per table
    destruct();
    RTMeta.total_input_size = total_input_size;
    RTMeta.total_output_size = total_output_size;
    RTMeta.total_num_tables = total_num_tables;
    RTMeta.replacement_data_size = replacement_data_size;
    RTMeta.num_blocks = num_blocks;
    RTMeta.num_tab_entries = _numTableEntries;
    RTMeta.num_threads = _nthreads;
    RTMeta.RP = RP;
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
    TableRP = new TableReplacementPolicy[1];
    *TableRP = RP;

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
    delete[] TableRP;
    #ifdef APPROX_DEV_STATS
    delete[] nthreads;
    delete[] accurate_count;
    delete[] approx_count;
    #endif // APPROX_DEV_STATS
  }
#pragma omp declare target
};

ApproxRuntimeDevDataEnv RTEnvd = ApproxRuntimeDevDataEnv();
ApproxRuntimeDevDataOutputEnv RTEnvdOpt = ApproxRuntimeDevDataOutputEnv();
ApproxRuntimeDevMetadata RTMeta = ApproxRuntimeDevMetadata();
static bool output_mapped = false;
#pragma omp end declare target

void resetDeviceTable(float thresh, int threads_per_block, int num_blocks, int num_input_items_per_entry, int num_output_items_per_entry, int num_tab_entries, int num_threads, TableReplacementPolicy RP){
  float threshold = thresh == -1.0 ? *RTEnvd.threshold : thresh;
  int numTabEntries = num_tab_entries == -1 ? *RTEnvd.tabNumEntries : num_tab_entries;
  RP = RP == DEFAULT ? RTMeta.RP : RP;
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
#pragma omp target exit data map(delete:RTEnvd, RTEnvd.threshold[0:1], RTEnvd.tabNumEntries[0:1], RTEnvd.inputIdx[0:RTMeta.total_num_tables], RTEnvd.ReplacementData[0:RTMeta.replacement_data_size], RTEnvd.ClockIndexes[0:RTMeta.total_num_tables], RTEnvd.iTable[0:RTMeta.total_input_size], RTEnvd.oTable[0:RTMeta.total_output_size], RTEnvd.accurate_count[0:RTMeta.num_threads], RTEnvd.approx_count[0:RTMeta.num_threads], RTEnvd.TableRP[0:1])
      #else
#pragma omp target exit data map(delete:RTEnvd, RTEnvd.threshold[0:1], RTEnvd.tabNumEntries[0:1], RTEnvd.inputIdx[0:RTMeta.total_num_tables], RTEnvd.ReplacementData[0:RTMeta.replacement_data_size], RTEnvd.ClockIndexes[0:RTMeta.total_num_tables], RTEnvd.iTable[0:RTMeta.total_input_size], RTEnvd.oTable[0:RTMeta.total_output_size], RTEnvd.TableRP[0:1])
      #endif //APPROX_DEV_STATS
    }
  int total_num_tables = num_blocks * ((threads_per_block/NTHREADS_PER_WARP)*TABLES_PER_WARP);
  int total_num_table_entries = total_num_tables * num_tab_entries;
  int total_input_size = total_num_table_entries * num_input_items_per_entry;
  int total_output_size = total_num_table_entries * num_output_items_per_entry;
  int replacement_data_size = total_num_tables * (1+std::max(1, num_tab_entries/8));
  int clock_data_size = total_num_tables;
  RTEnvd.resetTable(threshold, total_input_size, total_output_size, total_num_tables, replacement_data_size, num_blocks, num_tab_entries, num_threads, RTMeta, RP);
  #ifdef APPROX_DEV_STATS
#pragma omp target enter data map(to:RTEnvd, RTEnvd.threshold[0:1], RTEnvd.tabNumEntries[0:1], RTEnvd.inputIdx[0:total_num_tables], RTEnvd.ReplacementData[0:replacement_data_size], RTEnvd.ClockIndexes[0:clock_data_size], RTEnvd.iTable[0:total_input_size], RTEnvd.oTable[0:total_output_size], RTEnvd.accurate_count[0:num_threads], RTEnvd.approx_count[0:num_threads], RTEnvd.TableRP[0:1])
  #else
#pragma omp target enter data map(to:RTEnvd, RTEnvd.threshold[0:1], RTEnvd.tabNumEntries[0:1], RTEnvd.inputIdx[0:total_num_tables], RTEnvd.ReplacementData[0:replacement_data_size], RTEnvd.ClockIndexes[0:clock_data_size], RTEnvd.iTable[0:total_input_size], RTEnvd.oTable[0:total_output_size], RTEnvd.TableRP[0:1])
  #endif // APPROX_DEV_STATS

#pragma omp target update from(RTEnvd.threshold[0:1])
}

void resetDeviceOutputTable(float thresh, int num_output_items_per_entry, int pSize, int history_size, int window_size, int num_blocks, int num_threads)
{
  int nthreads = num_threads * num_blocks;
  int gtab_size = nthreads * num_output_items_per_entry * history_size;

  if(output_mapped)
    {
      printf("deleting the output table\n");
      // TODO: nthreads can be different here as well
      int del_gtab_size = nthreads * num_output_items_per_entry * *RTEnvdOpt.history_size;
#pragma omp target exit data map(delete:RTEnvdOpt, RTEnvdOpt.states[0:nthreads], RTEnvdOpt.predicted_values[0:nthreads], RTEnvdOpt.pSize[0:1], RTEnvdOpt.oTable[0:del_gtab_size], RTEnvdOpt.threshold[0:1], RTEnvdOpt.history_size[0:1], RTEnvdOpt.window_size[0:1], RTEnvdOpt.active_values[0:nthreads], RTEnvdOpt.cur_index[0:nthreads])
    }

  RTEnvdOpt.resetTable(thresh, num_output_items_per_entry, pSize, history_size, num_blocks, num_threads);
  #ifdef APPROX_DEV_STATS
  #pragma omp target enter data map(to:RTEnvdOpt, RTEnvdOpt.states[0:nthreads], RTEnvdOpt.predicted_values[0:nthreads], RTEnvdOpt.pSize[0:1], RTEnvdOpt.oTable[0:gtab_size], RTEnvdOpt.threshold[0:1], RTEnvdOpt.history_size[0:1], RTEnvdOpt.window_size[0:1], RTEnvdOpt.active_values[0:nthreads], RTEnvdOpt.cur_index[0:nthreads], RTEnvdOpt.accurate_count[0:nthreads], RTEnvdOpt.approx_count[0:nthreads], RTEnvdOpt.nthreads[0:1])
  #else
#pragma omp target enter data map(to:RTEnvdOpt, RTEnvdOpt.states[0:nthreads], RTEnvdOpt.predicted_values[0:nthreads], RTEnvdOpt.pSize[0:1], RTEnvdOpt.oTable[0:gtab_size], RTEnvdOpt.threshold[0:1], RTEnvdOpt.history_size[0:1], RTEnvdOpt.window_size[0:1], RTEnvdOpt.active_values[0:nthreads], RTEnvdOpt.cur_index[0:nthreads])

  #endif
  output_mapped = true;
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

void writeDeviceThreadStatistics(std::ostream& file, bool iact){
  #ifdef APPROX_DEV_STATS
  file << "THREAD,ACCURATE,APPROX,RATIO\n";
  if(iact)
    {
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
    }
  else
    {
  int nthreads = *RTEnvdOpt.nthreads;
#pragma omp target update from(RTEnvdOpt.accurate_count[0:nthreads], RTEnvdOpt.approx_count[0:nthreads])
  for(int tnum = 0; tnum < nthreads; tnum++)
    {
      int accur = RTEnvdOpt.accurate_count[tnum];
      int appro = RTEnvdOpt.approx_count[tnum];
      file << tnum << ","
           << accur << ","
           << appro << ","
           << (float) appro / (accur + appro)
           << "\n";
    }
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
  int historySize = -1;
  int predictionSize = -1;
  int perfoStep;
  int windowSize = 0;
  float perfoRate;
  float *randomNumbers;
  TableReplacementPolicy RP = CLOCK;
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

    env_p = std::getenv("WINDOW_SIZE");
    if(env_p) {
      windowSize = atoi(env_p);
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

    env_p = std::getenv("REPLACEMENT_CLOCK");
    if (env_p) {
      RP = CLOCK;
    }

    env_p = std::getenv("REPLACEMENT_ROUND_ROBIN");
    if (env_p) {
      RP = ROUND_ROBIN;
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
    printf("WARNING: HPAC will collect per-thread approximation statistics for %d threads, affecting runtime. For performance benchmarking, disable this feature\n", numBlocks*threadsPerBlock);
    #endif  //APPROX_DEV_STATS
    if(predictionSize == -1)
      {
      resetDeviceTable(threshold, threadsPerBlock, numBlocks, inputSize, outputSize, tableSize, numBlocks*threadsPerBlock, RP);
      }
    else
      {
        resetDeviceOutputTable(threshold, outputSize, predictionSize, historySize, windowSize, numBlocks, threadsPerBlock);
      }

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

  unsigned getMask(unsigned l, unsigned r)
  {
    return (((1 << (l - 1)) - 1) ^
            ((1 << (r)) - 1));
  }

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

#ifdef TAF_INTER
__attribute__((always_inline))
void __approx_device_memo_out(void (*accurateFN)(void *), void *arg, const void *region_info_out, const void *opt_access, void **outputs, const int nOutputs, const char init_done)
{
  const approx_region_specification *out_reg = (const approx_region_specification*) region_info_out;
  approx_var_access_t *opts = (approx_var_access_t*) opt_access;
  int tid_global = omp_get_thread_num() + omp_get_team_num() * omp_get_num_threads();
  int entry_index = -1;
  size_t i_tab_offset = 0;
  int n_output_values = 0;
  int offset = 0;
  // we're going to use inputIdx for the state machine and output table for the outputs

#pragma clang loop unroll(full)
  for(int i = 0; i < nOutputs; i++)
    {
      n_output_values += opts[i].num_elem;
    }

  char states [32];
  char cur_index [32];
  char active_values[32];
  real_t output_table[32*8];
  #pragma omp allocate(states) allocator(omp_pteam_mem_alloc)
  #pragma omp allocate(cur_index) allocator(omp_pteam_mem_alloc)
  #pragma omp allocate(active_values) allocator(omp_pteam_mem_alloc)
  #pragma omp allocate(output_table) allocator(omp_pteam_mem_alloc)

  int warpId = omp_get_thread_num() / NTHREADS_PER_WARP;
  int threadInWarp = omp_get_thread_num () % NTHREADS_PER_WARP;

  syncThreadsAligned();
  if(!init_done)
    {

      // TODO: bank conflicts?
      states[threadInWarp] = ACCURATE;
      cur_index[threadInWarp] = 0;
      active_values[threadInWarp] = 0;
      output_table[threadInWarp] = 0;
    }

  syncThreadsAligned();

  // NOTE: we will use predicted and active values as the same when the translation is done
  for(int h = 0; h < omp_get_num_threads() / NTHREADS_PER_WARP; h++)
    {
      if(warpId == h)
        {
          bool am_approx = states[threadInWarp] == APPROX;
          if(am_approx)
            {
              // we subtract one because 'cur_index' is the insertion point -- we want the last value inserted
              int k =cur_index[threadInWarp] - 1;
              if(k<0)
                k=(*RTEnvdOpt.history_size)-1;
              offset = 0;

              for(int j = 0; j < nOutputs; j++)
                {
                  for(int i = 0; i < opts[j].num_elem; i++)
                    {
                      // TODO: access index
                      size_t row_num = (k*n_output_values) + offset + i;
                      size_t col_num = threadInWarp;
                      size_t access_index = (row_num * NTHREADS_PER_WARP) + col_num;

                      convertFromSingleWithOffset(outputs[j], output_table, i, access_index,
                                                  (ApproxType) out_reg[j].data_type);

                    }
                  offset += opts[j].num_elem;
                }


              if (--active_values[threadInWarp] == 0) {
                states[threadInWarp] = ACCURATE;
              }

              // copy output
              // decrement counter
              // update state?
            }
          else
            {
              accurateFN(arg);

              int k = cur_index[threadInWarp];
              offset = 0;
              for(int j = 0; j < nOutputs; j++)
                {
                  for(int i = 0; i < opts[j].num_elem; i++)
                    {
                      size_t row_num = (k*n_output_values) + offset + i;
                      size_t col_num = threadInWarp;
                      size_t access_index = (row_num * NTHREADS_PER_WARP) + col_num;

                      convertToSingleWithOffset(output_table, outputs[j], access_index, i,
                                                (ApproxType) out_reg[j].data_type);

                    }
                  offset += opts[j].num_elem;
                }
              cur_index[threadInWarp] = (k+1) % (*RTEnvdOpt.history_size);
              active_values[threadInWarp]++;
            }

          if(states[threadInWarp] == ACCURATE && active_values[threadInWarp] >= *RTEnvdOpt.history_size)
            {
              real_t variance = 0.0;
              real_t avg = 0.0;

              for(int k = 0; k < *RTEnvdOpt.history_size; k++)
                {
                  offset = 0;
                  for(int j = 0; j < nOutputs; j++)
                    {
                      for(int i = 0; i < opts[j].num_elem; i++)
                        {
                          size_t row_num = (k*n_output_values) + offset + i;
                          size_t col_num = threadInWarp;
                          size_t access_index = (row_num * NTHREADS_PER_WARP) + col_num;

                          avg += output_table[access_index];

                        }
                      offset += opts[j].num_elem;
                    }
                }

              // average = sum / total_size
              avg /= (real_t) (*RTEnvdOpt.history_size * n_output_values);

              // for each entry in the table
              // for each value in the entry
              // tmp = val - avg
              // variance += temp^2


              for(int k = 0; k < *RTEnvdOpt.history_size; k++)
                {
                  offset = 0;
                  for(int j = 0; j < nOutputs; j++)
                    {
                      for(int i = 0; i < opts[j].num_elem; i++)
                        {
                          size_t row_num = (k*n_output_values) + offset + i;
                          size_t col_num = threadInWarp;
                          size_t access_index = (row_num * NTHREADS_PER_WARP) + col_num;

                          real_t tmp = output_table[access_index] - avg;
                          variance += tmp*tmp;
                        }
                      offset += opts[j].num_elem;
                    }
                }

              // variance /= total_size
              variance /= (real_t)(*RTEnvdOpt.history_size * n_output_values);

              // stdev = sqrt(variance)
              real_t stdev = sqrt(variance);
              real_t rsd = stdev;
              if(avg != 0.0f){
                rsd = stdev / avg;
              }
              if(rsd < 0.0) rsd = -rsd;

              if(rsd < *RTEnvdOpt.threshold)
                {
                  // switch the machine state to approx
                  states[threadInWarp] = APPROX;
                  active_values[threadInWarp] = *RTEnvdOpt.pSize;
                }
            }
        }

      syncThreadsAligned();
    }
}
#else

__attribute__((always_inline))
void __approx_device_memo_out(void (*accurateFN)(void *), void *arg, const void *region_info_out, const void *opt_access, void **outputs, const int nOutputs, const char init_done)
{
  const approx_region_specification *out_reg = (const approx_region_specification*) region_info_out;
  constexpr int TAF_REGIONS_PER_WARP = NTHREADS_PER_WARP/TAF_THREAD_WIDTH;
  constexpr int MAX_HIST_SIZE = 5;
  approx_var_access_t *opts = (approx_var_access_t*) opt_access;
  int tid_global = omp_get_thread_num() + omp_get_team_num() * omp_get_num_threads();
  int entry_index = -1;
  size_t i_tab_offset = 0;
  int n_output_values = 0;
  int offset = 0;
  // we're going to use inputIdx for the state machine and output table for the outputs

#pragma clang loop unroll(full)
  for(int i = 0; i < nOutputs; i++)
    {
      n_output_values += opts[i].num_elem;
    }

  // TODO: this '8' should be the number of warps per block
  char _states [8*TAF_REGIONS_PER_WARP];
  int _cur_index [8*TAF_REGIONS_PER_WARP];
  int _active_values[8*TAF_REGIONS_PER_WARP];
  // TODO: Assumes <= 256 threads per block, maximum history size of 5
  // This is constant across different TAF widths
  real_t _output_table[8*NTHREADS_PER_WARP*MAX_HIST_SIZE];
  #pragma omp allocate(_states) allocator(omp_pteam_mem_alloc)
  #pragma omp allocate(_cur_index) allocator(omp_pteam_mem_alloc)
  #pragma omp allocate(_active_values) allocator(omp_pteam_mem_alloc)
  #pragma omp allocate(_output_table) allocator(omp_pteam_mem_alloc)

  const int warpId = omp_get_thread_num() / NTHREADS_PER_WARP;
  const int sublaneInWarp = (omp_get_thread_num () % NTHREADS_PER_WARP) / TAF_THREAD_WIDTH;
  const int threadInSublane = omp_get_thread_num() % TAF_THREAD_WIDTH;

  int sm_offset = warpId * (TAF_REGIONS_PER_WARP);
  char *states = _states + sm_offset;
  int *cur_index = _cur_index + sm_offset;
  int *active_values = _active_values + sm_offset;
  // TODO: again a problem if history size is not width
  sm_offset = warpId * MAX_HIST_SIZE*NTHREADS_PER_WARP;
  real_t *output_table = _output_table + (sm_offset) + MAX_HIST_SIZE*sublaneInWarp*TAF_THREAD_WIDTH;
  int my_table_start = sm_offset + MAX_HIST_SIZE*sublaneInWarp*TAF_THREAD_WIDTH;

  const unsigned int myMask = TAF_THREAD_WIDTH == NTHREADS_PER_WARP ? FULL_MASK : getMask(1, TAF_THREAD_WIDTH) << (TAF_THREAD_WIDTH*sublaneInWarp);

  int globalSublaneID = (omp_get_team_num() * omp_get_num_threads() / TAF_THREAD_WIDTH) + (omp_get_thread_num() / TAF_THREAD_WIDTH);
  if(!init_done)
    {
      if(threadInSublane == 0)
        {
          states[sublaneInWarp] = ACCURATE;
          cur_index[sublaneInWarp] = 0;
          active_values[sublaneInWarp] = 0;
          output_table[sublaneInWarp] = 0;
        }
    }

  syncWarp(FULL_MASK);

  // NOTE: we will use predicted and active values as the same when the translation is done
  bool am_approx = states[sublaneInWarp] == APPROX;
  if(am_approx)
    {
      #ifdef APPROX_DEV_STATS
      RTEnvdOpt.approx_count[tid_global]++;
      #endif

      // we subtract one because 'cur_index' is the insertion point -- we want the last value inserted
      int k =cur_index[sublaneInWarp] - 1;
      if(k<0)
        k=(TAF_THREAD_WIDTH**RTEnvdOpt.history_size)-1;
      offset = 0;

      for(int j = 0; j < nOutputs; j++)
        {
          for(int i = 0; i < opts[j].num_elem; i++)
            {
              size_t access_index = k+offset+i;
              convertFromSingleWithOffset(outputs[j], output_table, i, access_index,
                                          (ApproxType) out_reg[j].data_type);

            }
          offset += opts[j].num_elem;
        }


      syncWarp(myMask);
      if(threadInSublane == 0)
        {
          active_values[sublaneInWarp]--;
          if (active_values[sublaneInWarp] == 0) {
            states[sublaneInWarp] = ACCURATE;
            cur_index[sublaneInWarp] = 0;
          }
        }
    }
  else
    {
      accurateFN(arg);
      #ifdef APPROX_DEV_STATS
      RTEnvdOpt.accurate_count[tid_global]++;
      #endif
      int k = cur_index[sublaneInWarp] + threadInSublane;
      offset = 0;
      for(int j = 0; j < nOutputs; j++)
        {
          for(int i = 0; i < opts[j].num_elem; i++)
            {
              size_t access_index = k + offset + i;
              convertToSingleWithOffset(output_table, outputs[j], access_index, i,
                                        (ApproxType) out_reg[j].data_type);

            }
          offset += opts[j].num_elem;
        }

      syncWarp(myMask);
      if(threadInSublane == 0)
        {
          cur_index[sublaneInWarp] = (cur_index[sublaneInWarp]+TAF_THREAD_WIDTH) % ((*RTEnvdOpt.history_size)* TAF_THREAD_WIDTH);
          active_values[sublaneInWarp] += 1;
        }
    }

  syncWarp(myMask);

  if(states[sublaneInWarp] == ACCURATE && active_values[sublaneInWarp] >= *RTEnvdOpt.history_size)
    {
      real_t variance = 0.0;
      real_t avg = 0.0;

      for(int k = threadInSublane; k < (*RTEnvdOpt.history_size*TAF_THREAD_WIDTH); k += TAF_THREAD_WIDTH)
        {
          offset = 0;
          for(int j = 0; j < nOutputs; j++)
            {
              for(int i = 0; i < opts[j].num_elem; i++)
                {
                  size_t access_index = k + offset + i;

                  avg += output_table[access_index];

                }
              offset += opts[j].num_elem;
            }
        }
      avg = reduceSumImpl(myMask, avg);

      // average = sum / total_size
      avg /= (real_t) (*RTEnvdOpt.history_size * n_output_values * TAF_THREAD_WIDTH);

      // for each entry in the table
      // for each value in the entry
      // tmp = val - avg
      // variance += temp^2


      for(int k = threadInSublane; k < (*RTEnvdOpt.history_size*TAF_THREAD_WIDTH); k += TAF_THREAD_WIDTH)
        {
          offset = 0;
          for(int j = 0; j < nOutputs; j++)
            {
              for(int i = 0; i < opts[j].num_elem; i++)
                {
                  size_t access_index = k + offset + i;

                  real_t tmp = output_table[access_index] - avg;
                  variance += tmp*tmp;
                }
              offset += opts[j].num_elem;
            }
        }

      // variance /= total_size
      variance = reduceSumImpl(myMask, variance);
      variance /= (real_t)(*RTEnvdOpt.history_size * n_output_values * TAF_THREAD_WIDTH);

      real_t stdev = sqrt(variance);
      real_t rsd = stdev;
      if(avg != 0.0f){
        rsd = stdev / avg;
      }
      if(rsd < 0.0) rsd = -rsd;

      // No need to sync here: warp reductions sync threads identified by the mask
      if(threadInSublane == 1 && rsd < *RTEnvdOpt.threshold)
        {
          // switch the machine state to approx
          states[sublaneInWarp] = APPROX;
          active_values[sublaneInWarp] = *RTEnvdOpt.pSize;
        }
      syncWarp(myMask);
    }
}

#endif //TAF_INTER

__attribute__((always_inline))
void __approx_device_memo_in(void (*accurateFN)(void *), void *arg, const void *region_info_in, const void *ipt_access, const void **inputs, const int nInputs, const void *region_info_out, const void *opt_access, void **outputs, const int nOutputs)
{
  const approx_region_specification *in_reg = (const approx_region_specification*) region_info_in;
  const approx_region_specification *out_reg = (const approx_region_specification*) region_info_out;
  const approx_var_access_t *ipts = (approx_var_access_t*) ipt_access;
  approx_var_access_t *opts = (approx_var_access_t*) opt_access;
  int tid_global = omp_get_thread_num() + omp_get_team_num() * omp_get_num_threads();
  real_t n_input_values = 0.0;
  int entry_index = -1;
  size_t i_tab_offset = 0;
  int n_output_values = 0;

  // FIXME: assume inputs are the same size
#pragma clang loop unroll(full)
  for(int i = 0; i < nInputs; i++)
    {
      i_tab_offset += ipts[i].num_elem;
    }

#pragma clang loop unroll(full)
  for(int i = 0; i < nOutputs; i++)
    {
      n_output_values += opts[i].num_elem;
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
                                                                                   RTEnvd.ReplacementData, RTEnvd.TableRP
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
#pragma clang loop unroll(full)
      for(int j = 0; j < nInputs; j++)
        {
          dist_total += _ipt_table.calc_distance(in_reg[j], ipts[j], inputs[j], ipts[j].num_elem, k, offset);
          offset += ipts[j].num_elem;
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
          for(int i = 0; i < opts[j].num_elem; i++)
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

              convertFromSingleWithOffset(outputs[j],
                                          RTEnvd.oTable,
                                          i, access_idx+gmem_start_o,
                                          (ApproxType) out_reg[j].data_type
                                          );
            }
          offset += opts[j].num_elem;
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
          _ipt_table.add_entry(in_reg, ipts, inputs, nInputs);
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
      #pragma clang loop unroll(full)
      for(int j = 0; j < nOutputs; j++)
        {
          for(size_t i = 0; i < opts[j].num_elem; i++)
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
              convertToSingleWithOffset(RTEnvd.oTable, outputs[j], access_idx+gmem_start_o, i,
                                        (ApproxType) out_reg[j].data_type);

            }
          offset += opts[j].num_elem;
        }
      }
    }

  // race condition between writer thread that has max dist and other threads
  syncThreadsAligned();
  _ipt_table.copy_to(RTEnvd.iTable+gmem_start);
}

__attribute__((always_inline))
void __approx_device_memo(void (*accurateFN)(void *), void *arg, int memo_type, const void *region_info_in, const void *ipt_access, const void **inputs, const int nInputs, const void *region_info_out, const void *opt_access, void **outputs, const int nOutputs, const char init_done)
{
  if(memo_type == MEMO_IN)
    {
      __approx_device_memo_in(accurateFN, arg, region_info_in, ipt_access, inputs, nInputs, region_info_out, opt_access, outputs, nOutputs);
    }
  else if(memo_type == MEMO_OUT)
    {
      __approx_device_memo_out(accurateFN, arg, region_info_out, opt_access, outputs, nOutputs, init_done);
    }
  else
    {
      printf("ERROR: Incorrect memo type");
    }
}

#pragma omp end declare target
