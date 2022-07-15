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
  int *tableSize = nullptr;
  float *threshold = nullptr;
  real_t *iTable = nullptr;
  real_t *oTable = nullptr;
  ApproxRuntimeDevDataEnv() = default;

  void resetTable(int _tableSize, float _threshold, size_t _iput_size, size_t _oput_size){
    destruct();
    tableSize = new int[1];
    threshold = new float[1];
    iSize = new size_t[1];
    oSize = new size_t[1];
    tableSize[0] = _tableSize;
    threshold[0] = _threshold;
    iSize[0] = _iput_size;
    oSize[0] = _oput_size;
    inputIdx = new char[*tableSize];
    iTable = new real_t[*tableSize];
    oTable = new real_t[*tableSize];

    std::fill(inputIdx, inputIdx+*tableSize, 0);
  }

  // allow explicit destruction.
  void destruct(){
    delete[] tableSize;
    delete[] threshold;
    delete[] iSize;
    delete[] oSize;
    delete[] inputIdx;
    delete[] iTable;
    delete[] oTable;
  }
};

ApproxRuntimeDevDataEnv RTEnvd = ApproxRuntimeDevDataEnv();
#pragma omp end declare target

void resetDeviceTable(int newSize, float newThresh, size_t newiSize, size_t newoSize){
  int tabSize = newSize == -1 ? *RTEnvd.tableSize : newSize;
  float threshold = newThresh == -1.0 ? *RTEnvd.threshold : newThresh;
  size_t iSize = newiSize == -1 ? *RTEnvd.iSize : newiSize;
  size_t oSize = newoSize == -1 ? *RTEnvd.oSize : newoSize;

  if(omp_target_is_present(RTEnvd.tableSize, 0))
    {
      int oldTabSize = *RTEnvd.tableSize;
#pragma omp target exit data map(delete:RTEnvd, RTEnvd.tableSize[0:1], RTEnvd.threshold[0:1], RTEnvd.iSize[0:1], RTEnvd.oSize[0:1], RTEnvd.inputIdx[0:oldTabSize], RTEnvd.iTable[0:oldTabSize], RTEnvd.oTable[0:oldTabSize])
    }
  RTEnvd.resetTable(tabSize, threshold, iSize, oSize);
#pragma omp target enter data map(to:RTEnvd, RTEnvd.tableSize[0:1], RTEnvd.threshold[0:1], RTEnvd.iSize[0:1], RTEnvd.oSize[0:1], RTEnvd.inputIdx[0:tabSize], RTEnvd.iTable[0:tabSize], RTEnvd.oTable[0:tabSize])

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
  int offloadTableSize;
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

    offloadTableSize = 0;
    env_p = std::getenv("OFFLOAD_TABLE_SIZE");
    if (env_p){
      offloadTableSize = atoi(env_p);
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

    // TODO: Initial device params from the environment
    resetDeviceTable(offloadTableSize, threshold, 0, 0);
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

#pragma omp declare target

void __approx_device_memo(void (*accurateFN)(void *), void *arg, int memo_type, void *in_data, int nInputs, void *out_data, int nOutputs)
{
  approx_var_info_t *in_vars = (approx_var_info_t*) in_data;
  approx_var_info_t *out_vars = (approx_var_info_t*) out_data;
  int tid_global = omp_get_thread_num() + omp_get_team_num() * omp_get_num_threads();
  int offset = 0;
  real_t dist_total = 0;
  int entry_index = -1;
  size_t i_tab_offset = 0;
  size_t o_tab_offset = 0;
  for(int i = 0; i < nInputs; i++)
    {
      i_tab_offset += in_vars[i].num_elem;
    }

  for(int i = 0; i < nOutputs; i++)
    {
      o_tab_offset += out_vars[i].num_elem;
    }

  // assume every thread is preceded by the same number of inputs and outputs
  // this assumption is valid even if the last thread does fewer iterations of the loop
  i_tab_offset *= tid_global;
  o_tab_offset *= tid_global;

  // TODO: Because of temp. locality, may be better to loop down
  for(int k = 0; k < RTEnvd.inputIdx[tid_global]; k++)
    {
      dist_total = 0;
      offset = 0;
      for(int j = 0; j < nInputs; j++)
        {
          for(int i = 0; i < in_vars[j].num_elem; i++)
            {
              real_t tab_val = RTEnvd.iTable[(k*(*RTEnvd.iSize))+i+offset+i_tab_offset];
              real_t in_val = ((real_t*)in_vars[j].ptr)[i];
              real_t in_val_conv = 0;
              convertToSingleWithOffset(&in_val_conv, in_vars[j].ptr, 0, i, (ApproxType) in_vars[j].data_type);
              real_t dist = fabs(RTEnvd.iTable[(k*(*RTEnvd.iSize))+offset+i+i_tab_offset] - in_val_conv);
              dist_total += dist;
            }
          offset += in_vars[j].num_elem;
        }
      // TODO: is divergence an issue?
      if(dist_total < *RTEnvd.threshold)
        {
          entry_index = k;
          break;
        }
    }

  if(entry_index != -1 && dist_total < *RTEnvd.threshold)
    {
      offset = 0;
      for(int j = 0; j < nOutputs; j++)
        {
          for(int i = 0; i < out_vars[j].num_elem; i++)
            {
              convertFromSingleWithOffset(out_vars[j].ptr,
                                          RTEnvd.oTable,
                                          i, i+offset+o_tab_offset+(*RTEnvd.oSize*entry_index),
                                          (ApproxType) out_vars[j].data_type
                                          );
            }
          offset += out_vars[j].num_elem;
        }
    }
  else
    {
      offset = 0;
      accurateFN(arg);
      // TODO: Update to be min(num_rows, what is currently below)
      entry_index = RTEnvd.inputIdx[tid_global];

      for(int j = 0; j < nInputs; j++)
        {
          for(int i = 0; i < in_vars[j].num_elem; i++)
            {
              convertToSingleWithOffset(RTEnvd.iTable, in_vars[j].ptr, i+offset+i_tab_offset+(*RTEnvd.iSize*entry_index), i,
                                        (ApproxType) in_vars[j].data_type);
            }

          offset += in_vars[j].num_elem;
        }

      offset = 0;
      // TODO: this should be size_t
      for(int j = 0; j < nOutputs; j++)
        {
          for(size_t i = 0; i < out_vars[j].num_elem; i++)
            {
              RTEnvd.inputIdx[offset+i+tid_global] += 1;
              convertToSingleWithOffset(RTEnvd.oTable, out_vars[j].ptr, offset+i+o_tab_offset+(*RTEnvd.oSize*entry_index), i,
                                        (ApproxType) out_vars[j].data_type);
            }
          offset += out_vars[j].num_elem;
        }
    }
}
#pragma omp end declare target

