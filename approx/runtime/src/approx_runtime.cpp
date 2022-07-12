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
  char *inputIdx;
  int tableSize;
  real_t *iTable;
  real_t *oTable;
  ApproxRuntimeDevDataEnv() = default;
  ~ApproxRuntimeDevDataEnv(){
    if(iTable)
      delete[] iTable;
    if(oTable)
      delete[] oTable;
  }

  void resetTable(int _tableSize){
    tableSize = _tableSize;
    inputIdx = new char[tableSize];
    iTable = new real_t[tableSize];
    oTable = new real_t[tableSize];

    std::fill(inputIdx, inputIdx+tableSize, 0);

#pragma omp target enter data map(to:this[:1], inputIdx[0:tableSize], tableSize, iTable[0:tableSize], oTable[0:tableSize])
  }

  void updateDevCopy(){
#pragma omp target update to(this[:1], inputIdx[0:tableSize], tableSize, iTable[0:tableSize], oTable[0:tableSize])
  }

  // allow explicit destruction.
  void destruct(){
   #pragma omp target exit data map(delete:this[:1], inputIdx[0:tableSize], tableSize, iTable[0:tableSize], oTable[0:tableSize])
    delete[] inputIdx;
    delete[] iTable;
    delete[] oTable;
  }
};

ApproxRuntimeDevDataEnv RTEnvd = ApproxRuntimeDevDataEnv();
#pragma omp end declare target

void resetDeviceTable(int newSize){
  int tabSize = newSize == -1 ? RTEnvd.tableSize : newSize;

  RTEnvd.destruct();
  RTEnvd.resetTable(tabSize);
  RTEnvd.updateDevCopy();
}


class ApproxRuntimeConfiguration{
  ExecuteMode Mode;
public:
  bool ExecuteBoth;
  int tableSize;
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

    RTEnvd.resetTable(tableSize);
    RTEnvd.updateDevCopy();
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
  // FIXME: enforce uniform team size
  int tid_global = omp_get_thread_num() + omp_get_team_num() * omp_get_num_threads();
  // int grid_size = omp_get_num_teams() * omp_get_num_threads();
  int grid_size = omp_get_num_teams() * omp_get_num_threads();
  real_t val = RTEnvd.iTable[tid_global];
  int offset = 0;
  real_t dists[5]{0, 0, 0, 0, 0};
  real_t my_dist = 0;
#pragma omp allocate(dists) allocator(omp_pteam_mem_alloc)

  for(int j = 0; j < nInputs; j++)
    {
      for(int i = tid_global; i < in_vars[j].num_elem; i += grid_size)
        {
          if(RTEnvd.inputIdx[i] == 1)
            {
              real_t tab_val = RTEnvd.iTable[offset+i];
              real_t in_val = ((real_t*)in_vars[j].ptr)[i];
              real_t in_val_conv = 0;
              convertToSingleWithOffset(&in_val_conv, in_vars[j].ptr, 0, i, (ApproxType) in_vars[j].data_type);
            real_t dist = fabs(RTEnvd.iTable[offset+i] - in_val_conv);
          // if(dist > 0.00001f)
            // {printf("Dist is %f, for thread %d iter [%d,%d] offset %d table val %f in_val %f in_val int %d converted %f\n", dist, omp_get_thread_num(), j,i,offset, tab_val, in_val, ((int*)in_vars[j].ptr)[i],in_val_conv);}
            my_dist += dist;
            }
        }

      #pragma omp atomic
      dists[0] += my_dist;
      #pragma omp atomic update
      dists[1] += 1;
      offset += in_vars[j].num_elem;
    }

#pragma omp flush(dists[0:2])
  while(dists[1] != omp_get_num_threads());

  real_t dist_total = dists[0];

  if(RTEnvd.inputIdx[tid_global] == 1 && dist_total < 1000)
    {
      // if(omp_get_thread_num() == 0 && omp_get_team_num() == 0)
        // printf("Approximately\n");

      for(int i = tid_global; i < out_vars[0].num_elem; i += grid_size)
        {
          if(RTEnvd.inputIdx[i] == 0) continue;
          convertFromSingleWithOffset(out_vars[0].ptr,
                                      RTEnvd.oTable,
                                      i, i,
                                      (ApproxType) out_vars[0].data_type
                                      );
        }
    }
  else
    {

      // if(omp_get_thread_num() == 0 && omp_get_team_num() == 0)
        // printf("Accurately\n");
      // we know that this thread can calculate accurate_fn without any issues
      accurateFN(arg);
      offset = 0;

      // but we need toe xplicitly map input space to threads
      for(int j = 0; j < nInputs; j++)
        {
          for(int i = tid_global; i < in_vars[j].num_elem; i += grid_size)
            {
              convertToSingleWithOffset(RTEnvd.iTable, in_vars[j].ptr, i+offset, i,
                                        (ApproxType) in_vars[j].data_type);
            }

          offset += in_vars[j].num_elem;
        }

      // race condition here: we can copy values that have not yet been calculated
      // TODO: this should be size_t
      for(size_t i = tid_global; i < out_vars[0].num_elem; i += grid_size)
        {

          // printf("INNER: Thread %d writing output value %d, gridsize %d\n", tid_global, i, grid_size);
          convertToSingleWithOffset(RTEnvd.oTable, out_vars[0].ptr, i, i,
                                    (ApproxType) out_vars[0].data_type);
          RTEnvd.inputIdx[i] = 1;
        }
    }

}
#pragma omp end declare target

