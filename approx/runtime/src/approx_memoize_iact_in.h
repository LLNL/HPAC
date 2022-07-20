//===--- approx_memoize_iact_in.h - runtime implementation of iACT  ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This files is the driver of the iACT approximate memoization 
///
//===----------------------------------------------------------------------===//

#include <cfloat>
#include <iostream>
#include <memory>
#include <unordered_map>
#include <stack>
#include <limits>
#include <cmath>
#include <cstring>
#include <omp.h>

#include "approx_compute_mape.h"
#include "approx_data_util.h"
#include "approx_internal.h"
#include "thread_storage.h"

#include <chrono> 
using namespace std::chrono; 

using namespace std;

class MemoizeInput {
    public:
        real_t **inTable;
        real_t **outTable;
        real_t *iTemp;
        real_t *oTemp;
        AccessWrapper2D<real_t> inTabWr;
        AccessWrapper2D<real_t> outTabWr;
        void (*accurate)(void *);
        int num_inputs;
        int num_outputs;
        int tSize;
        real_t threshold;
        int input_index, output_index;
        long accurately;
        long approximately;
        int iSize, oSize;
        MAPE mape_error;
        unsigned long Addr;
    public:
        MemoizeInput(void (*acc)(void *), int num_inputs, int num_outputs,
                approx_var_info_t *inputs, approx_var_info_t *outputs, int tSize, real_t threshold)
           : accurate(acc), num_inputs(num_inputs), num_outputs(num_outputs),
            tSize(tSize), threshold(threshold), input_index(0), output_index(0), 
            accurately(0), approximately(0) {
                inTable = nullptr;
                outTable = nullptr;
                iTemp = nullptr;
                if (tSize == 0){
                    printf("Should Never happen\n");
                    printf("Define table size\n");
                    exit(-1);
                }

                inTable = createTemp2DVarStorage<real_t>(inputs, num_inputs, tSize, &iSize);
                outTable = createTemp2DVarStorage<real_t>(outputs, num_outputs, tSize, &oSize);
                iTemp = createTemp1DVarStorage<real_t>(inputs, num_inputs, &iSize);
                inTabWr = AccessWrapper2D<real_t>(inTable[0], tSize, iSize);
                outTabWr = AccessWrapper2D<real_t>(outTable[0], tSize, oSize);
                Addr = (unsigned long) acc;
                int iSize1D = inTabWr.nrows*inTabWr.ncols;
                int oSize1D = outTabWr.nrows*outTabWr.ncols;

               #pragma omp target enter data map(to:this[:1])
               #pragma omp target enter data map(to:inTabWr)
               #pragma omp target enter data map(to:inTabWr.data[0:iSize1D])
               #pragma omp target enter data map(to:iTemp[0:iSize])
               #pragma omp target enter data map(to:outTabWr)
               #pragma omp target enter data map(to:outTabWr.data[0:oSize1D])
            }

        ~MemoizeInput(){
            if (inTable)
              {
                // NOTE: we cannot do this because at this point CUDA RTL is de-initialized
                // #pragma omp target exit data map(from:inTabWr, inTabWr.data[0:num])
                delete2DArray(inTable);
              }
            if (outTable)
                delete2DArray(outTable);
            if ( iTemp )
                delete [] iTemp;
        }

        double getStatistics(){
            return (double)(approximately) / (double)(accurately+approximately);
        }

        void execute(void *args, approx_var_info_t *inputs, approx_var_info_t *outputs){

            real_t minDist = std::numeric_limits<real_t>::max();
            // NOTE: Assume that if output is on the device (output is currently mapped to the device), then
            // input is also on the device and the computation being wrapped is a teams construct.
            // This may not always be valid, and we should add functionality to determine this in
            // CodeGen
            bool outputOnDevice = omp_target_is_present(outputs[0].ptr, 0);
            int index = -1;
            if (!outputOnDevice){
              packVarToVec(inputs, num_inputs, iTemp);
              calc_distance(index, minDist);
            } else{
              packVarToDeviceVec(inputs, num_inputs, iTemp);
              calc_distance_device(index, minDist);
            }

            if (minDist > threshold){
                index = -1;
            }

            if (index == -1){
                accurately++;
                accurate(args);
                if (input_index < tSize ){
                  if(!outputOnDevice){
                    std::memcpy(inTable[input_index], iTemp, sizeof(real_t)*iSize);
                    packVarToVec(outputs, num_outputs, outTable[input_index]);
                  }else{
                    void *dst = omp_get_mapped_ptr(&inTabWr(input_index, 0), 0);
                    void * src = omp_get_mapped_ptr(iTemp, 0);
                    omp_target_memcpy(dst, src, sizeof(real_t)*iSize,
                                      /*dest offset*/ 0, /*src offset*/0,
                                      /*dest device*/0, /*src device*/0
                                      );
                    packVarToDeviceVec(outputs, num_outputs, outTable[input_index]);
                  }
                    input_index +=1;
                }
            }
            else{
                approximately++;
                if(!outputOnDevice)
                  unPackVecToVar(outputs, num_outputs, outTable[index]);
                else
                  unpackVecToVarDevice(outputs, num_outputs, outTable[index]);
            }

        }

  void calc_distance(int &minIdx, real_t &minDist) {
    for (int i = 0; i < input_index; i++){
      real_t dist = 0.0f;
      for (int j = 0; j < iSize; j++){
        if (inTabWr(i,j) != 0.0f)
          dist += fabs((iTemp[j] - inTabWr(i,j))/inTabWr(i,j));
        else
          dist += fabs((iTemp[j] - inTabWr(i,j)));
      }
      dist = dist/(real_t)iSize;
      if (dist < minDist){
        minDist = dist;
        minIdx = i;
        if (minDist < threshold)
          break;
      }
    }
  }

      // assume vectorized inputs in iTemp on the device
        void calc_distance_device(int &minIdx, real_t &minDist) {
          real_t mind[1]{std::numeric_limits<real_t>::max()};
          int minidx[1]{-1};
          real_t dist[1]{0.0};

#pragma omp target data map(to:iSize, num_inputs, dist[0:1])
          {
            // TODO: How can we increase the granularity? Collapse? Won't work here because of the inner loop
            // we can maybe submit the kernels in parallel
            for (int i = 0; i < input_index; i++){
              #pragma omp target teams distribute parallel for reduction(+:dist[0]) default(shared)
              for (int j = 0; j < iSize; j++){
                if (inTabWr(i,j) != 0.0f)
                  dist[0] += fabs((iTemp[j] - inTabWr(i,j))/inTabWr(i,j));
                else
                  dist[0] += fabs((iTemp[j] - inTabWr(i,j)));
              }
              #pragma omp target update from(dist[0:1])
              dist[0] = dist[0]/(real_t)iSize;
              if (dist[0] < mind[0]){
                mind[0] = dist[0];
                minidx[0] = i;
              }
              dist[0] = 0.0;
              #pragma omp target update to(dist[0:1])
          if(minDist < threshold)
            break;
            }
          }
          minDist = mind[0];
          minIdx = minidx[0];
        }

        void execute_both(void *args, approx_var_info_t *inputs, approx_var_info_t *outputs){
            packVarToVec(inputs, num_inputs, iTemp);
            accurate(args);

            real_t minDist = std::numeric_limits<real_t>::max();
            int index = -1;
            // Iterate in table and find closest input value
            for (int i = 0; i < input_index; i++){
                real_t *temp = inTable[i];
                real_t dist = 0.0f;
                for (int j = 0; j < iSize; j++){
                    if (temp[j] != 0.0f)
                        dist += fabs((iTemp[j] - temp[j])/temp[j]);
                    else
                        dist += fabs((iTemp[j] - temp[j]));
                }
                dist = dist/(real_t)iSize;
                if (dist < minDist){
                    minDist = dist;
                    index = i;
                    if (minDist < threshold)
                        break;
                }
            }

            if (minDist > threshold){
                index = -1;
            }

            mape_error.registerAccurateOut(outputs, num_outputs);

            double diff = 0.0;
            if (index == -1){
                // I need to execute accurately
                mape_error.registerApproximateOut(outputs, num_outputs);
                accurately++;
                if (input_index < tSize ){
                    std::memcpy(inTable[input_index], iTemp, sizeof(real_t)*iSize);
                    packVarToVec(outputs, num_outputs, outTable[input_index]);
                    input_index +=1;
                }
            }
            else{
                approximately++;
                unPackVecToVar(outputs, num_outputs, outTable[index]);
                mape_error.registerApproximateOut(outputs, num_outputs);
            }
        }
};
