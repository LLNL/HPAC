//===--- approx_compute_map.h -  compute statistics of approximate execution ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This files computes the MAPE between accurate and approximate executions 
///
//===----------------------------------------------------------------------===//

#ifndef __APPROX_COMPUTE_MAPE__
#define __APPROX_COMPUTE_MAPE__

#include <iostream>
#include "approx_data_util.h"
#include "approx_internal.h"

class MAPE{
    typedef enum{
        ACCURATE,
        APPROXIMATE,
        ENDVERSIONS
    }executionTypes;
    double average[ENDVERSIONS];
    int invocations[ENDVERSIONS];
    public:
        unsigned long accurate;
    public:
        MAPE(){
            for (int i = 0; i < ENDVERSIONS; i++){
                invocations[i] = 0;
                average[i] = 0.0;
            }
        }
        MAPE(unsigned long accPtr): accurate(accPtr){
            for (int i = 0; i < ENDVERSIONS; i++){
                invocations[i] = 0;
                average[i] = 0.0;
            }
        }
        ~MAPE(){
        }

        double getStatistics(){
            for (int i = 1; i < ENDVERSIONS; i++){
                if (invocations[i] != invocations[i-1]){
                    std::cout << "This should never happen" << invocations[i-1] << " " << invocations[i]<<std::endl;
                }
                if (invocations[i] == 0){
                    return -1;
                }
            }
            double MAPE = fabs(average[ACCURATE] - average[APPROXIMATE])/fabs(average[ACCURATE]);
            return MAPE;
        }

        void registerAccurateOut(approx_var_info_t *vars, int num_vars){
            double out= 0.0;
            for (int i = 0; i < num_vars ; i++){
                out+= aggregate(vars[i].ptr, vars[i].num_elem,
                (ApproxType)vars[i].data_type);
             }
             average[ACCURATE] += out;
             invocations[ACCURATE] ++;
        }

        void registerApproximateOut(approx_var_info_t *vars, int num_vars){
            double out= 0.0;
            for (int i = 0; i < num_vars ; i++){
                out+= aggregate(vars[i].ptr, vars[i].num_elem,
                (ApproxType)vars[i].data_type);
             }
             average[APPROXIMATE] += out;
             invocations[APPROXIMATE] ++;
        }
};

#endif
