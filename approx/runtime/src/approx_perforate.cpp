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

ThreadMemoryPool<MAPE> errorStats;
void perforate(void (*accurate) (void *), void (*perfo) (void *), void *args, approx_var_info_t *input_vars, int num_inputs, 
            approx_var_info_t *output_vars, int num_outputs, bool ExecuteBoth){
    if (!ExecuteBoth){
        perfo(args);
        return;
    }

    // Execute Both case is more of an analysis case,
    // Thus we do not care about performance. 
    // At some point though, we should create Execution Policies
    // And avoid rewriting everything.
    int threadId = 0;
    MAPE *mape_error;
    if (omp_in_parallel()){
        threadId = omp_get_thread_num();
    }

    mape_error = errorStats.findMemo(threadId, (unsigned long)accurate);
    if (!mape_error){
        mape_error = errorStats.addNew(threadId, new MAPE((unsigned long)accurate));
    }

    int iSize;
    real_t *iTemp = createTemp1DVarStorage<real_t>(input_vars, num_inputs, &iSize);
    packVarToVec(input_vars, num_inputs, iTemp);
    accurate(args);
    mape_error->registerAccurateOut(output_vars, num_outputs);
    // Revert Inputs, and execute approximate
    unPackVecToVar(input_vars, num_inputs, iTemp);
    perfo(args);
    //Register erroneous result 
    mape_error->registerApproximateOut(output_vars, num_outputs);
    delete [] iTemp;
}