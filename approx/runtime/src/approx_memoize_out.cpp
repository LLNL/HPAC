#include <cfloat>
#include <iostream>
#include <memory>
#include <string>
#include <cstring>
#include <unordered_map>
#include <cmath>
#include <omp.h>

#include "approx_compute_mape.h"
#include "approx_data_util.h"
#include "approx_internal.h"
#include "thread_storage.h"

#define MAX_REGIONS 20
#define NOTFOUND -1

using namespace std;

//!  An Output Memoization class.
/*!
  This is an implementation of the output memoization described in :
  G. Tziantzioulis, N. Hardavellas and S. Campanoni, "Temporal
  Approximate Function Memoization," in IEEE Micro, vol. 38, no. 4,
  pp. 60-70, Jul./Aug. 2018, doi: 10.1109/MM.2018.043191126.

  There are certain assumption of the current implementation

  ASSUMPTION 1) : Each region is always going to be executed
  with the same shape. We do not support regions that change
  the size of their outputs during execution time. If a region
  is invoked the first time with a single output of 10 double
  elements. In all upcoming calls this region should work using
  the same "shape" single output with 10 double elements.

  ASSUMPTION 2) : When Computing the average value of multiple
  outputs we assume all outputs have the same weight. This might
  not be fair in case of outputs that are vectors. Assuming a case
  in which we have 2 outputs a vector of N elements (vec) and a
  scalar variable x. The average is compute as:
  avg = (avg(x) + avg(vec))/2. This implicitly reduces the
  importance of each element vec[i] in the computation of
  the average
  */
class MemoizeOutput {
    public:
        /// The State Machine of this technique.
        typedef enum : bool { ACCURATE, APPROXIMATE } executionState;
        /// The accurate function.
        void (*accurate)(void *);
        unsigned long Addr;
        /// The number of outputs this function produces.
        int num_outputs;
        /// The size of the prediction
        int prediction_size;
        /// The size of the history
        int history_size;
        /// The last values computed.
        real_t **window;
        /// The last values computed.
        real_t *computedValue;
        /// Number of values
        int numCols;
        /// The average value of the window
        real_t avg;
        /// The stdev of the window.
        real_t stdev;
        /// RSD = fabs(stdev/average)
        real_t rsd;
        /// User provided threshold
        real_t threshold;
        /// Index of storing in the window.
        int cur_index;
        /// Index of the last computed output value.
        int last_output;
        /// Counter, contains number of invocations done
        /// after the last approximation.
        int active_values;
        /// Counter, contains how many approximate invocations
        /// prior changing state.
        int predicted_values;
        /// Current state of the execution.
        executionState state;
        /// Stat. Counts how many invocations where performed accurately.
        int accurately;
        /// Stat. Counts how many invocations where performed approximately.
        int approximately;
        MAPE mape_error;
    public:
        MemoizeOutput(void(acc)(void *), approx_var_info_t *outputs, int num_outputs, int pSize, int hSize, float threshold)
            : accurate(acc), num_outputs(num_outputs), history_size(hSize), prediction_size(pSize), 
            avg(0.0), stdev(0.0), rsd(FLT_MAX), threshold(threshold),
            cur_index(0), active_values(0), predicted_values(0), state(ACCURATE),
            accurately(0), approximately(0) {

                window = createTemp2DVarStorage<real_t>(outputs,num_outputs, history_size, &numCols);
                computedValue = new real_t [numCols];
                Addr = (unsigned long ) acc;
            }


        ~MemoizeOutput() {
            delete2DArray(window);
            delete [] computedValue;
        }

        double getStatistics(){
            return (double)(approximately) / (double)(accurately+approximately);
        }

        /// The actual implementation of the approximation technique.
        void execute(void *arg, approx_var_info_t *outputs) {
            if (state == APPROXIMATE) {
                unPackVecToVar(outputs, num_outputs, window[last_output]);
                approximately++;
            } else if (state == ACCURATE) {
                accurate(arg);
                packVarToVec(outputs, num_outputs, window[cur_index]);
                accurately++;
                cur_index = (cur_index + 1) % history_size;
                active_values++;
            }

            // The following code is the actual overhead of.
            // the approximation technique. In the case of
            // output memoization this part of code could also be
            // executed by another "worker thread" to hide the latencies
            // of the approximation technique itself. In serial applications
            // this is kind of straight forward. In parallel application versions
            // this needs to be studied.

            if (state == ACCURATE && active_values >= history_size) {
                // When executed in accurate state we need to
                // compute the new avg, stdev, rsd, and change
                // state if necessary for the next invocation.
                real_t variance = 0.0;
                real_t avg = 0.0;
                if ( numCols == 1 ){ 
                    for (int i = 0; i < history_size; i++) {
                        avg += window[i][0];
                    }
                    avg = avg / real_t (history_size*numCols);

                    for (int i = 0; i < history_size; i++) {
                        real_t tmp = (window[i][0] - avg);
                        variance += tmp * tmp;
                    }
                    variance /= (real_t)(history_size);
                }
                else{
                    for (int i = 0; i < history_size; i++) {
                        for (int j = 0; j < numCols; j++){
                            avg += window[i][j];
                        }
                    }

                    avg = avg / real_t (history_size*numCols);

                    for (int i = 0; i < history_size; i++) {
                        for (int j = 0; j < numCols; j++){
                            real_t tmp = (window[i][j] - avg);
                            variance += tmp * tmp;
                        }
                    }
                    variance /= (real_t)(history_size*numCols);
                }
                /// We might consider using approximable
                // versions of math functions which are considerably
                // faster but ofcource the results will not be that accurate.
                stdev = sqrt(variance);
                rsd = stdev / avg;
                rsd = stdev;
                if ( avg != 0.0f){
                    rsd = stdev / avg;
                }
                if (rsd < 0.0) rsd = -rsd;

                if (rsd < threshold) {
                    last_output = cur_index;
                    state = APPROXIMATE;
                    predicted_values = prediction_size;
                }
            } else if (state == APPROXIMATE) {
                if (--predicted_values == 0) {
                    state = ACCURATE;
                    active_values = 0;
                }
            }
        }

        void execute_both(void *arg, approx_var_info_t *outputs) {
            accurate(arg);
            mape_error.registerAccurateOut(outputs, num_outputs);

            if (state == APPROXIMATE) {
                unPackVecToVar(outputs, num_outputs, window[last_output]);
                mape_error.registerApproximateOut(outputs, num_outputs);
                approximately++;
            } else if (state == ACCURATE) {
                mape_error.registerApproximateOut(outputs, num_outputs);
                packVarToVec(outputs, num_outputs, computedValue);
                accurately++;
            }

            // The following code is the actual overhead of.
            // the approximation technique. In the case of
            // output memoization this part of code could also be
            // executed by another "worker thread" to hide the latencies
            // of the approximation technique itself. In serial applications
            // this is kind of straight forward. In parallel application versions
            // this needs to be studied.

            if (state == ACCURATE) {
                // When executed in accurate state we need to
                // compute the new avg, stdev, rsd, and change
                // state if necessary for the next invocation.

                real_t new_value = 0.0;
                real_t old_value = 0.0;
                // Compute new incoming value. Here we
                // actually utilize assumption 1.
                // This should be further investigated.
                for (int i = 0; i < num_outputs; i++) {
                    new_value += computedValue[i]; 
                    old_value += window[cur_index][i];
                }

                std::memcpy(window[cur_index],computedValue, numCols * sizeof(real_t) );
                last_output = cur_index;
                cur_index = (cur_index + 1) % history_size;
                avg = avg + (new_value - old_value) / (real_t)(history_size + num_outputs);

                active_values++;
                real_t variance = 0.0;
                if (active_values >= history_size) {
                    for (int i = 0; i < history_size; i++) {
                        for (int j = 0; j < num_outputs; j++){
                            real_t tmp = (window[i][j] - avg);
                            variance += tmp * tmp;
                        }
                    }
                    variance /= (real_t)(history_size+num_outputs);

                    /// We might consider using approximable
                    // versions of math functions which are considerably
                    // faster but ofcource the results will not be that accurate.
                    stdev = sqrt(variance);
                    rsd = fabs(stdev / avg);
                    if (rsd < threshold) {
                        state = APPROXIMATE;
                        predicted_values = prediction_size;
                    }
                }
            } else if (state == APPROXIMATE) {
                if (--predicted_values == 0) {
                    state = ACCURATE;
                    active_values = history_size - predicted_values;
                    if (active_values < 0) {
                        active_values = 0;
                    }
                }
            }
        }
};

ThreadMemoryPool<MemoizeOutput> outMemo;

/**
 * Memoize code region using output memoize technique
 *
 * @param  accurate function pointer of the code region to be approximated.
 * @param  arg Arguments of accurate function
 * @param  outputs outputs produced by this code region.
 * @param  num_outputs number of outputs
 * @return void.
 *
 * @brief This function will try to approximate the code region specified
 * by the function pointer. It uses the implementation of the MemoizeOutput
 * class to perform all the required approximation steps.
 */


void memoize_out(void (*accurate)(void *), void *arg,
        approx_var_info_t *outputs, int num_outputs){
    static thread_local int threadId = -1;
    static thread_local MemoizeOutput *curr = nullptr;
    if (threadId == -1){
        if (omp_in_parallel())
            threadId = omp_get_thread_num();
        else
            threadId = 0;
    }

    if (curr && curr->Addr != (unsigned long) accurate){
        curr = outMemo.findMemo(threadId, (unsigned long)accurate);
    }

    if (!curr){ 
        curr = outMemo.addNew(threadId, new MemoizeOutput(accurate, outputs, num_outputs, getPredictionSize(), getHistorySize(), getThreshold()));
    }

    curr->execute(arg, outputs);
    /*
       if (ExecBoth)
       curr->execute_both(arg, outputs);
       else
       curr->execute(arg, outputs);
       */
}
