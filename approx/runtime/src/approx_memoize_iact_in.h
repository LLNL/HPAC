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
                Addr = (unsigned long) acc;
            }

        ~MemoizeInput(){
            if (inTable)
                delete2DArray(inTable);
            if (outTable)
                delete2DArray(outTable);
            if ( iTemp )
                delete [] iTemp;
        }

        double getStatistics(){
            return (double)(approximately) / (double)(accurately+approximately);
        }

        void execute(void *args, approx_var_info_t *inputs, approx_var_info_t *outputs){
            packVarToVec(inputs, num_inputs, iTemp);
            real_t minDist = std::numeric_limits<real_t>::max(); 
            int index = -1;
            if ( iSize != 1){
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
            }
            else{
                for (int i = 0; i < input_index; i++){
                    real_t temp = inTable[i][0];
                    real_t dist = 0.0f;
                    if (temp != 0.0f)
                        dist += fabs((iTemp[0] - temp)/temp);
                    else
                        dist += fabs((iTemp[0] - temp));
                    dist = dist/(real_t)iSize;
                    if (dist < minDist){
                        minDist = dist;
                        index = i;
                        if (minDist < threshold)
                            break;
                    }
                }
            }

            if (minDist > threshold){
                index = -1;
            }

            if (index == -1){
                accurately++;
                accurate(args);
                if (input_index < tSize ){
                    std::memcpy(inTable[input_index], iTemp, sizeof(real_t)*iSize);
                    packVarToVec(outputs, num_outputs, outTable[input_index]);
                    input_index +=1;
                }
            }
            else{
                approximately++;
                unPackVecToVar(outputs, num_outputs, outTable[index]);
            }
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
