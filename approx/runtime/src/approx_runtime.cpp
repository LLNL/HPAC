#include <stdint.h>
#include <string>
#include <cstring>
#include <cstdlib>
#include <chrono>
#include <unordered_map>
#include <random>
#include <approx.h>
#include <approx_data_util.h>
#include <approx_internal.h>

using namespace std;

#define MEMO_IN 1
#define MEMO_OUT 2

enum ExecuteMode: uint8_t{
  EXECUTE
};

class ApproxRuntimeConfiguration{
  ExecuteMode Mode;
  bool ExecuteBoth;
public:
  int approxStep;
  float approxRate;
  int tableSize;
  float threshold;
  int historySize;
  int predictionSize;
  int perfoStep;
  float perfoRate;
  float *randomNumbers;
  int count;

  ~ApproxRuntimeConfiguration(){
    delete []randomNumbers;
  }

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
  }

    env_p = std::getenv("PERFO_STEP");
    if (env_p) {
      perfoStep = atoi(env_p);
    }

    env_p = std::getenv("PERFO_RATE");
    if (env_p) {
      perfoRate = atof(env_p);
    }

 // This is not the optimal way. Since, we will 
 // always use the same random numbers.
    randomNumbers = new float[RAND_SIZE];
    static std::default_random_engine generator;
    static std::uniform_real_distribution<float> distribution(0.0f, 1.0f);

    for (int i = 0 ; i < RAND_SIZE; i++){
     randomNumbers[i] = distribution(generator);
    }
  }

  ~ApproxRuntimeConfiguration(){
    delete [] randomNumbers;
  }

   ExecuteMode getMode(){return Mode;}

  bool getExecuteBoth(){ return ExecuteBoth; }
};

ApproxRuntimeConfiguration RTEnv;

bool __approx_skip_iteration(unsigned int i, float pr) {
  static thread_local int index = 0;
    if (RTEnv.randomNumbers[(index++)%RAND_SIZE] <= pr) {
        return true;
    }
    return false;
}

void __approx_exec_call(void (*accurateFN)(void *), void (*perfoFN)(void *),
                        void *arg, bool cond, const char *region_name,
                        void *perfoArgs, int memo_type, void *inputs,
                        int num_inputs, void *outputs, int num_outputs) {
  approx_perfo_info_t *perfo = (approx_perfo_info_t *)perfoArgs;
  approx_var_info_t *input_vars = (approx_var_info_t *)inputs;
  approx_var_info_t *output_vars = (approx_var_info_t *)outputs;

    if (cond) {
      if ( perfoFN ){
          perforate(accurateFN, perfoFN, arg, input_vars, num_inputs, output_vars, num_outputs, RTEnv.getExecuteBoth());
      } else if (memo_type == MEMO_IN) {
        memoize_in(accurateFN, arg, input_vars, num_inputs, output_vars,
                   num_outputs, RTEnv.getExecuteBoth(), RTEnv.tableSize, RTEnv.threshold );
      } else if (memo_type == MEMO_OUT) {
        memoize_out(accurateFN, arg, output_vars, num_outputs, RTEnv.getExecuteBoth(), RTEnv.predictionSize, RTEnv.historySize, RTEnv.threshold);
      } else {
        accurateFN(arg);
      }
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
