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
#include <random>
#include <vector>
#include <algorithm>
#include "optional_omp.h"
#include <iostream>
#include <typeinfo>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <utility>

#include "approx.h"
#include "approx_data_util.h"
#include "approx_pack.h"
#include "approx_internal.h"
#include "thread_storage.h"
#include "database/database.h"
#include "approx_surrogate.h"
#include "approx_tensor.h"


using namespace std;

#define MEMO_IN 1
#define MEMO_OUT 2

#define PETRUBATE_IN 1
#define PETRUBATE_OUT 2


enum MLType: uint {
  ML_ONLINETRAIN = 1,
  ML_OFFLINETRAIN,
  ML_INFER,
  ML_END
};


#define RAND_SIZE  10000

float __approx_perfo_rate__;
int __approx_perfo_step__;

enum ExecuteMode: uint8_t{
  EXECUTE
};

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
  int64_t useChunk;
  int64_t iptChunkSize;
  int64_t optChunkSize;
  vector<int64_t> tensorShape;
  BaseDB *db;
  SurrogateModel<GPUExecutionPolicy, CatTensorTranslator<double>, double> Model{
      "/scratch/mzu/zanef2/surrogates/SurrogateBenchmarks/models/lulesh/model.pt", false};


  ApproxRuntimeConfiguration() {
      ExecuteBoth = false;
      count = 0;

    const char *env_p = std::getenv("EXECUTE_BOTH");
    if (env_p){
      ExecuteBoth = true;
    }

    env_p = std::getenv("HPAC_DB_FILE");
    if (env_p) {
      db = new HDF5DB(env_p);
    } else {
      db = new HDF5DB("test.h5");
    }

    env_p = std::getenv("SURROGATE_MODEL");
    if (env_p) {
      Model.set_model(env_p);
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

    env_p = std::getenv("USE_CHUNK");
    if (env_p) {
      useChunk = atoi(env_p);
      if (useChunk != 0 && useChunk != 1)
        std::cerr << "USE_CHUNK must be 0 or 1";
    } else {
      useChunk = 0;
    }

    const uint64_t thresholdChunk = 4ULL * 1000 * 1000 * 1000;
    const uint64_t defaultChunk = 4ULL * 1000 * 1000;

    env_p = std::getenv("IPT_CHUNK_SIZE");
    if (env_p) {
      iptChunkSize = atoi(env_p);
      std::cout << "input chunk size " << iptChunkSize << std::endl;
      if (iptChunkSize > thresholdChunk)
        std::cerr << "Chunk size exceeds 4GB limit\n";
    } else{
      iptChunkSize = defaultChunk;
    }

    env_p = std::getenv("OPT_CHUNK_SIZE");
    if (env_p) {
      optChunkSize = atoi(env_p);
      std::cout << "output chunk size " << optChunkSize << std::endl;
      if (optChunkSize > thresholdChunk)
        std::cerr << "Chunk size exceeds 4GB limit\n";
    } else{
      optChunkSize = defaultChunk;
    }

    env_p = std::getenv("TENSOR_SHAPE");
    if (env_p) {
      std::string myListStrCpp(env_p);
      std::stringstream ss(myListStrCpp);

      for (int64_t i; ss >> i;) {
        tensorShape.push_back(i);    
        if (ss.peek() == ',') {
            ss.ignore();
        }
      }
    } 

    env_p = std::getenv("PETRUBATE_TYPE");
    if (env_p) {
      const char *type = env_p;
      env_p = std::getenv("PETRUBATE_FILE");
      const char *fName = env_p; 
      register_petrubate(fName, type);
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
  }

  ~ApproxRuntimeConfiguration(){
    delete [] randomNumbers;
    delete db;
    deinitPetrubate();
  }

  ExecuteMode getMode(){return Mode;}

  bool getExecuteBoth(){ return ExecuteBoth; }

};

ApproxRuntimeConfiguration RTEnv;
ThreadMemoryPool<HPACRegion> HPACRegions;

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

static inline void
create_snapshot_packet(HPACPacket &dP, void (*user_fn)(void *),
                       const char *region_name, approx_var_info_t *inputs,
                       int num_inputs, approx_var_info_t *outputs,
                       int num_outputs) {
  // thread_local int threadId = -1;
  // thread_local HPACRegion *curr;
  // if(region_name == nullptr) {
  //   region_name = "unknown";
  // }
  // if (threadId == -1) {
  //   if (omp_in_parallel())
  //     threadId = omp_get_thread_num();
  //   else
  //     threadId = 0;
  // }

  // if (curr && (curr->accurate != (unsigned long)user_fn ||
  //              curr->getName() != region_name))
  //   curr = HPACRegions.findMemo(threadId, (unsigned long)user_fn, region_name);

  // if (!curr) {
  //   int IElem = computeNumElements(inputs, num_inputs);
  //   int OElem = computeNumElements(outputs, num_outputs);
  //   if (RTEnv.db != nullptr) {
  //     curr = new HPACRegion((uintptr_t)user_fn, IElem, OElem, NUM_CHUNKS,
  //                           region_name);
  //     void *dbRId =
  //         RTEnv.db->InstantiateRegion((uintptr_t)user_fn, region_name, inputs,
  //                                     num_inputs, outputs, num_outputs, curr->getNumRows());
  //     curr->setDB(RTEnv.db);
  //     curr->setDBRegionId(dbRId);
  //     HPACRegions.addNew(threadId, curr);
  //   } else {
  //     curr = new HPACRegion((uintptr_t)user_fn, IElem, OElem, NUM_CHUNKS,
  //                           region_name);
  //     HPACRegions.addNew(threadId, curr);
  //   }
  // }

  // double *dPtr = reinterpret_cast<double *>(curr->allocate());
  // dP.inputs = dPtr;
  // dP.outputs = dPtr + curr->IElem;
  // dP.feature = curr;
  // return;
}

// This is the main driver of the HPAC approach.
void __snapshot_call__(void (*_user_fn_)(void *), void *args,
                       const char *region_name, void *inputs, int num_inputs,
                       void *outputs, int num_outputs) {
  HPACPacket dP;
  approx_var_info_t *input_vars = (approx_var_info_t *)inputs;
  approx_var_info_t *output_vars = (approx_var_info_t *)outputs;

  create_snapshot_packet(dP, _user_fn_, region_name, input_vars, num_inputs,
                         output_vars, num_outputs);

  packVarToVec(input_vars, num_inputs, dP.inputs); // Copy from application
                                                   // space to library space
  // When true we will use HPAC Model for this output
  _user_fn_(args);
  packVarToVec(output_vars, num_outputs, dP.outputs);
}

enum class TensorsFound : char { NONE = 0, OUTPUT, INPUT, BOTH };

bool is_ml(MLType type) {
  return type < MLType::ML_END;
}

struct ml_argdesc_t {
  void (*accurateFN)(void *);
  void *accurateFN_arg;
  const char *region_name;
  TensorsFound have_tensors;
  approx_var_info_t *input_vars;
  approx_var_info_t *output_vars;
  std::vector<void *> ipts;
  std::vector<void *> opts;
};

void ml_infer(ml_argdesc_t &arg) {
  internal_repr_metadata_t *ipt_metadata = nullptr;
  internal_repr_metadata_t *opt_metadata = nullptr;

  switch(arg.have_tensors) {
    case TensorsFound::NONE:
      RTEnv.Model.evaluate(static_cast<ApproxType>(arg.input_vars[0].data_type),
                           arg.input_vars[0].num_elem, arg.ipts, arg.opts);
      break;
    case TensorsFound::INPUT:
      std::cerr << "Input only not supported yet\n";
      arg.accurateFN(arg.accurateFN_arg);
      // ipt_metadata = static_cast<internal_repr_metadata_t *>(input_vars[0].ptr);
      // RTEnv.Model.evaluate(static_cast<ApproxType>(input_vars[0].data_type),
                          //  input_vars[0].num_elem, ipt_metadata->Tensors[0], opts);
      break;
    case TensorsFound::OUTPUT:
      std::cerr << "Output only not supported yet\n";
      arg.accurateFN(arg.accurateFN_arg);
      // RTEnv.Model.evaluate(static_cast<ApproxType>(output_vars[0].data_type),
                          //  output_vars[0].num_elem, ipts, opts);
      break;
    case TensorsFound::BOTH:
      ipt_metadata = static_cast<internal_repr_metadata_t *>(arg.input_vars[0].ptr);
      opt_metadata = static_cast<internal_repr_metadata_t *>(arg.output_vars[0].ptr);
      RTEnv.Model.evaluate(static_cast<ApproxType>(arg.input_vars[0].data_type),
                           *ipt_metadata, *opt_metadata);
      break;
  }
}

void printIntArrayRef(TensorImpl::Shape arr) {
    std::cout << "[";
    for (size_t i = 0; i < arr.size(); ++i) {
        std::cout << arr[i];
        if (i < arr.size() - 1) {
            std::cout << ", ";
        }
    }
    std::cout << "]" << std::endl;
}

template <typename T>
void printVector(vector<T> vec) {
  std::cout << "[";
  std::copy(vec.begin(), vec.end(), std::ostream_iterator<int>(std::cout, ", "));
  std::cout << "\b\b]" << std::endl; // Hack to remove the last comma and space
}

size_t get_chunk_shape(TensorImpl::Shape shape, ApproxType DType, std::vector<int64_t>& chunk_vector, size_t chunkSize) {
  size_t element_size = TensorImpl::getElementSizeForType(TensorImpl::getTensorDataTypeTypeFromApproxType(DType));
  int64_t max_elements = chunkSize / element_size;
  size_t num_chunks = 1;
  int64_t curr_elements = 1;
  bool single = false;
  size_t largest_divisor = 1;
  int dim_index = chunk_vector.size() - 1;

  for (auto rit = shape.rbegin(); rit != shape.rend(); ++rit, --dim_index) {
    auto rit_val = *rit;
    if (single) {
      chunk_vector[dim_index] = 1;
      num_chunks *= rit_val;
    } else if (curr_elements * rit_val <= max_elements) {
      curr_elements *= rit_val;
      chunk_vector[dim_index] = rit_val;
    } else if (!single) {
      largest_divisor = 1;
      int64_t start = std::min(static_cast<int64_t>(rit_val / 2), max_elements/curr_elements);
      for (size_t i = start; i >= 2; --i) {
        if (rit_val % i == 0) {
          largest_divisor = i;
          break;
        }
      }
      chunk_vector[dim_index] = largest_divisor;
      num_chunks *= rit_val/largest_divisor;
      single = true;
    } else {
      cerr << "Should not be here" << endl;
    }
  }
  return num_chunks;
}

void update_slices(TensorImpl::Slices& slices, const std::vector<int64_t>& chunk_vector, const TensorImpl::Shape& total_shape, std::vector<int64_t>& index_vector) {
  for (int i = index_vector.size() - 1; i >= 0; --i) {
    if (index_vector[i] + chunk_vector[i] < total_shape[i]) {
      index_vector[i] += chunk_vector[i];
      TensorImpl::modSlices(slices, std::make_pair(index_vector[i], chunk_vector[i] + index_vector[i]), i);
      return; 
    } else {
      index_vector[i] = 0; 
      TensorImpl::modSlices(slices, std::make_pair(0, chunk_vector[i]), i);
    }
  }
}

void deviceToHost(std::mutex& buffer_mutex, std::condition_variable& buffer_cond_var, std::atomic<bool>& data_ready, TensorImpl::tensor_t& shared_buffer, const TensorImpl::tensor_options_t& pin_options, const TensorImpl::tensor_t& t, const TensorImpl::Shape& total_shape, const std::vector<int64_t>& chunk_vector, const TensorImpl::Shape& chunk_shape, std::vector<int64_t>& shared_index_vector, size_t num_chunks, TensorImpl::Slices& slices) {
  TensorImpl::InferenceGuard guard;
  std::vector<int64_t> index_vector(shared_index_vector.size(), 0);
  TensorImpl::tensor_t first_buffer = TensorImpl::empty(chunk_shape, pin_options);
  for (size_t i = 0; i < num_chunks; ++i) {
    auto indexed_tensor = TensorImpl::index(t, slices);
    // first_buffer = TensorImpl::to(indexed_tensor, TensorImpl::CPU);
    first_buffer.copy_(indexed_tensor);
    {
      std::unique_lock<std::mutex> lock(buffer_mutex);
      buffer_cond_var.wait(lock, [&data_ready] { return !data_ready; });
      // shared_buffer = first_buffer.clone();
      shared_buffer.copy_(first_buffer);
      std::copy(index_vector.begin(), index_vector.end(), shared_index_vector.begin());
      data_ready = true;
    }
    buffer_cond_var.notify_one(); 
    update_slices(slices, chunk_vector, total_shape, index_vector);
  }
}

void hostToDisk(std::mutex& buffer_mutex, std::condition_variable& buffer_cond_var, std::atomic<bool>& data_ready, HDF5DB* db, void* region_addr, internal_repr_metadata_t* metadata, TensorImpl::tensor_t& shared_buffer, const TensorImpl::tensor_options_t& no_pin_options, const TensorImpl::Shape& total_shape, const std::vector<int64_t>& chunk_vector, const TensorImpl::Shape& chunk_shape, const std::vector<int64_t>& shared_index_vector, size_t num_chunks, bool is_input) {
  TensorImpl::InferenceGuard guard;
  db->TensorToDBChunkInit(region_addr, total_shape, metadata->underlying_type, chunk_vector, num_chunks, is_input);
  TensorImpl::tensor_t second_buffer = TensorImpl::empty(chunk_shape, no_pin_options);
  std::vector<int64_t> index_vector(shared_index_vector.size(), 0);
  for (size_t i = 0; i < num_chunks; ++i) {
    {
      std::unique_lock<std::mutex> lock(buffer_mutex);
      buffer_cond_var.wait(lock, [&data_ready]{ return data_ready.load(); });
      // second_buffer = shared_buffer.clone();
      second_buffer.copy_(shared_buffer);
      std::copy(shared_index_vector.begin(), shared_index_vector.end(), index_vector.begin());
      data_ready = false;
    }
    buffer_cond_var.notify_one(); 
    db->TensorToDBChunk(region_addr, second_buffer, index_vector, is_input, i == 0);
  }
}

void pipelined_device_to_disk_sync(ml_argdesc_t &arg, internal_repr_metadata_t &ipt_metadata, bool isInput) {
    EventRecorder::GPUEvent comp = EventRecorder::CreateGPUEvent("");
    auto ipt = ipt_metadata.get_wrapped_tensor(0).perform_indirection();
    ApproxType iptType = ipt_metadata.underlying_type;
    std::vector<int64_t> ipt_chunk_vector(TensorImpl::dim(ipt), 0); 
    TensorImpl::Shape ipt_total_shape = ipt.sizes();
    size_t chunkSize = RTEnv.iptChunkSize;
    if(!isInput) {
      chunkSize = RTEnv.optChunkSize;
    }
    size_t ipt_num_chunks = get_chunk_shape(ipt_total_shape, iptType, ipt_chunk_vector, chunkSize); 
    TensorImpl::Shape ipt_chunk_shape = TensorImpl::shapeFromVector(ipt_chunk_vector);

    auto region_addr = RTEnv.db->InstantiateRegion((uintptr_t) arg.accurateFN, arg.region_name);
    HDF5DB *db = static_cast<HDF5DB *>(RTEnv.db);
    
	if(!isInput) {
        comp.recordStart();
        arg.accurateFN(arg.accurateFN_arg);
        comp.recordEnd();
        db->RuntimeToDB(region_addr, comp.elapsedTime());
	}

    TensorImpl::tensor_options_t tensor_options_pin = TensorImpl::tensor_options_t().dtype(TensorImpl::getTensorDataType(ipt)).device(TensorImpl::CPU).pinned_memory(true);
    TensorImpl::tensor_options_t tensor_options_no_pin = TensorImpl::tensor_options_t().dtype(TensorImpl::getTensorDataType(ipt)).device(TensorImpl::CPU);
    auto shared_buffer = TensorImpl::empty(ipt_chunk_shape, tensor_options_no_pin);
    std::vector<int64_t> ipt_shared_index_vector(ipt_chunk_vector.size(), 0);

    std::vector<std::pair<int64_t, int64_t>> ipt_bounds;
    for (const auto& value : ipt_chunk_vector) {
      ipt_bounds.emplace_back(0, value);
    }
    TensorImpl::Slices ipt_slices = TensorImpl::initSlices(ipt_bounds);

    std::mutex buffer_mutex_ipt;
    std::condition_variable buffer_cond_var_ipt;
    std::atomic<bool> data_ready_ipt(false);

    // idtodisk.recordStart();
    std::thread deviceThreadIpt(deviceToHost, std::ref(buffer_mutex_ipt), std::ref(buffer_cond_var_ipt), 
    std::ref(data_ready_ipt), std::ref(shared_buffer), std::ref(tensor_options_pin), std::ref(ipt), 
    std::ref(ipt_total_shape), std::ref(ipt_chunk_vector), std::ref(ipt_chunk_shape), 
    std::ref(ipt_shared_index_vector), ipt_num_chunks, std::ref(ipt_slices)
    );

    std::thread diskThreadIpt(hostToDisk, std::ref(buffer_mutex_ipt), 
    std::ref(buffer_cond_var_ipt), std::ref(data_ready_ipt), 
    db, region_addr, &ipt_metadata, std::ref(shared_buffer), 
    std::ref(tensor_options_no_pin), std::ref(ipt_total_shape), 
    std::ref(ipt_chunk_vector), std::ref(ipt_chunk_shape), 
    std::ref(ipt_shared_index_vector), ipt_num_chunks, isInput
    );  
    deviceThreadIpt.join();
    diskThreadIpt.join();
    // idtodisk.recordEnd();
    // EventRecorder::LogEvent(idtodisk);


}

void ml_offline_train(ml_argdesc_t &arg) {
  internal_repr_metadata_t *ipt_metadata = nullptr;
  internal_repr_metadata_t *opt_metadata = nullptr;

  switch(arg.have_tensors) {
    case TensorsFound::NONE:
      RTEnv.Model.evaluate(static_cast<ApproxType>(arg.input_vars[0].data_type),
                           arg.input_vars[0].num_elem, arg.ipts, arg.opts);
      break;
    case TensorsFound::INPUT:
      std::cerr << "Input only not supported yet\n";
      arg.accurateFN(arg.accurateFN_arg);
      // ipt_metadata = static_cast<internal_repr_metadata_t *>(input_vars[0].ptr);
      // RTEnv.Model.evaluate(static_cast<ApproxType>(input_vars[0].data_type),
                          //  input_vars[0].num_elem, ipt_metadata->Tensors[0], opts);
      break;
    case TensorsFound::OUTPUT:
      std::cerr << "Output only not supported yet\n";
      arg.accurateFN(arg.accurateFN_arg);
      // RTEnv.Model.evaluate(static_cast<ApproxType>(output_vars[0].data_type),
                          //  output_vars[0].num_elem, ipts, opts);
      break;
    case TensorsFound::BOTH:
      EventRecorder::GPUEvent total = EventRecorder::CreateGPUEvent("TOTAL TIME");
      // EventRecorder::GPUEvent idtodisk = EventRecorder::CreateGPUEvent("Input Device to Disk");
      // EventRecorder::GPUEvent odtodisk = EventRecorder::CreateGPUEvent("Output Device to Disk");
      total.recordStart();
      TensorImpl::InferenceGuard guard;
      ipt_metadata = static_cast<internal_repr_metadata_t *>(arg.input_vars[0].ptr);
      opt_metadata = static_cast<internal_repr_metadata_t *>(arg.output_vars[0].ptr);
      pipelined_device_to_disk_sync(arg, *ipt_metadata, /*isinput=*/ true);
      pipelined_device_to_disk_sync(arg, *opt_metadata, /*isinput=*/ false);

      total.recordEnd();
      EventRecorder::LogEvent(total);
      break;
  }
}

void ml_invoke(MLType type, void (*accurateFN)(void *), void *arg,
               const char *region_name, void *inputs, int num_inputs,
               void *outputs, int num_outputs) {
  approx_var_info_t *input_vars = (approx_var_info_t *)inputs;
  approx_var_info_t *output_vars = (approx_var_info_t *)outputs;

  TensorsFound have_tensors = TensorsFound::NONE;

  if(region_name == nullptr){
    region_name = "unknown";
  }

    if(input_vars[0].is_tensor) {
      assert(num_inputs == 1 && "Only one tensor input is supported");
      have_tensors = TensorsFound::INPUT;
    }
    if(output_vars[0].is_tensor) {
      if(have_tensors == TensorsFound::INPUT) {
        have_tensors = TensorsFound::BOTH;
      } else {
        have_tensors = TensorsFound::OUTPUT;
      }
    }

    std::vector<void *> ipts;
    std::vector<void *> opts;

    if (have_tensors != TensorsFound::NONE) {
      ipts.reserve(num_inputs);
      opts.reserve(num_outputs);

      for (int i = 0; i < num_inputs; i++) {
        ipts.push_back(input_vars[i].ptr);
      }
      for (int i = 0; i < num_outputs; i++) {
        opts.push_back(output_vars[i].ptr);
      }
    }

    ml_argdesc_t ml_arg = {accurateFN, arg, region_name, have_tensors,
                           input_vars, output_vars, ipts, opts};

  if(type == ML_INFER) {
    ml_infer(ml_arg);
  } else if(type == ML_ONLINETRAIN) {
    std::cerr << "Online training not supported yet\n";
    accurateFN(arg);
  } else if(type == ML_OFFLINETRAIN) {
    ml_offline_train(ml_arg);
  } else {
    std::cerr << "Unknown ML type\n";
    accurateFN(arg);
  }
}

void __approx_exec_call(void (*accurateFN)(void *), void (*perfoFN)(void *),
                        void *arg, bool cond, const char *region_name,
                        void *perfoArgs, int memo_type, int petru_type,
                        int ml_type, void *inputs,
                        int num_inputs, void *outputs, int num_outputs) {
  approx_perfo_info_t *perfo = (approx_perfo_info_t *)perfoArgs;
  approx_var_info_t *input_vars = (approx_var_info_t *)inputs;
  approx_var_info_t *output_vars = (approx_var_info_t *)outputs;

  TensorsFound have_tensors = TensorsFound::NONE;

  if (petru_type & PETRUBATE_IN){
    petrubate(accurateFN, input_vars, num_inputs, region_name);
  }

  if ( perfoFN ){
      perforate(accurateFN, perfoFN, arg, input_vars, num_inputs, output_vars, num_outputs, RTEnv.getExecuteBoth());
  } else if (memo_type == MEMO_IN) {
    memoize_in(accurateFN, arg, input_vars, num_inputs, output_vars,
               num_outputs, RTEnv.getExecuteBoth(), RTEnv.tableSize, RTEnv.threshold );
  } else if (memo_type == MEMO_OUT) {
    memoize_out(accurateFN, arg, output_vars, num_outputs);
  } 
  else if (is_ml((MLType) ml_type)){
    ml_invoke((MLType) ml_type, accurateFN, arg, region_name, inputs, num_inputs, outputs, num_outputs);
  } else if(petru_type & PETRUBATE_OUT){
    petrubate(accurateFN, output_vars, num_outputs, region_name);
  } else {
    std::cerr << "Unknown execution type\n";
    accurateFN(arg);
  }
}


const float approx_rt_get_percentage(){
  return RTEnv.perfoRate;
}

const int approx_rt_get_step(){
  return RTEnv.perfoStep;
}
