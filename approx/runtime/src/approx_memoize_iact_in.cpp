#include <cfloat>
#include <iostream>
#include <memory>
#include <unordered_map>

#include <approx_data_util.h>
#include <approx_internal.h>

using namespace std;

#define MEMORY_BLOCKS 1024

#define MAX_REGIONS 20
#define NOTFOUND -1
class MemoizeInput {

  typedef enum : uint8_t {
    INIT,
    CREATE,
    COPY_IN,
    COPY_OUT,
    SEARCH,
    INSERT,
    ACCURATE,
    APPROXIMATE,
    END
  } CODE_REGIONS;

  struct KeyData {
    MemoizeInput *parent;
    mutable uint8_t *ptr;
    mutable approx_var_info_t *inputs;
    bool operator==(const KeyData &other) const {
      const size_t *offsets = parent->input_offsets;
      const ApproxType *types = parent->input_types;
      const size_t *num_elements = parent->input_shape;
      const real_t threshold = parent->threshold;
      for (int i = 0; i < parent->num_inputs; i++) {
        void *this_ptr =
            (inputs == nullptr) ? (void *)&ptr[offsets[i]] : inputs[i].ptr;
        void *other_ptr = (other.inputs == nullptr)
                              ? (void *)&other.ptr[offsets[i]]
                              : other.inputs[i].ptr;
        if (rel_error_larger(this_ptr, other_ptr, num_elements[i], types[i],
                             threshold))
          return false;
      }
      return true;
    }
  };

  class KeyDataHasher {
  public:
    std::size_t operator()(const KeyData &key) const {
      size_t seed = 5381;
      const size_t *offsets = key.parent->input_offsets;
      const size_t *num_elements = key.parent->input_shape;
      const size_t *sz_types = key.parent->input_sz_type;
      const real_t threshold = key.parent->threshold;

      for (int i = 0; i < key.parent->num_inputs; i++) {
        size_t bytes = sz_types[i] * num_elements[i];
        uint8_t *ptr = (key.inputs == nullptr) ? &key.ptr[offsets[i]]
                                               : (uint8_t *)key.inputs[i].ptr;
        for (size_t j = 0; j < bytes; j++) {
          seed = ((seed << 5) + seed) + ptr[j];
        }
      }
      return seed;
    }
  };

  typedef std::unordered_map<KeyData, uint8_t *, KeyDataHasher> KeyDataHashMap;
  typedef std::unordered_map<KeyData, uint8_t *, KeyDataHasher>::iterator
      KeyDataHashMapIterator;

public:
  void (*accurate)(void *);
  int num_inputs;
  int num_outputs;
  real_t threshold;

  uint8_t *input_memory;
  uint8_t *output_memory;
  size_t input_index;
  size_t output_index;

  size_t *input_shape;
  ApproxType *input_types;
  size_t *input_offsets;
  size_t *input_sz_type;
  size_t total_input_size;
  int *output_shape;
  ApproxType *output_types;
  size_t *output_offsets;
  size_t total_output_size;
  KeyDataHashMap storage;
  std::chrono::duration<double> elapsed[END];

  /// Stat. Counts how many invocations where performed accurately.
  int accurately;
  /// Stat. Counts how many invocations where performed approximately.
  int approximately;
  int counter;

public:
  MemoizeInput(void (*acc)(void *), int num_inputs, int num_outputs,
               approx_var_info_t *inputs, approx_var_info_t *outputs)
      : accurate(acc), num_inputs(num_inputs), num_outputs(num_outputs),
        input_index(0), output_index(0), accurately(0), approximately(0) {

    int i;
    input_shape = new size_t[num_inputs];
    input_types = new ApproxType[num_inputs];
    input_offsets = new size_t[num_inputs];
    input_sz_type = new size_t[num_inputs];

    output_shape = new int[num_outputs];
    output_types = new ApproxType[num_outputs];
    output_offsets = new size_t[num_outputs];

    size_t curr_offset = 0;

    for (i = 0; i < num_inputs; i++) {
      input_shape[i] = inputs[i].num_elem;
      input_types[i] = (ApproxType)inputs[i].data_type;

      input_sz_type[i] = inputs[i].sz_elem;

      size_t rem = curr_offset % inputs[i].sz_elem;
      if (rem != 0)
        curr_offset += inputs[i].sz_elem - rem;

      input_offsets[i] = curr_offset;
      curr_offset += input_shape[i] * inputs[i].sz_elem;
    }

    size_t rem = curr_offset % sizeof(uint64_t);
    if (rem != 0) {
      curr_offset += sizeof(uint64_t) - rem;
    }
    total_input_size = curr_offset;
    size_t elements = total_input_size;
    input_memory = new uint8_t[elements * MEMORY_BLOCKS]();

    curr_offset = 0;
    for (i = 0; i < num_outputs; i++) {
      output_shape[i] = outputs[i].num_elem;
      output_types[i] = (ApproxType)outputs[i].data_type;
      size_t rem = curr_offset % outputs[i].sz_elem;
      if (rem != 0)
        curr_offset += outputs[i].sz_elem - rem;

      output_offsets[i] = curr_offset;
      curr_offset += output_shape[i] * outputs[i].sz_elem;
    }

    rem = curr_offset % sizeof(uint64_t);
    if (rem != 0) {
      curr_offset += sizeof(uint64_t) - rem;
    }
    total_output_size = curr_offset;
    elements = total_output_size;
    output_memory = new uint8_t[elements * MEMORY_BLOCKS]();

    counter = 0;
  }

  ~MemoizeInput() {
    for (int i = 0; i < num_inputs; i++) {
      cout << "Num Elements are :" << input_shape[i] << endl;
    }
    delete[] output_memory;
    delete[] input_memory;

    delete[] input_shape;
    delete[] input_types;
    delete[] input_offsets;
    delete[] input_sz_type;

    delete[] output_types;
    delete[] output_shape;
    delete[] output_offsets;

    cout << "Num Inputs :" << num_inputs << "num outputs " << num_outputs
         << endl;
    cout << "Total Input Size :" << total_input_size << "total_output_size "
         << total_output_size << endl;
    cout << "APPROX:"
         << (double)approximately / (double)(accurately + approximately) << ":"
         << approximately << ":" << accurately << endl;
    cout << "Performance:";
    for (int i = 0; i < END; i++)
      cout << elapsed[i].count() << ":";
    cout << endl;
    cout << "Threshold:" << threshold << endl;
  };

  uint8_t *copy_inputs(approx_var_info_t *inputs) {
    uint8_t *ptr = &input_memory[input_index];
    input_index += total_input_size;
    for (int i = 0; i < num_inputs; i++) {
      copyData((void *)&ptr[input_offsets[i]], inputs[i].ptr, input_shape[i],
               input_types[i]);
    }
    return ptr;
  }

  uint8_t *copy_outputs(approx_var_info_t *outputs) {
    uint8_t *ptr = &output_memory[output_index];
    output_index += total_output_size;
    for (int i = 0; i < num_outputs; i++) {
      copyData((void *)&ptr[output_offsets[i]], (void *)outputs[i].ptr,
               output_shape[i], output_types[i]);
    }
    return ptr;
  }

  void copy_results(uint8_t *values, approx_var_info_t *outputs) {
    for (int i = 0; i < num_outputs; i++) {
      copyData(outputs[i].ptr, (void *)&(values[output_offsets[i]]),
               output_shape[i], output_types[i]);
    }
  }

  void execute(void *args, approx_var_info_t *inputs,
               approx_var_info_t *outputs) {
    KeyData new_values = {this, nullptr, inputs};
    auto ret = storage.insert({new_values, nullptr});

    if (ret.second == false) {
      copy_results((uint8_t *)ret.first->second, outputs);
      approximately++;
    } else {
      (*ret.first).first.ptr = copy_inputs(inputs);
      (*ret.first).first.inputs = nullptr;

      accurately++;
      accurate(args);
      uint8_t *output_ptr = copy_outputs(outputs);
      (*ret.first).second = output_ptr;
    }
    counter++;
  }
};

int last_memoIn = 0;

MemoizeInput *memo_regions[MAX_REGIONS];

class GarbageCollectorMemoIn {
public:
  GarbageCollectorMemoIn(){};
  ~GarbageCollectorMemoIn() {
    for (int i = 0; i < last_memoIn; i++) {
      printf("Deleting memory region\n");
      delete memo_regions[i];
    }
  }
};

GarbageCollectorMemoIn memoCleaner;

int memo_find_index(unsigned long Addr) {
  for (int i = 0; i < last_memoIn; i++) {
    if (((unsigned long)(memo_regions[i]->accurate) == Addr))
      return i;
  }
  return NOTFOUND;
}
void memoize_in(void (*accurate)(void *), void *arg, approx_var_info_t *inputs,
                int num_inputs, approx_var_info_t *outputs, int num_outputs) {
  unsigned long Addr = (unsigned long)accurate;
  int curr_index = memo_find_index(Addr);
  if (curr_index == NOTFOUND) {
    if (last_memoIn >= MAX_REGIONS) {
      std::cout << "I Reached maximum memo_regions exiting\n";
      exit(0);
    }
    curr_index = last_memoIn;
    memo_regions[last_memoIn++] =
        new MemoizeInput(accurate, num_inputs, num_outputs, inputs, outputs);
  }
  memo_regions[curr_index]->execute(arg, inputs, outputs);
}
