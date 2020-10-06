#ifndef __APPROX_INTERNAL__
#define __APPROX_INTERNAL__

#include <stddef.h>
#include <stdint.h>
#include <string>

typedef double real_t;

/** @struct approx_perfo_info_t
 *  @brief This structure contains all the information the
 *  developer passed to the programming model on the perfo()
 *  clause.
 *  @var approx_perfo_info_t::type
 *  Member 'type' contains the type of the perforation.
 *  @var approx_perfo_info_t::region
 *  Member 'region' contains a unique id for the
 *  specific region of code.
 *  @var approx_perfo_info_t::step
 *  Member 'step' contains the stride the technique should use.
 *  @var approx_perfo_info_t::rate
 *  Member 'rate' contains a percentage which needs to be skipped.
 */
typedef struct approx_perfo_info_t {
  int type;
  int region;
  int step;
  float rate;
} approx_perfo_info_t;

/** @struct approx_var_info_t
 *  @brief This structure contains all the information the
 *  developer passed to the programming model on the in/out/inout()
 *  clauses.
 *  @var approx_var_info_t::ptr
 *  Member 'ptr' points to the data passed by the developer.
 *  @var approx_var_info_t::num_elem
 *  Member 'num_elem' number of elements.
 *  @var approx_var_info_t:sz_elem:
 *  Member 'sz_elem' size of each element in bytes.
 *  @var approx_var_info_t::data_type
 *  Member 'data_type' type of the data described by respective id.
 *  @var approx_var_info_t::dir
 *  Member 'dir' direction of the data in/out/inout.
 */
typedef struct approx_var_info_t {
  void *ptr;
  size_t num_elem;
  size_t sz_elem;
  int8_t data_type;
  uint8_t dir;
} approx_var_info_t;

enum ApproxType : int8_t {
#define APPROX_TYPE(Id, type, nameOfType) Id,
#include "clang/Basic/approxTypes.def"
  INVALID
#undef APPROX_TYPE
};

void memoize_out(void (*accurate)(void *), void *arg,
                 approx_var_info_t *outputs, int num_outputs);

void memoize_in(void (*accurate)(void *), void *arg, approx_var_info_t *inputs,
                int num_inputs, approx_var_info_t *outputs, int num_outputs);

bool rel_error_larger(void *ground, void *test, size_t numElements,
                       ApproxType Type, real_t threshold);
#endif