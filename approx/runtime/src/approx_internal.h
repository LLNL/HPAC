//===--- approx_internal.h -  data types used by the approximate runtime system ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This file defines datatypes used by the compiler to communicate information to the runtime system
///
//===----------------------------------------------------------------------===//

#ifndef __APPROX_INTERNAL__
#define __APPROX_INTERNAL__
#ifdef __cplusplus
extern "C" {
#endif
#include <stddef.h>
#include <stdint.h>

typedef float real_t;

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

#pragma omp declare target
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
  size_t stride;
  int8_t data_type;
  uint8_t dir;
} approx_var_info_t;

/* typedef struct approx_var_ptr_t { */
/*   void *ptr; */
/*   size_t num_elem; */
/*   size_t stride; */
/* } approx_var_ptr_t; */

typedef struct approx_var_access_t {
  const size_t num_elem;
  const size_t stride;
} approx_var_access_t;


typedef struct approx_region_specification {
  const size_t sz_elem;
  const int8_t data_type;
  const int8_t dir;
} approx_region_specification;

enum ApproxType : int8_t {
#define APPROX_TYPE(Id, type, nameOfType) Id,
#include "clang/Basic/approxTypes.def"
  INVALID
#undef APPROX_TYPE
};
#pragma omp end declare target

void memoize_out(void (*accurate)(void *), void *arg,
                 approx_var_info_t *outputs, int num_outputs);

void memoize_in(void (*accurate)(void *), void *arg, approx_var_info_t *inputs,
                int num_inputs, approx_var_info_t *outputs, int num_outputs, bool execBoth, 
                int tSize, real_t threshold);

void perforate(void (*accurate)(void *), void (*perfo)(void *),
                 void *args, approx_var_info_t *input_vars, 
                 int num_inputs, approx_var_info_t *output_vars,
                 int num_outputs, bool ExecuteBoth);

int getPredictionSize();
int getHistorySize();
int getTableSize();
float getThreshold();

int copy_vars_to_device(approx_var_info_t *vals, int numVals, int destDev, int srcDev);
int copy_var_to_device(approx_var_info_t *vals, int destDev, int srcDev);

#ifdef __cplusplus
}
#endif
#endif
