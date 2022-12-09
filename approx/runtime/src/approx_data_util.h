//===--- approx_data_util.h -  performs operations on opaque data types ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This files performs operations on opaque data types 
///
//===----------------------------------------------------------------------===//


#ifndef __APPROX_DATA_UTIL__
#define __APPROX_DATA_UTIL__

#include <cmath>
#include <iostream>
#include <stdint.h>
#include <cstring>
#include <omp.h>

#include "approx_internal.h"

template<typename T>
float aggregate(T *ptr, size_t numElements){
  float total = 0.0f;
  for (size_t i = 0; i < numElements; i++){
    total += (float) ptr[i];
  }
  return total;
}

template <typename T>
 void add(T *sum, T *augend, T *addend, size_t numElements) {
  for (size_t i = 0; i < numElements; i++) {
    sum[i] = augend[i] + addend[i];
  }
  return;
}

template <typename T>
 void sub(T *difference, T *minuend, T *subtrahend, size_t numElements) {
  for (size_t i = 0; i < numElements; i++) {
    difference[i] = minuend[i] - subtrahend[i];
  }
}

template <typename T>
 void multiply(T *product, T *multiplier, T *multiplicand,
                     size_t numElements) {
  for (size_t i = 0; i < numElements; i++) {
    product[i] = multiplier[i] * multiplicand[i];
  }
}

template <typename T>
 void divide(T *quotient, T *dividend, T *divisor, size_t numElements) {
  for (size_t i = 0; i < numElements; i++) {
    quotient[i] = dividend[i] / divisor[i];
  }
  return;
}

template <typename T>
 bool rel_error_larger(T *ground, T *test, size_t numElements,
                             real_t threshold) {
  for (size_t i = 0; i < numElements; i++) {
    real_t temp = (real_t) fabs((ground[i] - test[i]) / (real_t)ground[i]);
    if (temp > threshold) {
      return true;
    }
  }
  return false;
}

template <typename T> double average(T *data, size_t numElements) {
  double sum = 0.0;
  for (size_t i = 0; i < numElements; i++) {
    sum += (double)data[i];
  }
  sum /= (double)numElements;
  return sum;
}

template <typename T>  void copyData(T *dest, T *src, size_t numElements) {
    std::memcpy(dest, src, numElements*sizeof(T));
    return;
}

template <typename To, typename From> 
 void cast_and_assign(To *dest, From* src, size_t numElements){
  for (size_t i = 0; i < numElements; i++){
    dest[i] = (To) src[i];
  }
}

//template <typename To, typename From> 
  //void cast_and_assign_device(To *dest, From* src, size_t numElements){
  //#pragma omp target data map(to:numElements)
  //  {
  //#pragma omp target teams distribute parallel for
  //    for (int i = 0; i < numElements; i++){
  //      dest[i] = (To) src[i];
  //    }
  //  }
  //}


template <typename T>
void cast_and_assign(void *src, size_t numElements,
                      ApproxType Type, T *dest) {
  if (numElements == 1) {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    *dest = (T)(*(CType *)src);                                           \
    return;
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  } else {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    return cast_and_assign(dest, (CType *)src, numElements);
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  }
  return;
}

template<typename T>
void convertTo(T *dest, void *src, size_t numElements,
                      ApproxType Type) {
  if (numElements == 1) {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    *dest = (T)(*(CType *)src);                                           \
    return;
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  } else {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    return cast_and_assign(dest, (CType *)src, numElements);
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  }
  return;
}

#pragma omp declare target
template<typename T>
void convertToSingleWithOffset(T *dest, const void *src, size_t dest_offset,
                               size_t src_offset, ApproxType Type){
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    dest[dest_offset] = (T)((CType *)src)[src_offset];                         \
    return;
#include "clang/Basic/approxTypes.def"
    case INVALID:
      break;
    }
}

template<typename T>
void convertFromSingleWithOffset(void *dest, const T *src, size_t dest_offset,
                               size_t src_offset, ApproxType Type){
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    ((CType*)dest)[dest_offset] = (T)src[src_offset];        \
    return;
#include "clang/Basic/approxTypes.def"
    case INVALID:
      break;
    }
}

#pragma omp end declare target

template<typename T>
void convertFrom(void *dest, T *src, size_t numElements, ApproxType Type){
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    return cast_and_assign((CType *)dest, src, numElements);
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  return;
}


template <typename T>
T** create2DArray(unsigned int nrows, unsigned int ncols, const T& val = T() ){
    if ( nrows == 0 ){
        printf("Invalid argument rows are 0\n");
        exit(-1);
    }

    if ( ncols == 0 ){
        printf("Invalid cols are 0\n");
    }
    T **ptr = new T*[nrows];;
    T *pool = new T[nrows*ncols]{val};
    for (unsigned int i = 0; i < nrows; ++i, pool+= ncols){
        ptr[i] = pool;
    }
    return ptr;
}

template<typename T>
void delete2DArray(T** arr){
    delete [] arr[0];
    delete [] arr;
}

template<typename T>
T** createTemp2DVarStorage(approx_var_info_t *vars, int numVars, 
            int rows, int *cols){
    int columns = 0;
    for (int i = 0; i < numVars; i++){
       columns+=vars[i].num_elem; 
    }
    *cols = columns;
    T** ptr = create2DArray<T>(rows, columns); 
    return ptr;
}

template<typename T>
T* createTemp1DVarStorage(approx_var_info_t *vars, int numVars, 
             int *numElements){
    int elements = 0;
    for (int i = 0; i < numVars; i++){
       elements+=vars[i].num_elem; 
    }
    *numElements = elements;
    T* ptr = new T[elements];
    return ptr;
}

#pragma omp declare target
template<typename T>
class AccessWrapper2D
{
public:
  int nrows;
  int ncols;
  T *data;

  AccessWrapper2D(T *_data, int nrow, int ncol) :
    data{_data}, nrows{nrow}, ncols{ncol } {}
  AccessWrapper2D()
    : data{nullptr}, nrows{0}, ncols{0} {}

  T& operator()(int row, int col)
  {
    return data[row*ncols + col];
  }
};
#pragma omp end declare target


template<typename T>
void packVarToVec(approx_var_info_t *values, int num_values, T *vector){
  for (int i = 0; i < num_values; i++){
    if(values[i].stride != 1)
      {
        printf("ERROR: Runtime support for strided input access is not supported\n");
        abort();
      }
    convertTo(vector, values[i].ptr, values[i].num_elem, (ApproxType)values[i].data_type);
    vector += values[i].num_elem;
  }
}

// precond: omp_target_is_present(val, 0) is true for all input values and for vector
template<typename T>
void packVarToDeviceVec(approx_var_info_t *values, int num_values, T *vector)
{
  //  for(int i = 0; i < num_values; i++)
  //    {
  //      ApproxType DT = (ApproxType) values[i].data_type;
  //      void *HostPtr = values[i].ptr;
  //      size_t nElem = values[i].num_elem;
  //    switch (DT) {
  //#define APPROX_TYPE(Enum, CType, nameOfType)                        \
//  case Enum:                                                                   \
//    cast_and_assign_device(vector, (CType *)HostPtr, nElem);           \
//    break;
  //#include "clang/Basic/approxTypes.def"
  //    case INVALID:
  //      std::cout << "INVALID DATA TYPE passed in argument list\n";
  //      break;
  //    }
  //      vector += nElem;
  //    }
}

template<typename T>
void unpackVecToVarDevice(approx_var_info_t *values, int num_values, T *vector){
  //  for(int i = 0; i < num_values; i++){
  //    ApproxType DT = (ApproxType)values[i].data_type;
  //    void *HostPtr = values[i].ptr;
  //    size_t nElem = values[i].num_elem;
  //
  //    switch (DT) {
  //#define APPROX_TYPE(Enum, CType, nameOfType)                        \
//  case Enum:                                                                   \
//    cast_and_assign_device((CType *) HostPtr, vector, nElem);                  \
//    break;
  //#include "clang/Basic/approxTypes.def"
  //    case INVALID:
  //      std::cout << "INVALID DATA TYPE passed in argument list\n";
  //      break;
  //    }
  //      vector += nElem;
  //    }
}

template<typename T>
void unPackVecToVar(approx_var_info_t *values, int num_values, T *vector){
  for (int i = 0; i < num_values; i++){
    convertFrom(values[i].ptr, vector, values[i].num_elem, (ApproxType)values[i].data_type);
    vector += values[i].num_elem;
  }
}

void add(void *sum, void *augend, void *addend, ApproxType Type,
         size_t numElements);
void sub(void *difference, void *minuend, void *subtrahend, ApproxType Type,
         size_t numElements);
void multiply(void *product, void *multiplier, void *multiplicand,
              ApproxType Type, size_t numElements);
void divide(void *quotient, void *dividend, void *divisor, ApproxType Type,
            size_t numElements);

const char *getTypeName(ApproxType Type);
bool rel_error_larger(void *ground, void *test, size_t numElements,
                      ApproxType Type, real_t threshold);

double average(void *dataPtr, size_t numElements, ApproxType Type);
void copyData(void *dest, void *src, size_t numElements, ApproxType Type);
float aggregate( void *data, size_t numElements, ApproxType Type);
#endif
