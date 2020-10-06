#ifndef __APPROX_DATA_UTIL__
#define __APPROX_DATA_UTIL__

#include <iostream>
#include <stdint.h>

#include <approx_internal.h>

void add(void *sum, void *augend, void *addend, ApproxType Type, size_t numElements);
void sub(void *difference, void *minuend, void *subtrahend, ApproxType Type, size_t numElements);
void multiply(void *product, void *multiplier, void *multiplicand, ApproxType Type, size_t numElements);
void divide(void *quotient, void *dividend, void *divisor, ApproxType Type, size_t numElements);
double average(void *dataPtr, size_t numElements, ApproxType Type);
const char *getTypeName(ApproxType Type);

#endif