#include <cmath>
#include <iostream>
#include <stdint.h>

#include "approx_data_util.h"
#include "approx_internal.h"

/**
 * add vectors and store output to another vector.
 *
 * @param  sum vector to store the result.
 * @param  augend the augend of the operation.
 * @param  addend the addend of the operation.
 * @param  type type of the data.
 * @param  numelements number of elements within the vector.
 * @return "sum" holds the result of the operation.
 */
void add(void *sum, void *augend, void *addend, ApproxType Type,
         size_t numElements) {
  if (numElements == 1) {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    (*(CType *)sum) = (*(CType *)augend) + (*(CType *)addend);                 \
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
    return add((CType *)sum, (CType *)augend, (CType *)addend, numElements);
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  }
}

/**
 * subtract vectors and store output to another vector.
 *
 * @param  difference vector to store the result.
 * @param  minuend the minuend of the operation.
 * @param  subtrahend the subtrahend of the operation.
 * @param  type type of the data.
 * @param  numelements number of elements within the vector.
 * @return "difference" holds the result of the operation.
 */
void sub(void *difference, void *minuend, void *subtrahend, ApproxType Type,
         size_t numElements) {
  if (numElements == 1) {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    (*(CType *)difference) = (*(CType *)minuend) - (*(CType *)subtrahend);     \
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
    return sub((CType *)difference, (CType *)minuend, (CType *)subtrahend,     \
               numElements);
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  }
}

/**
 * multiply vectors and store output to another vector.
 *
 * @param  product vector to store the result.
 * @param  multiplier the multiplier of the operation.
 * @param  multiplicand the multiplicand of the operation.
 * @param  type type of the data.
 * @param  numelements number of elements within the vector.
 * @return "product" holds the result of the operation.
 */
void multiply(void *product, void *multiplier, void *multiplicand,
              ApproxType Type, size_t numElements) {
  if (numElements == 1) {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    (*(CType *)product) = (*(CType *)multiplier) * (*(CType *)multiplicand);   \
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
    return multiply((CType *)product, (CType *)multiplier,                     \
                    (CType *)multiplicand, numElements);
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  }
}

/**
 * divide vectors and store output to another vector.
 *
 * @param  quotient vector to store the result.
 * @param  dividend the dividend of the operation.
 * @param  divisor the divisor of the operation.
 * @param  type type of the data.
 * @param  numelements number of elements within the vector.
 * @return "quotinent" holds the result of the operation.
 */
void divide(void *quotient, void *dividend, void *divisor, ApproxType Type,
            size_t numElements) {
  if (numElements == 1) {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    (*(CType *)quotient) = (*(CType *)dividend) / (*(CType *)divisor);         \
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
    return divide((CType *)quotient, (CType *)dividend, (CType *)divisor,      \
                  numElements);
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  }
}

/**
 * Function returing a printable representation of the specified type.
 *
 * @param  Type Type of the data.
 * @return const char * of this data.
 */
const char *getTypeName(ApproxType Type) {
  switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    return nameOfType;
#include "clang/Basic/approxTypes.def"
  case INVALID:
    return "INVALID";
  }
}

bool rel_error_larger(void *ground, void *test, size_t numElements,
                      ApproxType Type, real_t threshold) {
  real_t temp;
  if (numElements == 1) {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    temp = (real_t)((*(CType *)ground) - (*(CType *)test)) /                   \
           (real_t(*(CType *)ground));                                         \
    return fabs(temp) > threshold;
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  } else {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    return rel_error_larger((CType *)ground, (CType *)test, numElements,       \
                            threshold);
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  }
  return 0.0;
}

/**
 * Average numbers of an opaque vector.
 *
 * @param  dataPtr data values to be accumulated.
 * @param  numElements number of elements within the vector.
 * @param  Type Type of the data.
 * @return average of `dataPtr`, or 0.0 if `dataPtr` is empty.
 */
double average(void *dataPtr, size_t numElements, ApproxType Type) {
  if (dataPtr == nullptr)
    return 0.0;

  if (numElements == 1) {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    return (double)(*(CType *)dataPtr);
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  } else {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    return average((CType *)dataPtr, numElements);
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  }
  return 0.0;
}

void copyData(void *dest, void *src, size_t numElements, ApproxType Type) {
  if (numElements == 1) {
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    *((CType *)dest) = *((CType *)src);                                        \
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
    return copyData((CType *)dest, (CType *)src, numElements);
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  }
}

float aggregate( void *data, size_t numElements, ApproxType Type){
    switch (Type) {
#define APPROX_TYPE(Enum, CType, nameOfType)                                   \
  case Enum:                                                                   \
    return aggregate((CType *)data, numElements);
#include "clang/Basic/approxTypes.def"
    case INVALID:
      std::cout << "INVALID DATA TYPE passed in argument list\n";
      break;
    }
  return 0.0f;
}
