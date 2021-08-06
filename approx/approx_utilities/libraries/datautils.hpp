//===--- datautils.hpp - allocation utilities with mixed prec data  ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This files is an allocation layer utility file
///
//===----------------------------------------------------------------------===//

#ifndef _LOOPS_DATA_UTILS_
#define _LOOPS_DATA_UTILS_
#include <stdlib.h>

#include "IO.hpp"
using namespace std;

void AllocateAndInitData(double*& ptr, int len);
void AllocateAndInitData(float*& ptr, int len);

void AllocateAndInitDataConst(double*& ptr, int len, double val);
void AllocateAndInitDataConst(float*& ptr, int len, double val);
void AllocateAndInitDataConst(float*& ptr, int len, float val);

void InitData(double*& ptr, int len);
void InitData(float*& d, int len);

void InitData(double& d);
void InitData(float& d);

void InitDataConst(double*& ptr, int len, double val);
void InitDataConst(float*& ptr, int len, double val);
void InitDataConst(float*& ptr, int len, float val);

void DeallocateData(double*& ptr);
void DeallocateData(float*& ptr);

void usage(int argc, char *argv[]);
void writeData(double* ptr, size_t size, int type, char *name);
void writeData(float* ptr, size_t size, int type, char *name);
void writeData(int* ptr, size_t size, int type, char *name);

void writeData(double* ptr, size_t size, int type, const char *const_name);
void writeData(float* ptr, size_t size, int type, const char *const_name);
void writeData(int* ptr, size_t size, int type, const char *const_name);

void writeDataSize(size_t size,FILE *fptr);   
void writeDataType(int type,FILE *fptr);   

void writeDataValue(double val, int type, FILE *fd);
void writeDataValue(float val, int type, FILE *fd);
void writeDataValue(int val, int type, FILE *fd);

void MP_memcpy(float *dst, double *src, size_t elements);
void MP_memcpy(double *dst, float *src, size_t elements);
void MP_memcpy(float *dst, float *src, size_t elements);
void MP_memcpy(double *dst, double *src, size_t elements);

void *MP_Malloc(size_t size, float *);
void *MP_Malloc(size_t size, double *);
void *MP_Malloc(int size, float *);
void *MP_Malloc(int size, double *);

void** MP_Malloc(int size, float **);
void** MP_Malloc(int size, double **);

void MP_Malloc2D(int sizeY, int sizeX, double ***mt);
void MP_Malloc2D(int sizeY, int sizeX, float ***mt);
void MP_Malloc2D(size_t sizeY, size_t sizeX, double ***mt);
void MP_Malloc2D(size_t sizeY, size_t sizeX, float ***mt);

void MP_Malloc3D(int sizeZ, int sizeY, int sizeX, float ****mt);
void MP_Malloc3D(int sizeZ, int sizeY, int sizeX, double ****mt);
void MP_Malloc3D(size_t sizeZ, size_t sizeY, size_t sizeX, double ****mt);
void MP_Malloc3D(size_t sizeZ, size_t sizeY, size_t sizeX, float ****mt);

void MP_MemSet(float *ptr, int numElements);
void MP_MemSet(double *ptr, int numElements);
void MP_MemSet(float *ptr, long numElements);
void MP_MemSet(double *ptr, long numElements);

void initDataRandom(float *ptr, size_t size);
void initDataRandom(double *ptr, size_t size);
void initDataRandom(double *ptr, int size);
void initDataRandom(float *ptr, int size);

void addLatency(float val);
void addLatency(double val);

void MP_read(FILE *fd, int type, int *ptr, int numElements);
void MP_read(FILE *fd, int type, float *ptr, int numElements);
void MP_read(FILE *fd, int type, double *ptr, int numElements);

#endif
