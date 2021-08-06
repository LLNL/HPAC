//===--- IO.hpp - perform IO operations with mixed prec data  ----------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
/// This files is an abstraction layer to write data to files 
///
//===----------------------------------------------------------------------===//

#ifndef __QUALITY_IO__
#define __QUALITY_IO__
int ReadQualityFiles(char *accurate, char *test,  void **accVals, void **tstVals, size_t *elements);
void writeQualityFile(char *fileName, void *ptr, int type, size_t numElements);
void readData(FILE *fd, int **ptr,    size_t* numElements);
void readData(FILE *fd, float **ptr,  size_t* numElements);
void readData(FILE *fd, double **ptr, size_t* numElements);
void readValue( FILE *fd, int *val );
void readValue( FILE *fd, long *val);

#define DOUBLE 0
#define FLOAT 1
#define INT 2
#define LONG 3
#endif


