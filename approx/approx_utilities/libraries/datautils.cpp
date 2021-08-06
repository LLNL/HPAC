//===--- datautils.cpp - allocation utilities with mixed prec data  ----------------------===//
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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <chrono>
#include <iostream>
#include "datautils.hpp"

#include "IO.hpp"
#include <stddef.h>

std::chrono::time_point<std::chrono::high_resolution_clock> start_time;
std::chrono::time_point<std::chrono::high_resolution_clock> end_time;
std::chrono::duration<double> elapsed;

int israndInit = 0;
struct timeval time_start, time_end; 

void AllocateAndInitData(double*& ptr, int len) {
    ptr = new double[len];
    InitData(ptr, len);
}

void AllocateAndInitData(float *& ptr, int len) {
    ptr = new float[len];
    InitData(ptr, len);
}



void AllocateAndInitDataConst(double*& ptr, int len, double val) {
    ptr = new double[len];
    InitDataConst(ptr, len, val);
}

void AllocateAndInitDataConst(float*& ptr, int len, float val) {
    ptr = new float[len];
    InitDataConst(ptr, len, val);
}

void AllocateAndInitDataConst(float*& ptr, int len, double val) {
    ptr = new float [len];
    InitDataConst(ptr, len, val);
}

void InitData(double*& ptr, int len) {
    srand(42);
    for (int i = 0; i < len; i++) {
        ptr[i] = double(rand())/(double)RAND_MAX;
    }
}


void InitData(float*& ptr, int len) {
    srand(42);
    for (int i = 0; i < len; i++) {
        ptr[i] = float(rand())/(float)RAND_MAX;
    }
}

void InitDataConst(double*& ptr, int len, double val) {
    for (int i = 0; i < len; i++) {
        ptr[i] = val;
    }
}

void InitDataConst(float *& ptr, int len, double val) {
    for (int i = 0; i < len; i++) {
        ptr[i] = (float) val;
    }
}

void InitDataConst(float *& ptr, int len, float val) {
    for (int i = 0; i < len; i++) {
        ptr[i] = val;
    }
}

void InitData(double& d) {
    d =  double(rand())/(double)RAND_MAX;
}


void InitData(float & d) {
    d =  float (rand())/(float)RAND_MAX;
}

void DeallocateData(double*& ptr) {
    if (ptr) {
        delete [] ptr;
        ptr = 0;
    }
}

void DeallocateData(float *& ptr) {
    if (ptr) {
        delete [] ptr;
        ptr = 0;
    }
}

void usage(int argc, char *argv[]){
    if (argc != 2){
        std::cout<<"Execute : " <<argv[0] << " 'output name'"<<endl;
        exit(0);
    }
}

void writeData(double* ptr, size_t size, int type, const char *const_name){
    int len = strlen(const_name);
    char *name = (char *) malloc (sizeof(char)*(len+1));
    strncpy(name, const_name,len);
    name[len]='\0';

    if ( type == DOUBLE){
        writeQualityFile(name, ptr, DOUBLE, size);
    }
    //This is a case in which some kind of analysis changed the data type
    //from float to double. But the original output should be in float type.
    //So I am converting back. 
    else if ( type == FLOAT ){
        float *tmp = (float *) malloc (sizeof(float )*size);
        size_t i;
        for ( i = 0; i < size; i++){
            tmp[i] = (float) ptr[i];
        }
        writeQualityFile(name,tmp,FLOAT,size);
        free(tmp);
        tmp = NULL;
    }
    free(name);
}

void writeData(float* ptr, size_t size, int type,  const char *const_name){
    int len = strlen(const_name);
    char *name = (char *) malloc (sizeof(char)*(len+1));
    strncpy(name, const_name,len);
    name[len]='\0';
    if ( type == DOUBLE){
        double *tmp = (double *) malloc (sizeof(double)*size);
        assert(tmp && "Could not allocate temporal buffer");
        size_t i;

        for ( i = 0; i < size; i++){
            tmp[i] = (double) ptr[i];
        }
        writeQualityFile(name,(void*) tmp, DOUBLE, size);
        free(tmp);
    }
    else if ( type == FLOAT) {
        writeQualityFile(name,(void*) ptr, FLOAT, size);
    }
    free(name);
    return;
}

void writeData(int* ptr, size_t size, int type,  const char *const_name){
    int len = strlen(const_name);
    char *name = (char *) malloc (sizeof(char)*(len+1));
    strncpy(name, const_name,len);
    name[len]='\0';
    size_t i;
    if ( type == DOUBLE){
        double *tmp = (double *) malloc (sizeof(double)*size);
        assert(tmp && "Could not allocate temporal buffer");
        for ( i = 0; i < size; i++){
            tmp[i] = (double) ptr[i];
        }
        writeQualityFile(name,(void*) tmp, DOUBLE, size);
        free(tmp);
    }
    else if ( type == FLOAT) {
        float *tmp = (float *) malloc (sizeof(double)*size);
        assert(tmp && "Could not allocate temporal buffer");
        for ( i = 0; i < size; i++){
            tmp[i] = (float) ptr[i];
        }
        writeQualityFile(name,(void*) tmp, FLOAT, size);
        free(tmp);
    }
    else if ( type == INT){
        writeQualityFile(name,(void*) ptr,INT , size);
    }
    free(name);
    return;
}



void writeData(double* ptr, size_t size, int type, char *name){
    if ( type == DOUBLE){
        writeQualityFile(name, ptr, DOUBLE, size);
    }
    //This is a case in which some kind of analysis changed the data type
    //from float to double. But the original output should be in float type.
    //So I am converting back. 
    else if ( type == FLOAT ){
        float *tmp = (float *) malloc (sizeof(float )*size);
        size_t i;
        for ( i = 0; i < size; i++){
            tmp[i] = (float) ptr[i];
        }
        writeQualityFile(name,tmp,FLOAT,size);
        free(tmp);
        tmp = NULL;
    }
}

void writeData(float* ptr, size_t size, int type,  char *name){
    if ( type == DOUBLE){
        double *tmp = (double *) malloc (sizeof(double)*size);
        assert(tmp && "Could not allocate temporal buffer");
        size_t i;

        for ( i = 0; i < size; i++){
            tmp[i] = (double) ptr[i];
        }
        writeQualityFile(name,(void*) tmp, DOUBLE, size);
        free(tmp);
    }
    else if ( type == FLOAT) {
        writeQualityFile(name,(void*) ptr, FLOAT, size);
    }
    return;
}

void writeData(int* ptr, size_t size, int type,  char *name){
    size_t i;
    if ( type == DOUBLE){
        double *tmp = (double *) malloc (sizeof(double)*size);
        assert(tmp && "Could not allocate temporal buffer");
        for ( i = 0; i < size; i++){
            tmp[i] = (double) ptr[i];
        }
        writeQualityFile(name,(void*) tmp, DOUBLE, size);
        free(tmp);
    }
    else if ( type == FLOAT) {
        float *tmp = (float *) malloc (sizeof(double)*size);
        assert(tmp && "Could not allocate temporal buffer");
        for ( i = 0; i < size; i++){
            tmp[i] = (float) ptr[i];
        }
        writeQualityFile(name,(void*) tmp, FLOAT, size);
        free(tmp);
    }
    else if ( type == INT){
        writeQualityFile(name,(void*) ptr,INT , size);
    }
    return;
}

void writeDataSize(size_t size, FILE *fd){
    assert(fd);
    fwrite(&size, sizeof(size_t),1,fd);
    return;
}

void writeDataType(int type, FILE  *fd){
    if ( type <0 || type > 3){
        printf("Type not supported for quality file exiting\n");
        exit(-1);
    }
    assert(fd);
    fwrite(&type, sizeof(int), 1, fd);
}

void writeDataValue(double val, int type, FILE *fptr){
    if ( type == DOUBLE) {
        fwrite(&val, sizeof(double), 1, fptr);
    }else if ( type == FLOAT){
        float tmp = (float)val;
        fwrite(&tmp, sizeof(float), 1, fptr);
    }else if ( type == INT){
        int tmp = (int)val;
        fwrite(&tmp, sizeof(int), 1, fptr);
    }else if ( type == LONG){
        long tmp = (long) val;
        fwrite(&tmp, sizeof(long), 1, fptr);
    }else{
        printf("Not supported data type exiting...\n");
        exit(0);
    }
}

void writeDataValue(float val, int type, FILE *fptr){
    if ( type == DOUBLE) {
        double tmp = (double) val;
        fwrite(&tmp, sizeof(double), 1, fptr);
    }else if ( type == FLOAT){
        fwrite(&val, sizeof(float), 1, fptr);
    }else if ( type == INT){
        int tmp = (int)val;
        fwrite(&tmp, sizeof(int), 1, fptr);
    }else if ( type == LONG){
        long tmp = (long) val;
        fwrite(&tmp, sizeof(long), 1, fptr);
    }else{
        printf("Not supported data type exiting...\n");
        exit(0);
    }
}

void writeDataValue(long val, int type, FILE *fptr){
    if ( type == DOUBLE) {
        double tmp = (double) val;
        fwrite(&tmp, sizeof(double), 1, fptr);
    }else if ( type == FLOAT){
        float tmp = (float) val;
        fwrite(&tmp, sizeof(float), 1, fptr);
    }else if ( type == INT){
        int tmp = (int)val;
        fwrite(&tmp, sizeof(int), 1, fptr);
    }else if ( type == LONG){
        fwrite(&val, sizeof(long), 1, fptr);
    }else{
        printf("Not supported data type exiting...\n");
        exit(0);
    }
}

void writeDataValue(int val, int type, FILE *fptr){
    if ( type == DOUBLE) {
        double tmp = (double) val;
        fwrite(&tmp, sizeof(double), 1, fptr);
    }else if ( type == FLOAT){
        float tmp = (float) val;
        fwrite(&tmp, sizeof(float), 1, fptr);
    }else if ( type == INT){
        fwrite(&val, sizeof(int), 1, fptr);
    }else if ( type == LONG){
        long tmp = (long) val;
        fwrite(&tmp, sizeof(long), 1, fptr);
    }else{
        printf("Not supported data type exiting...\n");
        exit(0);
    }
}

extern "C" {
void startMeasure(){
    start_time= std::chrono::high_resolution_clock::now();
}

void stopMeasure(){
    end_time = std::chrono::high_resolution_clock::now();
    elapsed = (end_time - start_time);
    printf("RUN_TIME:%g\n", elapsed.count());
}
}


void MP_memcpy(double *dst, double *src, size_t elements){
    memcpy(dst,src,sizeof(double)*elements);
}

void MP_memcpy(float *dst, float *src, size_t elements){
    memcpy(dst,src,sizeof(float)*elements);
}

void MP_memcpy(double *dst, float *src, size_t elements){
    size_t i;
    for ( i = 0 ; i < elements; i++) 
        dst[i] = (double) src[i];
}

void MP_memcpy(float *dst, double *src, size_t elements){
    size_t i;
    for ( i = 0 ; i < elements; i++) 
        dst[i] = (float) src[i];
}

void *MP_Malloc(size_t size, float* val){
    float *ptr = (float*) malloc (sizeof(float)*size); 
    return (void *)ptr;
}

void **MP_Malloc(int size, float **val){
    float **ptr = (float**) malloc(sizeof(float*)*size);
    return (void**) ptr;
}

void **MP_Malloc(int size, double **val){
    double **ptr = (double**) malloc (sizeof(double*)*size);
    return (void**)ptr;
}

void *MP_Malloc(size_t size, double *val){
    double *ptr = (double*) malloc (sizeof(double)*size);
    return (void *) ptr;
}

void *MP_Malloc(int size, float *val){
    float *ptr = (float*) malloc (sizeof(float)*size);
    return (void *) ptr;
}

void *MP_Malloc(int size, double *val){
    double *ptr = (double*) malloc (sizeof(double)*size);
    return (void *)ptr;
}

void MP_Malloc2D(int sizeY, int sizeX, double ***mt){
    int i;
    *mt = (double**)malloc(sizeY*sizeof(double*));
    (*mt)[0] = (double*) malloc (sizeX*sizeY*sizeof(double));
    for (i=1; i<sizeY; i++)
        (*mt)[i] = (*mt)[i-1] + sizeX;
}

void MP_Malloc3D(int sizeZ, int sizeY, int sizeX, double ****mt){
    int i,j;
    (*mt) =(double***)malloc(sizeZ* sizeof(double**));
    (*mt)[0] =(double**) malloc(sizeZ*sizeY * sizeof(double*));
    for (i=1; i<sizeZ; i++)
        (*mt)[i] = (*mt)[i-1] + sizeY;
    for (i=0; i<sizeZ; i++){
        for (j=0; j<sizeX; j++)
            (*mt)[i][j] = (double*)calloc(sizeX, sizeof(double));
    }
}

void MP_Malloc2D(int sizeY, int sizeX, float ***mt){
    int i;
    *mt = (float**)malloc(sizeY*sizeof(float*));
    (*mt)[0] = (float*) malloc (sizeX*sizeY*sizeof(float));
    for (i=1; i<sizeY; i++)
        (*mt)[i] = (*mt)[i-1] + sizeX;
}

void MP_Malloc3D(int sizeZ, int sizeY, int sizeX, float ****mt){
    int i,j;
    (*mt) =(float***)malloc(sizeZ* sizeof(float**));
    (*mt)[0] =(float**) malloc(sizeZ*sizeY * sizeof(float*));
    for (i=1; i<sizeZ; i++)
        (*mt)[i] = (*mt)[i-1] + sizeY;
    for (i=0; i<sizeZ; i++){
        for (j=0; j<sizeX; j++)
            (*mt)[i][j] = (float*)calloc(sizeX, sizeof(float));
    }
}


void MP_Malloc2D(size_t sizeY, size_t sizeX, double ***mt){
    size_t i;
    *mt = (double**)malloc(sizeY*sizeof(double*));
    (*mt)[0] = (double*) malloc (sizeX*sizeY*sizeof(double));
    for (i=1; i<sizeY; i++)
        (*mt)[i] = (*mt)[i-1] + sizeX;
}

void MP_Malloc3D(size_t sizeZ, size_t sizeY, size_t sizeX, double ****mt){
    size_t i,j;
    (*mt) =(double***)malloc(sizeZ* sizeof(double**));
    (*mt)[0] =(double**) malloc(sizeZ*sizeY * sizeof(double*));
    for (i=1; i<sizeZ; i++)
        (*mt)[i] = (*mt)[i-1] + sizeY;
    for (i=0; i<sizeZ; i++){
        for (j=0; j<sizeX; j++)
            (*mt)[i][j] = (double*)calloc(sizeX, sizeof(double));
    }
}

void MP_Malloc2D(size_t sizeY, size_t sizeX, float ***mt){
    size_t i;
    *mt = (float**)malloc(sizeY*sizeof(float*));
    (*mt)[0] = (float*) malloc (sizeX*sizeY*sizeof(float));
    for (i=1; i<sizeY; i++)
        (*mt)[i] = (*mt)[i-1] + sizeX;
}

void MP_Malloc3D(size_t sizeZ, size_t sizeY, size_t sizeX, float ****mt){
    size_t i,j;
    (*mt) =(float***)malloc(sizeZ* sizeof(float**));
    (*mt)[0] =(float**) malloc(sizeZ*sizeY * sizeof(float*));
    for (i=1; i<sizeZ; i++)
        (*mt)[i] = (*mt)[i-1] + sizeY;
    for (i=0; i<sizeZ; i++){
        for (j=0; j<sizeX; j++)
            (*mt)[i][j] = (float*)calloc(sizeX, sizeof(float));
    }
}

void initDataRandom(double *ptr, size_t size){
    size_t i;
    if ( israndInit == 0){
        srand(7);
        israndInit = 1;
    }

    for ( i = 0; i < size; i++){
        ptr[i] = (double) rand() / (double)RAND_MAX;
    }
}

void initDataRandom(float *ptr, size_t size){
    size_t i;
    if ( israndInit == 0){
        srand(7);
        israndInit = 1;
    }

    for ( i = 0; i < size; i++){
        ptr[i] = (double) rand() / (double)RAND_MAX;
    }
}

void initDataRandom(double *ptr, int size){
    int i;
    if ( israndInit == 0){
        srand(7);
        israndInit = 1;
    }

    for ( i = 0; i < size; i++){
        ptr[i] = (double) rand() / (double)RAND_MAX;
    }
}

void initDataRandom(float *ptr, int size){
    int i;
    if ( israndInit == 0){
        srand(7);
        israndInit = 1;
    }

    for ( i = 0; i < size; i++){
        ptr[i] = (double) rand() / (double)RAND_MAX;
    }
}

void addLatency(double val){
    return;
}

void addLatency(float val){
    sleep(1);
}

void MP_MemSet(float *ptr, int elements){
    memset(ptr,0, elements*sizeof(float));
}

void MP_MemSet(double *ptr, int elements){
    memset(ptr,0, elements*sizeof(double));
}

void MP_MemSet(float *ptr, long elements){
    memset(ptr,0, elements*sizeof(float));
}

void MP_MemSet(double *ptr, long elements){
    memset(ptr,0, elements*sizeof(double));
}
