#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "datautils.hpp"
#include "IO.hpp"

const char *typeNames[] = { "DOUBLE", "FLOAT", "INT", "LONG"};

int ReadQualityFiles(char *accurate, char *test,  void **accVals, void **tstVals, size_t *elements){
    FILE *acc = fopen(accurate, "rb"); 
    FILE *tst = fopen(test, "rb"); 
    char tmp[1000];
    assert (acc && tst && "Could not open files");
    int i;

    size_t accElements, tstElements;
    fread(&accElements, sizeof(size_t), 1, acc);
    fread(&tstElements, sizeof(size_t), 1, tst);
    printf("Accurate execution results to %d\n", accElements);
    printf("Test execution results to %d\n", tstElements);

    if (accElements != tstElements ){
        printf("test: failed");
        return -1;
    }
    *elements = accElements;

    int accType, tstType;
    fread(&accType, sizeof(int), 1, acc);
    fread(&tstType, sizeof(int), 1, tst);
    void *accPtr, *tstPtr;
    if ( accType == DOUBLE ){
        accPtr = malloc (sizeof(double)*accElements);
        tstPtr = malloc (sizeof(double)*accElements);
        assert(accPtr && tstPtr && "Could Not allocate elements");
        fread(accPtr, sizeof(double) , accElements, acc);
        if ( tstType == DOUBLE) {
            fread(tstPtr, sizeof(double) , accElements, tst);
        }
        else if ( tstType == FLOAT){
            float tmp;
            for ( i =0 ; i < tstElements; i++){
                fread(&tmp, sizeof(float),1 , tst);
                ((double *)tstPtr)[i] = (double) tmp;
            }
        }
        else if (tstType == INT){
            assert(0 && "This is not supported yet\n");
        }

    }
    else if ( accType == FLOAT) {
        accPtr = malloc (sizeof(float)*accElements);
        tstPtr = malloc (sizeof(float)*accElements);
        assert(accPtr && tstPtr && "Could Not allocate elements");
        fread(accPtr, sizeof(float) , accElements, acc);
        fread(tstPtr, sizeof(float) , accElements, tst);
    }
    else if ( accType == INT){
        accPtr = malloc (sizeof(int)*accElements);
        tstPtr = malloc (sizeof(int)*accElements);
        assert(accPtr && tstPtr && "Could Not allocate elements");
        fread(accPtr, sizeof(float) , accElements, acc);
        fread(tstPtr, sizeof(float) , accElements, tst);
    }
    *accVals= accPtr;
    *tstVals = tstPtr;
    fclose(tst);
    fclose(acc);
    return accType;
}

void writeQualityFile(char *fileName, void *ptr, int type, size_t numElements){
    FILE *fd = fopen(fileName, "wb");
    assert(fd && "Could Not Open File\n");
    fwrite(&numElements, sizeof(size_t), 1, fd);
    fwrite(&type, sizeof(int), 1, fd);
    if ( type == DOUBLE)
        fwrite(ptr, sizeof(double), numElements, fd);
    else if ( type == FLOAT)
        fwrite(ptr, sizeof(float), numElements, fd);
    else if ( type == INT)
        fwrite(ptr, sizeof(int), numElements, fd);
    else
        assert(0 && "Not supported data type to write\n");
    fclose(fd);
}

void readData(FILE *fd, double **data,  size_t* numElements){
    assert(fd && "File pointer is not valid\n");
    fread(numElements, sizeof(size_t),1,fd);
    size_t elements = *numElements;
    double *ptr = (double*) malloc (sizeof(double)*elements);
    assert(ptr && "Could Not allocate pointer\n");
    *data = ptr;
    size_t i;
    int type;
    fread(&type, sizeof(int), 1, fd); 
    if ( type == DOUBLE){
        fread(ptr, sizeof(double), elements, fd);
    }
    else if ( type == FLOAT){
        float *tmp = (float*) malloc (sizeof(float)*elements);
        fread(tmp, sizeof(float), elements,fd);
        for ( i = 0; i < elements; i++){
            ptr[i] = (double) tmp[i];
        }
        free (tmp);
    }
    else if( type == INT ){
        int *tmp = (int*) malloc (sizeof(int)*elements);
        fread(tmp, sizeof(int), elements, fd);
        for ( i = 0; i < elements; i++){
            ptr[i] = (double) tmp[i];
        }
        free(tmp);
    }
    return; 
}

void readData(FILE *fd, float **data,  size_t* numElements){
    assert(fd && "File pointer is not valid\n");
    fread(numElements, sizeof(size_t),1,fd);
    size_t elements = *numElements;

    float *ptr = (float*) malloc (sizeof(float)*elements);
    assert(ptr && "Could Not allocate pointer\n");
    *data = ptr;

    size_t i;
    int type;
    fread(&type, sizeof(int), 1, fd); 
    if ( type == FLOAT ){
        fread(ptr, sizeof(float), elements, fd);
    }
    else if ( type == DOUBLE){
        double *tmp = (double*) malloc (sizeof(double)*elements);
        fread(tmp, sizeof(double), elements,fd);
        for ( i = 0; i < elements; i++){
            ptr[i] = (float) tmp[i];
        }
        free (tmp);
    }
    else if ( type == INT ){
        int *tmp = (int*) malloc (sizeof(int) * elements);
        fread(tmp, sizeof(int), elements, fd);
        for ( i = 0; i < elements; i++){
            ptr[i] = (float) tmp[i];
        }
        free(tmp);
    }
    return; 
}

void readData(FILE *fd, int **data,   size_t* numElements){
    assert(fd && "File pointer is not valid\n");
    fread(numElements, sizeof(size_t),1,fd);
    size_t elements = *numElements;

    int *ptr = (int*) malloc (sizeof(int)*elements);
    assert(ptr && "Could Not allocate pointer\n");
    *data = ptr;

    size_t i;
    int type;
    fread(&type, sizeof(int), 1, fd); 
    if ( type == INT ){
        fread(ptr, sizeof(int), elements, fd);
    }
    else if ( type == DOUBLE){
        double *tmp = (double*) malloc (sizeof(double)*elements);
        fread(tmp, sizeof(double), elements,fd);
        for ( i = 0; i < elements; i++){
            ptr[i] = (int) tmp[i];
        }
        free (tmp);
    }
    else if( type == FLOAT ){
        float *tmp = (float*) malloc (sizeof(float)*elements);
        fread(tmp, sizeof(float), elements, fd);
        for ( i = 0; i < elements; i++){
            ptr[i] = (int) tmp[i];
        }
        free(tmp);
    }
    return; 
}

void readValue( FILE *fd, int *val){
    size_t numElements;
    fread(&numElements,sizeof(size_t), 1, fd);
    if (numElements != 1 ){
        printf("This should not happen, I want to read a value but I have a vector\n");
        exit(-1);
    }
    int type;
    fread(&type,sizeof(int),1,fd);
    if ( type != INT){
        if ( type <0 || type > 3){
            printf(" Unknown type stored in file\n");
        }
        else{
            printf("Trying to read Int but file contains %s\n", typeNames[type]);
        }
        exit(-1);
    }
    fread(val, sizeof(int), 1, fd);
    return ;
}

void readValue( FILE *fd , long *val){
    size_t numElements;
    fread(&numElements,sizeof(size_t), 1, fd);
    if (numElements != 1 ){
        printf("This should not happen, I want to read a value but I have a vector\n");
        exit(-1);
    }
    int type;
    fread(&type,sizeof(long),1,fd);
    if ( type != INT){
        if ( type <0 || type > 3){
            printf(" Unknown type stored in file\n");
        }
        else{
            printf("Trying to read Int but file contains %s\n", typeNames[type]);
        }
        exit(-1);
    }
    fread(val, sizeof(long), 1, fd);
}

void MP_read(FILE *fd, int type, double *ptr, int numElements){
    if ( type == DOUBLE ){
        fread(ptr, sizeof(double), numElements, fd); 
    }
    else if ( type == FLOAT ){
        float *tmp = (float *) malloc (sizeof(float)*numElements);
        fread(tmp, sizeof(float), numElements, fd);
        MP_memcpy(ptr, tmp, numElements);
        free(tmp);
    }
    else{
        printf("Such a type is not yet supported\n");
    }
}

void MP_read(FILE *fd, int type, float *ptr, int numElements){
    if ( type == DOUBLE ){
        double *tmp = (double*) malloc (sizeof(double)*numElements);
        fread(tmp, sizeof(double), numElements, fd); 
        MP_memcpy(ptr, tmp, numElements);
        free(tmp);
    }
    else if ( type == FLOAT ){
        fread(ptr, sizeof(float), numElements, fd);
    }
    else{
        printf("Such a type is not yet supported\n");
    }
}

void MP_read(FILE *fd, int type, int *ptr, int numElements){
   if ( type == INT){
    fread(ptr, sizeof(int), numElements, fd);
   }
   else{
       printf("We do not support such conversions yet\n");
   }
}
