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


