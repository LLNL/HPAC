#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>

#include "metrics.hpp"
#include "IO.hpp"


int main(int argc, char *argv[]){
    char *accurate = NULL;
    char *test= NULL;
    char *metric= NULL;
    int c;

    while ((c = getopt (argc, argv, "a:t:m:h")) != -1) {
        switch (c){
            case 'a':
                accurate = optarg;
                break;
            case 't':
                test = optarg;
                break;
            case 'm':
                metric = optarg;
                break;
            case 'h':
                printf("%s \n\t-a path to file containing accurate result\n\t-t path to file containing test result\n\t-m metric to use to compare qualities\n",argv[0]);
                return 0;
                break;
        }
    }

    if ( !( metric && test && accurate) ){
        printf("You need to specify an accurate and a test file as well as a metric\n");
        return 0;
    }

    if ( !checkMetrics(metric) ){
        printf("We do not support this metric \nSupported Metrics:\n");
        printSupportedMetrics();
        return 0;
    }

    void *acc = NULL;
    void *tst = NULL;
    size_t numElements;
    int type = ReadQualityFiles(accurate, test, &acc, &tst, &numElements);
    double quality = 0.0;
    if ( type == DOUBLE){
        quality = computeQuality((double *) acc,(double *)tst, numElements, metric);
    }
    else if ( type == INT){
        quality = computeQuality( (int *) acc, (int *) tst, numElements, metric);
    }
    else if ( type == -1 ){
        quality=INFINITY;
    }
    else{
        printf("I cannot compute other types of errors\n");
        return -1;
    }
    if ( acc )
        free(acc);
    if ( tst)
        free(tst);
    printf("OUTPUT_QUALITY:%g\n", quality);
}
