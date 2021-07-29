#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

const char *supportedMetrics[] = {
    "PRE",
    "MSE",
    "MAPE",
    "MAE",
    "RMSE",
    "R2",
    "MPP" //miss position percentage
};

int numMetrics = 7;

void printSupportedMetrics(){
    int i;
    for ( i = 0 ; i < numMetrics ; i++)
        printf("%s\n", supportedMetrics[i]);
}
const char *show_classification(double x) {
    switch(fpclassify(x)) {
        case FP_INFINITE:  return "Inf";
        case FP_NAN:       return "NaN";
        case FP_NORMAL:    return "normal";
        case FP_SUBNORMAL: return "subnormal";
        case FP_ZERO:      return "zero";
        default:           return "unknown";
    }
}
int isNumber(double *test, size_t elements){
    size_t i;
    size_t num_not_numbers = 0;
    for ( i = 0; i < elements; i++){
        if ( fpclassify(test[i]) != FP_ZERO && fpclassify(test[i]) != FP_NORMAL){
            printf("%d Not a number:%g:%s\n", i, test[i], show_classification(test[i]));
            num_not_numbers++;
        }
    }
    if ( num_not_numbers  != 0){
        printf("Not numbers are %ld\n", num_not_numbers);
    }
    return (num_not_numbers == 0);
}

double computeMAPE(double *correct, double *test, size_t elements){
    double averageC = 0.0;
    double averageT = 0.0;
    for ( int i = 0 ; i < elements; i++){
        averageC += correct[i];
        averageT += fabs(correct[i] - test[i]);
    }
    double error = averageT/fabs(averageC);
    return error;
}

double computeMAE(double *correct, double *test, size_t elements){
    double error =0.0;
    size_t i;
    double diff = 0.0;
    double sum=0.0;
    for ( i = 0 ; i < elements; i++){
        if ( *(long*) &correct[i] != *(long*) &test[i]){
            diff = fabs((correct[i] - test[i]));
            error += diff;
        }
    }
    error = error/ (double) elements;
    return error;
}

double computeMSE(double *correct, double *test, size_t elements){
    double error =0.0;
    double sqrtDiff= 0;
    double p2Diff = 0.0;
    size_t i;
    for ( i = 0 ; i < elements; i++){
        double tmp = correct[i] - test[i];
        p2Diff = pow(tmp,2.0);
        error += p2Diff;
    }
    error = error/(double)elements;
    return error;
}

double computeRMSE(double *correct, double *test, size_t elements){
    double error = computeMSE(correct,test,elements);
    error = sqrt(error);
    return error;
}

double computeRootSquared(double *correct, double *test, size_t elements){
    size_t i; 
    double avgValue = 0.0;
    double SSReg = 0.0;
    double SSTot = 0.0;

    for ( i = 0 ; i < elements; i++)
        avgValue += correct[i];
    avgValue = avgValue / (double) elements;

    for ( i = 0; i < elements; i++){
        double diff = test[i]-avgValue;
        SSReg += pow(diff,2); 
        diff = correct[i]-avgValue;
        SSTot += pow(diff,2);
    }
    return SSReg/SSTot;
}

double computeQuality(double *correct, double *test, size_t elements, char *metric){
    if  ( !isNumber(test, elements) ){
        printf("Result is not a number\n");
        return 1.0;
    }
    if (strcmp(metric, "PRE" ) == 0){
        if (elements != 1){
            printf("Precomputed output should always be of length 1\n");
            return 1.0;
        }
        return *test;
    }
    else if ( strcmp(metric,"MAE") == 0 )
        return computeMAE(correct, test, elements);
    else if (strcmp(metric, "MAPE") == 0)
        return computeMAPE(correct, test, elements);
    else if (strcmp( metric, "RMSE") == 0 )
        return computeRMSE(correct, test, elements);
    else if (strcmp( metric, "MSE") == 0 )
        return computeMSE(correct, test, elements);
    else if (strcmp( metric, "R2") == 0 )
        return computeRootSquared(correct, test, elements);
    else{
        printf("Please specify a valid quality metric\n");
        return 1.0;
    }
}

double computeMPP(int *correct, int *test, size_t elements){
    size_t i;
    double wrong = 0.0;
    for ( i = 0 ; i < elements; i++){
        if ( correct[i] != test[i] ){
            wrong+=1.0;
        }
    }
    return wrong / elements;
}

double computeQuality(int *correct, int* test, size_t elements, char *metric){
    if ( strcmp(metric,"MPP") == 0 ){
        return computeMPP(correct, test, elements);
    }
    else{
        printf("Please specify a valid quality metric\n");
        return 1.0;
    }
}

int checkMetrics(const char *metric){
    int i;
    for ( i = 0 ; i < numMetrics; i++){
        if (strcmp(metric,supportedMetrics[i])==0)
            return 1;
    }
    return 0;
}
