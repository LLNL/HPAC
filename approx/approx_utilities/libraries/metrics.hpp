#ifndef __METRIC__
#define __METRIC__
    double computeQuality(double* , double *, size_t , char *);
    double computeQuality(int* , int *, size_t , char *);
    void printSupportedMetrics();
    int checkMetrics(const char *metric);
#endif
