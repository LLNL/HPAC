#ifndef __THREAD_STORAGE__
#define __THREAD_STORAGE__

#include <iostream>
using namespace std;

#define MAX_REGIONS 20

template <typename T>
class ThreadMemoryPool{
  T ***memoryRegions;
  int *lastMemoIn;
  int totalThreads;
public:
  ThreadMemoryPool(){
     totalThreads = omp_get_num_procs(); 
     lastMemoIn = new int[totalThreads]();
     memoryRegions = new T**[totalThreads];
     for ( int i = 0; i < totalThreads; i++){
       memoryRegions[i] = new T*[MAX_REGIONS];
     }
  };

  ~ThreadMemoryPool() {
    for (int i = 0; i < totalThreads; i++) {
      for (int j = 0; j < lastMemoIn[i]; j++) {
        delete memoryRegions[i][j];
      }
      delete [] memoryRegions[i];
    }
    delete [] memoryRegions;
    delete [] lastMemoIn;
  }

  T *findMemo(int threadId, unsigned long Addr) {
    static thread_local int myIndex = -1;
    static thread_local T**thread_region = memoryRegions[threadId];

    if (myIndex != -1 && thread_region[myIndex] && (((unsigned long) (thread_region[myIndex]->accurate)) == Addr)){
      return thread_region[myIndex];
    }

    for (int i = 0; i < lastMemoIn[threadId]; i++) {
      if ((unsigned long)(thread_region[i]->accurate) == Addr)
        return memoryRegions[threadId][i];
    }
    return nullptr;
  }

  T *addNew(int threadId, T* newRegion){
    if ( lastMemoIn[threadId] >= MAX_REGIONS ){
      cout << " I Reach Maximum Regions Exiting..\n";
      exit(-1);
    }
    memoryRegions[threadId][lastMemoIn[threadId]++] = newRegion;
    return newRegion;
  }
};
#endif