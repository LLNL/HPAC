#ifndef DEVICE_INTRINSICS_HH_INCLUDED
#define DEVICE_INTRINSICS_HH_INCLUDED
#include <algorithm>
#pragma omp begin declare target
void syncThreadsAligned(){};
unsigned int warpReduceMax(unsigned mask, unsigned int value) {return 0;}
unsigned int warpBallot(unsigned int mask, bool pred) { return 0;}
unsigned int ffs(unsigned int val) {return 0;}
unsigned int popc(unsigned int val) {return 0;}
void syncWarp(unsigned int mask) {}
unsigned int activeMask() {return 0;}
float reduceMaxImpl(unsigned int mask, float value, unsigned int shift) {return 0.0f;}
float reduceSumImpl(unsigned int mask, float value) {return 0.0f;}
float warpShuffleXOR(unsigned int mask, float value, int lanemask){return 0.0f;}
unsigned int brev(unsigned int v) { return 0;}
#pragma omp end declare target
#pragma omp begin declare variant match(                                       \
    device = {arch(nvptx, nvptx64)}, implementation = {extension(match_any)})
void syncThreadsAligned() { __syncthreads(); }
unsigned int ffs(unsigned int val) { return __builtin_ffs(val);}
unsigned int popc(unsigned int val) { return __builtin_popcount(val);}
unsigned int warpReduceMax(unsigned int mask, unsigned int value) { return __nvvm_redux_sync_umax(value, mask);}
unsigned int warpBallot(unsigned int mask, bool pred) { return __nvvm_vote_ballot_sync(mask, pred);}
unsigned int brev(unsigned int v) { return __brev(v);}
void syncWarp(unsigned int mask) { return __nvvm_bar_warp_sync(mask);}
float warpShuffleXOR(unsigned int mask, float value, int lanemask){ return __nvvm_shfl_sync_bfly_f32(mask, value, lanemask, 0x1f);}

unsigned int activeMask() {
  unsigned int Mask;
  asm("activemask.b32 %0;" : "=r"(Mask));
  return Mask;
}

// max reduction among threads named in the mask.
// the threads named in 'mask' must be a power of 2
float reduceMaxImpl(unsigned int mask, float value, unsigned int shift)
{
  unsigned int num_participants = popc(mask);
  for(int i = 1; i < num_participants; i *= 2)
    {
      value = std::max(warpShuffleXOR(mask, value, i), value);
    }
  return value;
}


// max reduction among threads named in the mask.
// the threads named in 'mask' must be a power of 2
float reduceSumImpl(unsigned int mask, float value)
{
  unsigned int num_participants = popc(mask);
  for(int i = 1; i < num_participants; i *= 2)
    {
      value += warpShuffleXOR(mask, value, i);
    }
  return value;
}


#pragma omp end declare variant

#endif
