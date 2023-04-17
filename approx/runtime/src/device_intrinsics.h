#ifndef DEVICE_INTRINSICS_HH_INCLUDED
#define DEVICE_INTRINSICS_HH_INCLUDED
#include <algorithm>
#pragma omp begin declare target
namespace approx {
  namespace intr {
void syncThreadsAligned(){};
uint64_t warpReduceMax(unsigned mask, uint64_t value) {return 0;}
int32_t shuffle(uint64_t Mask, int32_t Var, int32_t SrcLane) {return 0;}
int32_t getWarpSize() {return -1;}
uint64_t warpBallot(uint64_t mask, bool pred) { return 0;}
uint64_t ffs(uint64_t val) {return 0;}
uint64_t popc(uint64_t val) {return 0;}
void syncWarp(uint64_t mask) {}
uint64_t activeMask() {return 0;}
float reduceMaxImpl(uint64_t mask, float value, uint64_t shift) {return 0.5f;}
float reduceSumImpl(uint64_t mask, float value, uint64_t n_p) {return 0.5f;}
float warpShuffleXOR(uint64_t mask, float value, int lanemask){return 0.0f;}
uint64_t brev(uint64_t v) { return 0;}
#pragma omp end declare target
#pragma omp begin declare variant match(                                       \
    device = {arch(nvptx, nvptx64)}, implementation = {extension(match_any)})
void syncThreadsAligned() { __syncthreads(); }
uint64_t ffs(uint64_t val) { return __builtin_ffs(val);}
uint64_t popc(uint64_t val) { return __builtin_popcount(val);}
uint64_t warpReduceMax(uint64_t mask, uint64_t value) { return __nvvm_redux_sync_umax(value, mask);}
int32_t getWarpSize() {return 32;}
template<typename T>
T warpBallot(T mask, bool pred) { return __nvvm_vote_ballot_sync(mask, pred);}
uint64_t brev(uint64_t v) { return __brev(v);}
void syncWarp(uint64_t mask) { return __nvvm_bar_warp_sync(mask);}
float warpShuffleXOR(uint64_t mask, float value, int lanemask){ return __nvvm_shfl_sync_bfly_f32(mask, value, lanemask, 0x1f);}

uint64_t activeMask() {
  uint64_t Mask;
  asm("activemask.b32 %0;" : "=r"(Mask));
  return Mask;
}

// max reduction among threads named in the mask.
// the threads named in 'mask' must be a power of 2
float reduceMaxImpl(uint64_t mask, float value, uint64_t shift)
{
  uint64_t num_participants = popc(mask);
  for(int i = 1; i < num_participants; i *= 2)
    {
      value = std::max(warpShuffleXOR(mask, value, i), value);
    }
  return value;
}


// max reduction among threads named in the mask.
// the threads named in 'mask' must be a power of 2
float reduceSumImpl(uint64_t mask, float value, uint64_t n_participants)
{
  uint64_t num_participants = popc(mask);
  for(int i = 1; i < num_participants; i *= 2)
    {
      value += warpShuffleXOR(mask, value, i);
    }
  return value;
}
#pragma omp end declare variant

#pragma omp begin declare variant match(                                       \
    device = {arch(amdgcn)}, implementation = {extension(match_any)})

void syncThreadsAligned() {__builtin_amdgcn_s_barrier();}
uint64_t ffs(uint64_t val) { return __builtin_ffsll(val);}
uint64_t popc(uint64_t val) { return __builtin_popcountll(val);}
int32_t getWarpSize() {return 64;}

int get_tid_x()
{
  return __builtin_amdgcn_workitem_id_x();
}

int32_t shuffle(uint64_t Mask, int32_t Var, int32_t SrcLane) {
  (void) Mask;
  int Width = getWarpSize();
  int Self = get_tid_x() % Width;
  int Index = SrcLane + (Self & ~(Width - 1));
  return __builtin_amdgcn_ds_bpermute(Index << 2, Var);
}
uint64_t warpBallot(uint64_t mask, bool pred) { return __builtin_amdgcn_ballot_w64(pred); }
int warpShuffleXOR(uint64_t mask, int Var, int lanemask) {
  int Width = getWarpSize();
  int Self = get_tid_x() % Width;

  return shuffle(mask, Var, Self ^ lanemask);

}

float reduceMaxImpl(uint64_t mask, float value, uint64_t n_participants)
{
  int num_participants = n_participants;
  for(int i = 1; i < num_participants; i *= 2)
    {
      int Var = *(int*)(&value);
      int _result = warpShuffleXOR(mask, Var, i);
      float result = *(float*)(&_result);
      value = std::max(value, result);
    }
  return value;
}

float reduceSumImpl(uint64_t mask, float value, uint64_t n_participants)
{
  uint64_t num_participants = n_participants;
  for(int i = 1; i < num_participants; i *= 2)
    {
      int Var = *(int*)(&value);
      int _result = warpShuffleXOR(mask, Var, i);
      float result = *(float*)(&_result);
      value += result;
    }
  return value;
}

// no independent thread scheduling support
void syncWarp(uint64_t mask) {(void)mask; __builtin_amdgcn_wave_barrier();}
#pragma omp end declare variant

  }
}
#endif
