#ifndef APROX_MEMO_TABLE_HH_INCLUDED
#define APROX_MEMO_TABLE_HH_INCLUDED
#include <omp.h>
#include <algorithm>
#include <iostream>
#include <bitset>
#include "approx_internal.h"
#include "device_intrinsics.h"

#pragma omp begin declare target device_type(nohost)

template<typename ValType, unsigned int SIZE_IN_BYTES,
  unsigned int THREADS_PER_WARP, unsigned int NUM_TABLES_PER_WARP>
class MemoTable{

  ValType *table;
  char *InputIdx;
  unsigned char *ReplacementData;
  ValType num_items_per_entry;
  int num_entries;
  int *clock_indexes;
  TableReplacementPolicy RP;

private:
  int calc_s_tab_size()
  {
    int tables_per_block = (omp_get_num_threads()/THREADS_PER_WARP) * NUM_TABLES_PER_WARP;
    return ((int)(num_items_per_entry)*tables_per_block*(num_entries));
  }
    public:
  MemoTable(int NumEntries, ValType *tab_ptr, ValType _num_items_per_entry, char *_InputIdx, int *_clock_indexes, unsigned char *_ReplacementData, TableReplacementPolicy *_RP){
        num_entries = NumEntries;
        table = tab_ptr;
        num_items_per_entry = _num_items_per_entry;
        InputIdx = _InputIdx;
        clock_indexes = _clock_indexes;
        ReplacementData = _ReplacementData;
        RP = *_RP;
    }

    ValType &operator()(int col)
    {
        return table[col];
    }

    void copy_from(ValType *src)
    {
        int s_tab_size = calc_s_tab_size();
        for(int i = omp_get_thread_num(); i < s_tab_size; i += omp_get_num_threads())
        {
            table[i] = src[i];
        }
    }

    void copy_to(ValType *dest)
    {
        int s_tab_size = calc_s_tab_size();
        for(int i = omp_get_thread_num(); i < s_tab_size; i += omp_get_num_threads())
        {
            dest[i] = table[i];
        }
    }

  ValType calc_distance(const approx_region_specification &rs, const approx_var_access_t &input_a, const void *ipt_ptr, int size, int entry, int offset)
    {
        int tid_in_block = omp_get_thread_num();
        int tid_in_warp = tid_in_block % NTHREADS_PER_WARP;
        int warp_in_block = tid_in_block / NTHREADS_PER_WARP;
        int table_in_warp = tid_in_warp / (NTHREADS_PER_WARP/TABLES_PER_WARP);
        int table_number = warp_in_block * TABLES_PER_WARP + table_in_warp;
        int tables_per_block = (omp_get_num_threads()/NTHREADS_PER_WARP) * TABLES_PER_WARP;

        ValType dist_total = 0.0;
        for(int i = 0; i < size; i++)
        {
            int row_number = entry * num_items_per_entry + offset + i;
            int column_number = table_number;
            int access_idx = (row_number * tables_per_block) + table_number;
            real_t in_val_conv = 0;
            convertToSingleWithOffset(&in_val_conv, ipt_ptr, 0, i*input_a.stride, (ApproxType) rs.data_type);
            real_t dist = 0;
            if(table[access_idx] != 0)
              dist = fabs(table[access_idx] - in_val_conv) / table[access_idx];
            else
              dist = fabs(table[access_idx] - in_val_conv);
            dist_total += dist;
        }
        return dist_total;
    }

    void registerAccess(int entryIdx)
    {
      if(RP == ROUND_ROBIN)
        return;

      int tid_in_block = omp_get_thread_num();
      int tid_in_warp = tid_in_block % NTHREADS_PER_WARP;
      int warp_in_block = tid_in_block / NTHREADS_PER_WARP;
      int table_in_warp = tid_in_warp / (NTHREADS_PER_WARP/TABLES_PER_WARP);
      int table_number = warp_in_block * TABLES_PER_WARP + table_in_warp;
      int tables_per_block = (omp_get_num_threads()/NTHREADS_PER_WARP) * TABLES_PER_WARP;

      // every table has at least 8 bits for access tracking
      // unsigned int access_index = (table_number * std::max(1,num_entries/8)) + (entryIdx / 8);
      unsigned int access_index = ((omp_get_team_num() * tables_per_block + table_number) * std::max(1,num_entries/8)) + (entryIdx/8);
      unsigned char mask = 1 << (entryIdx % 8);

      #pragma omp atomic update
      ReplacementData[access_index] |= mask;
      // update replacement data information in the table
    }

    int getSize()
    {
        int tid_in_block = omp_get_thread_num();
        int tid_in_warp = tid_in_block % THREADS_PER_WARP;
        int warp_in_block = tid_in_block / THREADS_PER_WARP;
        int table_in_warp = tid_in_warp / (THREADS_PER_WARP/NUM_TABLES_PER_WARP);
        int table_number = warp_in_block * NUM_TABLES_PER_WARP + table_in_warp;
        int tables_per_block = (omp_get_num_threads()/THREADS_PER_WARP) * NUM_TABLES_PER_WARP;

        return (int) InputIdx[tables_per_block * omp_get_team_num() + table_number];
    }

  unsigned char getMask(unsigned char l, unsigned char r)
  {
    return (((1 << (l - 1)) - 1) ^
            ((1 << (r)) - 1));
  }

  int getInsertionIdxChar( unsigned char &data,
                           int clockIndex)
  {
    unsigned char thisValue = data;
    // we want to set everything to the right of clock index to 1: this will ensure that these values cannot be considered
    unsigned char clockMask = getMask(1, clockIndex);
    unsigned char tVCM = thisValue | clockMask;

    unsigned int thisValueInt = (unsigned int) tVCM;
    auto insertionIdx = ffs(~thisValueInt);

    unsigned char mask = getMask(insertionIdx, 8);
    unsigned char firstMasked = thisValue & mask;
    mask = getMask(1, clockIndex);

    unsigned char secondMasked = thisValue & mask;
    thisValue = firstMasked | secondMasked;

    data = thisValue;
    return insertionIdx - 1;
  }

    int getInsertionIdx()
    {
      int tid_in_block = omp_get_thread_num();
      int tid_in_warp = tid_in_block % THREADS_PER_WARP;
      int warp_in_block = tid_in_block / THREADS_PER_WARP;
      int table_in_warp = tid_in_warp / (THREADS_PER_WARP/NUM_TABLES_PER_WARP);
      int table_number = warp_in_block * NUM_TABLES_PER_WARP + table_in_warp;
      int tables_per_block = (omp_get_num_threads()/THREADS_PER_WARP) * NUM_TABLES_PER_WARP;

      int size = (int) InputIdx[tables_per_block * omp_get_team_num() + table_number];
      if(size < num_entries)
        return size;

      if(RP == ROUND_ROBIN)
        {
          int access = tables_per_block * omp_get_team_num() + table_number;
          int retval = ReplacementData[access];
          ReplacementData[access] = (retval + 1) % num_entries;
          return retval;
        }

      int clockIndex = clock_indexes[tables_per_block * omp_get_team_num() + table_number];
      // base access index of our table's data
      unsigned int access_index = ((omp_get_team_num() * tables_per_block + table_number) * std::max(1,num_entries/8));
      int insertIdx = 8*(clockIndex/8);
      int CICopy = clockIndex;
      int numWholes = num_entries / 8;
      int remainder = num_entries % 8;

      int totalBytes = numWholes + (remainder!= 0);

      // if we have at most one byte, we need to call
      // getInsertionIdxChar twice to make sure we
      // search the whole buffer
      int totalLoops = totalBytes;
      if(num_entries <= 8 && clockIndex != 0)
        totalLoops = 2;
      for(int i = 0; i < totalLoops; i++)
        {
          int access = ((clockIndex+(i*8))/8) % totalBytes;
          int thisIdx = getInsertionIdxChar(ReplacementData[access + access_index], CICopy %8);
          insertIdx += thisIdx;
          CICopy = 0;

          int not_found = access == totalBytes - 1 && remainder ? remainder : 8;
          if(thisIdx != not_found)
            {
              break;
            }
        }

      // +1 because we don't want to immediately replace this value on the next call
      clockIndex = (insertIdx + 1) % num_entries;
      clock_indexes[tables_per_block * omp_get_team_num() + table_number] = clockIndex;
      return insertIdx % num_entries;
  }


  void add_entry(const approx_region_specification *rs, const approx_var_access_t *input_a, const void **inputs, int nInputs)
  {
    int idx_offset = 0;
    int offset = 0;
    int entry_index = getInsertionIdx();
    for(int j = 0; j < nInputs; j++)
      {
        for(int i = 0; i < input_a[j].num_elem; i++)
          {
            int tid_in_block = omp_get_thread_num();
            int tid_in_warp = tid_in_block % NTHREADS_PER_WARP;
            int warp_in_block = tid_in_block / NTHREADS_PER_WARP;
            int table_in_warp = tid_in_warp / (NTHREADS_PER_WARP/TABLES_PER_WARP);
            int table_number = warp_in_block * TABLES_PER_WARP + table_in_warp;
            int tables_per_block = (omp_get_num_threads()/THREADS_PER_WARP) * NUM_TABLES_PER_WARP;

            int row_number = offset + i;
            int column_number = table_number;
            int access_idx = (row_number * tables_per_block) + table_number;

            row_number = entry_index * num_items_per_entry + offset + i;
            access_idx = (row_number * tables_per_block) + table_number;

            convertToSingleWithOffset(table, inputs[j], access_idx, i*input_a[j].stride,
                                      (ApproxType) rs[j].data_type);

            idx_offset = tables_per_block * omp_get_team_num() + table_number;
          }
        offset += input_a[j].num_elem;
      }

    InputIdx[idx_offset] = std::min(num_entries, InputIdx[idx_offset]+1);

  }

};
#pragma omp end declare target


#endif // APROX_MEMO_TABLE_HH_INCLUDED
