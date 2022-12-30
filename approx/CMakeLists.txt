cmake_minimum_required(VERSION 3.14 FATAL_ERROR)
set( CMAKE_VERBOSE_MAKEFILE on )

# CMAKE_BUILD_TYPE was not set, default to Release.
if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

set(CMAKE_BUILD_TYPE Release)
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/;${CMAKE_MODULE_PATH}")

find_package(OpenMP REQUIRED)
set(OpenMP_CXX_FLAGS "${OpenMP_CXX_FLAGS} -Xopenmp-target -march=sm_86 -fopenmp-targets=nvptx64 -foffload-lto -g")

if (OPENMP_FOUND)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
    set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
    message("LINKER FLAGS ARE ${OpenMP_EXE_LINKER_FLAGS}")
endif()

option(DEV_STATS "Turn on the collection of device approximation statistics" 0)
if(DEV_STATS)
  add_compile_definitions(APPROX_DEV_STATS)
endif()

option(SHARED_MEMORY_SIZE "Amount of shared memory (in bytes) to use for reducing global memory traffic")
if(SHARED_MEMORY_SIZE)
  add_compile_definitions(SM_SZ_IN_BYTES=${SHARED_MEMORY_SIZE})
else()
  add_compile_definitions(SM_SZ_IN_BYTES=0)
endif()

option(TAF_TYPE "Type of TAF to use: either TAF_INTER for taf-inter, or TAF_INTRA for taf-intra")
if("${TAF_TYPE}" STREQUAL "TAF_INTER")
  add_compile_definitions(TAF_INTER=1)
else()
  add_compile_definitions(TAF_INTRA=1)
endif()

add_compile_definitions(MAX_HIST_SIZE=5)

option(GLOBAL_OUTTABLE "Turn on if the table for output memoization should be in global memory")
if(GLOBAL_OUTTABLE)
  add_compile_definitions(OTABLE_GLOBAL=1)
endif()

option(TAF_WIDTH "Number of adjacent threads per warp that will share TAF state. Must be a power of 2 at most 32.")
if(TAF_WIDTH)
  add_compile_definitions(TAF_THREAD_WIDTH=${TAF_WIDTH})
else()
  add_compile_definitions(TAF_THREAD_WIDTH=32)
endif()

option(TABLES_PER_WARP "Number of memoization input tables to store for each warp.")
if(TABLES_PER_WARP)
  add_compile_definitions(TABLES_PER_WARP=${TABLES_PER_WARP})
else()
  add_compile_definitions(TABLES_PER_WARP=2)
endif()

add_compile_definitions(NTHREADS_PER_WARP=32)

set(CMAKE_CXX_STANDARD 14 CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)

# use, i.e. don't skip the full RPATH for the build tree
set(CMAKE_SKIP_BUILD_RPATH FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# the RPATH to be used when installing, but only if it's not a system directory
list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
if("${isSystemDir}" STREQUAL "-1")
    set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
endif("${isSystemDir}" STREQUAL "-1")

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=native")
SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -march=native")

set(APPROX_INSTALL_LIBDIR "lib${LLVM_LIBDIR_SUFFIX}")
set(APPROX_LIBDIR_SUFFIX "" CACHE STRING
"Suffix of lib installation directory, e.g. 64 => lib64")
set(APPROX_INSTALL_LIBDIR "lib${APPROX_LIBDIR_SUFFIX}")

add_subdirectory(runtime)
