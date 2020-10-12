cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

# CMAKE_BUILD_TYPE was not set, default to Release.
if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()
set(CMAKE_BUILD_TYPE Release)
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=native")

set(CMAKE_CXX_STANDARD 14 CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)



set(APPROX_INSTALL_LIBDIR "lib${LLVM_LIBDIR_SUFFIX}")
set(APPROX_LIBDIR_SUFFIX "" CACHE STRING
"Suffix of lib installation directory, e.g. 64 => lib64")
set(APPROX_INSTALL_LIBDIR "lib${APPROX_LIBDIR_SUFFIX}")

add_subdirectory(runtime)
