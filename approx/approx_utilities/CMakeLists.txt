cmake_minimum_required(VERSION 3.14 FATAL_ERROR)
project(APPROX_BENCH LANGUAGES C CXX)

IF( NOT CMAKE_BUILD_TYPE )
    SET( CMAKE_BUILD_TYPE Release FORCE )
ENDIF()

set(CMAKE_SKIP_BUILD_RPATH FALSE)
find_package(Python COMPONENTS Interpreter Development)
set(CMAKE_CXX_STANDARD 14 CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)
set(CMAKE_UTIL_DIR ${CMAKE_CURRENT_BINARY_DIR})
set(LIBAPPROX_LIB_FILE "ApproxUtil")

#add_subdirectory(Template)
add_subdirectory(libraries)
add_subdirectory(benchmarks)
