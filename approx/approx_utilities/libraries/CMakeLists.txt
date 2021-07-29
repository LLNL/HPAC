
include_directories(
    ${CMAKE_CURRENT_BINARY_DIR}
    )

set (LIBBENCHSRC
    IO.cpp
    datautils.cpp
    metrics.cpp
    mp_math.cpp
    )

set(APPROX_LIB_UTIL_DIR ${CMAKE_CURRENT_BINARY_DIR} PARENT_SCOPE)
add_library(${LIBAPPROX_LIB_FILE} STATIC ${LIBBENCHSRC})
add_executable(quality quality.cpp)
set_target_properties( quality 
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
    )
target_link_libraries(quality ${LIBAPPROX_LIB_FILE})

set(LIBHEADERS IO.hpp datautils.hpp metrics.hpp mp_math.hpp executionTime.h)

foreach(hf ${LIBHEADERS})
    configure_file(${hf} "${CMAKE_CURRENT_BINARY_DIR}/${hf}" COPYONLY)
endforeach() 
