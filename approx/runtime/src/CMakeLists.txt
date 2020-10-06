configure_file(${LIBAPPROX_INC_DIR}/approx.h approx.h COPYONLY)

set(CLANG_SOURCE_INCLUDE "${LLVM_EXTERNAL_CLANG_SOURCE_DIR}/include/")

include_directories(
    ${CMAKE_CURRENT_BINARY_DIR}
    ${LIBAPPROX_INC_DIR}
    ${CLANG_SOURCE_INCLUDE}
)

set(LIBAPPROX_SOURCE_FILES
    approx_runtime.cpp
    approx_data_util.cpp
    approx_memoize_out.cpp
)

#We might need to use CXX as linker.
set(LIBAPPROX_LINKER_LANGUAGE C)
add_library(approx ${LIBAPPROX_LIBRARY_KIND} ${LIBAPPROX_SOURCE_FILES})

set_target_properties(approx PROPERTIES
    PREFIX "" SUFFIX "" OUTPUT_NAME "${LIBAPPROX_LIB_FILE}"
    LINK_FLAGS ""
    LINKER_LANGUAGE ${LIBAPPROX_LINKER_LANGUAGE})

target_link_libraries(approx ${CMAKE_DL_LIBS})
string(REGEX MATCH "[0-9]+\\.[0-9]+(\\.[0-9]+)?" CLANG_VERSION ${PACKAGE_VERSION})
set(LIBAPPROX_HEADERS_INSTALL_PATH "${APPROX_INSTALL_LIBDIR}/clang/${CLANG_VERSION}/include")

install(TARGETS approx ${LIBAPPROX_INSTALL_KIND} DESTINATION "${APPROX_INSTALL_LIBDIR}")
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/approx.h DESTINATION ${LIBAPPROX_HEADERS_INSTALL_PATH})
