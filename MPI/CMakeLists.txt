#cmake .. -DCMAKE_Fortran_COMPILER=gfortran -DUSE_SW=ON
#this is a cmake project example for HPC class
# jgao 2023 09

cmake_minimum_required (VERSION 2.6)

project (cmake_exmple)
#  启用C支持
enable_language(C)
set(
    CMAKE_RUNTIME_OUTPUT_DIRECTORY
       ${CMAKE_CURRENT_BINARY_DIR}/bin
    )

set(
    CMAKE_LIBRARY_OUTPUT_DIRECTORY lib
    ${CMAKE_CURRENT_BINARY_DIR}/lib )

set(
    CMAKE_ARCHIVE_OUTPUT_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/lib )

add_executable(pi pi.c)