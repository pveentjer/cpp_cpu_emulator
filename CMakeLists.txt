cmake_minimum_required(VERSION 3.28)
project(cpu_emulator)

set(CMAKE_CXX_STANDARD 17)

add_executable(cpu_emulator main.cpp
        instructions.cpp
        instructions.h
        utils.cpp
        utils.h)
