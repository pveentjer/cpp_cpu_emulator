cmake_minimum_required(VERSION 3.28)
project(cpu_emulator)

set(CMAKE_CXX_STANDARD 17)

add_executable(cpu_emulator main.cpp
        instructions.cpp
        include/instructions.h
        utils.cpp
        include/utils.h
        loader.cpp
        include/loader.h
        cpu.cpp
        cpu.h
        backend.cpp
        include/backend.h
        frontend.cpp
        include/frontend.h
        memory_subsystem.cpp
        include/memory_subsystem.h
        common.cpp
        include/common.h
)
