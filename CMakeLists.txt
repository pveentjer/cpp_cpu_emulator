cmake_minimum_required(VERSION 3.28)
project(cpu_emulator)

set(CMAKE_CXX_STANDARD 17)

add_executable(cpu_emulator src/main.cpp
        src/instructions.cpp
        include/instructions.h
        src/utils.cpp
        include/utils.h
        src/loader.cpp
        include/loader.h
        src/cpu.cpp
        include/cpu.h
        src/backend.cpp
        include/backend.h
        src/frontend.cpp
        include/frontend.h
        src/memory_subsystem.cpp
        include/memory_subsystem.h
        src/common.cpp
        include/common.h
)
