cmake_minimum_required(VERSION 3.0...3.25)
project(cubozoa_ecs VERSION 0.1.0 LANGUAGES CXX C)

option(CBZ_BUILD_EXAMPLES "Build examples" OFF)

add_library(${PROJECT_NAME}
            src/cubozoa_ecs.cpp)

target_include_directories(${PROJECT_NAME} PUBLIC include)
target_link_libraries(${PROJECT_NAME} PRIVATE spdlog) 
set_target_properties(${PROJECT_NAME} PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED ON
    CXX_EXTENSIONS OFF
    COMPILE_WARNING_AS_ERROR ON
)

if (MSVC)
    target_compile_options(${PROJECT_NAME} PRIVATE /W4)
else()
    target_compile_options(${PROJECT_NAME} PRIVATE -Wall -Wextra -pedantic)
    set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
endif()

if(CBZ_BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()
