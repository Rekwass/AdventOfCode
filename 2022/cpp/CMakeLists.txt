cmake_minimum_required(VERSION 3.17)

project("aoc" VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/../)

###############################################################################
# SET COMPILATION FLAGS
###############################################################################

if (CMAKE_COMPILER_IS_GNUCXX)
	set(CMAKE_CXX_FLAGS_DEBUG "-Wall -Wextra -Wshadow -Wno-unused-parameter -W")
endif()

set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -ggdb3")

set(CMAKE_CXX_FLAGS_RELEASE "-O3")

###############################################################################
# SOURCES
###############################################################################

set(PROJECT_SOURCES
    main.cpp
    day01/calorieCounting.cpp
    day02/rockPaperScissors.cpp
    day03/rucksackReorganization.cpp
    day04/campCleanup.cpp
    day05/supplyStacks.cpp
    day06/tuningTrouble.cpp
    day07/noSpaceLeftOnDevice.cpp
    day08/treetopTreeHouse.cpp
    day09/ropeBridge.cpp
    day10/cathodeRayTube.cpp
    day11/monkeyInTheMiddle.cpp
    day12/hillClimbingAlgorithm.cpp
    day13/distressSignal.cpp
    day14/regolithReservoir.cpp
    day15/beaconExclusionZone.cpp
    day16/proboscideaVolcanium.cpp
    day17/pyroclasticFlow.cpp
)

set(INCLUDES
    include/
)

###############################################################################
# CREATE EXECUTABLE
###############################################################################

add_executable(${PROJECT_NAME}
    main.cpp
    ${PROJECT_SOURCES}
)

target_include_directories(${PROJECT_NAME}
    PRIVATE
    ${INCLUDES}
)

target_link_libraries(${PROJECT_NAME}
    PRIVATE
    ${LIBRARIES}
)
