cmake_minimum_required(VERSION 3.16)
project(GuessingGame)

set(CMAKE_CXX_STANDARD 20)

add_executable(GuessingGame main.cpp cctype_wrappers.hpp input.hpp)