#!/bin/bash

set -eo pipefail

NoColor="\033[0m"
CyanColor="\033[0;36m"
RedColor="\033[0;91m"

args=""

function generatecc {
    args="$args -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
}

function compiledebug {
    args="$args -DCMAKE_BUILD_TYPE=Debug"
}

function compiledefault {
    args="$args -DCMAKE_BUILD_TYPE=Default"
}

function compilerelease {
    args="$args -DCMAKE_BUILD_TYPE=Release"
}

function compileproject {
    compilepath="."
    builddir="./build/"
}

function clean {
  echo -e "\n${CyanColor}Nothing to clean${NoColor}"
}

function fclean {
    clean
    rm -rf ./build/
}

function compile {
    mkdir -p $builddir && cd $builddir

    cmake ../ -G "Ninja" -DCMAKE_CXX_COMPILER=clang++ $args
    cmake --build .
}

scriptargs=" $* "

if [[ "$scriptargs" == *" -h "* ]] || [[ "$scriptargs" == *" --help "* ]] || [[ "$scriptargs" == *" help "* ]] || [[ "$scriptargs" == *" h "* ]]; then
  echo -e "Usage: $0"
  exit 0
fi

set +eo pipefail

cmakePath=$( which cmake )
if [ ! $? -eq 0 ]; then
  echo -e "\n${RedColor}Please install cmake first${NoColor}"
  exit 1
else
  echo -e "${CyanColor}Using cmake at $cmakePath${NoColor}"
fi

ninjaPath=$( which ninja )
if [ ! $? -eq 0 ]; then
  echo -e "\n${RedColor}Please install ninja first${NoColor}"
  exit 1
else
  echo -e "${CyanColor}Using ninja at $ninjaPath${NoColor}\n"
fi

set -eo pipefail

echo -e "\n${CyanColor}/!\\ Compiling project${NoColor}\n\n"
compileproject

if [[ "$scriptargs" == *" cc "* ]]; then
  echo -e "\n${CyanColor}Creating compile_commands.json file${NoColor}"
  generatecc
fi

if [[ "$scriptargs" == *" clean "* ]]; then
  clean
elif [[ "$scriptargs" == *" fclean "* ]]; then
  fclean
else
  if [[ "$scriptargs" == *" re "* ]]; then
    echo -e "\n${CyanColor}Recompiling all the project${NoColor}"
    fclean
  fi

  if [[ "$scriptargs" == *" debug "* ]]; then
    echo -e "\n${CyanColor}Compiling project in debug mode${NoColor}"
    compiledebug
  elif [[ "$scriptargs" == *" release "* ]]; then
    echo -e "\n${CyanColor}Compiling project in release mode${NoColor}"
    compilerelease
  else
    compiledefault
  fi

  compile
fi
