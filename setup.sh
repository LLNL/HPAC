#!/bin/bash

prefix=$1
threads=$2
current_dir=$(pwd)

NOCOLOR='\033[0m'
RED='\033[0;31m'
GREEN='\033[0;32m'
ORANGE='\033[0;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
LIGHTGRAY='\033[0;37m'
DARKGRAY='\033[1;30m'
LIGHTRED='\033[1;31m'
LIGHTGREEN='\033[1;32m'
YELLOW='\033[1;33m'
LIGHTBLUE='\033[1;34m'
LIGHTPURPLE='\033[1;35m'
LIGHTCYAN='\033[1;36m'
WHITE='\033[1;37m'

clang_bin=$prefix/bin/clang
#approx_runtime_lib=$prefix/lib/libapprox.so
approx_runtime_lib=/dev/null


if [ ! -f $clang_bin ]; then
  mkdir -p build_compiler
  mkdir -p $prefix
  pushd build_compiler
  cmake -G Ninja \
    -DCMAKE_INSTALL_PREFIX=$prefix \
    -DLLVM_CCACHE_BUILD='Off'\
    -DCMAKE_EXPORT_COMPILE_COMMANDS='On'\
    -DCMAKE_BUILD_TYPE='RelWithDebInfo' \
    -DLLVM_FORCE_ENABLE_STATS='On' \
    -DLLVM_ENABLE_PROJECTS='clang;openmp' \
    -DLLVM_OPTIMIZED_TABLEGEN='On' \
    -DCLANG_BUILD_EXAMPLES='On' \
    -DBUILD_SHARED_LIBS='On' \
    -DLLVM_ENABLE_ASSERTIONS='Off' \
    ../llvm

    ninja -j $threads
    ninja -j $threads install 
    popd
    echo "#!/bin/bash" > hpac_env.sh
    echo "export PATH=$prefix/bin/:\$PATH" >> hpac_env.sh
    echo "export LD_LIBRARY_PATH=$prefix/lib/:\$LD_LIBRARY_PATH" >> hpac_env.sh
    echo "export CC=clang" >> hpac_env.sh
    echo "export CPP=clang++" >> hpac_env.sh
fi

source hpac_env.sh

if [ ! -f $approx_runtime_lib ]; then
  mkdir build_hpac
  pushd build_hpac
  CC=clang CPP=clang++ cmake -G Ninja \
      -DCMAKE_INSTALL_PREFIX=$prefix \
      -DLLVM_EXTERNAL_CLANG_SOURCE_DIR=${current_dir}/clang/ \
      -DPACKAGE_VERSION=15.0.0 \
	  -DLIBAPPROX_ENABLE_SHARED=0 \
	  -DDEV_STATS=0 \
     ../approx
    ninja -j $threads
    ninja -j $threads install
    popd
fi
pushd ./approx/approx_utilities/

if [ ! -f 'original_src.tar.gz' ]; then
  wget -O original_src.tar.gz 'https://www.dropbox.com/s/pj11naf3twdrv23/original_src.tar.gz?dl=0'
fi

if [ ! -d 'original_src' ]; then
  tar -xvf original_src.tar.gz
fi

printf "${RED}Creating Benchmarks${NOCOLOR}\n"

if [ ! -d benchmarks/blackscholes ]; then
  cp -r original_src/parsec-3.0/pkgs/apps/blackscholes/src/ benchmarks/blackscholes
  printf "${GREEN}Blackscholes${NOCOLOR}\n"
  pushd benchmarks/blackscholes
  patch -p1 < ../patches/blackscholes.patch 
  popd
fi

if [ ! -d benchmarks/HPCCG ]; then
  printf "${GREEN}HPCCG${NOCOLOR}\n"
  cp -r original_src/HPCCG benchmarks/HPCCG
  pushd benchmarks/HPCCG
  patch -p1 < ../patches/HPCCG.patch
  popd
fi

if [ ! -d benchmarks/cfd ]; then
  printf "${GREEN}CFD${NOCOLOR}\n"
  cp -r original_src/rodinia_3.1/openmp/cfd/ benchmarks/cfd
  pushd benchmarks/cfd
  patch -p1 < ../patches/euler.patch
  popd
fi

if [ ! -d benchmarks/kmeans ]; then
  printf "${GREEN}K-Means${NOCOLOR}\n"
  cp -r original_src/rodinia_3.1/openmp/kmeans/kmeans_openmp/ benchmarks/kmeans/
  pushd benchmarks/kmeans
  patch -p1 < ../patches/kmeans.patch
  popd
fi

if [ ! -d benchmarks/lavaMD ]; then
  printf "${GREEN}lavaMD${NOCOLOR}\n"
  cp -r original_src/rodinia_3.1/openmp/lavaMD/ benchmarks/
  pushd benchmarks/lavaMD
  patch -p1 < ../patches/lavaMD.patch
  popd
fi

if [ ! -d benchmarks/particlefilter ]; then
  printf "${GREEN}Particle Filter${NOCOLOR}\n"
  cp -r original_src/rodinia_3.1/openmp/particlefilter/ benchmarks/particlefilter
  pushd benchmarks/particlefilter
  patch -p1 < ../patches/particlefilter.patch
  popd
fi

if [ ! -d benchmarks/lulesh ]; then
  printf "${GREEN} Lulesh ${NOCOLOR}\n"
  cp -r original_src/LULESH/ benchmarks/lulesh/
  pushd benchmarks/lulesh
  patch -p1 < ../patches/lulesh.patch
  popd
fi

if [ ! -d benchmarks/leukocyte ]; then
  printf "${GREEN}Leukocyte ${NOCOLOR}\n"
  cp -r original_src/rodinia_3.1/openmp/leukocyte/ benchmarks/leukocyte
  pushd benchmarks/leukocyte
  patch -p1 < ../patches/leukocyte.patch
  popd
fi

printf "${RED} Patched all benchmarks${NOCOLOR}\n"

printf "${GREEN} Copying Inputs${NOCOLOR}"

if [ ! -f benchmarks/blackscholes/random_input.bin ]; then
  printf "${RED} copying blackscholes ${NOCOLOR}\n"
  cp original_src/inputs/random_input.bin benchmarks/blackscholes 
fi

if [ ! -f  benchmarks/kmeans/kdd_bin ]; then
  printf "${RED} copying kdd_bin ${NOCOLOR}\n"
  cp original_src/inputs/kdd_bin benchmarks/kmeans 
fi

if [ ! -f  benchmarks/leukocyte/testfile.avi ]; then
  printf "${RED} copying leukocyte ${NOCOLOR}\n"
  cp original_src/inputs/testfile.avi benchmarks/leukocyte
fi

if [ ! -f benchmarks/cfd/fvcorr.domn.097K ]; then
  printf "${RED} copying cfd ${NOCOLOR}\n"
  cp original_src/rodinia_3.1/data/cfd/fvcorr.domn.097K benchmarks/cfd/
fi

printf "${GREEN} Creating Approximate search space${NOCOLOR}\n"
mkdir -p build
pushd build
cmake ../
make -j
popd
