#!/bin/bash

#module load python
# 1. build bufrlib
cd bufrlib
rm -rf build
mkdir build; cd build
cmake .. -DCMAKE_INSTALL_PREFIX=../install -DCMAKE_INSTALL_PREFIX=../install -DBUILD_TESTING=OFF #-DENABLE_PYTHON=ON
make -j4
make install
cd ../..

# 2. build datetime-fortran
cd datetime-fortran
rm -rf build
mkdir build; cd build
cmake ..
make
cd ../..


# 3. build pbdump
BUFRLIB="./bufrlib/install/lib64/libbufr_4.a"
TIMELIB="./datetime-fortran/build/lib/libdatetime.a"
TIMEINC="./datetime-fortran/build/include"

if which ifort &>/dev/null; then 
  ifort pbdump.f90 -I${TIMEINC} ${TIMELIB} ${BUFRLIB} -o pbdump
elif which ifx &>/dev/null; then
  ifx pbdump.f90 -I${TIMEINC} ${TIMELIB} ${BUFRLIB} -o pbdump
else
  echo "neither ifort nor ifx found"
  exit 1
fi
