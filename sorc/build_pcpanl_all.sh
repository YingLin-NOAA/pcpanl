#!/bin/sh

BASE=$(pwd)
export BASE

cd $BASE

#partial compile list
export BUILD_grib2_lib=yes
export BUILD_pcp_acc=yes
export BUILD_st4_mosaic=yes
export BUILD_st4_mrms_hrlywgts=yes
export BUILD_st4_oconus_convert=yes
export BUILD_st4_qpe6h_to_1h=yes

mkdir $BASE/logs
export logs_dir=$BASE/logs

set -x
#Phase2 . /usrx/local/Modules/default/init/sh
. /usrx/local/prod/lmod/lmod/init/ksh  #dell

module purge
module use ../modulefiles
module load PCPANL

module list

sleep 1

if [[ -z "$1" ]] ; then
   export DEBUG_OPT=0
fi

if [[ -n "$1" ]] ; then
   if [[ "$1" = "debug" ]] ; then
      export DEBUG_OPT=1
   else
      export DEBUG_OPT=0
   fi
fi

echo "DEBUG_OPT"=$DEBUG_OPT

#if all BUILD switches=yes clean up ./exec directory

env | grep BUILD | grep no
err=$?

if [ $err -ne 0 ] ; then 

if [ -d $BASE/../exec ]; then
  rm -f $BASE/../exec/*
else
  mkdir $BASE/../exec
fi

fi

##############################

if [ $BUILD_grib2_lib = yes ] ; then

echo " .... Building grib2/wgrib2 library .... "
cd ../lib/grib2
build_wgrib2.sh > $logs_dir/build_wgrib2_lib.log 2>&1

export BUILD_grib2_lib=yes


fi

cd $BASE

##############################

if [ $BUILD_pcp_acc = yes ] ; then

echo " .... Building pcp_acc .... "
./build_pcp_acc.sh > $logs_dir/build_pcp_acc.log 2>&1

fi

cd $BASE

##############################

if [ $BUILD_st4_mosaic = yes ] ; then

echo " .... Building st4_mosaic .... "
./build_st4_mosaic.sh > $logs_dir/build_st4_mosaic.log 2>&1

fi

cd $BASE

##############################

if [ $BUILD_st4_mrms_hrlywgts = yes ] ; then

echo " .... Building st4_mrms_hrlywgts .... "
./build_st4_mrms_hrlywgts.sh > $logs_dir/build_st4_mrms_hrlywgts.log 2>&1

fi

cd $BASE

##############################

if [ $BUILD_st4_oconus_convert = yes ] ; then

echo " .... Building st4_oconus_convert .... "
./build_st4_oconus_convert.sh > $logs_dir/build_st4_oconus_convert.log 2>&1

fi

cd $BASE

##############################

if [ $BUILD_st4_qpe6h_to_1h = yes ] ; then

echo " .... Building st4_qpe_6h_to_1h .... "
./build_st4_qpe6h_to_1h.sh > $logs_dir/build_st4_qpe6h_to_1h.log 2>&1

fi

cd $BASE

##############################

