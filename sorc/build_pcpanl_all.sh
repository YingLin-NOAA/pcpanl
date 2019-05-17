#!/bin/sh

BASE=$(pwd)
export BASE

cd $BASE

#partial compile list
export BUILD_st2_combdpa=yes
export BUILD_st2_dpa_decode=yes
export BUILD_st2_mosaic=yes
export BUILD_st2_prephads=yes
export BUILD_st2_prepmetar=yes
export BUILD_st2_remap=yes
export BUILD_st2_stageii=yes
export BUILD_pcp_acc=yes
export BUILD_st4_mosaic=yes
export BUILD_st4_mrms_hrlywgts=yes
export BUILD_st4_oconus_grid_shift=yes
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

sleep 5

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

if [ $BUILD_st2_combdpa = yes ] ; then

echo " .... Building st2_combdpa .... "
./build_st2_combdpa.sh > $logs_dir/build_st2_combdpa.log 2>&1

fi

cd $BASE

##############################

if [ $BUILD_st2_dpa_decode = yes ] ; then

echo " .... Building st2_dpa_decode .... "
./build_st2_dpa_decode.sh > $logs_dir/build_st2_dpa_decode.log 2>&1

fi

cd $BASE

##############################

if [ $BUILD_st2_mosaic = yes ] ; then

echo " .... Building st2_mosaic .... "
./build_st2_mosaic.sh > $logs_dir/build_st2_mosaic.log 2>&1

fi

cd $BASE

##############################

if [ $BUILD_st2_prephads = yes ] ; then

echo " .... Building st2_prephads .... "
./build_st2_prephads.sh > $logs_dir/build_st2_prephads.log 2>&1

fi

cd $BASE

##############################

if [ $BUILD_st2_prepmetar = yes ] ; then

echo " .... Building st2_prepmetar .... "
./build_st2_prepmetar.sh > $logs_dir/build_st2_prepmetar.log 2>&1

fi

cd $BASE

##############################

if [ $BUILD_st2_remap = yes ] ; then

echo " .... Building st2_remap .... "
./build_st2_remap.sh > $logs_dir/build_st2_remap.log 2>&1

fi

cd $BASE

##############################

if [ $BUILD_st2_stageii = yes ] ; then

echo " .... Building st2_stageii .... "
./build_st2_stageii.sh > $logs_dir/build_st2_stageii.log 2>&1

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

if [ $BUILD_st4_oconus_grid_shift = yes ] ; then

echo " .... Building st4_oconus_grid_shift .... "
./build_st4_oconus_grid_shift.sh > $logs_dir/build_st4_oconus_grid_shift.log 2>&1

fi

cd $BASE

##############################

if [ $BUILD_st4_qpe6h_to_1h = yes ] ; then

echo " .... Building st4_qpe6h_to_1h .... "
./build_st4_qpe6h_to_1h.sh > $logs_dir/build_st4_qpe6h_to_1h.log 2>&1

fi

cd $BASE

##############################

