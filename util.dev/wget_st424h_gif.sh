#!/bin/sh
set -x

day1=20190801
day1=20190802

day2=20191014

UTILROOT=/gpfs/dell1/nco/ops/nwprod/prod_util.v1.1.2
FINDDATE=$UTILROOT/ush/finddate.sh

day=$day1

datadir=/gpfs/dell2/ptmp/Ying.Lin/wget_st4
cd $datadir

while [ $day -le $day2 ];
do 
  URLPATH=https://ftp.emc.ncep.noaa.gov/mmb/precip/pcpanl.v4.0.0/pcpanl.$day/
  wget ${URLPATH}/st4_conus.${day}12.24h.gif

  day=`$FINDDATE $day d+1`
done

exit
