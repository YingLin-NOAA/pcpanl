#!/bin/ksh
set -x
# scp /com/hourly/prod/hourly.yyyymmdd/sfctbl.hh from prodwcoss.

if [ $# -eq 0 ]; then
  now=`date -u +%Y%m%d%H`
else
  now=$1
fi

day=`echo $now | cut -c 1-8`
hh=`echo $now | cut -c 9-10`

DIR=/gpfs/dell2/ptmp/Ying.Lin/pcpanl

HRLYDIR=$DIR/hry_mos.$day

if [ ! -d $HRLYDIR ]; then mkdir -p ${HRLYDIR}; fi

scp Ying.Lin@prodwcoss:/gpfs/dell1/nco/ops/com/mos/prod/hry_mos.$day/sfctbl.$hh $HRLYDIR/.

exit


