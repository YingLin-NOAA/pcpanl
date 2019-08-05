#!/bin/sh
set -x

rid=105
yyyymmddhh=2019080500
acc=06h
yyyymmdd=${yyyymmddhh:0:8}
DCOM=/gpfs/dell1/nco/ops/dcom/prod/$yyyymmdd/wgrbbul/qpe
PCPANL=/gpfs/dell1/nco/ops/nwpara/pcpanl.v3.1.3
if [ $rid -eq 105 ]; then
  reg=pr
elif [ $rid -eq 151 ]; then
  reg=ak
else
  echo 'unknown RFC ID for OCONUS.  Exit'
  exit
fi

ln -sf $DCOM/QPE.$rid.$yyyymmddhh.$acc    fort.11
ln -sf st4_${reg}.$yyyymmddhh.$acc        fort.51
$PCPANL/exec/st4_oconus_grid_shift << ioEOF
$yyyymmddhh
ioEOF

exit

