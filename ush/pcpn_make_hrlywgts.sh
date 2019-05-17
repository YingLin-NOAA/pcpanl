#!/bin/ksh
set -x
v6date=$1

cnrfcgrid="20 0 0 0 0 0 0 0 370 388 29200000 234351000 8 60000000 255000000 4762000 4762000 0 64"
nwrfcgrid="20 0 0 0 0 0 0 0 302 357 38435000 236957000 8 60000000 255000000 4763000 4763000 0 64"


ihrback=5

while [ $ihrback -ge 0 ];
do
  v1date=`$NDATE -${ihrback} $v6date`
  v1day=${v1date:0:8}
  v1hr=${v1date:8:2}
  mrms=GaugeCorr_QPE_01H_00.00_${v1day}-${v1hr}0000.grib2
  # Check to see of the MRMS file exists in the working directory already from
  # an earlier weights calculation.  If not, attempt to copy over from dcom
  if [ ! -s $mrms ] 
  then 
    cp $MRMSDIR/$mrms.gz .
    gunzip $mrms.gz
  fi

  if [ ! -s $mrms ]; then 
    echo $mrms not available. Exit - do not make weights for the 6h period.
    exit
  fi

  $MYWGRIB2 $mrms -rpn "dup:-3:!=:mask" -set_scaling -1 0 -set_bitmap 1 -set_grib_type c3 -grib_out mrms.$v1date
  # Copy it to the pcpurma.yyyymmdd for oconus fill:
  URMADIR=$COMURMA.$v1day
  if [ ! -s $URMADIR/mrms.$v1date.gz ]; then
    gzip -c mrms.$v1date > $URMADIR/mrms.$v1date.gz 
  fi

  if [ $RUN_ENVIR = dev ]; then   # for developers
    if [ ! -d $NOSCRUBDIR/$v1day ]; then 
      mkdir -p $NOSCRUBDIR/$v1day
    fi

    if [ ! -s $NOSCRUBDIR/$v1day/mrms.$v1date ]; then
      gzip -c mrms.$v1date > $NOSCRUBDIR/$v1day/mrms.$v1date.gz 
    fi
  fi  # dev: copy mrms file to noscrub     
  
  $COPYGB2 -g "$cnrfcgrid" -i3 -x mrms.$v1date mrms_153.$v1date
  $COPYGB2 -g "$nwrfcgrid" -i3 -x mrms.$v1date mrms_159.$v1date
  let ihrback=ihrback-1
done

v1datem5=`$NDATE -5 $v6date`
v1datem4=`$NDATE -4 $v6date`
v1datem3=`$NDATE -3 $v6date`
v1datem2=`$NDATE -2 $v6date`
v1datem1=`$NDATE -1 $v6date`

for rid in 153 159
do
  rm -f fort.*
  ln -sf mrms_$rid.$v1datem5          fort.11
  ln -sf mrms_$rid.$v1datem4          fort.12
  ln -sf mrms_$rid.$v1datem3          fort.13
  ln -sf mrms_$rid.$v1datem2          fort.14
  ln -sf mrms_$rid.$v1datem1          fort.15
  ln -sf mrms_$rid.$v6date            fort.16
  ln -sf hrlywgts_${rid}.$v6date.bin  fort.51
  $EXECpcpanl/st4_mrms_hrlywgts               
  export err=$?;err_chk
  echo 'st4_mrms_hrlywgts:  err=' $? 
done

v6day=${v6date:0:8}
cp hrlywgts_*.$v6date.bin $COMOUT/pcpanl.$v6day/.

exit
  

