#!/bin/ksh 
set -x

# This script creates a summary of transmission times for all hourly/6-hourly
# QPEs that have accumulation ending hours that's between [00Z-23Z] $day.  
# This is done by checks all available 
#  $DCOM/yyyymmdd/wgrbbul/qpe/QPE_processing_log
# files, 
#
# This script is called by scripts/expcpanl_stage4.sh.ecf
#

RFClist="105 150 151 152 153 154 155 156 157 158 159 160 161 162"

# Optional arguments:
# 1) yyyymmdd: day to make log for
# 2) 3-digit RFC id (must have yyyymmdd as first argument): single-RFC log.
#    Do not send data to RZDM (typically used to show an RFC how it is going)
#
singlerfc=NO
if [ $# = 0 ]; then
  #today=`date +%Y%m%d`
  #day0=`finddate.sh $today d-7`
  day0=`finddate.sh ${PDY:?} d-7`
else
  day0=$1
  if [ $# -gt 1 ]; then
    rid0=$2
    singlerfc=YES
    RFClist=$rid0
  fi
fi

WRKDIR=$DATAST4/st3log_day

if [ -d $WRKDIR ]; then 
# In case this script is run outside of a routine ST2/4 job: 
  rm -f $WRKDIR/*            
else
  mkdir -p $WRKDIR
fi
cd  $WRKDIR

day=day0

ls $DCOM/*/wgrbbul/qpe/QPE_processing_log > dcomloglist

fixedheader=$HOMEpcpanl/fix/st4_st3log_header.txt

# Put starting clock time on daily log:
date > st3log.$day0
echo '  ' >> st3log.$day0

# If doing a single RFC, write out that RFC's ID on the top of the 2nd column.
# Otherwise write out a full list of RFCs.
#
if [ $singlerfc = YES ]; then
   echo '                    RFC id:' $rid0 >> st3log.$day0
else
   cat $fixedheader >> st3log.$day0
fi 

typeset -R2 -Z hh delt
for delt in 01 06 24
do
  acc=${delt}h
  if [ $delt -eq 24 ]; then  # for 24h QPE, valid hour is only at 12Z. 
    hh=12
  else
    hh=00
  fi

  while [ $hh -le 23 ]; 
  do
    date0=${day0}${hh}
    hourheader=header.$date0.$acc
    echo $date0.$acc > $hourheader

    for dcomlog in `cat dcomloglist`
    do 
      if [ -s $dcomlog ]; then
        for rid in $RFClist
        do
#         2014-03-05 22:31 QPE.158.2014030522.01h
#         ----:----|----:----|----:----|----:----|
          grep QPE.$rid.$date0.$acc $dcomlog | cut -c 6-16 >> log.$rid.$date0.$acc
        done    
      fi  
    done # loop through all logs in $DCOM that might have contributed to this hour/6h
    echo '  ' >> st3log.$day0

#   header (2014030522.01h) contains 14 characters.  Each QPE file's incoming
#   time stamp (03-05 22:31) contains 11 characters.  14 RFCs in total 
#   (including Puerto Rico and Alaska).  Make each column 15 characters long.  
#   15*15=225.  But w200 works, 
#   w210 doesn't.  
    if [ $singlerfc = YES ]; then
       pr -t -m -e15 -w40 $hourheader log.$rid0.$date0.$acc >> st3log.$day0
    else
       pr -t -m -e15 -w225 $hourheader log.*.$date0.$acc >> st3log.$day0
    fi

    let hh=$hh+$delt

  done # Loop through each hour/6-hour segment ending in $day0
done   # go through acc=01h,06h and 24h

# Put ending clock time on daily log:
echo '  ' >> st3log.$day0
date >> st3log.$day0

if test $SENDCOM = 'YES'
then
  cp st3log.$day0 $COMOUT/${RUN}.$day0/.
  if [ $? -ne 0 ]; then
    echo "WARNING: The st3log was not successfully copied to $COMOUT/${RUN}.$day0"
  fi
fi

exit 0
