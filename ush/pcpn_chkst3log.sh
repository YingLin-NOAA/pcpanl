#!/bin/ksh 
#
# This script checks the current 
#   $DCOM/yyyymmdd/wgrbbul/qpe/QPE_processing_log
# compares against a saved version from the previous hour, and make a list
# of hourly/6-hourly periods that have new input from at least one RFC.  
# Future RFCs (the ones with valid yyyymmddhh that's later than the current
# yyyymmddhh) are not included in the list.  Updates made to $PDYm7 and older
# triggers an email to YL.  
# 
#   date/day: right now (yyyymmddhh) and yyyymmdd associated with it
#   datem1h/daymh1: an hour ago
# Steps:
#  1. Copy over /com/pcpanl/prod/pcpanl.$daym1h/qpe_log.prevhr
#     and the current $DCOM/$PDY/wgrbbul/qpe/QPE_processing_log
#     (and $DCOM/$daym1h/wgrbbul/qpe/QPE_processing_log if current
#     hour is 00Z (run at 00:33Z)
#     to the work directory
#  2. Save the wrkdir copy of 
#     $DCOM/$PDY/wgrbbul/qpe/QPE_processing_log to 
#        pcpanl/prod/pcpanl.$PDY/qpe_log.prevhr
#     (this should be the wrkdir copy, in case additional QPEs came in in the
#     short time gap between steps 1 and 2.  
#  3. Compare QPE_processing_log file (files, if 00Z) against the previous
#     hours qpe_log.prevhr to produce a list of new arrivals since the previous
#     ST4 run.  
#  4. Sort the above list to produce a list of unique accumulation periods 
#     with at list one RFC input.  If the yyyymmddhh on the list (the ending
#     hour of the accumulation period is earlier than ${PDYm7}${cyc}, send an 
#     email to YL.
# 

set -x

date=${PDY:?}${cyc:?}
datem1h=`$NDATE -1 $date`
daym1h=`echo $datem1h | cut -c1-8`

WRKDIR=$DATAST4/chkst3log

if [ -d $WRKDIR ]; then
  rm -f $WRKDIR/*
else
  mkdir -p $WRKDIR
fi

cd $WRKDIR

# for test only
for dday in $PDY $daym1h
do
  dir=$COMIN/${RUN}.${dday}
  if [ ! -d $dir ]; then
    mkdir -p $dir
  fi
done

# Put the whole day's worth of logs together; sort by dictionary order.
cp $COMIN/${RUN}.${daym1h}/qpe_log.prevhr .
cp $DCOM/$PDY/wgrbbul/qpe/QPE_processing_log dcom_qpe_log
cp dcom_qpe_log $COMIN/${RUN}.${PDY}/qpe_log.prevhr

if [ $cyc -eq 0 ]; then
  cp $DCOM/$daym1h/wgrbbul/qpe/QPE_processing_log dcom_qpe_log_yday
  diff qpe_log.prevhr dcom_qpe_log_yday | grep '^>' | sed 's/> //g' > list.newqpe.$date
  cat dcom_qpe_log >> list.newqpe.$date
else
  diff qpe_log.prevhr dcom_qpe_log | grep '^>' | sed 's/> //g' > list.newqpe.$date 
fi 

# This is what list.newqpe.$date should look like:
# 
# 2014-11-17 00:22 QPE.156.2014111621.01h
#                  ----:----|----:----|--
# ----:----|----:----|----:----|----:----|
# Also, some RFC sometimes send QPE files with a bad header:
# 2016-12-18 00:19 QPE.152..-232h
#   our logic below would toss out such files
# 
# In case the file contains fields such as "1404c1400", toss out records with 
# fewer than 3 fields (separated by a blank space), or fewer than 40 characters.
#

cat list.newqpe.$date | while read tmp 
do
  nfield=`echo $tmp | awk '{ print NF }'`
  nchar=`echo $tmp | awk '{ print length }'`
  rfcid=`echo $tmp | cut -c 22-24`
  # acc is for NWRFC/CNRFC - check to see if it's 06h - for hourly mosaic to-do
  acc=`echo $tmp | cut -c 37-39`
  if [[ $nfield -lt 3 || $nchar -lt 35 ]]; then
    # skip diff output lines like "1441a1442,1534"
    continue
  else
    if [ $rfcid -eq 105 ]; then
      echo $tmp | awk '{ print $NF }' >> checked.list.newqpe.pr.$date
    elif [ $rfcid -eq 151 ]; then
      echo $tmp | awk '{ print $NF }' >> checked.list.newqpe.ak.$date
    else
      echo $tmp | awk '{ print $NF }' >> checked.list.newqpe.conus.$date
      # if the input file is 6h QPE from CNRFC or NWRFC, we'll also count them
      # as 'to do' for hourly mosaic for these 6 hours, if the hourly weights
      # are available. 
      if [ $rfcid -eq 153 -o $rfcid -eq 159 ] && [ $acc = 06h ]
      then
        echo $tmp | awk '{ print $NF }' >> checked.list.newqpe.westcoast.$date
      fi
    fi
  fi
done

# Look at checked.list.newqpe.westcoast.$date (all 6h) make a list of unique
# yyyymmddhh.06h (e.g. if both CNRFC and NWRFC sent QPEs for the same time
# period, or if an RFC sent more than one in the past hour, put only one
# yyyymmddhh.06h on list.uniq. 
cat checked.list.newqpe.westcoast.$date | cut -c 9-22 | sort -u -r > list.uniq.qpehrs.westcoast.$date

# If there has been a new 6h QPE from either NWRFC or CNRFC, and the MRMS 
# weights files for this time period exist, add the corresponding six hourlies
# (not "6-hourly") to checked.list.newqpe.conus.$date as
# "yyyy-mm-dd mn:ss QPE.WST.yyyymmddhh.01h", with "WST" standing for 
# either '153' or '159' (it's possible to make separate "uniq" lists for each
# West Coast RFC, but too cumbersome and does not add anything to the process)
#
cat list.uniq.qpehrs.westcoast.$date | while read tmp
do
  endtime=`echo $tmp | awk -F"." '{print $1}'`
# endday=${enddate:0:8}
  endday=${endtime:0:8}
  if [ -s $COMOUT/pcpanl.$endday/hrlywgts_153.$endtime ]; then
    date1h=`$NDATE -5 $endtime`
    while [ $$date1h -le $endtime ]
    do
      #below: 2017-06-22 18:33 QPE.WST.2017060510.01h >> checked.list ... 
      echo ${tmp:0:16} QPE.WST.$date1h.01h >> checked.list.newqpe.$region.$date
      date1h=`$NDATE +1 $date1h`
    done
  fi
done

# use '-r' sort option so the most recent time will be listed first in the 
# list.uniq - we want to process the most recent Stage IV hourly first, so
# that it can be used for RTMA.

for region in ak pr conus
do 
  cat checked.list.newqpe.$region.$date | cut -c 9-22 | sort -u -r > list.uniq.qpehrs.$region.$date
done 

for region in conus ak pr 
do 
  cat list.uniq.qpehrs.$region.$date | while read tmp
  do
    endtime=`echo $tmp | awk -F"." '{print $1}'`
    acc=`echo $tmp | awk -F"." '{print $2}'`

    # if the accumulation time is later than current hour, skip this one.
    if [ $endtime -gt $date ]; then continue; fi

    # ConUS hourlies to-do list (excluding regularly scheduled) 
    # include hours with new incoming QPE, going back 23 hours. 
    # 
    # PR hourly/6h/24h
    # are re-done whenever there is at least one new hourly input (within the
    # past 7 days).  We're not expecting hourly QPE from AK. 
    if [ $acc = '01h' ]; then
      if [ $region = conus ]; then
        if [ $endtime -gt ${PDYm1}${cyc} ]; then 
          echo $tmp >> todo4.conus.01h.$date
        fi
      elif [ $region = pr ]; then
        if [ $endtime -gt ${PDYm8}12 ]; then   # don't go back more than 8 days
          endday=`echo $endtime | cut -c 1-8`
          enddayp1=`finddate.sh $endday d+1`
          endhr=`echo $endtime | cut -c 9-10`
          # hourly: ---------------------------------------------
          echo $tmp >> todo4.pr.01h.$date.presort

          # 06h:    ---------------------------------------------
          if [ $endhr -eq 00 ]; then
            date6h=${endday}00
          elif [ $endhr -le 06 ]; then
            date6h=${endday}06
          elif [ $endhr -le 12 ]; then
            date6h=${endday}12
          elif [ $endhr -le 18 ]; then
            date6h=${endday}18
          else
            date6h=${enddayp1}00
          fi

          # Add the ending 6h to the 06h to-do list, if it's earlier than
          # the current hour:
          if [ $date6h -le $date ]; then  
            echo $date6h.06h >> todo4.pr.06h.$date.presort
          fi
           
          # 24h:    ---------------------------------------------
          if [ $endhr -le 12 ]; then
            date24h=${endday}12
          else
            date24h=${enddayp1}12
          fi

          if [ $date24h -le $date ]; then  
            echo ${date24h}.24h >> todo4.pr.24h.$date.presort
          fi
        fi # check to see that ending time of accum is not more than 8 days back
      elif [ $region = ak ]; then
        echo 'Not expecting hourly QPE from Alaska.'
      fi # for ac=01h, region is conus, pr, ak?

    # For Conus 6-hourlies, the to-do list (excluding regularly scheduled) 
    #   include those 6-hourlies with new incoming 6-hourly QPE, going back 
    #   42 hours, 
    # AK 6-hourlies are re-done whenever there are new incoming AK QPE06
    # PR 24-hour is from hourly QPEs, not from the 6h QPE.
    elif [ $acc = '06h' ]; then
      if [ $region = conus ]; then
        if [ $endtime -gt ${PDYm2}${cyc} ]; then 
          echo $tmp >> todo4.conus.06h.$date
        fi
      elif [ $region = ak ]; then
        if [ $endtime -gt ${PDYm8}12 ]; then 
          echo $tmp >> todo4.ak.06h.$date.presort
        fi
      fi
    elif [ $acc = '24h' ]; then
      # Only Alaska 24h is triggered by new incoming 24h QPE. This list
      # needs to be sorted to make entries unique.
      if [ $region = ak ]; then
        echo $tmp >> todo4.ak.24h.$date.presort
      fi
    fi # acc in the new QPE file is 01/06/24?
  done # going through each item in list.uniq.qpehrs.$region.$date
done # for region in conus ak pr 

# Re-run hourly analysis for 1/3/5/7 days ago:
cat >> todo4.conus.01h.$date <<EOF01h
$PDYm7$cyc.01h
$PDYm5$cyc.01h
$PDYm3$cyc.01h
$PDYm2$cyc.01h
$PDYm1$cyc.01h
EOF01h

# Re-run PR hourly analysis for 7 days ago:
cat >> todo4.pr.01h.$date <<EOF01h
$PDYm7$cyc.01h
EOF01h

# At 15:33Z, re-run 1-hourly analyses for the 1h ending 13Z-14Z 
# the previous day (NWRFC 6h ending at 18Z $PDYm1 usually arrive
# between 14-15Z $PDY.  15-18Z hourlies would have been taken care of by
# the 24h reruns, but 13-14Z $PDYm1 are just over a day old when 
# QPE.159.${PDYm1}18.06h arrives
#
if [ $cyc -eq 15 ]; then
  cat >> todo4.conus.01h.$date <<EOF01h
${PDYm1}13.01h
${PDYm1}14.01h
EOF01h
fi

# At 12:33Z, re-run 6h and 24h analyses and for the 24h periods ending at 
# 12Z $PDYm7, $PDYm5, $PDYm3, $PDYm2, $PDYm1 for ConUS, $PDYm7 for OConUS.  

if [ $cyc -eq 12 ]; then
  cat >> todo4.conus.06h.$date <<EOF06h
${PDYm8}18.06h
${PDYm7}00.06h
${PDYm7}06.06h
${PDYm7}12.06h
${PDYm6}18.06h
${PDYm5}00.06h
${PDYm5}06.06h
${PDYm5}12.06h
${PDYm4}18.06h
${PDYm3}00.06h
${PDYm3}06.06h
${PDYm3}12.06h
${PDYm3}18.06h
${PDYm2}00.06h
${PDYm2}06.06h
${PDYm2}12.06h
${PDYm2}18.06h
${PDYm1}00.06h
${PDYm1}06.06h
${PDYm1}12.06h
EOF06h

# Re-run PR hourly analysis for 7 days ago:
cat >> todo4.pr.01h.$date.presort <<EOF01h
$PDYm7$cyc.01h
EOF01h

  cat >> todo4.ak.06h.$date.presort <<EOF06h
${PDYm8}18.06h
${PDYm7}00.06h
${PDYm7}06.06h
${PDYm7}12.06h
EOF06h

  cat >> todo4.pr.06h.$date.presort <<EOF06h
${PDYm8}18.06h
${PDYm7}00.06h
${PDYm7}06.06h
${PDYm7}12.06h
EOF06h

  cat >> todo4.conus.24h.$date <<EOF24h
${PDY}12.24h
${PDYm1}12.24h
${PDYm2}12.24h
${PDYm3}12.24h
${PDYm5}12.24h
${PDYm7}12.24h
EOF24h

  cat >> todo4.ak.24h.$date.presort <<EOF24h
${PDYm7}12.24h
EOF24h

  cat >> todo4.pr.24h.$date.presort <<EOF24h
${PDYm7}12.24h
EOF24h
fi # $cyc -eq 12?

# At 18:33Z, re-run ConUS 24-hour mosaic for the 24h periods ending at 
#  12Z $PDYm1.  This is the additional 30-h rerun requested by WPC's Greg Carbin
#  on 2019/05/10, so that an RFC update for the previous day (24h ending at 
#  12Z $daym1 might be incorporated into the AHPS water.weather.gov page
#  before the next day's 12Z cycle run.  
#
if [ $cyc -eq 18 ]; then
  cat >> todo4.conus.24h.$date <<EOF24h
${PDYm1}12.24h
EOF24h
fi 

# run 24h ConUS mosaic for the day at 12:33/13:33 .../23:33Z (the 12:33 case 
# is covered above.
if [ $cyc -gt 12 ]; then
  cat >> todo4.conus.24h.$date <<EOF24h
${PDY}12.24h
EOF24h
fi

# Sort the ak and pr todo lists to ensure that the entries are unique.  
# ConUS list was make unique earlier (ConUS logic somewhat different because
# there are 12 RFCs contributing to the ConUS mosaic)

for acc in 01h 06h 24h
do 
  for region in ak pr
  do
    cat todo4.$region.$acc.$date.presort | sort -u -r >> todo4.$region.$acc.$date
  done
done

# toplot4.$date is made in scripts/expcpn_stage4.sh, before calling
# ush/nam_pcpn_st4mosaic.sh and nam_pcpn_st4oconus.sh

cp list.* todo4.*.$date $COMOUT/${RUN}.${PDY}/

exit
