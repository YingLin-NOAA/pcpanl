#!/bin/ksh
# 
# Prepare Alaska and Puerto Rico QPE for Stage IV and URMA.  
#   AK: has 6h and 24h QPEs. Use them directly, after grid shifting.
#   PR: has 1h and 6h QPEs.  Use hourlies to make 1h/6h/24h ST4, after grid 
#       shifting.  For 6h ST4/URMA, if an hourly is missing, use
#       the QPE.105.yyyymmddhh.06h received at $DCOM.  24h st4_pr is from 
#       hourlies only.
#
# For both AK and PR: map 6-hourlies to URMA domains.  
#   
set -x 

region=$1
date=$2
ac=$3

day=`echo $date | cut -c 1-8`

# Flag to determine whether the needed QPE file(s) exist before applying
# shift by 1/2 of a grid size. 
# For AKQPE's run triggered by the presence of new QPE in log, if we are here, 
# then the needed QPE file does exist, 
#  (since no adding is done).  Though in the case of the final 7-day re-run,
#  this would not be the case - since it's checked on a fixed schedule.  We'll
#  set qpeaok=NO then.
# For PRQPE
#   if ac=01h, then qpeaok=YES.  
#   if ac=06h, then qpeaok=YES if all the six 1h QPE files going into
#     the 6h total exist, OR if the corresponding 6h QPE exist.  
#   if ac=24h, then qpeaok=YES if all the twenty-four 1h QPE files going into
#     the 24h total exist. 
qpeaok=YES

if [ $region = pr ]; then
  rid=105
elif [ $region = ak ]; then
  rid=151
fi

cd $DATAST4

postmsg $jlogfile "Stage4: process OConUS $date $ac"

# Process hourly (PR only), 6h (and URMA), then 24h.
if [ $ac = 01h ]; then
  qpedir=$DCOM/$day/wgrbbul/qpe

elif [ $ac = 06h ]; then
  if [ $region = ak ]; then
    qpedir=$DCOM/$day/wgrbbul/qpe
  elif [ $region = pr ]; then
    rid=105
    qpedir=$DATAST4/sum6h
    if [ ! -d sum6h ]; then mkdir sum6h; fi 
    cd sum6h
    # flag to track if there are missing 1hr QPEs.  If so, copy over 6-hourly
    # QPE for that RFC, rather than accumulate from hourlies.  
    qpe1hok=YES                 
    accdate=`$NDATE -5 $date`
    cat > input_acc <<EOF
obs
QPE.105.
EOF
    while [ $accdate -le $date ]
    do 
      accday=`echo $accdate | cut -c 1-8`
      dcomdir=$DCOM/$accday/wgrbbul/qpe
      qpe1hfile=$dcomdir/QPE.105.$accdate.01h
      if [ -s $qpe1hfile ]; then
        echo $qpe1hfile >> input_acc
        accdate=`$NDATE +1 $accdate`
      else
        qpe1hok=NO
        echo $dcomdir/QPE.105.$accdate.01h not found.  Use 6h PRQPE.
        break
      fi
    done

    if [ $qpe1hok = YES ]; then
      export pgm=pcp_acc
      $EXECpcpanl/pcp_acc < input_acc
      export err=$?; echo "After $pgm, err=$err"; err_chk
    else
      # note that we must use the path $DCOM/$day/wgrbbul/qpe/ below, 
      # rather than $dcomdir above.  E.g. if, say, 1h QPE ending at 2016012419
      # from MARFC is missing, we need to look for the 6-hourly QPE in 
      # $DCOM/20160125/, rather than $DCOM/20160124, where the hourly QPEs come
      # from.  
      cp $DCOM/$day/wgrbbul/qpe/QPE.105.$date.06h .
      err=$?
    
      if [ $err -ne 0 ]; then 
        qpeaok=NO
      fi
    fi
    cd $DATAST4
  fi # is region ak or pr?

elif [ $ac = 24h ]; then
  if [ $region = ak ]; then
    qpedir=$DCOM/$day/wgrbbul/qpe
  elif [ $region = pr ]; then
    # 24h PR totals are summed from hourly.  If an hourly is missing, do not
    # make 24h sum (6h PR QPE not used)
    qpedir=$DATAST4/sum24h
    if [ ! -d sum24h ]; then mkdir sum24h; fi 
    cd sum24h

    # flag to track if there are missing 1hr QPEs.  If so, copy over 6-hourly
    # QPE for that RFC, rather than accumulate from hourlies.  
    qpe1hok=YES                 
    accdate=`$NDATE -23 $date`
    cat > input_acc <<EOF
obs
QPE.105.
EOF
    while [ $accdate -le $date ]
    do 
      accday=`echo $accdate | cut -c 1-8`
      dcomdir=$DCOM/$accday/wgrbbul/qpe
      qpe1hfile=$dcomdir/QPE.105.$accdate.01h
      if [ -s $qpe1hfile ]; then
        echo $qpe1hfile >> input_acc
        accdate=`$NDATE +1 $accdate`
      else
        qpe1hok=NO
        qpeaok=NO
        echo $dcomdir/QPE.105.$accdate.01h not found.  Do not make 24h sum.
        break
      fi
    done

    if [ $qpe1hok = YES ]; then
      export pgm=pcp_acc
      $EXECpcpanl/pcp_acc < input_acc
      export err=$?; echo "After $pgm, err=$err"; err_chk
    fi
  fi # AK or PR?  (inside of $ac=24h)
fi   #ac=01h/06h/24h?

cd $DATAST4
# Shift the 1h/6h/24h QPE by 1/2 dx/dy:
QPEfile=$qpedir/QPE.$rid.$date.$ac
# double-check for the existence of the input QPE:
if [[ $qpeaok = YES && -s $QPEfile ]]; then
  export pgm=st4_oconus_convert
  ln -sf $QPEfile                        fort.11
  ln -sf $HOMEpcpanl/parm/grib2_pds.tbl  fort.12
  ln -sf st4_${region}.$date.$ac.grb2    fort.51
  $EXECpcpanl/st4_oconus_convert << ioEOF
$date
ioEOF
  export err=$?; echo "After $pgm, err=$err"; err_chk

  # we are in $DATAST4.  'toplot4' is in the parent directory.
  echo $date.$ac.$region >> $DATA/toplot4.${PDY}${cyc}

else
  echo "Either qpeaok=NO or $QPEfile does not exist."
  qpeaok=NO
fi

##########################################################################

if [ $qpeaok = YES ] &&  [ $ac = 06h -o $ac = 01h -a $region = pr ]
then
  # Add the item to the todo_urma list for this cycle:
  # Note that we are assuming that ush/pcpn_st4mosaic.sh and pcpn_st4oconus.sh
  # are running sequentially, so items are added to todo_urma list conus first,
  # then OConUS (the two ush scripts won't write to the todo list at the 
  # same time.
  echo $date.$ac.$region >> $DATA/todo_urma.$PDY$cyc
fi # when ac=06, create the precip URMA.

if [ $qpeaok = YES ]; then
  if test $SENDCOM = 'YES'
  then
    cp st4_${region}.$date.$ac.grb2 $COMOUT/${RUN}.$day/.
  fi

  if test $SENDDBN = 'YES'
  then
    $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day/st4_${region}.$date.$ac.grb2
  fi

fi # qpeaok=YES?
#####################################################################
# GOOD RUN
postmsg $jlogfile "$0 completed normally"
#####################################################################


############## END OF SCRIPT #######################

# exit does not work
