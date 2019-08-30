#!/bin/ksh
#######################################################################
#  Purpose: Produce hourly/6-hourly/24h 4km Stage IV pcpn analysis from 
#    the regional Stage III analyses provided by the 12 CONUS RFCs.
#    Also, make hourly/6h/24h st4_pr and 6h/24h st4_ak.
#  ConUS ST4:
#    hrly:     from QPEs from the RFCs (except NW and CN)
#    6-hourly: from 6h QPEs from NW/CN/CB/MB RFCs, and hourlies from
#      the other 8 central/eastern RFCs.  If not all hourlies are available
#      from a central/eastern RFC, then the 6-hourly QPE from that RFC is
#      used. 
#    24-hourly: from 24h QPE from NW and MB RFC (if not available, use 6hr QPE)
#      from 6h QPE from CN and CB RFC, 
#      and from hourly QPE from the 8 central/eastern RFCs
#    Hourlies re-made every hour in the first 23h after validation time
#      (if there is new hourly QPE input), then again at 1/3/5/7-days after
#      validation time
#    6-hourlies re-made 
#  Puerto Rico:
#    hourly:   from hourly PR QPE
#    6-hourly: from hourly PR QPE (convert to URMA_pr)
#    24h:      from hourly PR QPE
#    st4_pr hourly and 24-hourly re-made whenever a new hourly arrives
#      (24h re-made if it's past 12Z and all 24 hours are present)
#      6-hourly re-made whenever a 6h QPE arrives. 
#    Make a final re-run for hourly/6hourly/24h 168 hours (7 days) after 
#      validation time.  
#
#  Alaska:
#    6-hourly: from 6-hourly AK QPE (convert to URMA_ak)
#    24h:      from 24h AKQPE
#    st4_ak made-remade whenever a new QPE is received from AK (within 7 days)
#    Make a final re-run for 6hourly/24h 168 hours (7 days) after 
#      validation time.  
#######################################################################
#
echo "------------------------------------------------"
echo "JPCPN_ANAL Stage IV processing                  "
echo "------------------------------------------------"
# "History: Sept 1, 2002 - First implementation of this script."
#
#######################################################################

set -x

postmsg $jlogfile "Begin Stage IV analysis for $job"

########################################

export TZ=UTC
JDATA=$DATA

# 
# 
#
# Check $DCOM/PDY/wgrbbul/qpe/QPE_processing_log (and if hr0=00, 
# also check the QPE_processing_log from $day0m1), compare to 
# /com/pcpanl/prod/pcpanl.$daym1h/qpe_log.prevhr, to come up with
# to-do lists for hourly and 6-hourly mosaics, including 1/3/5/7-day reruns.
#
# At 02:33/08:33/14:33/20:33Z (i.e. 2h33m after the valid time of each 6h
#   accumulation period) make MRMS weights.  
# Do this step, before pcpn_chkst3log.sh, so that more QPEs might come in 
#   before we check for them. 
if [ $cyc -eq 02 -o $cyc -eq 08 -o $cyc -eq 14 -o $cyc -eq 20 ]; then
  v6date=`$NDATE -2 ${PDY}$cyc`
  $USHpcpanl/pcpn_make_hrlywgts.sh $v6date
fi

# 
$USHpcpanl/pcpn_chkst3log.sh 
export err=$?; err_chk

for region in conus ak pr
do
  # AK does not have hrly
  cat $COMIN/${RUN}.${PDY}/todo4.$region.01h.${PDY}${cyc} \
      $COMIN/${RUN}.${PDY}/todo4.$region.06h.${PDY}${cyc} \
      $COMIN/${RUN}.${PDY}/todo4.$region.24h.${PDY}${cyc} \
        > todo4.$region.${PDY}${cyc}
done

# Perform mosaicking for the analysis hours/6-hours that had new Stage III 
# data coming in (the hours/6hours are listed in todo4.${PDY}${cyc}):
#
cp $FIXpcpanl/stage3_mask.grb $DATAST4/.

for region in conus ak pr
do 
  if [ -s todo4.$region.${PDY}${cyc} ]; then
    for st4anl in `cat todo4.$region.${PDY}${cyc}`
    do
      time=`echo $st4anl | cut -c 1-10`
      ac=`echo $st4anl | cut -c 12-14`
      if [ $region = conus ]; then
        echo $st4anl.$region >> toplot4.${PDY}${cyc}
        $USHpcpanl/pcpn_st4mosaic.sh $time $ac
        export err=$?; err_chk
      else
        # for the single-source OConUS analysis, an item on the todo list don't
        # always result in an analysis.  Add item to the 'toplot' list at the
        # end of pcpn_st4oconus.sh.
        $USHpcpanl/pcpn_st4oconus.sh $region $time $ac
        export err=$?; err_chk
      fi
    done
  fi 
done
#

if test $SENDCOM = 'YES'
then
  cp todo_urma.${PDY}${cyc} $COMOUT/$RUN.$PDY/.
  cp toplot4.${PDY}${cyc} $COMOUT/$RUN.$PDY/.
fi

# Kick off the URMA job
if [ $RUN_ENVIR = dev ]; then
  # HOMEurma defined in ecf/dev/pcp_anl.ecf
  bsub < $HOMEurma/suggested_LSF_settings/pcpurma/PCPURMA.lsf
  echo '  '
else
  ## FOR NCO
  ecflow_client --event stage4_done
fi

if [ $cyc -eq 23 ]; then
# Run the daily summary log:
$USHpcpanl/pcpn_st3log_summary.sh $PDYm9
export err=$?; err_chk
$USHpcpanl/pcpn_st3log_summary.sh $PDYm7
export err=$?; err_chk
$USHpcpanl/pcpn_st3log_summary.sh $PDYm5
export err=$?; err_chk
$USHpcpanl/pcpn_st3log_summary.sh $PDYm3
export err=$?; err_chk
$USHpcpanl/pcpn_st3log_summary.sh $PDYm1
export err=$?; err_chk
$USHpcpanl/pcpn_st3log_summary.sh $PDY
export err=$?; err_chk
fi

mv $DATA/st4/$pgmout $JDATA/$pgmout.st4

postmsg $jlogfile "$0 completed normally"

exit


