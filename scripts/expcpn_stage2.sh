#!/bin/ksh

# 9/8/97 do only one hour and change way biases are stored
# 9/11/97 put 15km stuff back on to /nfsptmp
# 11/26/97 use BUFR data instead of gage file on nic
# 6/17/98  set up script on nmc
# 2/7/00   set up script on SP
# 2001/6/3 Streamlined for JIF'ing
# 2001/6/11 Convert to ksh from csh
# 2001/11/29 Save HADS hourly gauge listing (for future gauge QC)
# 2006/3/28: Mapping ST2ml* to NDFD grid as Precip RTMA
# 2006/8/08: Add process Precip. RTMA for AWIPS
# 2010/6/16: Add 2.5km precip RTMA
#######################################################################
#  Purpose: Produce hry 4km stage2 pcpn analysis.
#######################################################################
#
echo "------------------------------------------------"
echo "JPCPN_ANAL Stage II processing                  "
echo "------------------------------------------------"
# "History: JULY 1, 2001 - First implementation of this new scripts."
#
#######################################################################

set -x

run=$1

export JDATA=$DATA 
DATA=$DATA/$run
mkdir -p $DATA
cd $DATA

# $PDY and $cyc are exported in J-job.
if [ $run = erly ]; then
   date0=${PDY}${cyc}
elif [ $run = mid ]; then
   date0=`$NDATE -6 ${PDY}${cyc}`
elif [ $run = late ]; then
   date0=`$NDATE -18 ${PDY}${cyc}`
else
   err_exit "parm run set incorrectly - should be erly, mid, or late"
fi

postmsg $jlogfile "Begin Stage II analysis for $run version of $date0"

########################################

day0=`echo $date0 | cut -c1-8`
hr0=`echo $date0 | cut -c9-10`
daym1=`$NDATE -1 $date0 | cut -c1-8`
hrm1=`$NDATE -1 $date0 | cut -c9-10`

export ST2_ST1=$DATA/test
export ST2_BIAS=$DATA/bias
export ST2_PARAM=$DATA/cmp
export ST2_OUT=$DATA/out
export ST2_DAT=$DATA/data

#
# clean out output directories
#
mkdir -p $ST2_DAT
mkdir -p $ST2_ST1
mkdir -p $ST2_OUT
mkdir -p $ST2_PARAM
mkdir -p $ST2_BIAS
#
#  get bias (grt files)
#

cd $ST2_BIAS

delt=-10

while [ $delt -le 0 ]; do
  if ! [ $run = erly -a $delt -eq 0 ]; then
    dat=`$NDATE $delt $date0`
    dat1=`echo $dat |cut -c 1-8`
    # If a tar file does not exist (e.g. if there has been a dev machine 
    # or data outage and ST2 was not made), this is not a fatal error for 
    # Stage II - it would proceed without the benefit of bias info from that 
    # particular hour in the 10-hour history. 
    if [ -s $COMIN/${RUN}.$dat1/tar_bias.$dat ]; then
      cp $COMIN/${RUN}.$dat1/tar_bias.$dat .
      tar -xf tar_bias.$dat
    fi
  fi # For erly run, tar_bias.$dat does not exist yet.  Skip.  
  let "delt=$delt+1"
done

#
#  get stuff
#

cd $ST2_DAT

data_check() {
   file=$1
   if [ ! -f $file ]; then
      date
      echo "WARNING: $file still not available after waiting three minutes..."
      echo -e "Missing $file - Digital Precipitation Array (DPA) radar data with headers SDUS?? that comes from River Forecast Centers through the NCF.\n" >> $JDATA/emailmsg.txt
   fi
}

data_check $DCOM/$daym1/wtxtbul/dpa_${hrm1}
data_check $DCOM/$day0/wtxtbul/dpa_${hr0}

cp $DCOM/$daym1/wtxtbul/dpa_${hrm1} .
cp $DCOM/$day0/wtxtbul/dpa_${hr0} .

if [ $run = erly ]; then
   cp $COMINhry_mos.$day0/sfctbl.${hr0} sfctbl.${hr0}
   if [ $? -eq 0 ]; then
     GAUGEDAT=YES
     grep AO2 sfctbl.${hr0} > ao2
     wc -l ao2
   else
     GAUGEDAT=NO
     postmsg $jlogfile "$COMINhry_mos.$day0/sfctbl.${hr0} not found!"
     echo "WARNING: $COMINhry_mos.$day0/sfctbl.${hr0} is not available!"
   fi
else
   dumpjb $date0 1 000 011 >> $DATA/$pgmout 2>&1
   if [ $? -eq 0 ]; then
     GAUGEDAT=YES
     cp $DATA/000.ibm $ST2_DAT
     cat $DATA/000.out >> $DATA/$pgmout
   else
     GAUGEDAT=NO
     postmsg $jlogfile "dumpjb for HADS at $date0 failed"
     echo "WARNING: dumpjb for HADS at $date0 failed"
   fi
fi

#
#  run stuff
#
cd $DATA

pwd

# Step 1
# Combine two consecutive hours' DPAs into one single DPA:
DPAEXIST=YES
if [[ -s ${ST2_DAT}/dpa_${hrm1} && -s ${ST2_DAT}/dpa_${hr0} ]]; then
  export pgm=st2_combdpa
  . prep_step
  ln -sf ${ST2_DAT}/dpa_${hrm1}     fort.11
  ln -sf ${ST2_DAT}/dpa_${hr0}      fort.12
  ln -sf ${ST2_DAT}/dpa.$date0      fort.51
  startmsg
  $EXECpcpanl/st2_combdpa 
  export err=$?; echo "     err=$err"; err_chk
  rm fort.11 fort.12 fort.51
elif [ -s ${ST2_DAT}/dpa_${hrm1} ]; then
  echo "WARNING: $ST2_DAT/dpa_${hr0} does not exist!"
  ln -sf ${ST2_DAT}/dpa_${hrm1} ${ST2_DAT}/dpa.$date0
elif [ -s ${ST2_DAT}/dpa_${hr0} ]; then
  echo "WARNING: $ST2_DAT/dpa_${hrm1} does not exist!"
  ln -sf ${ST2_DAT}/dpa_${hr0} ${ST2_DAT}/dpa.$date0
else
  DPAEXIST=NO
  postmsg $jlogfile "NO DPA file for $Date0"
fi

# If this hour has no DPA file at all, or is missing the gauge file
#   (METAR for erly, HADS for mid/late), skip the rest of the processing.

if [[ $DPAEXIST = YES && $GAUGEDAT = YES ]]; then
  # Step 2
  # Decode the DPA file.  The 'inventory' option is not used.  
  # DPAs centered +/- 10 minutes around $date0 are written out in unit 52
  # one file for each DPA record (${RAD}${yyyymmddhh}Z). Unit 52 is opened 
  # in Fortran code, since we have use it over and over for each of the approx. 
  # 150 radars, and we do not know before hand which radar is present.  A 
  # list of radar sites with reports falling into the +/- 10-minute window 
  # around $date0 is written to Unit 71 ('cards.parm') for later use.

  export pgm=st2_dpa_decode
  . prep_step

  export INVENTORY=F
  ln -sf ${ST2_DAT}/dpa.$date0                 fort.11
  ln -sf ${FIXpcpanl}/st2_master_radar_list    fort.17
  ln -sf ${ST2_PARAM}/cards.parm               fort.71
  startmsg
  $EXECpcpanl/st2_dpa_decode << ioEOF >> $pgmout 2> errfile
$date0
ioEOF
  export err=$?; echo "     err=$err"; err_chk
  rm fort.11 fort.17 fort.71

# Step 3
# Read in the hour's gauge data, group them according to which radar umbrella
# they fall into.  Write out each individual group (there should be 150 or so
# groups) using unit 51 (opened in Fortran code, since we won't know ahead of
# time how many radars are present for this hour).  A list of available radars
# is read in in 'cards.parm'.    
#   The 'hidden' Unit 51 outputs: gage.${RAD}${yyyymmddhh}Z
# For 'erly', the gauge data are from the hourly METAR.  For 'mid' and 'late',
# use the gauge data from the BUFR dump of SHEF data (i.e. HADS precip data).

  cp $COMINgaugeqc/current.evalH .

  if [ $run = erly ]; then
    export pgm=st2_prepmetar
    . prep_step

    ln -sf current.evalH                                fort.11
    ln -sf ${ST2_DAT}/sfctbl.${hr0}                     fort.15
    ln -sf ${ST2_DAT}/ao2                               fort.16
    ln -sf ${ST2_PARAM}/cards.parm                      fort.18
    ln -sf metar_prob.$date0                            fort.52
    ln -sf metar_raw.$date0                             fort.53
    startmsg
    $EXECpcpanl/st2_prepmetar << ioEOF >> $pgmout 2> errfile
$date0
ioEOF
    export err=$?; echo "     err=$err"; err_chk
    rm fort.11 fort.15 fort.16 fort.18 fort.52 fort.53

    if test $SENDCOM = 'YES'
    then
      cp metar_raw.$date0 $COMOUT/${RUN}.$day0/metar_raw.$date0
      if [ -e metar_prob.$date0 ]; then 
        cp metar_prob.$date0 $COMOUT/${RUN}.$day0/metar_prob.$date0
      fi
    fi

  else  # mid/late run

    export pgm=st2_prephads
    . prep_step
    ln -sf current.evalH                                fort.11
    ln -sf ${ST2_PARAM}/cards.parm                      fort.18
    ln -sf $ST2_DAT/000.ibm                             fort.20
    ln -sf hads_prob.$date0                             fort.52
    ln -sf hads_raw.$date0                              fort.53
    startmsg
    $EXECpcpanl/st2_prephads << ioEOF >> $pgmout 2> errfile
$date0
ioEOF
    export err=$?; echo "     err=$err"; err_chk
    rm fort.11 fort.18 fort.20 fort.52 fort.53

    if test $SENDCOM = 'YES'
    then
      cp hads_raw.$date0 $COMOUT/${RUN}.$day0/hads_raw.$date0
      if [ -e hads_prob.$date0 ]; then
        cp hads_prob.$date0 $COMOUT/${RUN}.$day0/hads_prob.$date0
      fi
    fi

  fi  # whether run is erly [use metar] or mid/late [use hads]

# Step 4
# Merge the radar and gauge data under each radar umbrella.  A list of 
# available radars for this our is read in from Unit 11.  This program
# reads in the radar and gauge data produced by 'decode' and 
# 'prepmetar/prephads' and writes out various analysis for each available
# radar site.  For each of the available radar sites, there are a number 
# of I/O units opened in the Fortran code, listed below:
# 
#    Unit   Unit name   File
#     31     LURAD      ${RAD}${yyyymmddhh}Z    Radar DPA data for ${RAD}
#                                                (generated by 'decode')
#     32     LUGAG      gage.${rad}/date//'Z'   Gage data for ${RAD}
#                                                (generated by either 
#                                                prepmetar or prephads)
#     51     LUGAFD     ${RAD}gg${yyyymmddhh}z  Gage-only analysis under ${RAD}
#     52     LUMTFD     ${RAD}ml${yyyymmddhh}z  Multi-sensor anl   under ${RAD}
#     53     LUBIAS     ${RAD}un${yyyymmddhh}z  Gage-adjusted radar-only 
#                                                under ${RAD} (i.e. with bias
#                                                adjustment
#     82     LUGRTB     ${RAD}${yyyymmddhh}.grt gage/radar data used in bias
#                                               removal (read/write unit)

  export pgm=st2_stageii 
  . prep_step

  ln -sf ${ST2_PARAM}/cards.parm               fort.11
  ln -sf ${FIXpcpanl}/st2_qual.con             fort.12
  ln -sf ${FIXpcpanl}/st2_stat.con             fort.13
  ln -sf ${FIXpcpanl}/st2_gageo.con            fort.14
  ln -sf ${FIXpcpanl}/st2_mult.con             fort.15
  ln -sf ${FIXpcpanl}/st2_flags.dat            fort.16
  startmsg
  $EXECpcpanl/st2_stageii << ioEOF >> $pgmout 2> errfile
$date0
ioEOF
  export err=$?; echo "     err=$err"; err_chk
  rm fort.11 fort.12 fort.13 fort.14 fort.15 fort.16

# Step 5
# Mosaicking the gauge-only, multi-sensor, bias-adjusted radar-only and
# radar-only analyses generated in the previous steps for each radar 
# sites into a national product.  A list of available radars is read in
# from unit 11.  All the individual analyses (4 x number of available radars,
# so there are approx 600 files) are read in from unit 20 (opened in Fortran 
# code).  The four mosaicked national products are written out in GRIB 
# format, in unit 51-54.

  export pgm=st2_mosaic
  . prep_step

  ln -sf ${ST2_PARAM}/cards.parm                      fort.11
  ln -sf ${ST2_OUT}/ST2gg${date0}.Grb                 fort.51
  ln -sf ${ST2_OUT}/ST2ml${date0}.Grb                 fort.52
  ln -sf ${ST2_OUT}/ST2un${date0}.Grb                 fort.53
  ln -sf ${ST2_OUT}/ST2rd${date0}.Grb                 fort.54
  startmsg
  $EXECpcpanl/st2_mosaic << ioEOF >> $pgmout 2> errfile
$date0
ioEOF
  export err=$?; echo "     err=$err"; err_chk
  rm fort.11 fort.51 fort.52 fort.53 fort.54

# Step 6
# Map the 4km analyses to a 15km grid (a simple 3x3 mapping):

  export pgm=st2_remap
  . prep_step

  ln -sf ${ST2_OUT}/ST2gg$date0.Grb                   fort.11
  ln -sf ${ST2_OUT}/gage15.$date0                     fort.51
  startmsg
  $EXECpcpanl/st2_remap
  export err=$?; echo "     err=$err"; err_chk
  rm fort.11 fort.51

  export pgm=st2_remap
  . prep_step
  ln -sf ${ST2_OUT}/ST2ml$date0.Grb                   fort.11
  ln -sf ${ST2_OUT}/multi15.$date0                    fort.51
  startmsg
  $EXECpcpanl/st2_remap
  export err=$?; echo "     err=$err"; err_chk
  rm fort.11 fort.51

  export pgm=st2_remap
  . prep_step
  ln -sf ${ST2_OUT}/ST2un$date0.Grb                   fort.11
  ln -sf ${ST2_OUT}/radunb15.$date0                   fort.51
  startmsg
  $EXECpcpanl/st2_remap
  export err=$?; echo "     err=$err"; err_chk
  rm fort.11 fort.51

  export pgm=st2_remap
  . prep_step
  ln -sf ${ST2_OUT}/ST2rd$date0.Grb                   fort.11
  ln -sf ${ST2_OUT}/rad15.$date0                      fort.51
  startmsg
  $EXECpcpanl/st2_remap
  export err=$?; echo "     err=$err"; err_chk
  rm fort.11 fort.51

#######################################################################

  gzip ${ST2_OUT}/ST2gg$date0.Grb
  gzip ${ST2_OUT}/ST2ml$date0.Grb
  gzip ${ST2_OUT}/ST2un$date0.Grb
  gzip ${ST2_OUT}/ST2rd$date0.Grb

  gzip ${ST2_OUT}/gage15.$date0
  gzip ${ST2_OUT}/multi15.$date0
  gzip ${ST2_OUT}/radunb15.$date0
  gzip ${ST2_OUT}/rad15.$date0

  cd $ST2_BIAS

  # For erly run, there isn't an existing tar_bias.$date0.
  if [ -e tar_bias.$date0 ]; then
    rm tar_bias.$date0
  fi

  tar -cf tar_bias.$date0 *$date0.grt
  if test $SENDCOM = 'YES'
  then
   cp tar_bias.$date0 $COMOUT/${RUN}.$day0/tar_bias.$date0
  fi

  cd $ST2_OUT

  # For debugging purposes, do not remove bias. 
  #rm -rf $ST2_BIAS
  
  ##############if ! [ -d ${ST2_ANL}/$day0 ]; then
  ############   mkdir -p ${ST2_ANL}/$day0
  ################fi

  # COPY FILES TO COM
  #######################################################################

  if test $SENDCOM = 'YES'
  then
    cp ST2gg$date0.Grb.gz  $COMOUT/${RUN}.$day0/.
    cp ST2ml$date0.Grb.gz  $COMOUT/${RUN}.$day0/.
    cp ST2un$date0.Grb.gz  $COMOUT/${RUN}.$day0/.
    cp ST2rd$date0.Grb.gz  $COMOUT/${RUN}.$day0/.

    cp multi15.$date0.gz   $COMOUT/${RUN}.$day0/.
    cp gage15.$date0.gz    $COMOUT/${RUN}.$day0/.
    cp radunb15.$date0.gz  $COMOUT/${RUN}.$day0/.
    cp rad15.$date0.gz     $COMOUT/${RUN}.$day0/.

  fi

  if [ $SENDDBN = 'YES' ]; then
    if [ -f $COMOUT/${RUN}.$day0/ST2gg$date0.Grb.gz ]; then
      $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day0/ST2gg$date0.Grb.gz
    fi
    $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day0/ST2ml$date0.Grb.gz
    $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day0/ST2un$date0.Grb.gz
    $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day0/ST2rd$date0.Grb.gz
    if [ -f $COMOUT/${RUN}.$day0/gage15.$date0.gz ]; then
      $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day0/gage15.$date0.gz
    fi
    $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day0/multi15.$date0.gz
    $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day0/radunb15.$date0.gz
    $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day0/rad15.$date0.gz
  fi # if SENDDBN=YES

  # In erly run, kick off the RTMA job
  if [ $run = erly ]; then
    if [ $RUN_ENVIR = dev ]; then
#     bsub < $HOMErtma/suggested_LSF_settings/pcprtma/PCPRTMA.lsf
    else
      ## FOR NCO
      ecflow_client --event stage2erly_done
    fi
  fi

fi # If at least one DPA file for this hour exists

mv $DATA/$pgmout $JDATA/$pgmout.st2.$run

#####################################################################
# GOOD RUN
postmsg $jlogfile "$0 completed normally"
#####################################################################

############## END OF SCRIPT #######################
