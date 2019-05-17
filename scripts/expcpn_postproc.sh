#!/bin/ksh

#######################################################################
#  Purpose: Post-processing of the Stage II/IV analyses
#    Plot Stage II/IV analyses (plus the RTMA version) using GEMPAK
#######################################################################
#
# Steps:
#   1. Prep; copy over the two GEMPAK fix files (coltbl.xwp.wbg and 
#      wmogrib.tbl) 
#   2. Plot Stage II hourly multi-sensor analysis for early/mid/late
#   3. Plot 6h and 24h Stage II analysis (08/14/20Z only)
#   4. Plot hourly/6-hourly/24h Stage IV that are made at this hour (if any),
#      based on the 'todo4' list.

set -x

# cyc used to determine whether it is time to process the 6h/24h Stage II
date0=${PDY}${cyc}
datem6h=`$NDATE -6 $date0`
datem18h=`$NDATE -18 $date0`

daym6h=`echo $datem6h | cut -c1-8`
daym18h=`echo $datem18h | cut -c1-8`

export PLOTDIR=$DATA/plot

mkdir -p $PLOTDIR

cd $PLOTDIR

pgmout=out.$date0

# For a white background:
# cp /nwprod/gempak/fix/coltbl.xwp.wbg coltbl.xwp
cp $GEMFIX/coltbl.xwp.wbg coltbl.xwp

# Missing value for precip set to -9999. so we can distinguish zero value 
# areas (plotted in vanilla) from no data areas (white):
# cp /nwprod/gempak/fix/wmogrib.tbl .
cp $GEMFIX/wmogrib.tbl .

# On the CCS, the files are compressed with *.Z.  On WCOSS, they are gzip'd 
# into *.gz.  Both can be gunzip'd.  Rather than "cp file.Z ." or "cp file.gz",
# we are now doing "cp file.* .", followed by "gunzip file" (zcat), since at the 
# moment of transition, we might need to process previous hours'/day's *.Z
# files copied over from the CCS.
#   
# 1. Plot Stage II analysis:

cat > toplot2.$date0 <<EOF
$date0.01h
$datem6h.01h
$datem18h.01h
EOF

if [[ $cyc -eq 08 || $cyc -eq 14 || $cyc -eq 20 ]]; then
  if [ $cyc -le 12 ]; then
    day=$PDYm1
  else
    day=$PDY
  fi
  daym1=`finddate.sh $day d-1`

  cat >> toplot2.$date0 <<EOF
${daym1}18.06h
${day}00.06h
${day}06.06h
${day}12.06h
${day}12.24h
EOF

fi

if [ $SENDCOM = 'YES' ]; then
  cp toplot2.$date0 $COMOUT/$RUN.$PDY/
fi

for item in `cat toplot2.$date0`
do 
  date=`echo $item | cut -c1-10`
  day=`echo $item | cut -c1-8`
  ac=`echo $item | cut -c12-13`
  if [ $ac -eq 01 ]; then
    st2file=multi15.$date
    st2plot=st2vu.$date.gif
  else
    st2file=multi15.$date.${ac}h
    st2plot=st2ml.$date.${ac}h.gif
  fi
  mkdir $st2file
  zcat $COMIN/$RUN.$day/$st2file.gz >$st2file/$st2file
  if [ $? -eq 0 ]; then
    echo "$USHpcpanl/pcpn_plotpcp.sh $st2file $st2plot $date $ac \"Stage II Multi-Sensor\"" >>poescript
  else
    echo -e "WARNING: $COMIN/$RUN.$day/$st2file.gz was not generated successfully and therefore cannot be plotted.\n" >>$DATA/emailmsg.txt
  fi
done

# 2. Stage IV: check the opnl 'todo' list for any new items for this hour.
#    Post-process the items on the 'todo' list.

cp $COMIN/$RUN.$PDY/toplot4.$date0 .

if [ -s toplot4.$date0 ]; then
  for item in `cat toplot4.$date0`
  do
    date=`echo $item | awk -F"." '{print $1}' | cut -c1-10`
    day=`echo $date | cut -c1-8`
    ac=`echo $item | awk -F"." '{print $2}' | cut -c1-2`
    region=`echo $item | awk -F"." '{print $3}'`
    if [ $region = conus ]; then
      ST4file=st4.$date.${ac}h
      ST4file_in=ST4.$date.${ac}h  # CONUS ST4 data files still use uppercase in COM
    else
      ST4file=st4_${region}.$date.${ac}h
      ST4file_in=$ST4file
    fi
    mkdir $ST4file
    zcat $COMIN/$RUN.$day/$ST4file_in.gz >$ST4file/$ST4file
    if [ $? -eq 0 ]; then
      # Now make the plot:
      echo "$USHpcpanl/pcpn_plotpcp.sh $ST4file $ST4file.gif $date $ac \"Stage IV\" $region" >>poescript
    else
      echo -e "WARNING: $COMIN/$RUN.$day/$ST4file_in.gz was not generated successfully and therefore cannot be plotted.\n" >>$DATA/emailmsg.txt
    fi
  done # finished processing ST4 to-plot list
fi # ST4 to-plot list exists?

if [ -s $DATA/emailmsg.txt ]; then
  if [ "${RUN_ENVIR}" = "dev" ]; then
#   Mail -s "pcpanl alert" Ying.Lin@noaa.gov < $DATA/emailmsg.txt
#   dev: how to send email from dev compute node? 
    echo '############### NON-EMAIL ALERT #####################################'
    cat $DATA/emailmsg.txt
    echo '#####################################################################'
  else
    if [ -n "$MAILCC" ]; then
      mail.py -c $MAILCC ${MAILTO:?} $DATA/emailmsg.txt
    else
      mail.py < $DATA/emailmsg.txt
    fi
  fi
fi

echo 
echo Here is the poescript for making plots:
cat poescript
echo 

mpirun -l cfp poescript
export err=$?; err_chk

if [ -s $DATA/emailmsg.txt ]; then
  postmsg $jlogfile "$0 completed with errors"
else
  postmsg $jlogfile "$0 completed normally"
fi

exit

