#!/bin/ksh

#######################################################################
#  Purpose: Post-processing of the Stage II/IV analyses
#    Plot Stage II/IV analyses (plus the RTMA version) using GEMPAK
#######################################################################
#
# Steps:
#   1. Prep; copy over the two GEMPAK fix files (coltbl.xwp.wbg and 
#      wmogrib.tbl) into each $ST4item/ dir
#   2. Plot hourly/6-hourly/24h Stage IV that are made at this hour (if any),
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

# On the CCS, the files are compressed with *.Z.  On WCOSS, they are gzip'd 
# into *.gz.  Both can be gunzip'd.  Rather than "cp file.Z ." or "cp file.gz",
# we are now doing "cp file.* .", followed by "gunzip file" (zcat), since at the 
# moment of transition, we might need to process previous hours'/day's *.Z
# files copied over from the CCS.
#   
# check the opnl 'todo' list for any new Stage IV for this hour.
#    Post-process the items on the 'todo' list.

cp $COMIN/$RUN.$PDY/toplot4.$date0 .

if [ -s toplot4.$date0 ]; then
  for item in `cat toplot4.$date0`
  do
    date=`echo $item | awk -F"." '{print $1}' | cut -c1-10`
    day=`echo $date | cut -c1-8`
    ac=`echo $item | awk -F"." '{print $2}' | cut -c1-2`
    region=`echo $item | awk -F"." '{print $3}'`
    ST4item=st4_${region}.$date.${ac}h

    mkdir $ST4item
    # For a white background:
    cp $GEMFIX/coltbl.xwp.wbg $ST4item/coltbl.xwp
    # Missing value for precip set to -9999. so we can distinguish zero value 
    # areas (plotted in vanilla) from no data areas (white):
    cp $GEMFIX/g2varswmo2.tbl $ST4item/.

    cp $COMIN/$RUN.$day/${ST4item}.grb2 $ST4item/.
    if [ $? -eq 0 ]; then
      # Now make the plot.  Note that ST4item below is the subdir containing
      # $ST4item.grb2:
      echo "$USHpcpanl/pcpn_plotpcp.sh $ST4item $ST4item.gif $date $ac \"Stage IV\" $region" >>poescript
    else
      echo -e "WARNING: $COMIN/$RUN.$day/$ST4item.grb2 was not generated successfully and therefore cannot be plotted.\n" >>$DATA/emailmsg.txt
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
