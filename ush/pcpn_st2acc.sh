#!/bin/ksh
# 
# This script calculates the four 6-hourly accumulations
# (12-18Z, 18-00Z, 00-06Z, 06-12Z) and the 12-12Z accumulation, for the 24h
# period ending the previous 12Z.  Run at 14Z, 20Z and the next day's 08Z.

set -x

date=$1

# reset WRKDIR to be consistent with 'plotone':

WRKDIR=$DATA/accst2
if [ -e $WRKDIR ]; then
  rm -f $WRKDIR/*
else
  mkdir -p $WRKDIR
fi

cd $WRKDIR

# Figure out the ending time of the 24h period:
hh=`echo $date | cut -c9-10`
date1=`echo $date | cut -c1-8`12
if [ $hh -lt 12 ]; then
  date1=`$NDATE -24 $date1`
fi

# The beginning hour:
date0=`$NDATE -23 $date1`

msg="st2acc: from $date0 to $date1"
postmsg "$jlogfile" "$msg"

date=$date0

# Copy over the existing ST2* files from the data directory
# (e.g. /com/pcpanl/prod/pcpanl.$day) so we can do the 6hr/24h
# accumulation computation.  During tests, $COMOUT might be different from
# $COMIN, in that case copy over both ($COMOUT after $COMIN, since $COMOUT 
# would contain the most up-to-date data).  
# 
# Suffix after .Grb can be either 'Z' or 'gz' (ccs or wcoss)
while [ $date -le $date1 ]; do
  #day=`echo $date | cut -c1-8`
  #if [ $COMIN != $COMOUT ]; then
    cpreq $COMIN/${RUN}.${date:0:8}/ST2*${date}.Grb.* .
  #fi
  #cp $COMOUT/${RUN}.$day/ST2*${date}.Grb.* .
  date=`$NDATE +1 $date`
done

ls -1 *Grb.* > files.list

for file in `cat files.list`
do 
  gunzip $file
done

date12z=`$NDATE -24 $date1`
date18z=`$NDATE -18 $date1`
date00z=`$NDATE -12 $date1`
date06z=`$NDATE -06 $date1`

cat  > input_ml24 << EOF
obs
ST2ml
EOF

cat  > input_gg24 << EOF
obs
ST2gg
EOF

cat  > input_rd24 << EOF
obs
ST2rd
EOF

cat  > input_un24 << EOF
obs
ST2un
EOF

export pgm=pcp_acc

for datebeg in $date12z $date18z $date00z $date06z
do
  dateend=`$NDATE +6 $datebeg`
  date=`$NDATE +1 $datebeg`

cat  > input_ml06 << EOF
obs
ST2ml
EOF
echo ST2ml${dateend}.06h >> input_ml24

cat  > input_gg06 << EOF
obs
ST2gg
EOF
echo ST2gg${dateend}.06h >> input_gg24

cat  > input_rd06 << EOF
obs
ST2rd
EOF
echo ST2rd${dateend}.06h >> input_rd24

cat  > input_un06 << EOF
obs
ST2un
EOF
echo ST2un${dateend}.06h >> input_un24

  while [ $date -le $dateend ]; do
    echo ST2ml$date.Grb >> input_ml06
    echo ST2gg$date.Grb >> input_gg06
    echo ST2rd$date.Grb >> input_rd06
    echo ST2un$date.Grb >> input_un06
    date=`$NDATE +1 $date`
  done

  . prep_step
  startmsg
  $EXECpcpanl/pcp_acc < input_ml06
  export err=$?;err_chk
  echo '     err=' $? 

  . prep_step
  startmsg
  $EXECpcpanl/pcp_acc < input_gg06
  export err=$?;err_chk
  echo '     err=' $? 

  . prep_step
  startmsg
  $EXECpcpanl/pcp_acc < input_rd06
  export err=$?;err_chk
  echo '     err=' $? 

  . prep_step
  startmsg
  $EXECpcpanl/pcp_acc < input_un06
  export err=$?;err_chk
  echo '     err=' $? 
done

. prep_step
startmsg
$EXECpcpanl/pcp_acc < input_ml24
export err=$?;err_chk
echo '     err=' $? 

. prep_step
startmsg
$EXECpcpanl/pcp_acc < input_gg24
export err=$?;err_chk
echo '     err=' $? 

. prep_step
startmsg
$EXECpcpanl/pcp_acc < input_rd24
export err=$?;err_chk
echo '     err=' $? 

. prep_step
startmsg
$EXECpcpanl/pcp_acc < input_un24
export err=$?;err_chk
echo '     err=' $? 

# Now map all 06h and 24h accumulations to 15km:

for datebeg in $date12z $date18z $date00z $date06z
do
  dateend=`$NDATE +6 $datebeg`

  ln -sf ST2ml${dateend}.06h            fort.11
  ln -sf multi15.${dateend}.06h         fort.51
  $EXECpcpanl/st2_remap
  export err=$?;err_chk
  echo '     err=' $? 

  ln -sf ST2gg${dateend}.06h            fort.11
  ln -sf gage15.${dateend}.06h          fort.51
  $EXECpcpanl/st2_remap
  export err=$?;err_chk
  echo '     err=' $? 

  ln -sf ST2rd${dateend}.06h            fort.11
  ln -sf rad15.${dateend}.06h           fort.51
  $EXECpcpanl/st2_remap
  export err=$?;err_chk
  echo '     err=' $? 

  ln -sf ST2un${dateend}.06h            fort.11
  ln -sf radunb15.${dateend}.06h        fort.51
  $EXECpcpanl/st2_remap
  export err=$?;err_chk
  echo '     err=' $? 
done

ln -sf ST2ml${date1}.24h                fort.11
ln -sf multi15.${date1}.24h             fort.51
$EXECpcpanl/st2_remap
export err=$?;err_chk
echo '     err=' $? 

ln -sf ST2gg${date1}.24h                fort.11
ln -sf gage15.${date1}.24h              fort.51
$EXECpcpanl/st2_remap
export err=$?;err_chk
echo '     err=' $? 

ln -sf ST2rd${date1}.24h                fort.11
ln -sf rad15.${date1}.24h               fort.51
$EXECpcpanl/st2_remap
export err=$?;err_chk
echo '     err=' $? 

ln -sf ST2un${date1}.24h                fort.11
ln -sf radunb15.${date1}.24h            fort.51
$EXECpcpanl/st2_remap
export err=$?;err_chk
echo '     err=' $? 

gzip *.06h *.24h

day0=`echo $date0 | cut -c1-8`
day1=`echo $date1 | cut -c1-8`

if ! [ -d $COMOUT/${RUN}.$day0 ]; then
  mkdir -p $COMOUT/${RUN}.$day0
fi

if ! [ -d $COMOUT/${RUN}.$day1 ]; then
  mkdir -p $COMOUT/${RUN}.$day1
fi

#send to com and dbnet

ls -1 *${day0}*.06h.gz > files.list1
for file in `cat files.list1`
do 
   if test $SENDCOM = 'YES'
   then
      cp $file $COMOUT/${RUN}.$day0/$file
   fi
   if test $SENDDBN = 'YES'
   then
      $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day0/$file 
   fi
done
ls -1 *${day1}*.06h.gz > files.list2
for file in `cat files.list2`
do 
   if test $SENDCOM = 'YES'
   then
      cp $file $COMOUT/${RUN}.$day1/$file
   fi
   if test $SENDDBN = 'YES'
   then
      $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day1/$file 
   fi
done
ls -1 *${date1}.24h.gz > files.list3
for file in `cat files.list3`
do 
   if test $SENDCOM = 'YES'
   then
      cp $file $COMOUT/${RUN}.$day1/$file
   fi
   if test $SENDDBN = 'YES'
   then
      $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day1/$file 
   fi
done

cat $pgmout 

#####################################################################
# GOOD RUN
postmsg $jlogfile "$0 completed normally"
#####################################################################


############## END OF SCRIPT #######################
