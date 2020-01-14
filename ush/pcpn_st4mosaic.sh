#!/bin/ksh
# 
# Perform mosaicking of the ConUS Stage IV data for the accumulation period of
# '$2' ('01h','06h','24h'), for the hour ending '$1' (yyyymmddhh)
#
# If the accumulation period is '06h', we need to create the regional 
#   6-hourlies for the central/eastern RFCs and, starting in v4.0.0
#   (spring 2020, CBRFC) (150,152,154,155,157,158,160,161,162) 
#   from the hourlies (if an hourly QPE is missing for an RFC, use 6h QPE for
#   that RFC).  
#   For the three western RFCs (153,156,159), copy over the 6-hourlies from
#   dcom.  
#
# If the accumulation period is '24h', we use the following:
#   24h QPEs from MBRFC (#156); from 6h QPE if MBRFC 24h QPE is not available. 
#   6h QPEs from CNRFC and NWRFC (153,159), 
#     (Note that NWRFC stopped sending 24h QPEs in Oct 2019)
#   and hourly QPEs from the central/eastern RFCs, plus CBRFC:
#     (150,152,154,155,157,158,160,161,162) 

set -x 

date=$1
# export ac for mosaic.f (when ac=01h, do not use data from CNRFC or NWRFC)
export ac=$2

cd $DATAST4

postmsg $jlogfile "Stage4: process ConUS $date $ac"
day=`echo $date | cut -c 1-8`

# Create the stage IV analysis from the Stage IIIs:

# If the accumulation period is 01h, make 1h QPEs for NWRFC and CNRFC from 
#   their 06h QPEs (if exist)
# If the accumulation period is 06h or 24h, make 06h/24h sums for select RFCs:
if [ $ac = 01h ]; then
  st3dir=$DCOM/$day/wgrbbul/qpe
  if [ ! -d west1h ]; then mkdir west1h; fi 
  cd west1h
  for rid in 153 159 
  do
    # The dis-aggregated 1h QPE might already exist from an earlier 1h conus
    # mosaic from this run cycle.  If not, attempt to make the 1h QPE, if both
    # the 6h QPE and the MRMS weights file exist. 
    if ! [ -s QPE.$rid.$date.$ac ]
    then
      # find the appropriate ending time of the 6h QPE that covers this 
      # 1-h period.  
      hh=${date:8:10}
      if [ $hh -eq 00 ]; then
        v6date=${date:0:8}00
      elif [ $hh -gt 00 -a $hh -le 06 ]; then
        v6date=${date:0:8}06
      elif [ $hh -gt 06 -a $hh -le 12 ]; then
        v6date=${date:0:8}12
      elif [ $hh -gt 12 -a $hh -le 18 ]; then
        v6date=${date:0:8}18
      elif [ $hh -gt 18 ]; then
        datep24h=`$NDATE +24 $date`
        v6date=${datep24h:0:8}00
      fi
      v6day=${v6date:0:8}

      # weights for this 6h period/this RFC might already in this directory,
      # e.g. an earlier 1h-mosaic-over-conus falls in the same 6h period).  
      weights=hrlywgts_${rid}.$v6date.bin

      # attempt to make weights, if they don't exist already:
      if [ ! -s $weights ]; then
        if [ -s $COMOUT/pcpanl.$v6day/$weights ]; then
          cp $COMOUT/pcpanl.$v6day/$weights .
        else
          echo $weights not available yet, try to make it          
          $USHpcpanl/pcpn_make_hrlywgts.sh $v6date
        fi
      fi

      # Perform the time-disaggregation only if both the weights and
      # the QPE6h files are available.  
      QPE6h=$DCOM/$v6day/wgrbbul/qpe/QPE.$rid.$v6date.06h

      if [ -s $weights -a -s $QPE6h ]; then
        $USHpcpanl/pcpn_st4_qpe6h_to_1h.sh $v6date $rid
      fi
      
    fi # if QPE.$rid.$date.01h has not already been disaggregated in this 
       # run cycle

  done  # go through RFC ids 153 and 159 (CNRFC, NWRFC)
  cd $DATAST4

elif [ $ac = 06h ]; then
  if [ ! -d sum6h ]; then mkdir sum6h; fi 
  cd sum6h
  for rid in 150 152 154 155 157 158 160 161 162
  do
    # flag to track if there are missing 1hr QPEs.  If so, copy over 6-hourly
    # QPE for that RFC, rather than accumulate from hourlies.  
    qpe1hok=YES                 
    accdate=`$NDATE -5 $date`
    cat > input_acc <<EOF
obs
QPE.$rid.
EOF
    while [ $accdate -le $date ]
    do 
      accday=`echo $accdate | cut -c 1-8`
      dcomdir=$DCOM/$accday/wgrbbul/qpe
      qpe1hfile=$dcomdir/QPE.$rid.$accdate.01h
      if [ -s $qpe1hfile ]; then
        echo $qpe1hfile >> input_acc
        accdate=`$NDATE +1 $accdate`
      else
        qpe1hok=NO
        echo $dcomdir/QPE.$rid.$accdate.01h not found.  Use 6h QPE for this RFC.
        break
      fi
    done

    if [ $qpe1hok = YES ]; then
      $EXECpcpanl/pcp_acc < input_acc
      export err=$?;err_chk
      echo 'pcp_acc   err=' $? 
    else
      # note that we must use the path $DCOM/$day/wgrbbul/qpe/ below, 
      # rather than $dcomdir above.  E.g. if, say, 1h QPE ending at 2016012419
      # from MARFC is missing, we need to look for the 6-hourly QPE in 
      # $DCOM/20160125/, rather than $DCOM/20160124, where the hourly QPEs come
      # from.  
      cp $DCOM/$day/wgrbbul/qpe/QPE.$rid.$date.06h .
    fi
  done

  for rid in 153 156 159
  do
    cp $DCOM/$day/wgrbbul/qpe/QPE.$rid.$date.06h .
  done
 
  st3dir=sum6h
  cd $DATAST4

elif [ $ac = 24h ]; then
  # Sum 6h QPE from CN and NWRFC (153, 159).  If 24h QPE is not available from
  # MBRFC (156) sum that from 6h QPEs too. 
  rfc6to24list="153 159"
  if [ ! -d sum24h ]; then mkdir sum24h; fi 
  cd sum24h

  # Copy over 24h QPE, if available.  If not, add to the list of RFCs that
  # contribute to 24h ST4 from 6h QPEs (for this run):
  for rid in 156
  do
    cp $DCOM/$day/wgrbbul/qpe/QPE.$rid.$date.24h .
    err=$?
    if [ $err -ne 0 ]; then
      rfc6to24list="$rfc6to24list $rid"
    fi
  done

  # Sum up 24h total from 6h QPEs:
  for rid in $rfc6to24list
  do
    # flag to track if there are missing 6hr QPEs.  If so, skip summing for
    # this RFC. 
    qpe6hok=YES
    accdate=`$NDATE -18 $date`
    cat > input_acc <<EOF
obs
QPE.$rid.
EOF
    while [ $accdate -le $date ]
    do 
      accday=`echo $accdate | cut -c 1-8`
      dcomdir=$DCOM/$accday/wgrbbul/qpe
      qpe6hfile=$dcomdir/QPE.$rid.$accdate.06h
      if [ -s $qpe1hfile ]; then
        echo $qpe6hfile >> input_acc
        accdate=`$NDATE +6 $accdate`
      else
        qpe6hok=NO
        echo $dcomdir/QPE.$rid.$accdate.06h not found.  Skip 24hacc for RFC $rid
        break
      fi
    done

    if [ $qpe6hok = YES ]; then
      export pgm=st4_mosaic
      . prep_step
      startmsg
      $EXECpcpanl/pcp_acc < input_acc
      export err=$?;err_chk
      echo '     err=' $? 
    fi
  done

  for rid in 150 152 154 155 157 158 160 161 162
  do 
    # flag to track if there are missing 1hr QPEs.  If so, skip summing for
    # this RFC. 
    qpe1hok=YES
    accdate=`$NDATE -23 $date`
    cat > input_acc <<EOF
obs
QPE.$rid.
EOF
    while [ $accdate -le $date ]
    do 
      accday=`echo $accdate | cut -c 1-8`
      dcomdir=$DCOM/$accday/wgrbbul/qpe
      qpe1hfile=$dcomdir/QPE.$rid.$accdate.01h
      if [ -s $qpe1hfile ]; then
        echo $qpe1hfile >> input_acc
        accdate=`$NDATE +1 $accdate`
      else
        qpe1hok=NO
        echo $dcomdir/QPE.$rid.$accdate.01h not found.  Skip 24hacc for RFC $rid
        break
      fi
    done

    if [ $qpe1hok = YES ]; then
      export pgm=st4_mosaic
      . prep_step
      startmsg
      $EXECpcpanl/pcp_acc < input_acc
      export err=$?;err_chk
      echo '     err=' $? 
    fi
  done
  st3dir=sum24h
  cd $DATAST4
fi # acc=06h/24h?  Make 6h/24 sums for select RFC

export pgm=st4_mosaic
. prep_step
startmsg

rm -f fort.*
ln -sf stage3_mask.grb                  fort.11
ln -sf $HOMEpcpanl/parm/grib2_pds.tbl   fort.12
ln -sf st4_conus.$date.$ac.grb2         fort.51
ln -sf $st3dir/QPE.150.$date.$ac        fort.150
ln -sf $st3dir/QPE.152.$date.$ac        fort.152
ln -sf $st3dir/QPE.154.$date.$ac        fort.154
ln -sf $st3dir/QPE.155.$date.$ac        fort.155
ln -sf $st3dir/QPE.156.$date.$ac        fort.156
ln -sf $st3dir/QPE.157.$date.$ac        fort.157
ln -sf $st3dir/QPE.158.$date.$ac        fort.158
ln -sf $st3dir/QPE.160.$date.$ac        fort.160
ln -sf $st3dir/QPE.161.$date.$ac        fort.161
ln -sf $st3dir/QPE.162.$date.$ac        fort.162
if [ $ac = 01h ]; then
  ln -sf west1h/QPE.153.$date.$ac       fort.153
  ln -sf west1h/QPE.159.$date.$ac       fort.159
else
  ln -sf $st3dir/QPE.153.$date.$ac      fort.153
  ln -sf $st3dir/QPE.159.$date.$ac      fort.159
fi
# for diagnosis:
# if [ $RUN_ENVIR = dev ]; then
#   mkdir QPE.$date.$ac.dir
#  cp -p $st3dir/QPE.*.$date.$ac QPE.$date.$ac.dir/.
#   ls -l fort.*
# fi
#

$EXECpcpanl/st4_mosaic << ioEOF
$date
ioEOF
export err=$?;err_chk
echo '     err=' $? 

if [ $err -eq 0 ] && [ $ac = 01h -o $ac = 06h ]
then
  # Note that we are assuming that ush/pcpn_st4mosaic.sh and pcpn_st4oconus.sh
  # are running sequentially, so items are added to todo_urma list conus first,
  # then OConUS (the two ush scripts won't write to the todo list at the 
  # same time.
  echo $date.$ac.conus >> $DATA/todo_urma.${PDY}${cyc}
fi

if test $SENDCOM = 'YES'
then
  cp st4_conus.$date.$ac.grb2 $COMOUT/${RUN}.$day/.
#  $CNVGRIB -g21 st4_conus.$date.$ac.grb2 ST4.$date.$ac
#  gzip -c  ST4.$date.$ac > $COMOUT/${RUN}.$day/ST4.$date.$ac.gz
fi

if test $SENDDBN = 'YES'
then
  $DBNROOT/bin/dbn_alert MODEL PCPANL $job $COMOUT/${RUN}.$day/st4_conus.$date.$ac.grb2
fi

#####################################################################
# GOOD RUN
postmsg $jlogfile "$0 completed normally"
#####################################################################


############## END OF SCRIPT #######################

# exit does not work
