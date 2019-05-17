#!/bin/ksh
set -x

v6date=$1
rid=$2

v6day=${v6date:0:8}
qpedir=$DCOM/$v6day/wgrbbul/qpe

v1datem5=`$NDATE -5 $v6date`
v1datem4=`$NDATE -4 $v6date`
v1datem3=`$NDATE -3 $v6date`
v1datem2=`$NDATE -2 $v6date`
v1datem1=`$NDATE -1 $v6date`


# The existence of the two input files (6h QPE and hrlywgts) have been checked 
# in the calling script (ush/pcpn_st4mosaic.sh) before this script is called. 
#
# Read in v6date from unit 5, in order to provide date information to the 1h 
#   QPE files.  In pcpanl.v3.0.0 the date info was read from the "description"
#   session in the in the GRIB2 hourly weights, which in turn was inherited 
#   from the MRMS description session for that hour in st4_mrms_hrlywgts.
# 
export pgm=st4_qpe6h_to_1h
. prep_step
rm -f fort.*
ln -sf $qpedir/QPE.$rid.$v6date.06h fort.11
ln -sf hrlywgts_${rid}.$v6date.bin  fort.12
ln -sf QPE.${rid}.$v1datem5.01h     fort.51
ln -sf QPE.${rid}.$v1datem4.01h     fort.52
ln -sf QPE.${rid}.$v1datem3.01h     fort.53
ln -sf QPE.${rid}.$v1datem2.01h     fort.54
ln -sf QPE.${rid}.$v1datem1.01h     fort.55
ln -sf QPE.${rid}.$v6date.01h       fort.56
$EXECpcpanl/st4_qpe6h_to_1h << ioEOF
$v6date
ioEOF
export err=$?; echo "     err=$err"; err_chk


exit


