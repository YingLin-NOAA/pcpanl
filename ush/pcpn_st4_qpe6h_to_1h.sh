#!/bin/ksh
set -x
#
# NWRFC and CNRFC provide only 6h QPEs.  For hourly mosaic to fully cover the
# ConUS, we create "artificial" 1h QPEs by using 1h gauge-corrected MRMS as
# weights to do a time-disaggregation of 6h QPE into hourly amounts.  
#
# This script creates hourly QPEs for the 6h period ending at $v6date, for 
# either NWRFC (rid=159) or CNRFC (rid=153).    
# The existence of the two input files (6h QPE and hrlywgts) have been checked 
# in the calling script (ush/pcpn_st4mosaic.sh) before this script is called. 
#
v6date=$1
rid=$2

v6day=${v6date:0:8}
qpedir=$DCOM/$v6day/wgrbbul/qpe

v1datem5=`$NDATE -5 $v6date`
v1datem4=`$NDATE -4 $v6date`
v1datem3=`$NDATE -3 $v6date`
v1datem2=`$NDATE -2 $v6date`
v1datem1=`$NDATE -1 $v6date`

#
# Read in v6date from unit 5, in order to provide date information to the 1h 
#   QPE files. In v3.0.0 the hrlywgts was in GRIB2, created by 
#   exec/st4_mrms_hrlywgts, but it was found that that executable had a memory
#   leak, and the only way I could fix the memory leak problem was to output
#   hrlywgts as a binary file.  Without the date info formerly contained in the
#   grib2 header in hrlywgts, now we need to read in the date info from unit 5.
#   We are not using the date info in RFC's 6h QPE because the RFC 6h QPEs 
#   follow a peculiar convention of making the 6h QPEs look like 6h QPFs, e.g. 
#   the 6h QPE ending at, e.g. 12Z 6 Jul 2020 has the grib header that makes
#   it look like the 18-24h forecast from the reference time of 12Z 5 Jul 2020. 
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


