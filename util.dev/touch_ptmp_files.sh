#!/bin/sh
set -x
# during parallel runs, need to 'touch' the MRMS and CMORPH files on wexp grid
# on "$COMOUT" directory on ptmp, otherwise these files disappear after 5
# days and won't be available for the 7-day rerun!
#
# each day go 'touch' the mrms and cmorph files on wexp grid for 
#  $daym3,daym4,$daym5

# 2019/10/04 temporarily adding daym6,$daym7 at Yan's request

COMPCPANL=/ptmpp2/emc.rtmapara/com/pcpanl/para/pcpanl
COMPCPURMA=/ptmpp2/emc.rtmapara/com/urma/para/pcpurma
today=`date +%Y%m%d`
daym7=`date +%Y%m%d -d "7 days ago"`
daym6=`date +%Y%m%d -d "6 days ago"`
daym5=`date +%Y%m%d -d "5 days ago"`
daym4=`date +%Y%m%d -d "4 days ago"`
daym4=`date +%Y%m%d -d "3 days ago"`

for day in $daym7 $daym6 $daym5 $daym4 $daym3
do
  cd $COMPCPANL.$day
  touch hrlywgts*
  cd $COMPCPURMA.$day
  touch mrms*wexp cmorph*wexp
done

exit