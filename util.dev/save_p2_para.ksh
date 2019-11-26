#!/bin/sh
# Save the pcpanl.v4.0.0 and pcprtma/pcpurma v2.8 para runs.
#  Run this sometime after 13Z each day. 
#  pcprtma: save $daym1
#  pcpanl,pcpurma:  save $daym8 
#
#  Archive at: /meso/noscrub/Ying.Lin/rtma2p8.arch
#
set -x

archdir=/meso/noscrub/Ying.Lin/rtma2p8.arch
pcpanlpfx=/ptmpp2/emc.rtmapara/com/pcpanl/para/pcpanl
rtmapfx=/ptmpp2/emc.rtmapara/com/rtma/para/pcprtma
urmapfx=/ptmpp2/emc.rtmapara/com/urma/para/pcpurma

if [ $# -eq 1 ]
then
  rtmaday=$1
  urmaday=$1
else
  rtmaday=`date +%Y%m%d -d "1 day ago"`
  urmaday=`date +%Y%m%d -d "7 day ago"`
fi

cd $pcpanlpfx.$urmaday 
tar cvfz $archdir/pcpanl.$urmaday.tar.gz . 

cd $urmapfx.$urmaday 
tar cvfz $archdir/pcpurma.$urmaday.tar.gz . --exclude wmo

cd $rtmapfx.$rtmaday 
tar cvfz $archdir/pcprtma.$rtmaday.tar.gz . --exclude wmo

exit












