#!/bin/bash
#BSUB -J pcpanl_send2rzdm
#BSUB -P RTMA-T2O
#BSUB -o /ptmpp1/Ying.Lin/cron.out/send2rzdm_pcpanl_p2.%J
#BSUB -e /ptmpp1/Ying.Lin/cron.out/send2rzdm_pcpanl_p2.%J
#BSUB -n 1
#BSUB -q "transfer"
#BSUB -W 0:10
#BSUB -R "rusage[mem=300]"
#BSUB -R affinity[core(1)]

set -x

module purge
module load gnu/4.8.5
module load ips/18.0.1.163
module load prod_util/1.1.0

# for userdev, called by jnam_pcpn_anal.ecf after jobs/JNAM_PCPN_ANAL to 
# send ST4 files to rzdm.

# The job is submitted at the end of the Stage II/IV job to the transfer queue,
# so it doesn't have a way to directly 'inherit' the run hour (pdyhh) from 
# the Stage II/IV job for which the transfer is to be done.  If the Stage II/IV
# job took too long - a risk especially for the 12Z run, which for 18 Dec 2014
# finished at 12:57Z - this script might not get submitted until after the top
# of the next hour.  So check for the current minute: if it's less than '33',
# subtract one hour from $date0.
# to be submitted to the transfer queue.  If the Stage II/IV

# So that we can quickly find our place in the output:
echo Actual output starts here:

if [ $# -eq 1 ]; then
  date0=$1
else                      
  minute=`date +%M`
  if [ $minute -gt 33 ]; then
    date0=`date +%Y%m%d%H`
  else
    date0=`date +%Y%m%d%H -d "1 hour ago"`
  fi
fi
hr0=`echo $date0 | cut -c 9-10`

COMOUT=/ptmpp2/emc.rtmapara/com/pcpanl/para

RUN=pcpanl

day0=`echo $date0 | cut -c 1-8`
hr0=`echo $date0 | cut -c 9-10`
todo4=$COMOUT/${RUN}.$day0/toplot4.$date0

for item in `cat $todo4`
do
  date=`echo $item | cut -c 1-10`
  day=`echo $item | cut -c 1-8`
  acc=`echo $item | cut -c 12-14`
  region=`echo $item | awk -F"." '{print $3}'`
  st4pfx=st4_${region}.${date}.$acc
  RZDMDIR=/home/ftp/emc/mmb/precip/pcpanl.v4.0.0_p2/pcpanl.$day
  ssh wd22yl@emcrzdm "mkdir -p $RZDMDIR"
  cd $COMOUT/${RUN}.$day

#  scp $st4pfx.* $st4gifpfx.* wd22yl@emcrzdm:$RZDMDIR/.
  scp $st4pfx.grb2 $st4pfx.gif wd22yl@emcrzdm:$RZDMDIR/.
done

exit

