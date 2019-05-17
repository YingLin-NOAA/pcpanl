set -x

##############################

BASE=$(pwd)
export BASE

set -x
. /usrx/local/Modules/default/init/sh
module purge
module use ../modulefiles
module load PCPANL

module list

cd ${BASE}/st2_prephads.fd
make clean
make
make mvexec
make clean

##############################

