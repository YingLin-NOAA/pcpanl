set -x

##############################

BASE=$(pwd)
export BASE

set -x
. /usrx/local/prod/lmod/lmod/init/ksh
module purge
module use ../modulefiles
module load PCPANL

module list

cd ${BASE}/st4_qpe6h_to_1h.fd
make clean
make
make mvexec
make clean

##############################

