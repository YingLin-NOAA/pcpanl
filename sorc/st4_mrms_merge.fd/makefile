SHELL=		/bin/sh
LIB_PATH=  /nwprod/lib
LIBS =          -L${LIB_PATH} -lw3nco_4 -lw3emc_4 -lbacio_4
FC=		ifort
LDFLAGS=        
FFLAGS=		-O -FR -convert big_endian
ESSL=
SRCS=		st4_mrms_merge.f 
OBJS=		st4_mrms_merge.o 
CMD=		pcpanl_st4_mrms_merge

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) $(ESSL) -o $(@) $(OBJS) $(LIBS)
clean:
	rm -rf $(OBJS) $(CMD)

