SHELL=		/bin/sh

LIBS=           ${G2_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}
FC=             ifort
FFLAGS=         -FR -cpp 

DEBUG_OPT ?= 1
ifeq ($(DEBUG_OPT), 1)
   FFLAGS +=  -g -traceback
endif

LDFLAGS=
SRCS=		st4_oconus_convert.f grib2_wrt_g2func_hrap.f
OBJS=		st4_oconus_convert.o grib2_wrt_g2func_hrap.o
CMD=		st4_oconus_convert

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f *.o *.lst $(CMD)
mvexec:
	-mv $(CMD) ../../exec/.
