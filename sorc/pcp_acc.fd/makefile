SHELL=		/bin/sh

LIBS=           ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}
FC=             ifort
FFLAGS=         -FR -cpp 

DEBUG_OPT ?= 1
ifeq ($(DEBUG_OPT), 1)
   FFLAGS +=  -g -traceback
endif

LDFLAGS=
SRCS=		acc.f 
OBJS=		acc.o
CMD=		pcp_acc

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f *.o *.lst $(CMD)
mvexec:
	-mv $(CMD) ../../exec/.

