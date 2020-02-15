SHELL = /bin/sh
LIBS  = ${WGRIB2LIB}
MOD   = -I${WGRIB2INC}

FC      =ifort
FFLAGS   = -qopenmp

DEBUG_OPT ?= 1
ifeq ($(DEBUG_OPT), 1)
   FFLAGS +=  -g -traceback
endif

SRCS = st4_mrms_hrlywgts.f90
OBJS = st4_mrms_hrlywgts.o
CMD  = st4_mrms_hrlywgts

all: $(CMD)

$(CMD): $(OBJS)
	$(FC) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

$(OBJS): $(SRCS)
	$(FC)  $(FFLAGS) $(SRCS) -c $(MOD)

clean:
	rm -f *.o *.lst $(CMD)
mvexec:
	mv $(CMD) ../../exec/.

