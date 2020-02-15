SHELL = /bin/sh
LIBS  = ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}

FC      =ifort
FFLAGS   =

DEBUG_OPT ?= 1
ifeq ($(DEBUG_OPT), 1)
   FFLAGS +=  -g -traceback
endif

SRCS = st4_qpe6h_to_1h.f90
OBJS = st4_qpe6h_to_1h.o
CMD  = st4_qpe6h_to_1h

all: $(CMD)

$(CMD): $(OBJS)
	$(FC) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

$(OBJS): $(SRCS)
	$(FC)  $(FFLAGS) $(SRCS) -c 

clean:
	rm -f *.o *.lst $(CMD)
mvexec:
	mv $(CMD) ../../exec/.

