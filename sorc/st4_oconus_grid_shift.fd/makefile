SHELL=		/bin/sh

LIBS=           ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}
FC=             ifort
FFLAGS=         -FR -cpp 

LDFLAGS=
SRCS=		oconus_grid_shift.f 
OBJS=		oconus_grid_shift.o 
CMD=		st4_oconus_grid_shift

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f *.o *.lst $(CMD)
mvexec:
	-mv $(CMD) ../../exec/.
