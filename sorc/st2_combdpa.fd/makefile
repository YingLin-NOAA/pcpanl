SHELL=		/bin/sh 

LIBS=           ${W3NCO_LIB4} ${W3EMC_LIB4}
FC=             ifort
FFLAGS=         -FR -convert big_endian -assume byterecl -cpp 

LDFLAGS=
SRCS=		combdpa.f 
OBJS=		combdpa.o 
CMD=		st2_combdpa

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f *.o *.lst $(CMD)
mvexec:
	-mv $(CMD) ../../exec/.
