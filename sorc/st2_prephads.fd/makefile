SHELL=		/bin/sh

LIBS=           ${W3NCO_LIB4} ${W3EMC_LIB4} ${BUFR_LIB8}
FC=             ifort
FFLAGS=         -O -i8 -r8 -FR -convert big_endian -cpp 

LDFLAGS=
SRCS=		prephads.f 
OBJS=		prephads.o 
CMD=		st2_prephads

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f *.o *.lst $(CMD)
mvexec:
	-mv $(CMD) ../../exec/.
