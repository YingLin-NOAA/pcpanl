SHELL=		/bin/sh

LIBS=           ${W3NCO_LIB4} ${W3EMC_LIB4}
FC=             ifort
FFLAGS=         -FR -convert big_endian -cpp 

LDFLAGS=
SRCS=		decode.f 
OBJS=		decode.o 
CMD=		st2_dpa_decode

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f *.o *.lst $(CMD)
mvexec:
	-mv $(CMD) ../../exec/.

