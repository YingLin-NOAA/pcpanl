SHELL=		/bin/sh

LIBS=           ${W3NCO_LIB4} ${W3EMC_LIB4}
FC=             ifort
FFLAGS=         -FR -cpp -convert big_endian -cpp 

LDFLAGS=	
SRCS=		prepmetar.f 
OBJS=		prepmetar.o 
CMD=		st2_prepmetar

all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

debug:  FFLAGS = $(D_FFLAGS_CHKBNDS)
clean:
	-rm -f *.o *.lst $(CMD)
mvexec:
	-mv $(CMD) ../../exec/.

