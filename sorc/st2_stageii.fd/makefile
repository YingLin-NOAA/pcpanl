SHELL=     /bin/sh

LIBS=      ${W3NCO_LIB4} ${W3EMC_LIB4}
FC=        ifort
FFLAGS=    -FR -convert big_endian -cpp 

LDFLAGS=	

SRCS=      ibins.f \
           bfbias.f bias.f rdgrt.f \
           mngage_new.f \
           mnqual.f mnstat.f nearn.f \
           outrem.f \
           rdargs.f \
           rdmisc.f rdprcp.f \
           readgg.f readrd.f \
           savrg.f \
           statgg.f statrd.f \
           wrinpt.f calggo1.f \
           calggo2.f calggo3.f calggo4.f \
           check_multiple_gages.f cor_scale.f covmatgg.f \
           covmatml.f covvecgg.f covvecml.f \
           doe_radar_only.f find1c.f gammf.f \
           hlpfnd.f ll2hrap.f \
           local_stat.f lsolve.f \
           lubksb.f ludcmp.f merge_doe.f \
           merge_soe.f mnmult_new.f posc.f \
           radar_local_stat.f radll2hr.f \
           rdparam.f soe_radar_only.f \
           sortc.f stati2.f wrgrfl.f main.f stageii.f

OBJS=      ibins.o \
           bfbias.o bias.o rdgrt.o \
           mngage_new.o \
           mnqual.o mnstat.o nearn.o \
           outrem.o \
           rdargs.o \
           rdmisc.o rdprcp.o \
           readgg.o readrd.o \
           savrg.o \
           statgg.o statrd.o \
           wrinpt.o calggo1.o \
           calggo2.o calggo3.o calggo4.o \
           check_multiple_gages.o cor_scale.o covmatgg.o \
           covmatml.o covvecgg.o covvecml.o \
           doe_radar_only.o find1c.o gammf.o \
           hlpfnd.o ll2hrap.o \
           local_stat.o lsolve.o \
           lubksb.o ludcmp.o merge_doe.o \
           merge_soe.o mnmult_new.o posc.o \
           radar_local_stat.o radll2hr.o \
           rdparam.o soe_radar_only.o \
           sortc.o stati2.o wrgrfl.o main.o stageii.o

CMD=       st2_stageii

all:	   $(CMD)

$(CMD):	   $(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f *.o *.lst $(CMD)
mvexec:
	-mv $(CMD) ../../exec/.
