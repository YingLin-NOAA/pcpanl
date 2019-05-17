      SUBROUTINE mnmult_new(NGAGE,IHLOC,JHLOC,DATE,IHOUR,                     &
                 rmesh,bpps,b,iml,XMHAT,XMGAG,PHAT,XMXR,XMXG,RHAT)
!***********************************************************************
!
!   MULTI-SENSOR PRECIP ANALYSIS ALGORITHM MAIN PROGRAM
!   CALLING SUBROUTINE: MAIN PROGRAM
!   SUBROUTINES CALLED: BIAS,OBSWT,DSPFLD
!   performs merging of radar adjusted by StageII bias with rain gage data
!
! Version Feb 3, 1996 by D-J Seo at NWS/HRL
!
      parameter (ndat=1000,nx=131,ny=131,ngmax=20)

      INTEGER*2 IFLD(131,131)
      integer*2 ndata(nx,ny),nposi(nx,ny)
      CHARACTER DATE*10
      CHARACTER outdir*180
      LOGICAL TEST1,TEST2,TEST3,TEST4,TEST5
!
      DIMENSION FLDMLT(131,131),FLDGGO(131,131),RADAR(131,131)
      DIMENSION ihloc(1000),jhloc(1000),gagval(1000),ih(1000),jh(1000)

      COMMON/RADCOM/RADAR
      COMMON/FLDM/FLDMLT
      COMMON/FLDG/FLDGGO
      INCLUDE 'lumult'
      INCLUDE 'cnst'
      INCLUDE 'ppinp'
      INCLUDE 'raddat'
      INCLUDE 'ppout'
      INCLUDE 'bins'
      INCLUDE 'hrg'
      INCLUDE 'mis'
      INCLUDE 'ggoparam'
      INCLUDE 'mltparam'
      INCLUDE 'flags'
      common/inform/iactrv(2),nactrv,iordrv(2)
      common/gag2rad/istart(nx),itermi(nx)
      common/big1/u(ndat),v(ndat),z(ndat)
      common/srtord/ilist(ngmax)
      common/srtdst/rlist(ngmax)
      common/big3/irv(ndat)
      dimension summ1(nx,ny),summ2(nx,ny)
!
      TEST1=(XMHAT.GE.THR1)
      TEST2=(XMGAG.GE.THR1)
      TEST3=(PHAT.LE.THR2)
      TEST4=(XMXR.GE.THR3)
      TEST5=(XMXG.GE.THR3)
      IF(IDEBUG.GT.0) WRITE(6,2) TEST1,TEST2,TEST3,TEST4,TEST5
!
!   IF ANY TEST IS TRUE, THEN CALCULATE MULTI-SENSOR PRECIP FIELD
!   IF NO RADAR DATA IS AVAILABLE, THEN SKIP RADAR BIAS ADJUSTMENT
!   B=RADAR MEAN FIELD BIAS
!
!MEB HERE AGAIN I DON'T SEE ANY REAL NEED FOR THIS TEST
!MEB TRYING TO TAKE IT OUT
!     IF(TEST1.OR.TEST2.OR.TEST3.OR.TEST4.OR.TEST5) THEN
!
!
!             OPEN FILE FOR STAGEII OUTPUT
!
         OUTDIR = ' '
         CALL GETENV('ST2_OUT',OUTDIR)
         LENDIR = INDEX(OUTDIR,' ') -1
         IF (LENDIR.LE.0) LENDIR = LEN(OUTDIR)

!     CALL GET_APPS_DEFAULTS('st3_input',9,OUTDIR,lendir)
      FILMUL=outdir(1:lendir)//'/'//RADID//'ml'//DATE//'z'
      OPEN(LUMTFD,FILE=FILMUL,FORM='UNFORMATTED')
!
!          CALCULATE MEAN FIELD BIAS
!          adjust radar field
!
      CALL BIAS(DATE,bpps,B)
      WRITE(6,3) B
!
      DO 10 IBIN=1,NOK
      i=IOK(IBIN)
      j=JOK(IBIN)
      radar(i,j)=radar(i,j)*b
   10 continue
      FILRUN=outdir(1:lendir)//'/'//RADID//'un'//DATE//'z'
      OPEN(LUBIAS,FILE=FILRUN,FORM='UNFORMATTED')
         WRITE(LUBIAS) RADID,RLAT,RLON
         WRITE(LUBIAS) date
         WRITE(LUBIAS) RADAR
         close(LUBIAS)
!
! check for number of gage data
! if number of gages is less than number of nearest neighbors, then
!   set ml field to radar field
! 
      if(ngage.lt.nbrsml) then
         write(6,*) 'ngage < nbrsml',ngage,nbrsml
         iml=1
!
!        DO 25 J=1,131
!        DO 25 I=1,131
!        IFLD(I,J)=radar(I,J)*100
!  25    continue
         WRITE(LUMTFD) RADID,RLAT,RLON
         WRITE(LUMTFD) date
         WRITE(LUMTFD) RADAR
!        WRITE(LUMTFD) IFLD
         close(lumtfd)
         return
      endif
!
! estimate spatial correlation coefficients
! if real-time estimates of spatial correlation scales are
! not available, use climatological estimates
!
      call cor_scale(radar,scali,scale,rmesh,iflag)
      iflag=1
!
      if(iflag.eq.0) then
         if(scali.lt.rmesh) scali=rmesh
         if(scale.lt.rmesh) scale=rmesh
         if(scali.gt.scali_max) scali=scali_max
         if(scale.gt.scale_max) scale=scale_max
!
         scali=scali/rmesh
         scale=scale/rmesh
      else
         scali=scali_def
         scale=scale_def
      endif
!
! specify the radius of influence
!
      radi1=max(scali,scale)
      write(6,9) scali,scale,radi1
!
! if radi1 is greater than the radius of influence used in gage-
! only analysis, set it to radius
!
      if(radi1.gt.radius) radi1=radius
!
! compute local statistics where rain gage data exist within
! the radius of influence
!
      call radar_local_stat(radi1,radar,ndata,nposi,summ1,summ2)
!
! specify indicator correlation parameters;
!
! cng= lag-0+ indicator correlation coefficient,
! cg = lag-0 indicator correlation coefficient,
! rg = indicator correlation scale of gage rainfall, and
! rc = indicator cross-correlation scale of between
!      gage and radar rainfall
!
      cor0pig=1.
      cor0ig=1.
      rangeig=scali
      rangeic=scali
!
! specify conditional correlation parameters;
!
! cng1= lag-0+ conditional correlation coefficient,
! cg1 = lag-0 conditional correlation coefficient,
! rg1 = conditional correlation scale of gage rainfall, and
! rc1 = conditional cross-correlation scale between
!       gage and radar rainfall
!
      cor0pcg=1.
      cor0cg=1.
      rangecg=scale
      rangecc=scale
!
! specify lag-zero cross-correlation coefficients
!
! cnc  = lag-0+ indicator cross-correlation coefficient
!        between gage and radar rainfall,
! cc   = lag-0 indicator cross-correlation coefficient
!        between gage and radar rainfall,
! cnc  = lag-0+ conditional cross-correlation coefficient
!        between gage and radar rainfall,
! cc   = lag-0 conditional cross-correlation coefficient
!        between gage and radar rainfall,
!
      cor0pic=crscori
      cor0ic=crscori
      cor0pcc=crscorc
      cor0cc=crscorc
!
! specify variables for nearest-point search
!
      nactrv=1
      iactrv(1)=1
      iactrv(2)=0
      iordrv(1)=1
      iordrv(2)=0
!
! check for multple gages within a single HRAP bin
!
      do 20 i=1,ngage
      ih(i)=ihloc(i)
      jh(i)=jhloc(i)
      gagval(i)=hr1gag(ihour,i)
   20 continue
!
      ngndup=ngage
      call check_multiple_gages(ngndup,ih,jh,gagval)
      write(6,11) ngndup
!
! copy rain gage data arrays
!
      do 30 j=1,ngndup
      irv(j)=1
      u(j)=ih(j)
      v(j)=jh(j)
      z(j)=gagval(j)
   30 continue
! 
! sort gage measurements 
!
      call sortc(ngndup)
!
! ****** start estimation *****
!
!
!   if HRAP bin is missing or has been edited out, substitute gage
!     only field
!
!MEB  MOVED THIS OUTSIDE OF 100 LOOP SO IT WOULD WORK ON ENTIRE
!MEB  131x131 GRID
!
      DO I=1,NX
       DO J=1,NY
        IF(MISBIN(I,J).EQ.0) fldmlt(i,j) = fldggo(i,j)
       ENDDO
      ENDDO
!
!   loop on all HRAP bins of interest
!
      DO 100 IBIN=1,NOK
      u0=IOK(IBIN)
      v0=JOK(IBIN)
!
      i=IOK(IBIN)
      j=JOK(IBIN)
      IF (MISBIN(I,J).EQ.1) THEN
! 
! find nearest neighbors within the radius of influence
!    
      call find1c(ngndup,0,u0,v0,nbrsml,iflag)
      if(iflag.ne.0) then
         write(6,*) 'error in find1c'
         iml=0
         return
      endif
!
      do 60 jj=1,nbrsml
      if(rlist(jj).gt.radi1) then
         ng=jj-1
         go to 65
      endif
   60 continue
!
      ng=nbrsml
   65 continue

      if(ng.gt.0) then
!
! check if local statistics of radar rainfall have already been computed
!
      if(ndata(i,j).gt.0) then
      numb=ndata(i,j)
      sum1=summ1(i,j)
      sum2=summ2(i,j)
      npos=nposi(i,j)
      else
      write(6,*) 'newly calculating local statistics,i,j ',i,j
      call local_stat(u0,v0,radi1,radar,1,1,numb,sum1,sum2,npos)
      endif
!
! add rain gage data
!
      do 15 k=1,ng
      numb=numb+1
      gage=z(ilist(k))
      if(gage.ge.rainminml) npos=npos+1
      sum1=sum1+gage
      sum2=sum2+gage*gage
   15 continue
!
! if npos is equal to zero, assume that there is no rain within the
! field of view
!
      if(npos.eq.0) then
         fldmlt(i,j)=0.
         varia=0.
         go to 100
      endif
!
! compute unconditional statistics...there should exist at least two
! data points, i.e., the collocated radar-gage pair
!
      uave=sum1/numb
      uvar=sum2/(numb-1)-sum1*sum1/(numb*(numb-1))
!
! compute fractional coverage
!
      f0=float(npos)/numb
!
! compute indicator variance
!
      vari=f0*(1.-f0)
!
! compute conditional mean
!
      cave=uave/f0
!
! check for positivity of unconditional variance
!
      if(uvar.le.0.) uvar=cave*cave*vari + 0.01
!
! compute conditional variance
!
      cvar=uvar-cave*cave*vari
      cvar=cvar/f0
!
! check for positivity of conditional variance
!
      if(cvar.le.0.) then
      cvar=0.01
      uvar=f0*cvar+cave*cave*vari
      if(uvar.le.0.) write(6,*) 'uvar le 0 ',uvar
      endif
!
! if there are no gages within the radius of radi1, perform
! progressive smoothing from radar-gage merging to radar-only
! mapping
!
      else

      dinc=radi1/4.
      diff=rlist(1)-radi1
      if(diff.lt.0.5) radi2=radi1
      if(diff.ge.0.5.and.diff.lt.1.5) radi2=radi1-dinc
      if(diff.ge.1.5.and.diff.lt.2.5) radi2=radi1-2*dinc
      if(diff.ge.2.5.and.diff.lt.3.5) radi2=radi1-3*dinc
!
! the nearest gage is too far from the point of estimation...
! set the estimate equal to the radar rainfall
!
      if(diff.ge.3.5) then
      fldmlt(i,j)=radar(i,j)
      varia=-99.
      go to 100
      endif

      call local_stat(u0,v0,radi2,radar,1,1,numb,sum1,sum2,npos)
!
! check if enough data points exist
!
      if(numb.le.1) then
      fldmlt(i,j)=radar(i,j)
      varia=-99.
      go to 100
      endif
!
! if there is no radar data reporting rain, assume no rain
!
      if(npos.eq.0) then
      fldmlt(i,j)=0.
      varia=0.
      go to 100
      endif
!
! compute unconditional statistics
!
      uave=sum1/numb
      uvar=sum2/(numb-1)-sum1*sum1/(numb*(numb-1))
!
! check for positivity of unconditional variance
!
      if(uvar.le.0.) then
      fldmlt(i,j)=radar(i,j)
      varia=-99.
      go to 100
      end if
!
! compute fractional coverage
!
      f0=float(npos)/numb
!
! compute indicator variance
!
      vari=f0*(1.-f0)
!
! compute conditional mean and variance
!
      cave=uave/f0
      cvar=uvar-cave*cave*vari
      cvar=cvar/f0
!
! check if conditional variance is positive
!
      if(cvar.le.0.) then
         fldmlt(i,j)=radar(i,j)
         varia=-99.
         go to 100
      endif
!
! perform radar-only estimation
!
      if(ityml.eq.1) then
      call soe_radar_only(crscori,crscorc,uave,uvar,f0,vari,                  &
        cave,cvar,radar(i,j),fldmlt(i,j),ifvar,varia)
      go to 100
      else
      call doe_radar_only(crscori,crscorc,rainminml,f0,vari,                  &
        cave,cvar,radar(i,j),fldmlt(i,j),ifvar,varia)
      go to 100
      endif

      endif

! perform radar-gage merging

      if(ityml.eq.1) then
         call merge_soe(uave,uvar,f0,vari,cave,cvar,ng,                       &
           radar,i,j,u0,v0,fldmlt(i,j),varia)
      else
         call merge_doe(f0,vari,cave,cvar,ng,                                 &
           radar,i,j,u0,v0,fldmlt(i,j),varia)
      endif
      ENDIF
  100 continue
!
!       OUTPUT AND/OR PRINT MULTI-SENSOR PRECIP FIELD
!       MULTIPLY VALUES BY 100 AND CHANGE TO INTEGER BEFORE OUTPUT
!
         iml=1
         DO 250 J=1,131
         DO 250 I=1,131
         if(fldmlt(i,j).lt.threshmin) then
            ifld(i,j)=0
         else
            IFLD(I,J)=FLDMLT(I,J)*100
         end if
  250    continue
         WRITE(LUMTFD) RADID,RLAT,RLON
         WRITE(LUMTFD) date
         WRITE(LUMTFD) FLDMLT
!        WRITE(LUMTFD) IFLD
         close(lumtfd)
!
! zero field detected
!
!     ELSE
!        iml=2
!MEB
!  GO AHEAD AND WRITE OUT ZERO FIELD
!MEB
!      do j=1,ny
!        jdis=j-66
!       do i=1,nx
!        idis=i-66
!        if(idis*idis+jdis*jdis.le.4356) fldmlt(i,j)=0.0
!       enddo
!      enddo
!        WRITE(LUMTFD) RADID,RLAT,RLON
!        WRITE(LUMTFD) date
!        WRITE(LUMTFD) FLDMLT
!     end if
!
      RETURN
    2 FORMAT(/,3X,'MNMULT:  TEST1=',L1,2X,'TEST2=',L1,2X,'TEST3=',L1,         &
         2X,'TEST4=',L1,2X,'TEST5=',L1)
    3 FORMAT(/,2X,'BIAS=',F5.2)
    9 format(/,2x,'scali=',f6.3,2x,'scale=',f6.3,                             &
         2x,'radi1=',f6.3)
   11 format(/,2x,'number of non duplicate gages = ',i3)
   66 format(/,2x,'filename=',a80)
      end
