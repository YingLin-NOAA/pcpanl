      SUBROUTINE MNGAGE_NEW(NGAGE,IHLOC,JHLOC,DATE,IHOUR,                     &
                            igg,XMHAT,XMGAG,PHAT,XMXR,XMXG,ngaghr)
!
!   GAGE ONLY PRECIP ANALYSIS ALGORITHM MAIN PROGRAM
!   CALLING SUBROUTINE: MAIN PROGRAM
!   subroutine tests gage-only analysis procedures
!   version Jan 2, 1996 by D.-J. Seo at HRL/NWS
!
!   igg = gage only field calculation flag
!       = 0 -- no calculation
!       = 1 -- calculated
!       = 2 -- zero field
!
      parameter (ndat=1000,ngmax=20,nx=131,ny=131)
      common/inform/iactrv(2),nactrv,iordrv(2)
      common/gag2rad/istart(nx),itermi(nx)
      common/big1/u(ndat),v(ndat),z(ndat)
      common/srtord/ilist(ngmax)
      common/srtdst/rlist(ngmax)
      common/big3/irv(ndat)
      COMMON/FLDG/FLDGGO
      INCLUDE 'lugage'
      INCLUDE 'cnst'
      INCLUDE 'ppinp'
      INCLUDE 'ppout'
      INCLUDE 'bins'
      INCLUDE 'cnqu'
      INCLUDE 'mis'
      INCLUDE 'hrg'
      INCLUDE 'raddat'
      INCLUDE 'ggoparam'
      INCLUDE 'mltparam'
!
      INTEGER*2 IFLD(131,131)
      CHARACTER DATE*10
      CHARACTER outdir*180
      LOGICAL TEST1,TEST2,TEST3,TEST4,TEST5
!
      DIMENSION FLDGGO(131,131)
      DIMENSION IHLOC(1000),JHLOC(1000),gagval(1000),ih(1000),jh(1000)
!
      TEST1=(XMHAT.GE.THR1)
      TEST2=(XMGAG.GE.THR1)
      TEST3=(PHAT.LE.THR2)
      TEST4=(XMXR.GE.THR3)
      TEST5=(XMXG.GE.THR3)
      IF(IDEBUG.GT.0) WRITE(6,2) TEST1,TEST2,TEST3,TEST4,TEST5
!
!   IF ANY TEST IS TRUE, THEN CALCULATE GAGE ONLY PRECIP FIELD
!MEB WONDERING IF WE NEED THIS TEST FOR GAGE ONLY FIELD
!MEB WE'D LIKE SOME SORT OF GAGE ONLY FIELD, EVEN IF ALL GAGES ARE ZERO
!
!     IF(TEST1.OR.TEST2.OR.TEST3.OR.TEST4.OR.TEST5) THEN
!
!
!              OPEN FILE FOR GAGE ONLY OUTPUT
!
!     CALL GET_APPS_DEFAULTS('st3_input',9,OUTDIR,lendir)
         OUTDIR = ' '
         CALL GETENV('ST2_OUT',OUTDIR)
         LENDIR = INDEX(OUTDIR,' ') -1
         IF (LENDIR.LE.0) LENDIR = LEN(OUTDIR)
      FILGAO=outdir(1:lendir)//'/'//RADID//'gg'//DATE//'z'
      OPEN(LUGAFD,FILE=FILGAO,FORM='UNFORMATTED')
!
! specify variables for nearest-point search 
!
      nactrv=1
      iactrv(1)=1
      iordrv(1)=1
      iactrv(2)=0
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
!
! copy gage data arrays
!
      ngag=0
      do 30 j=1,ngndup
      ngag=ngag+1
      irv(ngag)=1
      u(ngag)=ih(j)
      v(ngag)=jh(j)
      z(ngag)=gagval(j)
   30 continue
!
! check for number of gage data
!
!MEB THIS IS SAYING THAT IF WE HAVE LESS THAN 4 GAGES, DON'T DO
!MEB THE GAGE-ONLY ANALYSIS, I'M GOING TO TEST CHANGING THIS
!     if(ngag.lt.nbrsgg) then
      if(ngag.lt.1) then
         igg=0
         return
      end if
!
! sort gage measurements
!
      call sortc(ngag)
!
! loop on all HRAP bins of interest
!
      do 110 j=1,ny
      jdis=j-66
!
! initialize counters and locators
!
      knt=0
      istart(j)=0
      itermi(j)=0

      do 100 i=1,nx
      idis=i-66
!MEB THIS IS ONLY DOING THE GAGE ANALYSIS INSIDE THE RADAR UMBRELLA,
!MEB WHICH IS UNNECESSARY
!     if(idis*idis+jdis*jdis.gt.4356) go to 100
      u0=i
      v0=j
! 
! find nearest neighbors 
! 
      call find1c(ngag,0,u0,v0,nbrsgg,iflag)
      if(iflag.ne.0) then
        write(6,*) 'error in find1c'
        igg=0
        return
      endif
!
! count the number of gages within the radius of influence
!
         do 40 jj=1,nbrsgg
         if(rlist(jj).gt.radius) then
            ng=jj-1
            go to 45
         endif
   40    continue
         ng=nbrsgg
!
! keep track of starting and terminating bins for the precalculation
! of local statistics of radar rainfall
!
   45 if(ng.gt.0) then
      knt=knt+1
      if(knt.eq.1) istart(j)=i
      itermi(j)=i
      endif
!
! if no gages are available within the radius of influence, assume that
! there is no rainfall
!
      if(ng.eq.0) then
!        fldggo(i,j)=0.
!
!MEB SET TO MISSING
!
         fldggo(i,j)=-99.
         varia=0.
         go to 100
      endif
!
! if only one gage exists within the radius of influence, use RDS
! (i.e., take the nearest gage value)
!
      if(ng.eq.1) then
         call calggo1(ng,fldggo(i,j))
         varia=-99.
         go to 100
      endif
! 
! there exist at least two gage data within the radius of influence...
! compute probability of rainfall 
! 
      n0=0
      do 50 l=1,ng
      if(z(ilist(l)).ge.rainmingg) n0=n0+1
   50 continue
      f0=float(n0)/ng
!
! if all gage data report no rainfall, assume that there is no rainfall
!
      if(f0.eq.0.) then
         fldggo(i,j)=0.
         varia=0.
         go to 100
      endif
!
! compute unconditional sample statistics of gage rainfall
!
      call stati2(ng,z,umean,uvari)

      if(uvari.le.0.) then
!
! cannot perform SOE or DOE...use RDS
!
      call calggo1(ng,fldggo(i,j))
      varia=-99.
      go to 100
      endif
!
! compute conditional statistics
!
      vari=f0*(1.-f0)
      cmean=umean/f0
      cvari=uvari-cmean*cmean*vari
      cvari=cvari/f0
!
! check if cvari is nonpositive
!
      if(cvari.le.0.) then
!
! cannot perform SOE or DOE...use RDS
!
      call calggo1(ng,fldggo(i,j))
      varia=-99.
      go to 100
      endif
!
! if the local rainfall coverage is full, SOE is identical to DOE...
! use RDS or OE depending on the user's choice
!
      if(f0 .eq. 1.) then

         if(itygg.eq.1) then
! 
! perform RDS
!
            call calggo1(ng,fldggo(i,j))
            varia=-99.
            jtype1=jtype1+1
            go to 100
         else
!
! perform OE
!
            call calggo4(ng,u0,v0,f0,umean,uvari,cmean,cvari,                 &
                         fldggo(i,j),varia,iflag)
!
! OE could not be performed...use RDS
!
            if(iflag.ne.0) then
               call calggo1(ng,fldggo(i,j))
               varia=-99.
               jtype1=jtype1+1
               go to 100
            else
               jtype4=jtype4+1
               go to 100
            end if
         end if
!
! if the local rainfall coverage is fractional, use RDS, SOE,
! or DOE depending on the user's choice
!
      else if(f0.gt.0.0.and.f0.lt.1.) then

         if(itygg.eq.1) then
!
! perform RDS
!
            call calggo1(ng,fldggo(i,j))
            varia=-99.
            jtype1=jtype1+1
            go to 100
         else if(itygg.eq.2) then
!
! perform SOE
!
            call calggo2(ng,u0,v0,f0,umean,uvari,cmean,cvari,                 &
                         fldggo(i,j),varia,iflag)

            if(iflag.ne.0) then
!
! SOE could not be performed...use RDS
!
               call calggo1(ng,fldggo(i,j))
               varia=-99.
               jtype1=jtype1+1
               go to 100
            else
               jtype2=jtype2+1
               go to 100
            end if

         else if(itygg.eq.3) then
!
! perform DOE
!
            call calggo3(ng,u0,v0,f0,umean,uvari,cmean,cvari,                 &
                         fldggo(i,j),varia,iflag)
!
! DOE could not be performed...use RDS
! 
            if(iflag.ne.0) then
               call calggo1(ng,fldggo(i,j))
               varia=-99.
               jtype1=jtype1+1
               go to 100
            else
               jtype3=jtype3+1
               go to 100
            end if
         end if
      end if
  100 continue
  110 continue
!
       igg=1
       DO 200 J=1,131
       DO 200 I=1,131
       if(fldggo(i,j).lt.threshmin) then
          ifld(i,j)=0
       else
          IFLD(I,J)=FLDGGO(I,J)*100
       end if
  200  CONTINUE
!n
       WRITE(LUGAFD) RADID,RLAT,RLON
       WRITE(LUGAFD) date
       WRITE(LUGAFD) FLDGGO
!      WRITE(LUGAFD) IFLD
!
!     ELSE IF(NGAGHR.GT.0) THEN
!        igg=2
!MEB
! GO AHEAD AND WRITE OUT A ZERO FIELD
!MEB
!      do j=1,ny
!        jdis=j-66
!       do i=1,nx
!        idis=i-66
!        if(idis*idis+jdis*jdis.le.4356) fldggo(i,j)=0.0
!       enddo
!      enddo
!      WRITE(LUGAFD) RADID,RLAT,RLON
!      WRITE(LUGAFD) date
!      WRITE(LUGAFD) FLDGGO
!     END IF
!
      RETURN
    2 FORMAT(/,3X,'MNGAGE:  TEST1=',L1,2X,'TEST2=',L1,2X,'TEST3=',L1,         &
         2X,'TEST4=',L1,2X,'TEST5=',L1)
      END
