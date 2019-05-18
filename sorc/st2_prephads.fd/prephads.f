      PROGRAM NAM_PREPHADS

!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: NAM_PREPHADS
!   PRGMMR: KEYSER           ORG: NP22        DATE: 2006-02-02
!
! ABSTRACT: CREATE A LIST OF HOURLY PRECIP REPORTS FROM A BUFR DUMP
!   CREATED FROM DUMPJB.
! 
! PROGRAM HISTORY LOG:
! ????-??-??  M. Baldwin - Original author (prephads.bufr.f)
! 2001-01-09  Y. Lin     - Convert to 4 digit year, yyyymmddhh in
!     output file names
! 2006-02-02  D. Keyser  - Replaced call to BUFRLIB routine READIBM
!     with call to BUFRLIB routine READMG (READIBM obsolete with
!     1/31/2006 version of BUFRLIB)
!
! 2008-03-11  Use gauge QC output current.evalH to screen out bad gauges
! 2008-09-09  Dennis cleaned up/updated the code
! 2012-12-18  Y. Lin updated for ifort
! 2015-10-26  Gross error check: set limit to 5"/hr instead of 1"/hr.  
! 2019-04-01  Removed 'RAWRPT' - no longer needed (see Jeff Ator email today)
! 2019-05-17  There are many more "missing" points (designated with
!             values=100000000000).  Do not write them out to hads_prob - 
!             that file should just contain gauges rejected due to either 
!             gross error check or listed in the gaugeqc/current.evalH.  
!
! USAGE:
!   INPUT FILES:
!     UNIT 5   - STANDARD INPUT
!     UNIT 11  - RAIN GAUGE BLACKLIST
!     UNIT 18  - PRECIP DATA CLOSE TO RADAR STATIONS
!     UNIT 20  - BUFR DUMP FILE CONTAINING PRECIP DATA
!
!   OUTPUT FILES:
!     UNIT 06  - STANDARD OUTPUT PRINT
!     UNIT 51  - BINARY FILE CONTAINING PRECIP DATA for each radar site
!                fname: gage.${rid}${yyyymmddhh}Z
!     UNIT 52  - ASCII LISTING OF HRLY REPORTS rejected due to QC, gross err chk
!     UNIT 53  - ASCII LISTING OF HOURLY PRECIP REPORTS that are presumed 'good'
!
!   SUBPROGRAMS CALLED:
!     LIBRARY:
!       SYSTEM:  - GETENV
!       W3LIB :  - W3TAGB   W3TAGE
!       BUFRLIB: - DATELEN  OPENBF   READMG   READSB   UFBINT
!
!   EXIT STATES:
!     COND =   0 - SUCCESSFUL RUN
!     COND >   0 - ABNORMAL TERMINATION
!
! REMARKS:
!   Contents of NC000011 (b000/xx011) - AFOS PRODUCTS (PRECIP) (SHEF)
!      YEAR     -- observation year
!      MNTH     -- observation month
!      DAYS     -- observation day
!      HOUR     -- observation hour
!      MINU     -- observation minute
!      RPID     -- report id (8 characters)
!      CLAT     -- latitude (degrees: N+, S-)
!      CLON     -- longitude (degrees: E+, W-)
!      TP01     -- total precipitation past  1 hour  (kg/meter**2)
!      TP03     -- total precipitation past  3 hours (kg/meter**2)
!      TP06     -- total precipitation past  6 hours (kg/meter**2)
!      TP12     -- total precipitation past 12 hours (kg/meter**2)
!      TP24     -- total precipitation past 24 hours (kg/meter**2)
!      .DTHMXTM -- duration of time in for maximum temperature (hours)
!      MXTM     -- maximum temperature (K)
!      .DTHMITM -- duration of time in for minimum temperature (hours)
!      MITM     -- minimum temperature (K)
!      TOSD     -- total snow depth (meters)
!      TOPC     -- total precipitation/total water equivalent
!                  (kg/meter**2)
!      DOFS     -- depth of fresh snow (meters)
!      SOGR     -- state of the ground (code table 0-20-062)
!      RRSTG    -- raw report string (8 characters, replicated)
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM-SP
!
!$$$

        real*8 a1(16),a5(16),a7(255),xtemp
        real apcp(99999)
        real alat(99999),alon(99999),rmiss(99999)
        real*4 glat(99999),glon(99999),gpcp(99999)

        integer mnth(12)
        integer*4 krad,iyr4,imn4,iday4,ihrget4

        CHARACTER fname*160,outdir*100
        CHARACTER sti*5,radid*3,date*10
        CHARACTER stid(99999)*8,stidbad*8,gid(99999)*8

        character * 8 stblst(5000)
        character * 8 chrid,subset

        equivalence(xid,chrid)
        data bmiss/10e10/

        data mnth/31,28,31,30,31,30,31,31,30,31,30,31/

      real*8 timef

      CALL W3TAGB('NAM_PREPHADS',2008,0252,0060,'NP22')

!  Keep track of CPU time:

      btim=timef()
      cpu_time=0.

!  init arrays

        apcp=0.
        rmiss=0.

	lunin=20


!  open files, blacklist, output path, radar cards, choose hour
!               unit 11    unit 51      unit 18

       call getenv("ST2_ST1", outdir)
       outdir=trim(outdir)//"/"

       read(5,'(a10)') date
       read(date,81) iyr1,imn1,ida1,ihrget

!  read flagged gauge list from daily Gauge QC:

      iblst=0

!  Skip the first line, which is "yyyymmdd.evalH":
      READ(11,*)
 330  continue

      READ(11,'(A8)',end=103,err=980) stidbad
       iblst=iblst+1
       stblst(iblst)=stidbad

 980  continue

      goto 330

  103 continue

!  LEAP YEAR

       if (mod(iyr1,4).eq.0) mnth(2)=29

   80  format(a80)
   81  format(i4,3i2)

!  start reading BUFR data

        call datelen(10)
	call openbf(LUNIN,'IN',LUNIN)

        irec = 0
        lk = 0

  888	continue

        CALL READMG(LUNIN,SUBSET,IDATE,IRET)
        irec = irec + 1

        if ( iret .eq. -1 ) then
          print 887,irec
  887     format(1x,'eof on bufr file ',i7,' recs read')
          goto 900
        end if

  886   continue

        call readsb(lunin,jret)

	if(jret.eq.-1)go to 888

!  get stn id, date and temps    

        a1=bmiss
        a5=bmiss

        call ufbint(lunin,a1,16,1,nr1,                                        &
     &     'RPID CLAT CLON YEAR MNTH DAYS HOUR MINU')    

!  get hourly precip

        call ufbint(lunin,a5,16,1,nr5,'TP01 TP03')

!    compute dates

        kyr   = a1(4)
        iyr   = kyr
        imn   = a1(5)
        iday   = a1(6)
        ihr   = a1(7)
        imin  = a1(8)
        if (imin.ge.31) ihr=ihr+1
        if (ihr.ge.24) then
         ihr=mod(ihr,24)
         iday=iday+1
         if (iday.gt.mnth(imn)) then
           iday=1
           imn=imn+1
           if (imn.gt.12) then
             imn=1
             iyr=iyr+1
           endif
         endif
        endif

!    get station ids

        xid  = a1(1) 
        clat = a1(2)
        clon = a1(3)

!  output precip data -- total precipitation past  1 hour
!  If the value coming out of dumpjb is 'missing' (=100000000000), skip the 
!  set it to 9999. (designated as 'missing' within this program and move onto
!  the next record:
        IF ( abs(a5(1)) .GE. 99999. ) then
         a5(1) = 9999.
        ELSE

         IF ( a5(1) .GT. 127. ) then     ! amount > 5"/hr?
           if (abs(clat).ge.999.) clat=-999.
           if (abs(clon).ge.999.) clon=-999.
           write(52,7774)                                                     &
     &       chrid,clat,clon,a5(1),iyr,imn,iday,ihr,imin
 7774     format(a8,2f8.2,f10.2,3x,i4,2i2.2,1x,2i2.2,'  gross QC')
           a5(1) = 9999
         ENDIF

!  check against blacklist

         do k1=1,iblst
           if (chrid.eq.stblst(k1)) then
             if (abs(clat).ge.999.) clat=-999.
             if (abs(clon).ge.999.) clon=-999.
             write(52,7775)                                                   &
     &         chrid,clat,clon,a5(1),iyr,imn,iday,ihr,imin
 7775       format(a8,2f8.2,f10.2,3x,i4,2i2.2,1x,2i2.2,'  reject list')
            a5(1) = 9999
           endif
         enddo

! meb 6/11 not going to worry about the time during the hour to
! start with... need to figure something out later

!        if (ihr.eq.ihrget.and.(imin.ge.45.or.imin.le.15)) then

         if (iyr.eq.iyr1.and.imn.eq.imn1.and.iday.eq.ida1.and.                &
     &           ihr.eq.ihrget) then

         if (a5(1).lt.9999.and.a5(1).ge.0.and.clat.lt.9999.) then
          lk=lk+1
          stid(lk)=chrid
          alat(lk)=clat
          alon(lk)=-1.0*clon
          apcp(lk)=a5(1)
          rmiss(lk)=1.
          write(53,222) lk,chrid,clat,clon,a5(1),iyr,imn,iday,ihr,imin
         endif

         endif  
        ENDIF  ! abs(a5(1)) >= 99999.? 
  222   format(i5,2x,a8,3f8.2,3x,i4,2i2.2,1x,2i2.2)

!   end of station loop

        go to 886

  900   continue

        ktot=lk

!  data read, now write out data that is close to each radar location

  151 continue

      READ(18,33,END=96) RADID,RLAT,RLON
   33 FORMAT(A3,2F10.3)
      READ(18,*,END=96)
         close(unit=51)
         LENDIR = INDEX(OUTDIR,' ') -1
         IF (LENDIR.LE.0) LENDIR = LEN(OUTDIR)
         fname=outdir(1:lendir)//'/gage.'//RADID//DATE//'Z'

      open(unit=51,file=fname,form='UNFORMATTED')

      radius=3.7

  152 continue

      krad=0
      do i=1,ktot
       if (rmiss(i).gt.0.0) then
         dlat=abs(rlat-alat(i))
         dlon=abs(rlon-alon(i))
         if (dlat.lt.radius.and.dlon.lt.radius) then
           krad=krad+1
           gpcp(krad)=real(apcp(i),4)
           glat(krad)=real(alat(i),4)
           glon(krad)=real(alon(i),4)
           gid(krad) =stid(i)
         endif
       endif
      enddo

!  expand search radius if no luck

      if (krad.eq.0) then
        radius=radius*2.
        write(6,*) ' NO GAUGES FOUND WITHIN ',radius,' DEG OF ',radid
        if (radius.gt.45.0) then
         write(6,*) ' GIVING UP ON ',radid,' RADIUS =',radius
         krad=1
         gpcp(1)=0.
         glat(1)=34.
         glon(1)=170.
        endif
        if (radius.le.45.0) goto 152
      endif

!  write out stuff      

!     write(6,*) ' writing ',krad,' gauges for ',radid
      write(6,*) ' writing ',krad,' gauges for ',radid, ' fname=',fname
      iyr4=int(iyr,4)
      imn4=int(imn,4)
      iday4=int(iday,4)
      ihrget4=int(ihrget,4)
      write(51) krad,iyr4,imn4,iday4,ihrget4
      write(51) (gpcp(i),i=1,krad)
      write(51) (glat(i),i=1,krad)
      write(51) (glon(i),i=1,krad)
      write(51) (gid(i),i=1,krad)
      goto 151 

   96 continue

      write(6,*) ' finished writing gauge data for all radars'
      cpu_time=cpu_time+timef()-btim
      write(6,*) 'Time spent in PREPHADS (sec)=', cpu_time*0.001

      CALL W3TAGE('NAM_PREPHADS ')

  999 continue

      stop

      end
