      PROGRAM PREPMETAR
!
!$$$  main program documentation block
!
! MAIN PROGRAM: PREPMETAR    Program to process METAR precip data
!   Programmer: Ying lin           ORG: NP22        Date: 2001-06-01  
!
! ABSTRACT: (MEB) use prephads code to process METAR data
!   Unit for reports in METAR is hundredth of an inch.  Converted to mm for
!   output to units 51-53.
!
! PROGRAM HISTORY LOG:
!   1/29/96 M. Baldwin
!        program to preprocess hrly METAR data
!        original name was prephads.metar.f
!   2001/1/25 Y. Lin 
!        changed from 2-digit year to 4-digit year
!   2006/12/06 Y. Lin
!        When raw report is negative, classify it as 'failed gross QC'
!   2007/7/27  Y. Lin 
!        Toss out the obs if the weather condition is 'CLR' but the 
!        precipitation accumulation is positive.  Two cases of erroneous 
!        precipitation amounts under 'CLR' weather were found in 23 Jul 2007
!        (16Z and 19Z).  It is, however, possible to have real precip accum
!        for the previous hour while the wx is CLR, since the sky observation 
!        is on a much shorter time frame than the precipitation.  The safest
!        thing to do is not to use these obs, rather than re-setting them to
!        zero and using them.
!   2008/3/11 Y. Lin
!        Use gauge QC current.eval to screen out bad gauges.  
!        Note:
!          1) ASOS gauges are screened by gauge QC
!          2) AWOS gauges are not
!          3) Drop the first letter from the METAR station ID to match
!             that in the gauge QC list.  For example:
!                In METAR data:          In current.goodH:
!                   KAZO                     AZO
!   2012/08/01, Y. Lin: convert for WCOSS
!   2015/10/26, Gross error check: set limit to 5"/hr instead of 1"/hr.  
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!$$$
!
!
      CHARACTER TITLE*45,fname*120,outdir*200
      CHARACTER sti*5,radid*3,date*10, obshhmm*4, wxc*3
      CHARACTER stid(9999)*8,stid1*5,stid21*7,gid(9999)*8
      CHARACTER*8 stidbad, stblst(9999)
      DIMENSION apcp(9999),gpcp(9999)
      DIMENSION alat(9999),alon(9999),rmiss(9999)
      DIMENSION glat(9999),glon(9999)
!
      CALL W3TAGB('PREPMETAR ',2001,0152,0060,'NP22   ')
!
!  init arrays
!
        apcp=0.

        rmiss=0.
!
! I/O units
!  Input: 
!    fort.5:   yyyymmddhh to be processed
!    fort.11:  bad gauges list
!    fort.15:  sfctbl file, from which iyr,imn,iday,ihr are read in
!    fort.16:  ao2 file - precip amounts
!    fort.18:  ST2_PARAM/cards.parm
!  Output: 
!    fort.51:  gauge group for each radar site
!    fort.52:  problem METAR reports
!    fort.53:  ascii METAR reports being used (good)
!
!
!  read flagged gauge list from daily Gauge QC:

      iblst=0

!  Skip the first line, which is "yyyymmdd.evalH":
      READ(11,*)
 10   continue

      READ(11,'(A8)',end=25,err=20) stidbad
       iblst=iblst+1
       stblst(iblst)=stidbad
 20   continue

      goto 10

 25   continue
      call getenv("ST2_ST1",outdir)
      outdir=trim(outdir)//"/"
      read(5,'(a10)') date
      read(date,81) iyr1,imn1,ida1,ihrget
 81   format(i4,3i2)
!
!  read data
!
!  Read the date from 
      i=0
      read(15,7777,end=200) iyr,imn,iday,ihr
 7777 FORMAT(30x,i4,3i2)

 103  read(16,7773,end=200) stid21,rlat,rlon,obshhmm,wxc,ipcx
! 7773 FORMAT(a8,6x,f6.2,1x,f7.2,x,a4,112x,i4)
! 7773 FORMAT(a8,6x,f6.2,1x,f7.2,x,a4,64x,a3,45x,i4)
!yl Skip the first character of staid, so it'll match the IDs in gauge QC
 7773 FORMAT(1x,a7,6x,f6.2,1x,f7.2,x,a4,64x,a3,45x,i4)
!yl   if (ipcx.ge.0) pcx=ipcx*0.01
!yl   if (ipcx.lt.0) pcx=0.004
      pcx=ipcx*0.01
         if (pcx.gt.5.0 .or. pcx.lt.0.) then
          write(52,7774)                                                      &
            stid21,rlat,rlon,pcx*25.4,iyr,imn,iday, obshhmm
 7774     format(a7,2f8.2,f10.2,3x,i4,2i2.2,1x,a4,'  gross QC')
          goto 103
         endif
! Check against bad gauge list:
         do k1=1,iblst
           if (stid21.eq.stblst(k1)(1:7)) then
             write(52,7771)                                                   &
               stid21,rlat,rlon,pcx*25.4,iyr,imn,iday, obshhmm
 7771        format(a7,2f8.2,f10.2,3x,i4,2i2.2,1x,a4,'  reject list')
             go to 103
           endif
         enddo
!
         if (pcx.gt.0. .and. wxc.eq.'CLR') then
           write(52,7772)                                                     &
             stid21,rlat,rlon,pcx*25.4,iyr,imn,iday, obshhmm, wxc
 7772      format(a7,2f8.2,f10.2,3x,i4,2i2.2,1x,a4,2x,a3, ' Sky')
           go to 103
         endif
!
         if (iyr.eq.iyr1.and.imn.eq.imn1.and.iday.eq.ida1.and.                &
                 ihr.eq.ihrget) then
!
!  convert inch to mm
!
            i=i+1
            write(53,7775)                                                    &
              i, stid21, rlat, rlon, pcx*25.4, iyr,imn,iday, obshhmm
 7775       format(i5,2x,a8,3f8.2,3x,i4,2i2.2,1x,a4)
            apcp(i)=pcx*25.4
            alat(i)=rlat
            alon(i)=rlon
            rmiss(i)=1.0
            stid(i)=stid21
            idum=1
         endif
      goto 103
 200  continue
      ktot=i
      write(6,*) ' read in ',ktot,' stations '
!
!  data read, now write out data that is close to each radar location
!
 151  READ(18,33,END=96) RADID,RLAT,RLON
      READ(18,*,END=96) 
 33   FORMAT(A3,2F10.3)
         close(unit=51)
         LENDIR = INDEX(OUTDIR,' ') -1
         IF (LENDIR.LE.0) LENDIR = LEN(OUTDIR)
         fname=outdir(1:lendir)//'/gage.'//RADID//DATE//'Z'
!
       open(unit=51,file=fname,form='UNFORMATTED')
!
      radius=3.7
 152  krad=0
      do i=1,ktot
       if (rmiss(i).gt.0.0) then
         dlat=abs(rlat-alat(i))
         dlon=abs(rlon-alon(i))
         if (dlat.lt.radius.and.dlon.lt.radius) then
           krad=krad+1
           gpcp(krad)=apcp(i)
           glat(krad)=alat(i)
           glon(krad)=alon(i)
           gid(krad) =stid(i)
         endif
       endif
      enddo
!      
!  expand search radius if no luck
!      
      if (krad.eq.0) then
        radius=radius*2.
        write(6,*) ' NO GAGES FOUND WITHIN ',radius,' DEG OF ',radid
        if (radius.gt.45.0) then
         write(6,*) ' GIVING UP ON ',radid,' RADIUS =',radius
         krad=1
         gpcp(1)=0.
         glat(1)=34.
         glon(1)=170.
        endif
        if (radius.le.45.0) goto 152
      endif
!      
!  write out stuff      
!      
      write(6,*) ' writing ',krad,' gages for ',radid
      write(51) krad,iyr,imn,iday,ihrget
      write(51) (gpcp(i),i=1,krad)
      write(51) (glat(i),i=1,krad)
      write(51) (glon(i),i=1,krad)
      write(51) (gid(i),i=1,krad)
      goto 151 
  96  continue
      write(6,*) ' finished writing gage data for all radars'
      CALL W3TAGE('PREPMETAR ')
 999  stop
      end
