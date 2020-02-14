      program st4_oconus_convert
!
! Read in a QPE file (GRIB1) from an OConUS RFC (either the original QPE or 
! accumulated into 6h and 24 at NCEP - in case of PR QPE).  Assume that the 
! RFC's definition of the coordinate of the (1,1) box is the lower-left corner 
! of the box; redefine the grid so that the coordinate of the (1,1) box is 
! the center of the box. Output it in GRIB2. 
!
! Grid is assumed to be polar-stereographic (call to w3fb07)
! 
!   Programmer: Ying lin    Date: 2016-11-18
! Input: 
!   Unit 11: input QPE
!   Unit 12: parm file: GRIB2 table for APCP
! Output:
!   Unit 51: output (ST4_ak[pr].yyyymmddhh.xxh.grb2)
! History:
!   2017-05-02: do not apply data mask for PR: NBM would like coverage for
!       entire domain (data mask for AK: removed this item earlier since
!       we cannot be confident that some remote land areas in AK actually has
!       coverage; having a land-only mask for AK might imply more confidence
!       in the AKQPE than warranted).
!   2019-07-12: output to GRIB2 rather than GRIB1.  
!   2020-01-21: check for QPE array dimensions, neither nx nor ny should be
!     greater than 600 . AK QPE is currently 460x530.  Leave some room for 
!     "growth" just in case, as RFCs occasionally do change their output grid.  
!     This is to guard against rogue input (not actual QPEs) from blowing up 
!     the lmax limit). 
!
      parameter(lmax=1000000)
      integer jpds(25), jgds(22), kpds(25), kgds(22)
      dimension pcp(lmax), bmask(lmax)
!  bit is the dummy bitmap file (as a place holder in getgb, not actually used)
      logical*1  bit(lmax)
      integer iptable(13)
      real(4) alat1, alon1,alat1p5, alon1p5

      character varnam*19, vdate*10
!
      read(5,'(a10)') vdate
!
! read in the input precip file using getgb:
!
      jpds = -1
      call baopenr(11,'fort.11',ibaret) 
      write(6,*) 'after baopenr, ibaret=', ibaret
      call getgb(11,0,lmax,-1,jpds,jgds,kf,k,kpds,kgds,bit,pcp,iret)
      if ( ibaret .ne. 0 .or. iret .ne. 0 ) then
        write(6,*) 'Error reading in the QPE. ibaret, iret=',ibaret,iret
        stop
      endif
!
      nx=kgds(2)
      ny=kgds(3)
      if ( nx.gt.600 .or. ny.gt.600) then
         write(6,*) 'Incorrect OConUS QPE array dimension, nx,ny=',nx,ny
         stop
      endif
!
      alat1=float(kgds(4))/1000.
      alon1=float(kgds(5))/1000.
      alonv=kgds(7)/1000.           ! LOV grid orientation
      dx=kgds(8)
!
      write(6,*) 'RFC QPE kpds(14) and kpds(15) = ', kpds(14),kpds(15) 
!
! For their 6-hourly analyses, the RFCs use the previous 12Z as the "reference"
! time in the GRIB heading (grib PDS octets 13-17), and the accumulation 
! beginning/ending times are given in octets 19-20:
!
!      RFC QPE              Actual accum        Oct 13-16      Oct 19   Oct 20
!      file name            ending time       ref yymmddhh      P1       P2
!   QPE.rid.2019032618.06h   2019032618         19032612         0        6
!   QPE.rid.2019032700.06h   2019032700         19032612         6       12
!   QPE.rid.2019032706.06h   2019032706         19032612        12       18
!   QPE.rid.2019032712.06h   2019032712         10032612        18       24
! 
! In GRIB1 Stage IV we followed the RFC convention and kept the time range
!   indicators used by the RFCs (i.e. use the previous day's 12Z as reference
!   time and pretend that the analysis is the 00-06/06-12/12-18/18-24h 
!   'forecasts' and made correction for this when converting the ST4 to URMA. 
!   For GRIB2 version of ST4, we're using the valid time read in from unit 5,
!   and the length of accumulation is P2-P1 [kpds(15)-kpds(14)]:
!   
      acc=kpds(15)-kpds(14)
!
!
!  Read in GRIB2 table to APCP:
      do k = 1, 42
        read(12,*)
      end do
      read (12,*) varnam, (iptable(i),i=1,13)
!
! For PRQPE, create a bitmap mask by matching the value of the data mask to 
!   the RFC ID in the GRIB header.  
! Don't create a mask for AKQPE, since we cannot be sure whether an area with
!   zero precip actually has no data.
!
      rfcid=kpds(23)
      write(6,*) 'RFCid=',rfcid, ' Original alat/alon:', alat1, alon1
!
! Shift the grid by 0.5 dx/dy:
      write(6,*) '   alonv, dx:', alonv, dx
! Find the alat, alon for grid coord of (1.5,1.5)
      call w3fb07(1.5,1.5,alat1,alon1,dx,alonv,alat1p5,alon1p5)
      write(6,*) 'shifted alat/alon:', alat1p5, alon1p5
!
! AK and PR QPE seems to have 2-3 significant digits after the decimal point.  
! 
      call baopenw(51,'fort.51',ibaret)
      write(6,*) 'alat1,alon1,alat1p5,alon1p5=',                              &
        alat1,alon1,alat1p5,alon1p5
! For some reason a negative alon1p5 (-70.58200) doesn't work, so add 360 to
!   it to make it positive:
      call grib2_wrt_g2func_hrap(nx,ny,alat1p5,360.+alon1p5,                  &
                             pcp,bit,vdate,acc,iptable,51,iret)
      write(6,*) 'output oconus st4 in grb2, ibaret,iret=',ibaret,iret
!
      stop
      end
