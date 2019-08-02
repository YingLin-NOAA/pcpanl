      program oconus_grid_shift
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
! Output:
!   Unit 51: output (ST4_ak[pr].yyyymmddhh.xxh)
! History:
!   2017-05-02: do not apply data mask for PR: NBM would like coverage for
!       entire domain (data mask for AK: removed this item earlier since
!       we cannot be confident that some remote land areas in AK actually has
!       coverage; having a land-only mask for AK might imply more confidence
!       in the AKQPE than warranted).
!   2019-07-12: output to GRIB2 rather than GRIB1.  
!
      parameter(lmax=1000000)
      integer jpds(25), jgds(22), kpds(25), kgds(22)
      dimension pcp(lmax), bmask(lmax)
!  bit is the dummy bitmap file (as a place holder in getgb, not actually used)
      logical*1  bit(lmax)
      integer iptable(13)
!
      read(5,'(a10)',err=101) vdate
!
! read in the input precip file using getgb:
!
      jpds = -1
      call baopenr(11,'fort.11',ibaret) 
      call getgb(11,0,lmax,-1,jpds,jgds,kf,k,kpds,kgds,bit,pcp,iret)
      if ( ibaret .ne. 0 .or. iret .ne. 0 ) then
        write(6,*) 'Error reading in the QPE. ibaret, iret=', ibaret, iret
        stop
      endif
!
      nx=kgds(2)
      ny=kgds(3)
      alat1=float(kgds(4))/1000.
      alon1=float(kgds(5))/1000.
      alonv=kgds(7)/1000.           ! LOV grid orientation
      dx=kgds(8)
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
      kgds( 4)= nint(alat1p5*1000.) 
      kgds( 5)= nint(alon1p5*1000.)
!
! AK and PR QPE seems to have 2-3 significant digits after the decimal point.  
! 
      kpds(22)= 3       ! Decimal scale factor.  
      call baopenw(51,'fort.51',ibaret)
      call putgb(51,nx*ny,kpds,kgds,bit,pcp,iret)
      call grib2_wrt_g2func(pcp,bit,vdate,1.,iptable,51,iret)
      write(6,*) 'PUTGB to unit 51, iret=', iret,' iretba=', iretba
      stop
      write(6,*) 'Write out shifted QPE file, ibaret,iret=',ibaret,iret
!
      stop
      end
