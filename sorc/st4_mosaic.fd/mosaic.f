      program mosaic
!
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! MAIN PROGRAM: MOSAIC (actually more like quilt) the local RFC data into
! a national Stage IV product.
!  
!   Programmer: Ying lin           ORG: NP22        Date: 2001-07-17
!
! ABSTRACT: Read in the 12 CONUS RFCs' stage3 analysis (hourly or 6-hourly)
! and combine into a CONUS product on the national HRAP grid.
!
! 2008/3/21: set KPDS(5)=61, and KPDS(19)=2 before outputting the mosaicked
!   analysis. Some RFCs are outputting their earlier (non-QC'd) hourly files 
!   as 'Table 128, parameter 237'. 
!
! 2012/3/5: set KPDS(4)=192 (flag: include both GDS/BMS).  Purpose: found out
!   that one of the RFCs (NWRFC, id=159) has PDS(4)=128.  Without this, if 
!   analyses from RFCs #160-162 are missing but anl from RFC #159 is persent, 
!   then the resulting Stage IV mosaic would also have PDS(4)=128, and the
!   resulting analysis would appear to have no missing data (and large areas
!   of false zero precip).
! 2012/8/1: convert for WCOSS.
!
! 2015/8/17:
!   1. Hourly: exclude data from CNRFC (153) and NWRFC (159; normally no
!      hourly input anyway)
!   2. If an area 'belongs' to an RFC (has mask value of 150, 152, ..., 162), 
!      it is not filled in by data from neighboring RFCs
!   3. Areas with mask value of '0' (off the Pacific coast, Canada [except for
!      the small area belonging to the NWRFC domain] and Mexico) are not filled
!      in - desigated as 'no data'
!   4. Areas with mask value of '1' (Gulf of Mexico, off the Atlantic Coast)
!      are filled in with the average of all available RFC coverage
!
! 2017/3/27
!   Hourly: do not exclude CNRFC (153)  and NWRFC (159) - we're creating
!     QPE1h files for these two RFCs from their QPE6h, using hourly MRMS
!     as weights.
! 
! Input: Unit     11: 'bytemap' - a mask on the HRAP grid that shows which 
!                     RFC each grid point belongs to (150, 152,..., 160);
!                     '1' if in Gulf of Mexico or off the Atlantic Coast;
!                     '0' elsewhere. 
!        Units 150,152,153,154, ..., 161, 162:
!                     Stage III analyses from RFCs 150, 152, 153, ..., 162
! Output:
!        Unit     51: mosaicked national Stage IV product
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!$$$
      parameter(nx0=1121,ny0=881,nx1=500,ny1=500)
      parameter(dx=4762.5,alonv=-105.)
      parameter(alat0old=23.098,alon0old=-119.036,                            &
                alat0new=23.117,alon0new=-119.023)
      integer bm(nx0,ny0), icnt(nx0,ny0),                                     &
            jpds(200), jgds(200), kgds(200), kpds(200)
      logical*1 bit0(nx0,ny0),bit1(nx1,ny1), btmp(nx1*ny1),ids(150:162)       &
              , flag
      logical*4 fexist
      real p0(nx0,ny0), p1(nx1,ny1), ptmp(nx1*ny1)
      character infile(150:162)*8,outfile*80,maskfile*80
!
      CALL W3TAGB('MOSAIC ',2001,0151,0060,'NP22   ')
!
! Read in the 'bytemap' that maps out the RFC territories:
      jpds = -1
      call baopenr(11,'fort.11',ibaret)
      call getgb(11,0,nx0*ny0,0,jpds,jgds,kf,k,kpds,kgds,bit0,p0,iret)
! (use p0 as a temp array, since the mask is stored as a real array in the
! grib file)
!
! Check to see which files exist.  We are not doing Alaska (No. 151):
! nor Puerto Rico (No. 105)
!
! 'flag' to keep track of whether at least one RFC has data.  If none
! of the RFCs has sent in data yet for the hour (or 6-hour), exit without
! creating a mosaic.
!
      ids = .false.
      flag = .false.
!
      do 10 id = 150, 162
! We are not doing Alaska:
        if (id .eq. 151) go to 10
!
        write(infile(id),'(a5,i3)') 'fort.', id
        inquire(file=infile(id),exist=fexist)     
        if (fexist) then
          ids(id) = .true.
          flag = .true.
        endif
        write(6,*) infile(id), ids(id)
 10   continue
!
      if (.not. flag) then
        write(6,*) 'No Stage III file has come in yet.  Stop'
        stop
      endif
!
!  Create a 'bytemap' on the national HRAP grid based on
!     1) existing bytemap
!     2) whether analysis from an RFC exists for this hour
!
      bm = 0
!
      do 30 j = 1, ny0
        do 20 i = 1, nx0
          itmp = nint(p0(i,j))
          if (itmp.eq.1) then
            bm(i,j)=1
          elseif (itmp .gt. 100) then
            if (ids(itmp)) bm(i,j) = itmp
          endif
 20     continue
 30   continue
!
! Big loop, loop through all the files for this hour:
!
      p0 = 0.
      icnt = 0
      bit0 = .false.
      irfc = 0
! irfc keeps track of how many RFC's have reports, for this hour.
!
      do 100 id = 150, 162
!       if no data from this RFC
        if (.not.ids(id)) go to 100
        call baopenr(id,infile(id),ibaret)
        jpds = -1
        call getgb(id,0,nx1*ny1,-1,jpds,jgds,kf,k,kpds,kgds,btmp,          &
           ptmp,iret)
        write(6,40) id, ibaret, iret, kgds(2), kgds(3)
 40     format('rfcid=',i3,' iret(baopen)=',i3,' iret(getgb)=',i3,            &
           ' nx=',i3,' ny=', i3)
        if (iret.ne. 0) go to 100
!
        irfc = irfc + 1
        nx = kgds(2)
        ny = kgds(3)
        alat1=float(kgds(4))/1000.
        alon1=float(kgds(5))/1000.
!
! Copy ptmp and btmp into p1 and b1 (the way they were read in by getgb,
! they were essentially 1-d arrays.  2-dimensionalize them.
        indx = 0
        do j = 1, ny
          do i = 1, nx
            indx = indx + 1
            p1(i,j) = ptmp(indx)
            bit1(i,j) = btmp(indx)
          enddo
        enddo
! 
! Loop through all the points on this local HRAP grid:
        do 60 j = 1, ny
          do 50 i = 1, nx
            if (.not.bit1(i,j)) go to 50                      
            call w3fb07(float(i),float(j),alat1,alon1,dx,alonv                &
                        ,xlat,xlon)
            call w3fb06(xlat,xlon,alat0old,alon0old,dx,alonv,x0,y0)          
            i0=nint(x0)
            j0=nint(y0)
            if (i0.lt.1 .or. i0.gt.nx0 .or. j0.lt.1 .or. j0.gt.ny0)           &
              go to 50
!
            if (bm(i0,j0) .eq. id) then
              bit0(i0,j0) = .true.
              p0(i0,j0) = p1(i,j)
            elseif (bm(i0,j0).eq.1) then
              bit0(i0,j0) = .true.
              p0(i0,j0) = p0(i0,j0) + p1(i,j)
              icnt(i0,j0) = icnt(i0,j0) + 1
            endif
 50       continue
 60     continue
!
        call baclose(id,ibaclret)
 100  continue
!
! For points not covered by 'home RFC' but covered by other RFC's, take
! the average value:
!
      do 120 j = 1, ny0
        do 110 i = 1, nx0
          if ( bit0(i,j) .and. bm(i,j).eq.1) then
            p0(i,j) = p0(i,j)/float(icnt(i,j))
          endif
 110    continue
 120  continue
!
!  Now modify KPDS and KGDS (based on the last RFC stage 3 analysis read in)
!
      kpds(1) = 7       ! NCEP
      kpds(2) = 182     ! Generating process: RFC QPE mosaic generated by NCEP
      kpds(3) = 255     ! Equiv. of HRAP grid (grid coord is the center, rather
                        !   than the lower-left corner, as hydrologists 
                        !   defines).
      kpds(4) = 192     ! Both GDS/BMS are present
      kpds(5) = 61      ! Precipitation accumulation
      kpds(19)= 2       ! GRIB Table 2
      kpds(22)= 2       ! Decimal scale factor.  
!  kpds(22) is a mystery.  The input Stage III data have two significant 
!  digits after the decimal point (e.g., 30.25) but have the scale factor 
!  of 0, and if we use the same factor, the mosaicked file has no significant 
!  digits (the aforementioned value would be rounded off to '30').  
!  'gribscan' would show that both have the same decimal scale factor, 
!  though 'wgrib -V' can tell the difference.  I am sick of trying to 
!  figure this out, so I'll just expicitly specify this.  
!
!  There must be a 'binary scale factor' in addtion to the 'decimal scale
!  factor' in grib.  'wgrib -V' of the stage III file shows that
!     'DecScale 0 BinScale -3'
!  Before we specified kpds(22)=2 (it was 0 before), we had
!     'DecScale 0 BinScale 0'
!  Afterwards, we have 
!     'DecScale 2 BinScale 0'
!  WHY?  Since we read in all 200 elements of Stage III kpds in getgb.
!  I cannot figure this out.
!
      kpds(23)= 4       ! Subcenter: EMC
!
      kgds( 2)= nx0     ! nx
      kgds( 3)= ny0     ! ny
      kgds( 4)= nint(alat0new*1000.)         ! lat(1,1)*1000
      kgds( 5)= nint(alon0new*1000.)  ! lon(1,1)*1000
      kgds( 7)= alonv*1000.           ! LOV grid orientation
! Grid spacing is 4762.5m.  The RFC's had the grid spacing as '4762'.  
! '4763' is really closer, so we round it up.
      kgds( 8)= nint(dx)
      kgds( 9)= nint(dx)
!
      call baopen(51,'fort.51',ibaret)
      call putgb(51,nx0*ny0,kpds,kgds,bit0,p0,iret)
      write(6,*) 'write mosaicked ST4 file'
      write(6,*) '  iret(baopen)=', ibaret, ' iret(putgb)=', iret
!
      call baclose(51,ibaret)
      CALL W3TAGE('MOSAIC ')
      stop
      end

