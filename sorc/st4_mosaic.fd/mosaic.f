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
! 2019/8/6 mosaic to GRIB2 output instead of GRIB1. 
!
! 2020/1/21: ConUS RFC QPEs all have nx/ny less than 500 (AK has the largest
!   domain of (460,530), but that's not part of the ConUS mosaic program). 
!   When reading in each ConUS RFC QPE, check to make sure neither nx nor ny
!   in the input grib record exceeds 500, just in case.  
! 
! Input: 
!        Unit      5: yyyymmddhh   (vdate*10)
!        Unit     11: 'bytemap' - a mask on the HRAP grid that shows which 
!                     RFC each grid point belongs to (150, 152,..., 160);
!                     '1' if in Gulf of Mexico or off the Atlantic Coast;
!                     '0' elsewhere. 
!        Unit     12: GRIB2 table for APCP
!
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
      character varnam*19, vdate*10
      integer iptable(13)
!
      CALL W3TAGB('MOSAIC ',2001,0151,0060,'NP22   ')
!
      read(5,'(a10)') vdate
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
        nx = kgds(2)
        ny = kgds(3)
        if (nx.gt.500 .or. ny.gt.500) then
          write(6,*) 'Skip QPE, wrong dimension: id,nx,ny=',id,nx,ny
          go to 100
        endif
        irfc = irfc + 1
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
! This means we're getting the accumulation hour from the last GRIB file we're
! reading in (ordered by RFC ID)

      write(6,*) 'RFC QPE kpds(14) and kpds(15) = ', kpds(14),kpds(15)
      acc=kpds(15)-kpds(14)
!
!
!  Read in GRIB2 table to APCP:
      do k = 1, 42
        read(12,*)
      end do
      read (12,*) varnam, (iptable(i),i=1,13)
!
!
      call baopenw(51,'fort.51',ibaret)
      call grib2_wrt_g2func_hrap(nx0,ny0,alat0new,360.+alon0new,              &
                            p0,bit0,vdate,acc,iptable,51,iret)
      write(6,*) 'write mosaicked ST4 file'
      write(6,*) '  iret(baopenw)=', ibaret, ' iret(grib2_wrt)=', iret
!
      call baclose(51,ibaret)
      CALL W3TAGE('MOSAIC ')
      stop
      end

