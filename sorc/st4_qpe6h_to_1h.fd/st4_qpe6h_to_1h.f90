     PROGRAM ST4_QPE6H_TO_1H
!
!  Read in an 6h QPE (in GRIB1, from NWRFC or CNRFC) and the binary file 
!  containing hourly weights derived from hour MRMS files for these six hours.
!
!  Jun 2018: 
!    Note that in pcpanl.v3.0.0, hourly weights were written out in GRIB2 using
!    wgrib2api in st4_mrms_hrlywgts.fd.  There is a memory leak problem with
!    that api - no leak in the 'read' function using grb2_inq, but there is 
!    no way (for a user) to make grb2_wrt leak-proof.  So starting in 
!    pcpanl.v3.1.0, the weights are written out/reading in as binary files.
!
!    About fort.5 input: In pcpanl.v3.0.0 the date info for each hourly QPE
!      was read from the "description" session in the in the GRIB2 hourly 
!      weights, which in turn was inherited from the MRMS description session 
!      for that hour in st4_mrms_hrlywgts - easy peasy.  Unfortunately to 
!      avoid memory leak in outputting hrlywgts, in v3.1.0 the weights written
!      out (in st4_mrms_hrlywgts) and read in (here) as binary files instead,
!      so we lost that handy hourly date info in the header.  The RFC 6h QPEs'
!      date info follows the odd convention of using the previous 12Z as
!      the reference hour, so e.g. QPE.153.2018062912.06h is the "18-24hr acc"
!      from reference time of "2018062812" - very tiresome.  So we're reading
!      in the real valid hour (in the case of this example, 2018061812) from
!      unit 5. 
!
!  Input:
!    fort.5:  valid time of the 6h QPE.  
!    fort.11: 6h QPE from either NWRFC or CNRFC (GRIB1)
!    fort.12: weights from MRMS for these 6 hours (binary)
!  Output:
!    fort.51-56: synthetic 1h QPE for the RFC for these 6 hours (GRIB1)
!
!  lmax is the max nx*ny for either NWRFC or CNRFC.  They are not likely to 
!  increase by much.  
!    As of June 2018, NWRFC: 302x357=107,814
!                     CNRFC: 370x388=143,560
!    
     parameter(lmax=300000, itest=156, jtest=318)
     real tmpqpe6h(lmax)
     logical(kind=1) tmpbitmap(lmax)
     real, allocatable :: qpe6h(:,:), qpe1h(:,:), wgts(:,:)
     integer (kind=8) :: date
     character (len=7) :: fortdxx
!
!  For GRIB1 input QPE6h and output QPE1h:
!    
     integer jpds(200), jgds(200), kgds(200), kpds(200)
     logical(kind=1), allocatable :: bitmap(:,:)
!
!  idat6: valid time of the 6h QPE, read in from unit 5
!  idat1: reference time of the 1h QPE, derived from idat6 using movdat (it's
!         the starting (not ending) time of the 1h accumulation)
     integer         idat6(8), idat1(8)
     real            rinc(5)  
!  idat needs to be initialized to zero, otherwise idat(4) and idat(6) would 
!  contain a strange value of '32765', and movdat yields an increment of
!  ~23 days.
     idat6=0        
     idat1=0
     read(5,10) idat6(1),idat6(2),idat6(3),idat6(5)
 10  format(i4,3i2)

     rinc=0.
     rinc(2)=-6.
!
     fortdxx(1:5)='fort.'
!

!
!  Read in the QPE6h file, find the nx,ny for this run (it's either that
!    of NWRFC or CNRFC) from the input 6h QPE:
     jpds = -1
     call baopenr(11,'fort.11',ibaret)
! getgb to an unallocated qpe6h and bitmap does not seem to work.  So read
! them into temp arrays, then copy the values to actual arrays after 
! allocation.
!    call getgb(11,0,lmax,0,jpds,jgds,kf,k,kpds,kgds,bitmap,qpe6h,iret)
     call getgb(11,0,lmax,0,jpds,jgds,kf,k,kpds,kgds,tmpbitmap,tmpqpe6h,iret)
     write(*,*) 'Reading input QPE6H.  ibaret, iret=', ibaret, iret
!    write(*,*) 'qpe6h at point ', itest, jtest, '=', qpe6h(itest,jtest)
!
     nx=kgds(2)
     ny=kgds(3)
!
     ALLOCATE(qpe6h(nx,ny), STAT=istat)
     ALLOCATE(qpe1h(nx,ny), STAT=istat)
     ALLOCATE(wgts(nx,ny), STAT=istat)
     ALLOCATE(bitmap(nx,ny), STAT=istat)
!
     do j = 1, ny
     do i = 1, nx
       l = (j-1)*nx + i
       qpe6h(i,j) = tmpqpe6h(l)
       bitmap(i,j) = tmpbitmap(l)
     enddo
     enddo
!
! Now that wgts array has been allocated, go through each hour in the 6h
! period, read in the binary weights, and use the weights to disaggregate
! the 6h QPE into hourly QPE:

     do ihr = 1, 6
       read(12) wgts
! 
!      use movdat to find the valid time for ihr's QPE
!      This is what rinc(2) for movdat should be:
!      ln -sf QPE.${rid}.$v1datem5.01h     fort.51    rinc(2)=-6
!      ln -sf QPE.${rid}.$v1datem4.01h     fort.52    rinc(2)=-5
!      ln -sf QPE.${rid}.$v1datem3.01h     fort.53    rinc(2)=-4
!      ln -sf QPE.${rid}.$v1datem2.01h     fort.54    rinc(2)=-3
!      ln -sf QPE.${rid}.$v1datem1.01h     fort.55    rinc(2)=-2
!      ln -sf QPE.${rid}.$v6date.01h       fort.56    rinc(2)=-1
!
       rinc(2)=ihr-7
       call w3movdat(rinc,idat6,idat1)
!
! Dis-aggregate the 6h QPE into hourly amounts using the weights, if wgts is
! between 0 and 1, otherwise just divide by 6. 
       do j = 1, ny
       do i = 1, nx
         if (bitmap(i,j)) then
           if (qpe6h(i,j) .gt. 0.) then
             if (wgts(i,j).ge. 0. .and. wgts(i,j).le. 1.) then
               qpe1h(i,j) = qpe6h(i,j)*wgts(i,j)
             else
               qpe1h(i,j) = qpe6h(i,j)/6.
             endif
           else
             qpe1h(i,j) = 0.
           endif
         endif
         if (i==itest .and. j==jtest) then
           write(*,*) 'wgt, qpe1h=', wgts(i,j), qpe1h(i,j)
         endif
       enddo
       enddo
!
! Output the 1h QPE file.  
!
!      ----:----|----:----|----:----|----:----|----:----|----:----|----:
    
!          (8)            - Year of century (2000=100, 2001=1)
       kpds(8)  = mod(idat1(1),100)
       if (kpds(8).eq.0) kpds(8) = 100
!          (21)           - Century of reference time of data
       kpds(21)=idat1(1)/100 + 1 - kpds(8)/100
       kpds(9) =idat1(2)  ! Month of year
       kpds(10)=idat1(3)  ! Day of month
       kpds(11)=idat1(5)  ! Hour of day
       kpds(12)=0         ! Minute of hour
       kpds(13)=1         ! Indicator of forecast time unit
       kpds(14)=0         ! Time range 1
       kpds(15)=1         ! Time range 2
       kpds(16)=4         ! Time range flag
       kpds(22)=2         ! Decimal scale factor:  pack to 0.01mm precision
!     
       write(fortdxx(6:7),'(i2.0)') 50+ihr
       call baopen(50+ihr,fortdxx,ierrba)
       call putgb(50+ihr,nx*ny,kpds,kgds,bitmap,qpe1h,ierrput)
       write(*,20) idat1(1),idat1(2),idat1(3),idat1(5), ierrba, ierrput
  20   format('Output 1h QPE for hour start ', i4, 3(i2.2),                               &
    &         ' ierr for baopen, putgb',2i4)
     enddo ! looped through hours 1-6
!
     DEALLOCATE(qpe6h,  STAT=istat)
     DEALLOCATE(qpe1h,  STAT=istat)
     DEALLOCATE(wgts,   STAT=istat)
     DEALLOCATE(bitmap, STAT=istat)
!
     stop
     end
