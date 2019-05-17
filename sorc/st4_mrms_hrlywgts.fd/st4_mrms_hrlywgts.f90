     PROGRAM ST4_MRMS_HRLYWGTS
!
!  Read in six consecutive hourly MRMS precip files, compute weights
!  (e.g. for hour 1, wgt1=pcp1/pcp6)
!
     use wgrib2api

     parameter(itest=156, jtest=318)
     real, allocatable :: grid(:,:), mrms(:,:,:), sum6(:,:), wgts(:,:)

     character (len=200) :: inv, desc(6), metadata
     character (len=7) :: fortdxx, template
!
     inv = '@mem:0'

     fortdxx(1:5)='fort.'

     sum6 = 0.
     do ihr = 1, 6
       write(*,*) 'ihr=', ihr
       write(fortdxx(6:7),'(i2.0)') 10+ihr
!      make inv file, save in memory file #0
       iret = grb2_mk_inv(fortdxx, inv)
       if (iret.ne.0) stop 1

       iret = grb2_inq(fortdxx,inv,nx=nx,ny=ny,data2=grid,desc=desc(ihr),regex=1)
       write(*,*) 'desc=', desc(ihr)
       if (iret.ne.1) then
         if (iret.eq.0) write(*,*) 'could not find message'
         if (iret.gt.1) write(*,*) 'found multiple messages ', iret
         stop 2
       endif
!
! Allocate arrays if this is the first call:
       if (ihr ==1) then
         ALLOCATE(grid(nx,ny), STAT=istat)
         ALLOCATE(mrms(nx,ny,6), STAT=istat)
         ALLOCATE(sum6(nx,ny), STAT=istat)
         ALLOCATE(wgts(nx,ny), STAT=istat)
         template=fortdxx 
       endif

       do j = 1, ny
       do i = 1, nx
         mrms(i,j,ihr)=grid(i,j)
         if (grid(i,j) >= 0. .and. grid(i,j) <= 500.) then
           sum6(i,j) = sum6(i,j)+grid(i,j)
         else
           sum6(i,j) = -999.
         endif
       enddo
       enddo
!       write(*,*) '  at itest,jtest, mrms1h=', grid(itest,jtest)
     enddo ! ihr = 1, 6
!     write(*,*) '  at itest,jtest, mrms6h=', sum6(itest,jtest)
!
     do ihr = 1, 6
       write(*,*) 'ihr=',  ihr
       wgts = -999.
       do j = 1, ny
       do i = 1, nx
         if (sum6(i,j) > 0.) then
           wgts(i,j)=mrms(i,j,ihr)/sum6(i,j)
         endif
       enddo
       enddo
       write(*,*) '  at itest,jtest, wgts=', wgts(itest,jtest)
! test: set(1,1) point to make sure I'm reading them back correctly.
!       wgts(1,1)=ihr
!
! desc(ihr) D=20170216130000:GaugeCorrQPE01H:0 m above mean sea level:anl:
!           ----:----|----:----|----:----|----:----|----:----|----:----|----:
! metadata  D=20170216130000:wgts:1000mb:anl:
! 
! 'encode' and 'grib_max_bits=' are from Wesley's example (14 Feb 2017 seminar,
! slide on grib2_write(..):precision).  Without setting these two, weights of
! 0.1462402 to 0.2246094 all rounded up to '0.25'.  Not sure of encode and
! grib_max_bits could be lower.  
!
! 2018/6/19: grb2_wrt causes memory leak, even with fixed-length arrays.  Write
!   them out in binary instead.  
!   
!       metadata=desc(ihr)(1:16)//':TMP:1000mb:anl:encode 22bits:grib_max_bits=25:'
!       write(*,*) 'metadata=', metadata
       write(51) wgts
!       iret = grb2_wrt('fort.51',template,1,data2=wgts,meta=metadata)
!       write(*,*) 'ihr, wrt_iret=', ihr, iret
     enddo

! Deallocate the arrays:
     DEALLOCATE(grid, STAT=istat)
     DEALLOCATE(mrms, STAT=istat)
     DEALLOCATE(sum6, STAT=istat)
     DEALLOCATE(wgts, STAT=istat)
     stop
     end

                
