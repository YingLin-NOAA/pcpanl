PROGRAM ST4_MRMS_MERGE
!
! PURPOSE: fill in the CNRFC and NWRFC areas in Stage IV hourly with MRMS.  
! 
! INPUT FILES:
!   Unit 11  Stage 4 analysis
!   Unit 12  MRMS mapped to HRAP
!   Unit 13  River Forecast Center (RFC) domain mask
!
! OUTPUT FILE:
!   Unit 51  Stage IV hourly with CNRFC/NWRFC areas filled with MRMS
! 
! SUMMARY:
!   Merge the Stage 4 hourly from RFCs with MRMS in CNRFC/NWRFC domains.  
!     If MRMS value is less than zero, set it to "missing".
!
! RECORD OF REVISIONS
!    Date     Programmer  Description of Change
! ==========  ==========  ====================================================
! 2017-02-06  Ying Lin    Started from 
!                         /nwprod/nam.v3.1.22/sorc/nam_merge2n4.fd/merge2n4.f
! LANGUAGE: Fortran 90/95
! 
IMPLICIT NONE

! Dimension of Stage 2/4 analysis files (on the 4km HRAP grid):
INTEGER, PARAMETER :: nx=1121, ny=881

! Stage 2/4 precipitation files and their bit masks; merged file and bit 
! mask; RFC domains mask (real array) and its bitmask (bitr)
REAL,    DIMENSION(nx,ny) :: p4, pmrms, pmerge, rfcmask
LOGICAL(KIND=1), DIMENSION(nx,ny) :: bit4, bitmrms, bitmerge

! RFC mask file, converted to integer (just a scalar variable):
INTEGER :: imask

! GDS and PDS for 1) getgb ('j')
!                 2) Stage 2 ('4')
!                 3) Stage 4 ('mrms')
!                 4) Stage 2/4 merged ('merge')
!                 5) RFC mask ('mask')

INTEGER, DIMENSION(200) :: jgds, kgds4, kgdsmrms, kgdsmerge, kgdsmask
INTEGER, DIMENSION(200) :: jpds, kpds4, kpdsmrms, kpdsmerge, kpdsmask

! For do loop index:
INTEGER :: i, j

! Misc for baopenr and getgb:
INTEGER :: kf, k, iret, iret4, iretmrms

! For 'getgb' searches:
    jpds = -1
 
! Read in Stage IV:
    call baopenr(11,'fort.11',iret) 
    write(*,*) 'baopenr on unit 11, iret=', iret
    call getgb(11,0,nx*ny,0,jpds,jgds,kf,k,kpds4,kgds4,bit4,p4,iret4)
    write(*,10) 'ST4', kpds4(21)-1,kpds4(8),kpds4(9),kpds4(10),kpds4(11),iret4
 10 format('GETGB for ',a3,x, 5i2.2, ' iret=', i2)

! Read in MRMS:
    call baopenr(12,'fort.12',iret) 
    write(*,*) 'baopenr on unit 12, iret=', iret
    call getgb(12,0,nx*ny,0,jpds,jgds,kf,k,kpdsmrms,kgdsmrms,bitmrms,         &
   &           pmrms,iretmrms)
    write(*,10) 'mrms', kpdsmrms(21)-1,kpdsmrms(8),kpdsmrms(9),kpdsmrms(10),  &
   &           kpdsmrms(11),iretmrms

! Read in RFC mask (real array), borrow the bitmap, kpds, kgds from ST2:
    call baopenr(13,'fort.13',iret) 
    write(*,*) 'baopenr on unit 13 for RFC mask, iret=', iret
    call getgb(13,0,nx*ny,0,jpds,jgds,kf,k,kpdsmask,kgdsmask,bitmask,rfcmask,  &
   &           iret)
    write(*,*) 'getgb for RFC mask, iret=', iret

! Now merge Stage IV/MRMS: 
    IF (iret4 == 0 .AND. iretmrms == 0) THEN   ! both analyses exist
      kgdsmerge=kgds4
      kpdsmerge=kpds4

      DO j = 1, ny
      DO i = 1, nx
        imask = INT(rfcmask(i,j))

!       Use Stage IV if all the following criteria are met:
!         1) Stage IV mask indicate that it has valid data
!         2) If this point is outside of any of the RFC's domain proper (i.e. 
!            in Canada or over water), it's precip value is not zero
!         3) This point is not in CNRFC or NWRFC

        IF ( imask /= 153 .AND. imask /= 159 ) THEN
          pmerge(i,j)=p4(i,j)
          bitmerge(i,j)=bit4(i,j)
        ELSE
          IF ( pmrms(i,j) < 0. ) THEN
            bitmerge(i,j)=.FALSE.
          ELSE
            pmerge(i,j)=pmrms(i,j)
            bitmerge(i,j)=bitmrms(i,j)
          ENDIF
        ENDIF
      END DO
      END DO
    END IF 

! Output the merged Stage 2/4 file:
    call baopen(51,'fort.51',iret) 
    write(*,*) 'baopen for merged Stage 4/MRMS, iret=', iret
    call putgb(51,nx*ny,kpdsmerge,kgdsmerge,bitmerge,pmerge,iret)
    write(*,*) 'PUTGB for merged Stage 4/MRMS, iret=', iret
!
STOP
END PROGRAM ST4_MRMS_MERGE
