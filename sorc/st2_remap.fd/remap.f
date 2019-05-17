      program remap
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! MAIN PROGRAM: REMAP      program to map the 4km precip analysis to the
!                          15km grid.
!
!   Programmer: Ying lin           ORG: NP22        Date: 2001-06-01  
!
! ABSTRACT:
!
!  1/21/00  MEB
!    program to read in 4km precip file, remap it to
!    3x3 boxes (~15km grid) and pack into grib
!    (for stage2/4)
!
! PROGRAM HISTORY LOG:
!  2001/01/23 YL Changed to official national HRAP grid
!  2012/08/01 YL modified for WCOSS
!  
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
      PARAMETER (IMR=1121,JMR=881,IJOUT=IMR*JMR)
      PARAMETER (IM3=IMR/3,JM3=JMR/3,IJOUT3=IM3*JM3)
      integer jpds(200),jgds(200),kpds(200),kgds(200)
      integer kpdso(200),kgdso(200)
      logical*1 imiss(imr,jmr),lisum(im3,jm3),li(ijout),aok
      real pcp4(ijout)
      REAL PREC(IMR,JMR),PREC15(IM3,JM3)
      character*16 datstr
!
      CALL W3TAGB('REMAP ',2001,0152,0060,'NP22   ')
!
!  set up grid
!
       alat1=23.117
       alon1=360.-119.023
       dx=4762.5
       alonv=255.
       dx3=dx*3.
       call W3FB07(2.,2.,ALAT1,ALON1,DX,ALONV,ALAT3,ALON3)
       ilat1=nint(alat3*1000.)
       ilon1=nint(alon3*1000.)
       idx=nint(dx3)
       ilonv=nint(alonv*1000.)
!
!  dx3 is the grid spacing for the 15km grid
!  alat3 is the lower left corner lat for the 15km grid
!  alon3 is the lower left corner lon for the 15km grid
!
        lisum=.false.
        prec15=0.
!
        jpds=-1
        jgds=-1
!
!   get precip
!
        jpds(5)=61
        call baopenr(11,'fort.11',ibaoerr)
        call getgb(11,0,ijout,0,jpds,jgds,kf,kr,kpds,kgds,li,pcp4,iret)
        call baclose(11,ibacerr)
        write(6,*) 'REMAP read from unit 11, iba,igetgb=', ibaoerr,iret

        if (ibaoerr.eq.0 .and. iret.eq.0) then
!  sum up precip and get missing mask
         IM=KGDS(2)
         JM=KGDS(3)
         k=0
         rmax=0.
         rmin=0.
         DO J=1,JM
          DO I=1,IM
           k=k+1
           imiss(i,j)=li(k)
           prec(i,j)=pcp4(k)
           if (prec(i,j).gt.rmax) rmax=prec(i,j)
           if (prec(i,j).lt.rmin) rmin=prec(i,j)
          ENDDO
         ENDDO
!  average 3x3 boxes together
         rmax=0.
         rmin=0.
         jjj=0
         DO J=2,JM-1,3
          jjj=jjj+1
          iii=0
          DO I=2,IM-1,3
           iii=iii+1
           tots=0.
           summ=0.
           do ii=1,3
            do jj=1,3
             if (imiss(i+ii-2,j+jj-2)) then
               tots=tots+1.0
               summ=summ+prec(i+ii-2,j+jj-2)
             endif
            enddo 
           enddo 
           if (tots.gt.0.0) then
             prec15(iii,jjj)=summ/tots
             lisum(iii,jjj)=.true.
             if (prec15(iii,jjj).gt.rmax) rmax=prec15(iii,jjj)
             if (prec15(iii,jjj).lt.rmin) rmin=prec15(iii,jjj)
           endif
          ENDDO
         ENDDO
!
!  set up kpdso, write it out
         kpdso=kpds
         KPDSO(3)=241
!
!  set up kgdso
!
         KGDSO=0
!   DATA REP TYPE
         KGDSO( 1)=5
!   NO PTS X DIR
         KGDSO( 2)=III
!   NO PTS Y DIR
         KGDSO( 3)=JJJ
!   LAT OF 1ST POINT IN MILLIDEG
         KGDSO( 4)=ILAT1
!   LON OF 1ST POINT IN MILLIDEG
         KGDSO( 5)=ILON1
!   RES AND COMP FLAG
         KGDSO( 6)=8
!   ORIENT OF GRID IN MILLIDEG
         KGDSO( 7)=ILONV
!   X DIR GRID LENGTH IN METERS
         KGDSO( 8)=IDX
!   Y DIR GRID LENGTH IN METERS
         KGDSO( 9)=IDX
!   SCANNING MODE
         KGDSO(11)=64
!
         KGDSO(20)=255

         kfout=iii*jjj
         CALL BAOPEN(51,'fort.51',ibaoerr)
         call putgb(51,kfout,kpdso,kgdso,lisum,prec15,iret)
         CALL BACLOSE(51,ibacerr)
         write(6,*) 'REMAP write to unit 51, iba,igetgb=', ibaoerr,iret
        endif
      CALL W3TAGE('REMAP ')
      stop
      end
