      SUBROUTINE WRINPT(DATE)
!
!   THIS SUBROUTINE PRINTS THE INPUT SUMMARY
!   DATE = YYYYMMDDHH OF DATA BEING PROCESSED (= CURRENT HOUR - 1)
!   CALLING SUBROUTINE: MAIN PROGRAM
!
      CHARACTER DATE*10
!
      include 'raddat'
      include 'ppinp'
      INCLUDE 'ppout'
      INCLUDE 'cnst'
      INCLUDE 'ggoparam'
      INCLUDE 'mltparam'
      INCLUDE 'cnqu'
      INCLUDE 'prdflg'
      include 'flags'
!
      WRITE(6,22) DATE
!
      WRITE(6,8) RADID
      if(ifltyp.eq.1) then
         WRITE(6,12) rainmingg,distmingg,cor0pi,cor0i,rangei,cor0pc,          &
            cor0c,rangec,radius,itygg,nbrsgg
      else
         write(6,9) IDMAX,IWIND,GRMIN,IDRMIN
      end if
!
      WRITE(6,13) TMIN,RMAX
      WRITE(6,14) GMIN,VMIN,B1,B2,B3,B4,SC,KMAX,errmax
      if(ifltyp.eq.1) WRITE(6,18) rainminml,distminml,crscori,                &
         crscorc,scali_def,scale_def,scali_max,scale_max,                     &
         threshmin,ityml,nbrsml
      WRITE(6,16) THR1,THR2,THR3,RCLI
      WRITE(6,17) RLAT,RLON,AZH,AZHMM
!
!   PRINT REQUESTED options
!
      write(6,61)
      IF(IFAPR.EQ.0) THEN
         WRITE(6,62)
      ELSE
         WRITE(6,68)
      END IF
      IF(ifltd.eq.1) WRITE(6,63)
!
      if(ifltyp.eq.1) then
         if(ifvar.eq.0) then
            WRITE(6,66)
            WRITE(6,67)
         else
            WRITE(6,69)
            WRITE(6,71)
         end if
      else
         WRITE(6,72)
         WRITE(6,73)
      end if
      RETURN
    1 FORMAT(/,2X,'server name = ',A30,5x,'database name=',a30)
    8 FORMAT(/,2X,'RADAR IDENTIFIER = ',A3)
    9 FORMAT(/,2X,'IDMAX=',I3,2X,'IWIND=',I4,2X,'GRMIN=',F4.2,2X,             &
         'IDRMIN=',I2)
   12 FORMAT(/,2X,'rainmingg=',f5.3,2X,'distmingg=',f5.2,/,2x,'cor0pi=',      &
         F4.2,2x,'cor0i=',f5.3,2X,'rangei=',f5.1,2X,'cor0pc=',F4.2,2X,        &
         'cor0c=',f4.2,2X,'rangec=',f5.1,/,2x,                                &
         'radius=',f5.1,2X,                                                   &
         'itygg=',i1,2X,'nbrsgg=',i2)                                       
   13 FORMAT(/,2X,'TMIN=',F9.2,2X,'RMAX=',F9.2)                             
   14 FORMAT(/,2X,'GMIN=',F7.2,2X,'VMIN=',F7.2,2X,'B1=',F7.2,2X,              &
         'B2=',F7.2,2X,'B3=',F7.2,2X,'B4=',F7.2,2X,                           &
         'SC=',F7.2,2X,'KMAX=',I2,2x,                                         &
         'errmax=',F4.2)
   16 FORMAT(/,2X,'THR1=',F6.2,3X,'THR2=',F6.2,3X,'THR3=',F6.2,3X,            &
         'RCLI=',F5.2)
   17 FORMAT(/,2X,'RLAT=',F5.2,2X,'RLON=',F7.2,2X,'AZH=',F5.1,                &
         2X,'AZHMM=',F5.3)
   18 FORMAT(/,2X,'rainminml=',f5.2,2X,'distminml=',f5.2,2X,                  &
         'crscori=',f5.3,2X,'crscorc=',f5.3,2X,'scali_def=',F5.1,/,2x,        &
         'scale_def=',f5.1,2X,                                                &
         'scali_max=',f5.1,2X,'scale_max=',f5.1,2X,                           &
         'threshmin=',f5.2,/,2x,                                              &
         'ityml=',i1,2X,'nbrsml=',i2)
   21 FORMAT('1',15X,'STAGE II PROCESSING -- ',6(I2,1X))
   22 FORMAT(/,2X,'Ending DATE/HOUR=',A10)
   23 FORMAT(/,2X,'OUTPUT FILE FOR GAGE ONLY FIELD =',A40)
   24 FORMAT(/,2X,'OUTPUT FILE FOR MULTISENSOR FIELD =',A40)
   27 FORMAT(/,2X,'GAGE/RADAR TABLE FILE = ',A40)
   43 FORMAT(' ',F7.3,3X,F7.3,6X,A1)
   61 format(/,2x,'requested options:')
   62 FORMAT(' ',2X,'QUALITY CONTROL -- AP REMOVAL TEST OFF')
   63 FORMAT(' ',2X,'TIME DISTRIBUTION')
   66 FORMAT(' ',2X,'GAGEONLY -- new method, no variance')
   67 FORMAT(' ',2X,'MULTISENSOR -- new method, no variance')
   68 FORMAT(' ',2X,'QUALITY CONTROL -- AP REMOVAL TEST ON')
   69 FORMAT(' ',2X,'GAGEONLY -- new method, variance')
   71 FORMAT(' ',2X,'MULTISENSOR -- new method, variance')
   72 FORMAT(' ',2X,'GAGEONLY -- old method')
   73 FORMAT(' ',2X,'MULTISENSOR -- old method')
      END
