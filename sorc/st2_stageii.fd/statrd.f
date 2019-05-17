      SUBROUTINE STATRD(XMHAT,VHAT,PHAT,RHAT,XMXR)
!
!   THIS SUBROUTINE CALCLATES THE RADAR RELATED STATISTICS
!   CALLING SUBROUTINE: MNSTAT
!
      DIMENSION RADAR(131,131)
!
      COMMON/RADCOM/RADAR
      INCLUDE 'cnst'
      INCLUDE 'bins'
      INCLUDE 'mis'
      INCLUDE 'ppinp'
!
      IF(IFINRD.GT.0) THEN
         XM=0.0
         VH=0.0
         RR=0.0
         XMXR=0.0
         IPH=0
!
         DO 100 IBIN=1,NOK
         I=IOK(IBIN)
         J=JOK(IBIN)
!
         RAD=RADAR(I,J)*MISBIN(I,J)
         IF(RAD.GT.0.0) THEN
            XM=XM + RAD
            VH=VH + RAD*RAD
            IF(J.NE.131) RR=RR + RAD*RADAR(I,J+1)*MISBIN(I,J+1)
!
            IF(RAD.GT.XMXR) XMXR=RAD
            IPH=IPH+1
         END IF
  100    CONTINUE
!
         XMHAT=XM/NBIN
	 VHAT=(VH-(NBIN*XMHAT*XMHAT))/NBIN
         RR=RR/NBIN1
         PHAT=1.0 - (FLOAT(IPH)/NBIN)
!
         IF(VHAT.GT.0.0) THEN
            RHAT=(RR - (XMHAT*XMHAT))/VHAT
            IF(RHAT.LT.0.01) RHAT=0.01
!
            if(rhat.gt.1.0) then
               rhat=0.5
               write(6,4)
            end if
         ELSE
            RHAT=RCLI
         END IF
!
         WRITE(6,1) XMHAT,VHAT,RHAT,PHAT
         WRITE(6,2) XMXR
      ELSE
         XMHAT=0.0
         PHAT=9.0
         XMXR=0.0
      END IF
!
      RETURN
    1 FORMAT(/,5X,'MHAT=',F7.3,3X,'VHAT=',F7.3,3X,'RHAT=',F7.3,3X,            &
         'PHAT=',F7.3)
    2 FORMAT(/,5X,'MAX RADAR VALUE =',F5.1,1X,'MM')
    4 format(/,3x,'*** WARNING: rhat calculated as > 1.0 - reset to',         &
         1x,'0.5 ***')
      END
