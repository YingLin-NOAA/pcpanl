      SUBROUTINE BFBIAS(BFR,SFR,KFR,NGAG,BL)
!
!   THIS SUBROUTINE CALCULATES THE FORWARD OR BACKWARD LOG BIAS AND
!     VARIANCE AS AN INTERMEDIATE STEP FOR THE BIAS CALCULATION
!   CALLING SUBROUTINE: BIAS
!
!   BFR = FORWARD/BACKWARD LOG BIAS
!   SFR =     "     "      VARIANCE
!   KFR = NUMBER OF HOURS FORWARD/BACK
!   BL = SAMPLE LOG BIAS FOR EACH HOUR
!   NGAG = NUMBER OF GAGES REPORTING IN EACH HOUR
!
      DIMENSION NGAG(0:23),BL(0:23)
      INCLUDE 'mltparam'
!
      IF(KFR.GT.0) THEN
         H1=B1*B1
         H2=B2*(1.0-H1)
!
         DO 100 K=0,KFR
         IF(NGAG(K).GT.0.AND.BL(K).NE.-99.) THEN
            H=H1*SFR+H2
            TERM=H/((B3*NGAG(K)**B4)+H)
!
            BFR=B1*BFR+TERM*(BL(K)-B1*BFR)
            SFR=H*(1.0-TERM)
         ELSE
            BFR=B1*BFR
            SFR=H1*SFR+H2
         END IF
  100    CONTINUE
      END IF
      RETURN
      END
