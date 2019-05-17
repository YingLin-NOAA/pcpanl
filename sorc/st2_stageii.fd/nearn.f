      SUBROUTINE NEARN(IH,JH,GAGVAL,CLOSE,XMIN,XMAX)
!
!   THIS SUBROUTINE SELECTS THE 9 NEAREST NEIGHBOR BINS
!     TO THE GAGE AND SELECTS THE RADAR VALUE CLOSEST TO THE GAGE
!     VALUE ALONG WITH THE MAXIMUM AND MINIMUM VALUES.
!
!   CALLING SUBROUTINE: SAVRG
!
      DIMENSION HRAP(9)
      DIMENSION RADAR(131,131)
      COMMON/RADCOM/RADAR
!
      IF(IH.EQ.1) GO TO 100
      IF(IH.EQ.131) GO TO 200
!
      IF(JH.EQ.1) GO TO 300
      IF(JH.EQ.131) GO TO 400
!
!   1<IH<131
!   1<JH<131
!
      NUMBIN=9
      HRAP(1)=RADAR(IH-1,JH)
      HRAP(2)=RADAR(IH+1,JH)
      HRAP(3)=RADAR(IH-1,JH+1)
      HRAP(4)=RADAR(IH,JH+1)
      HRAP(5)=RADAR(IH+1,JH+1)
      HRAP(6)=RADAR(IH-1,JH-1)
      HRAP(7)=RADAR(IH,JH-1)
      HRAP(8)=RADAR(IH+1,JH-1)
      HRAP(9)=RADAR(IH,JH)
      GO TO 450
!
  100 CONTINUE
      IF(JH.EQ.1) GO TO 120
      IF(JH.EQ.131) GO TO 140
!
!   IH=1
!   1<JH<131
!
      NUMBIN=6
      HRAP(1)=RADAR(2,JH)
      HRAP(2)=RADAR(1,JH+1)
      HRAP(3)=RADAR(2,JH+1)
      HRAP(4)=RADAR(1,JH-1)
      HRAP(5)=RADAR(2,JH-1)
      HRAP(6)=RADAR(1,JH)
      GO TO 450
!
!   IH=1
!   JH=1
!
  120 CONTINUE
      NUMBIN=6
      HRAP(1)=RADAR(2,1)
      HRAP(2)=RADAR(1,2)
      HRAP(3)=RADAR(2,2)
      HRAP(4)=RADAR(1,131)
      HRAP(5)=RADAR(2,131)
      HRAP(6)=RADAR(1,1)
      GO TO 450
!
!   IH=1
!   JH=131
!
  140 CONTINUE
      NUMBIN=6
      HRAP(1)=RADAR(2,131)
      HRAP(2)=RADAR(1,1)
      HRAP(3)=RADAR(2,1)
      HRAP(4)=RADAR(1,131-1)
      HRAP(5)=RADAR(2,131-1)
      HRAP(6)=RADAR(1,131)
      GO TO 450
!
  200 CONTINUE
      IF(JH.EQ.1) GO TO 220
      IF(JH.EQ.131) GO TO 240
!
!   IH=131
!   1<JH<131
!
      NUMBIN=6
      HRAP(1)=RADAR(131-1,JH)
      HRAP(2)=RADAR(131-1,JH+1)
      HRAP(3)=RADAR(131,JH+1)
      HRAP(4)=RADAR(131-1,JH-1)
      HRAP(5)=RADAR(131,JH-1)
      HRAP(6)=RADAR(131,JH)
      GO TO 450
!
!   IH=131
!   JH=1
!
  220 CONTINUE
      NUMBIN=6
      HRAP(1)=RADAR(131-1,1)
      HRAP(2)=RADAR(131-1,2)
      HRAP(3)=RADAR(131,2)
      HRAP(4)=RADAR(131-1,131)
      HRAP(5)=RADAR(131,131)
      HRAP(6)=RADAR(131,1)
      GO TO 450
!
!   IH=131
!   JH=131
!
  240 CONTINUE
      NUMBIN=6
      HRAP(1)=RADAR(131-1,131)
      HRAP(2)=RADAR(131-1,1)
      HRAP(3)=RADAR(131,1)
      HRAP(4)=RADAR(131-1,131-1)
      HRAP(5)=RADAR(131,131-1)
      HRAP(6)=RADAR(131,131)
      GO TO 450
!
!   1<IH<131
!   JH=1
!
  300 CONTINUE
      NUMBIN=9
      HRAP(1)=RADAR(IH-1,1)
      HRAP(2)=RADAR(IH+1,1)
      HRAP(3)=RADAR(IH-1,2)
      HRAP(4)=RADAR(IH,2)
      HRAP(5)=RADAR(IH+1,2)
      HRAP(6)=RADAR(IH-1,131)
      HRAP(7)=RADAR(IH,131)
      HRAP(8)=RADAR(IH+1,131)
      HRAP(9)=RADAR(IH,1)
      GO TO 450
!
!   1<IH<131
!   JH=131
!
  400 CONTINUE
      NUMBIN=9
      HRAP(1)=RADAR(IH-1,131)
      HRAP(2)=RADAR(IH+1,131)
      HRAP(3)=RADAR(IH-1,1)
      HRAP(4)=RADAR(IH,1)
      HRAP(5)=RADAR(IH+1,1)
      HRAP(6)=RADAR(IH-1,131-1)
      HRAP(7)=RADAR(IH,131-1)
      HRAP(8)=RADAR(IH+1,131-1)
      HRAP(9)=RADAR(IH,131)
!
!   FIND CLOSEST, MINIMUM AND MAXIMUM
!
  450 CONTINUE
      DIFMIN=9999.
      XMX=-9999.
      XMN=9999.
!
      DO 500 I=1,NUMBIN
      IF(HRAP(I).GT.XMX) XMX=HRAP(I)
      IF(HRAP(I).LT.XMN) XMN=HRAP(I)
!
      DIF=ABS(HRAP(I)-GAGVAL)
      IF(DIF.LT.DIFMIN) THEN
         INDEX=I
         DIFMIN=DIF
      END IF
  500 CONTINUE
!
      CLOSE=HRAP(INDEX)
      XMIN=XMN
      XMAX=XMX
      RETURN
      END
