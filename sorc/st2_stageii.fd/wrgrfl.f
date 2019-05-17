      SUBROUTINE WRGRFL(IHOUR,DATE,NGAGE,xmhat,phat,bpps)
!
!   THIS SUBROUTINE WRITES THE GAGE AND RADAR VALUES TO A
!     FILE FOR USE BY THE BIAS REMOVAL.
!   CALLING SUBROUTINE: MAIN PROGRAM
!
      CHARACTER DATE*10,outdir*180
!
      INCLUDE 'luprod'
      INCLUDE 'ppinp'
      INCLUDE 'ppout'
      INCLUDE 'hrg'
      INCLUDE 'mhg'
      INCLUDE 'ragg'
      INCLUDE 'gagloc'
      INCLUDE 'near9'
!
!   GET GAGE/RADAR TABLE PATHNAME
!
         OUTDIR = ' '
         CALL GETENV('ST2_BIAS',OUTDIR)
         LENDIR = INDEX(OUTDIR,' ') -1
         IF (LENDIR.LE.0) LENDIR = LEN(OUTDIR)
         FILGRT=outdir(1:lendir)//'/'//RADID//DATE//'.grt'
      OPEN(LUGRTB,FILE=FILGRT)
      WRITE(LUGRTB,1) xmhat,phat,bpps
!
!   WRITE GAGE/RADAR VALUES FOR USE IN BIAS REMOVAL
!
      DO 100 I=1,NGAGE
      RAD1=RADGAG(IHOUR,I)
!     IF(RAD1.GT.0.) RAD1=RAD1*BS
!
      IF(HR1GAG(IHOUR,I).NE.-99.0.AND.RAD1.GT.-99.)                           &
        WRITE(LUGRTB,2) GAGEID(I),                                            &
         IHLOC(I),JHLOC(I),HR1GAG(IHOUR,I),RAD1,CLOSE9(I),XMIN9(I),           &
         XMAX9(I)
  100 CONTINUE
      CLOSE(LUGRTB)
      RETURN
    1 FORMAT(3F18.8)
    2 FORMAT(' ',A8,1X,I3,1X,I3,5(1X,F8.2))
      END
