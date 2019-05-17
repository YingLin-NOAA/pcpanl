      SUBROUTINE RDGRT(DATE,NGAGE,xmhat,phat,bpps,                            &
                       gag,rad,gid,ih1,jh1,ierr)
!
!   THIS SUBROUTINE READS THE GAGE AND RADAR VALUES FROM A
!     FILE FOR USE BY THE BIAS REMOVAL.
!   CALLING SUBROUTINE: BIAS
!
      CHARACTER*8 GID(300)
      DIMENSION GAG(300),RAD(300)
      DIMENSION IH1(300),JH1(300)
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
         NGAGE=0
         IERR=0
         xmhat=0.
         phat=0.
         gag=0.
         rad=0.
         OUTDIR = ' '
         CALL GETENV('ST2_BIAS',OUTDIR)
         LENDIR = INDEX(OUTDIR,' ') -1
         IF (LENDIR.LE.0) LENDIR = LEN(OUTDIR)
         FILGRT=outdir(1:lendir)//'/'//RADID//DATE//'.grt'
      OPEN(LUGRTB,FILE=FILGRT)
      READ(LUGRTB,1,END=900) xmhat,phat,bpps
!
!   WRITE GAGE/RADAR VALUES FOR USE IN BIAS REMOVAL
!   Normally there are far fewer than 300 gauges under each radar umbrella.
!   If there are more than 300 reports (e.g. when duplicate check fails),  
!   we stop the reading at 300.
!
      I=1
  100 IF(I.EQ.301)GOTO 999
      READ(LUGRTB,2,END=999) GID(I),IH1(I),JH1(I),GAG(I),RAD(I)
      I=I+1
      IERR=1
      GOTO 100
  999 NGAGE=I-1
      CLOSE(LUGRTB)
  900 RETURN
    1 FORMAT(3F18.8)
    2 FORMAT(1X,A8,1X,I3,1X,I3,2F9.2)
      END
