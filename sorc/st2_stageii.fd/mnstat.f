      SUBROUTINE MNSTAT(XMHAT,VHAT,PHAT,RHAT,XMXR,XMGAG,XMXG,                 &
                        NGAGHR,IHOUR,iflign)
!
!   SUMMARY STATISTICS ALGORITHM MAIN PROGRAM
!   CALLING SUBROUTINE: MAIN PROGRAM
!   SUBROUTINES CALLED: STATRD,STATGG,post_sst
!
!   XMHAT = MEAN RADAR PRECIP VALUE
!   VHAT = VARIANCE IN ABOVE
!   PHAT = FRACTION OF RADAR AREA RECEIVING NO PRECIP
!   RHAT = LAG 1 SPATIAL CORRELATION OF RADAR PRECIP VALUES
!   XMXR = MAXIMUM RADAR PRECIP VALUE
!   XMGAG = MEAN OF GAGE OBSERVATIONS (HOURLY GAGES ONLY)
!   XMXG = MAXIMUM GAGE VALUE (HOURLY GAGES ONLY)
!
      integer*2 ih2
      integer*4 irc
!
      INCLUDE 'ppinp'
!
!   CALCULATE RADAR RELATED STATISTICS
!   WRITE XMHAT,PHAT TO DATABASE
!
      CALL STATRD(XMHAT,VHAT,PHAT,RHAT,XMXR)
!
!     IF(IFINRD.GT.0) then
!        if(iflign.eq.0) then
!    ih2=ihour
!
!    call post_sst(radid,idat,eih2,xmhat,phat,irc)
!    if(irc.ne.0.and.irc.ne.-256.and.irc.ne.-239)
!    *         write(6,1) irc
!        else
!           write(6,2)
!        end if
!     end if
!
!   CALCULATE GAGE RELATED STATISTICS
!
      CALL STATGG(IHOUR,XMGAG,XMXG,NGAGHR)
!
      RETURN
    1 format(/,2x,'*** Informix error#=',i6,' from post_sst ***')
    2 format(/,2x,'*** summary statistics not written to',                    &
         1x,'database due to ignore radar ***')
      END
