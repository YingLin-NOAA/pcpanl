      subroutine savrg(ihour,ngage,ihloc,jhloc,iflign)
!
!     This subroutine CALCULATES THE closest, minimum and
!     maximum of the 9 nearest neighbor radar bin values
!     TO THE GAGE VALUE.  It saves the closest in value of
!     the 9 nearest neighbors in the radgag array for use by the
!     bias and time distribution calculations.
!   If the gage is outside of a circle of radius 65 HRAP bins, then
!     radgag,xmax,xmin,close are set to -99. and the nearest neighbor
!     bin selection process is not done.  A radius of 65 is used instead
!     of 66 so as not to select a bin outside of a radius of 66 bins
!     where the radar field is undefined.
!   calling subroutine: main
!   subroutines called: nearn
!
      dimension radar(131,131)
      dimension ihloc(1000),jhloc(1000)
!
      common/radcom/radar
      include 'ragg'
      include 'ppinp'
      include 'mis'
      include 'hrg'
      include 'near9'
!
      do 100 i=1,ngage
      ih=ihloc(i)
      jh=jhloc(i)
!
      if(ifinrd.gt.0.and.iflign.eq.0) then
         if(misbin(ih,jh).eq.0) then
            radgag(ihour,i)=-999.
            close9(i)=-999.0
            xmin9(i)=-999.
            xmax9(i)=-999.
         else if(radar(ih,jh).ge.0.0) then
	    radgag(ihour,i)=-99.0
	    xmin9(i)=-99.
	    xmax9(i)=-99.
	    close9(i)=-99.
!
	    if(((ih-65)*(ih-65)+(jh-65)*(jh-65)).lt.4225) then
	       call nearn(ih,jh,hr1gag(ihour,i),close9(i),xmin9(i),           &
                  xmax9(i))
	       radgag(ihour,i)=close9(i)
	    end if
         end if
      else
         radgag(ihour,i)=-99.0
         close9(i)=-99.0
         xmin9(i)=-99.
         xmax9(i)=-99.
      end if
  100 continue
      return
      end
