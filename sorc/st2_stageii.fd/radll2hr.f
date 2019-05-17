      subroutine radll2hr
!
!   This subroutine converts the radar lat/lon to National HRAP
!     coordinates
!   Calling subroutine: main program
!   Subroutines called: w3fb06
!
!   ihrad,jhrad = coordinates of radar on National HRAD grid
!
      include 'raddat'
      include 'hrap'
!
      call w3fb06(rlat,360.-rlon,alat1,alon1,dx,alonv,xi,xj)
!
      ihrad=xi
      jhrad=xj
!
      return
      end
