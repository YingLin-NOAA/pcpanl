      subroutine ll2hrap(xlat,xlon,ih,jh)
!
!   This subroutine calculates the location of the gage on the hrap
!     grid
!   calling subroutines: bias,readgg
!   subroutines called: w3fb06
!
!   xlat,xlon = latitude/longitude (decimal degrees) of gage
!   ihnat,jhnat = coordinates of gage on national hrap grid
!   ih,jh       =      "      "    "   " local hrap grid
!   ihrad,jhrad =      "      "  radar on national hrap grid
!
      include 'raddat'
      include 'hrap'
!
!   convert from latitude,longitude to hrap coordinates (national grid)
!
      call w3fb06(xlat,360.-xlon,alat1,alon1,dx,alonv,x,y)
!
      ihnat=nint(x)
      jhnat=nint(y)
!
!   convert national to local hrap grid coordinates
!
      ih=ihnat - ihrad + 66
      jh=jhnat - jhrad + 66
      return
      end
