      subroutine rdprcp(ngaghr,ngagmh,npseudo,ngrep,                          &
                        ihour,ngage,nhrtd,date,ictt,tsurf,bpps)
!
!   This subroutine calls the subroutines for reading the radar,
!     satellite, hourly and multiple hourly gage precip,
!     and surface temperature data for the current hour.
!   calling subroutine: main program
!   subroutines called: readrd,readgg,readst,readtp
!
!   ictt = satellite temperature for each mdr box
!   tsurf = minimum surface temperature for all stations reporting
!           if no stations are reporting for current hour, then
!               tsurf from previous hour is used
!
      character date*10
      dimension ictt(15,15)
!
      include 'ppinp'
      include 'luqual'
!
!   read radar precip data
!
      call readrd(lurad,date,bpps)
!
!   read hourly and multiple hourly gage precip data
!
      call readgg(ihour,ngaghr,ngagmh,npseudo,ngrep,ngage,nhrtd,date)
!
      return
      end
