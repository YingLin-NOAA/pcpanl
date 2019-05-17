!**********************************************************************
      subroutine gammf(dr,cn,c,r,h,covar)
!**********************************************************************
! cn - limiting lag-0 correlation coefficient
! c  - lag-0 correlation coeffcient
! r  - correlation distance
      if(r.eq.0.) then
      write(6,*) 'r eq 0 in gammf...stop'
      stop
      endif
      if(h.le.dr) then
      covar=c
      else
      covar=cn*exp(-h/r)
      endif
      return
      end
