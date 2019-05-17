!**********************************************************************
      subroutine covvecgg(ng,dr,cn,c,r,var,u0,v0,b)
!**********************************************************************
      parameter (ndim=20,ndat=1000,ngmax=20)
      common/big1/u(ndat),v(ndat),z(ndat)
      common/srtord/ilist(ngmax)
      common/big3/irv(ndat)
      dimension b(ndim)
! construct gage rainfall covariance vector
      do 10 j=1,ng
      i=ilist(j)
      h=(u0-u(i))**2+(v0-v(i))**2
      h=sqrt(h)
      call gammf(dr,cn,c,r,h,b(j))
      b(j)=var*b(j)
   10 continue
!
      return
      end
