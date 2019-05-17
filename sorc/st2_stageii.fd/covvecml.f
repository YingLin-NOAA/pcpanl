!**********************************************************************
      subroutine covvecml(u0,v0,varg,varr,ng,dr,cng,cg,rg,cc,b)
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
      call gammf(dr,cng,cg,rg,h,b(j))
      b(j)=varg*b(j)
   10 continue
! construct cross-covariance entry
      b(ng+1)=cc*sqrt(varg*varr)
      return
      end
