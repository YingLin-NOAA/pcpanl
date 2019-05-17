!**********************************************************************
      subroutine covmatml(u0,v0,varg,varr,ng,dr,cng,cg,rg,cnc,cc,rc,cov)
!**********************************************************************
! input
! u0   - x-coordinate of the estimation point
! v0   - y-coordinate of the estimation point
! varg - (indicator) variance of gage rainfall
! varr - (indicator) variance of radar rainfall
! ng   - number of nearest rain gage measurements used
! dr   - nugget distance
! cng  - limiting lag-zero (indicator) correlation coefficient
!        of gage rainfall
! cg   - lag-zero (indicator) correlation coefficient of gage
!        rainfall
! rg   - (indicator) correlation scale of gage rainfall
! cnc  - limiting lag-zero (indicator) cross-correlation coefficient
!        between gage and radar rainfall
! cc   - lag-zero (indicator) cross-correlation coefficient between
!        gage and radar rainfall
! rc   - (indicator) cross-correlation scale
! output
! cov  - covariance matrix
      parameter (ndim=20,ndat=1000,ngmax=20)
      common/big1/u(ndat),v(ndat),z(ndat)
      common/srtord/ilist(ngmax)
      common/big3/irv(ndat)
      dimension cov(ndim,ndim)
! specify the size of the matrix
          iend=ng+1
! initialize covariance matrix
          do 5 irow=1,iend
          do 5 icol=1,irow
          cov(irow,icol)=0.
 5        continue
! construct indicator covariance matrix of gage rainfall
          do 20 j1=1,ng
          i1=ilist(j1)
          do 20 j2=1,j1
          i2=ilist(j2)
          h=(u(i1)-u(i2))**2+(v(i1)-v(i2))**2
          h=sqrt(h)
          call gammf(dr,cng,cg,rg,h,cov(j1,j2))
          cov(j1,j2)=varg*cov(j1,j2)
   20     continue
! construct indicator cross-covariance vector
          do 8 j2=1,ng
          i2=ilist(j2)
          h=(u0-u(i2))**2+(v0-v(i2))**2
          h=sqrt(h)
          call gammf(dr,cnc,cc,rc,h,cov(ng+1,j2))
          cov(ng+1,j2)=sqrt(varg*varr)*cov(ng+1,j2)
   8      continue
! specify indicator variance for radar rainfall
          cov(ng+1,ng+1)=varr
!
! fill using symmetry
!
      do 70 j1= 1,ng+1
      do 80 j2=j1,ng+1
      cov(j1,j2)=cov(j2,j1)
   80 continue
   70 continue
          return
          end
