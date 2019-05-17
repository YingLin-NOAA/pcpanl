!**********************************************************************
      subroutine covmatgg(ng,dr,cn,c,r,var,cov)
!**********************************************************************
! input
! ng     - number of nearest gage rainfall data
! cg     - sample variance of gage rainfall
! type   -
! cordis - correlation distance of radar rainfall
! dr     - minimum distance required between two points
! cgm    -
! output
! cov    - ng x ng lower triangular covariance matrix
      parameter (ndim=20,ndat=1000,ngmax=20)
      common/big1/u(ndat),v(ndat),z(ndat)
      common/srtord/ilist(ngmax)
      common/big3/irv(ndat)
      dimension cov(ndim,ndim)
          iend=ng
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
          call gammf(dr,cn,c,r,h,cov(j1,j2))
          cov(j1,j2)=var*cov(j1,j2)
   20     continue
! fill using symmetry
      do 70 j1= 1,iend
      do 80 j2=j1,iend
      cov(j1,j2)=cov(j2,j1)
   80 continue
   70 continue
          return
          end
