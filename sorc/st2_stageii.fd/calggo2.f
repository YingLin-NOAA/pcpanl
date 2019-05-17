!*************************************************************
      subroutine calggo2(ng,u0,v0,f0,umean,uvari,cmean,cvari,                 &
                         estim,varia,iflag)
!*************************************************************
!
! subroutine performs SOE under fractional coverage conditions
!
! Version Oct 28, 1995 by D.-J. Seo at HRL/NWS
!
      parameter (ndim=20,ndat=1000,ngmax=20)
      include 'ggoparam'
      common/big1/u(ndat),v(ndat),z(ndat)
      common/srtord/ilist(ngmax)
      common/srtdst/rlist(ngmax)
      dimension cov(ndim,ndim),cori(ndim,ndim),cor(ndim,ndim)
      dimension w(ndim),s(ndim),wri(ndim),wr(ndim)
      include 'flags'
!
! compute indicator variance
!
      vari=f0*(1.-f0)
!
! construct indicator correlation matrix
!
      call covmatgg(ng,distmingg,cor0pi,cor0i,rangei,1.,cori)
!
! construct indicator correlation vector
!
      call covvecgg(ng,distmingg,cor0pi,cor0i,rangei,1.,u0,v0,wri)
!
! construct conditional correlation coefficient matrix
!
      call covmatgg(ng,distmingg,cor0pi,cor0i,rangei,1.,cor)
!
! construct conditional correlation coefficient vector
!
      call covvecgg(ng,distmingg,cor0pi,cor0i,rangei,1.,u0,v0,wr)
!
! construct unconditional covariance matrix 
!
      do 140 ii=1,ng
      do 150 jj=1,ii
      cov(ii,jj)=(cvari*cor(ii,jj)+cmean*cmean)*                              &
     &            (vari*cori(ii,jj)+f0*f0)-cmean*cmean*f0*f0
  150 continue
  140 continue
!
! copy using symmetry
!
      do 151 ii= 1,ng
      do 152 jj=ii,ng
      cov(ii,jj)=cov(jj,ii)
  152 continue
  151 continue
!
! construct unconditional covariance vector 
!
      do 160 ii=1,ng
      w(ii)=(cvari*wr(ii)+cmean*cmean)                                        &
     &     *(vari*wri(ii)+f0*f0)                                              &
     &     -cmean*cmean*f0*f0
  160 continue
!
! copy w to s before calling lsolve
!
      if(ifvar.eq.1) then
         do 170 ii=1,ng
         s(ii)=w(ii)
  170    continue
      end if
!
! solve linear system 
!
      call lsolve(ng,cov,w,iflag)

      if(iflag.ne.0) then
      write(6,*) 'error in lsolve in unconditional estimation...return'
      return
      endif
!
! compute unconditional kriging estimate 
!
      varia=f0*cvari+cmean*cmean*vari
      estim=f0*cmean
      do 180 k=1,ng
      estim=estim+w(k)*(z(ilist(k))-f0*cmean)
      if(ifvar.eq.1) varia=varia-w(k)*s(k)
  180 continue
!
! check nonnegativity of estimation variance and estimate
!
      if(ifvar.eq.1.and.varia.lt.-0.0001) then
         write(6,*) 'varia negative...',varia,f0
         write(6,*) 'ng,f0,umean,uvari ',ng,f0,umean,uvari
!
         do 33 jj=1,ng
         write(6,*) 'jj,rlist,w,s ',jj,rlist(jj),w(jj),s(jj)
   33    continue
         iflag=1
         return
      endif

      if(estim.lt.0.) estim=0.

      iflag=0
      return
      end
