!****************************************************************
      subroutine calggo4(ng,u0,v0,f0,umean,uvari,cmean,cvari,                 &
                         estim,varia,iflag)
!****************************************************************
!
! subroutine performs gage-only analysis using optimal estimation
! under full coverage conditions
!
! Version Oct 28, 1995 by D.-J. Seo at HRL/NWS
!
      parameter (ndim=20,ndat=1000,ngmax=20)
      include 'ggoparam'
      common/big1/u(ndat),v(ndat),z(ndat)
      common/srtord/ilist(ngmax)
      common/srtdst/rlist(ngmax)
      dimension w(ndim),s(ndim)
      dimension cov(ndim,ndim)
      include 'flags'
! 
! construct covariance matrix
!
      call covmatgg(ng,distmingg,cor0pc,cor0c,rangec,cvari,cov)
!
! construct convariance vector
!
      call covvecgg(ng,distmingg,cor0pc,cor0c,rangec,cvari,u0,v0,w)
!
! copy w to s
!
      if(ifvar.eq.1) then
         do 160 ii=1,ng
         s(ii)=w(ii)
  160    continue
      end if
!
! solve linear system 
!
      call lsolve(ng,cov,w,iflag)

      if(iflag.ne.0) then
      write(6,*) 'error in lsolve in calggo4...skip'
      return
      endif
!
! compute estimate and estimation variance
!
      varia=cvari
      estim=cmean
      do 180 k=1,ng
      estim=estim+w(k)*(z(ilist(k))-cmean)
      if(ifvar.eq.1) varia=varia-w(k)*s(k)
  180 continue
!
! check nonnegativity of estimation variance
!
      if(ifvar.eq.1.and.varia.lt.-0.0001) then
         write(6,*) 'varia negative in calggo4 ',varia,u0,v0,f0
         iflag=1
         return
      endif
!
! check nonnegativity of estimate
!
      if(estim.lt.0.) estim=0.

      iflag=0
      return
      end
