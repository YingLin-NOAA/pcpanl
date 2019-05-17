!*******************************************************************
      subroutine calggo3(ng,u0,v0,f0,umean,uvari,cmean,cvari,                 &
                         estim,varia,iflag)
!*******************************************************************
!
! subroutine performs DOE under fractional coverage conditions
!
! Version Oct 28, 1995 by D.-J. Seo at HRL/NWS
!
      parameter (ndim=20,ndat=1000,ngmax=20)
      include 'ggoparam'
      common/big1/u(ndat),v(ndat),z(ndat)
      common/srtord/ilist(ngmax)
      common/srtdst/rlist(ngmax)
      dimension cov(ndim,ndim),cori(ndim,ndim),cor(ndim,ndim)
      dimension w(ndim),s(ndim),si(ndim),wri(ndim),wr(ndim)
      include 'flags'
!
! ----------------------------------------------
! ---------- Step 1 of Double Kriging ----------
! ----------------------------------------------
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
! retain indicator covariance matrix
!
      do 50 ii=1,ng
      do 60 jj=1,ng
      cov(ii,jj)=cori(ii,jj)*vari
   60 continue
   50 continue
!
! retain indicator covariance vector
!
      do 70 jj=1,ng
      w(jj)=wri(jj)*vari
   70 continue
!
! copy w to si
!
      if(ifvar.eq.1) then
         do 75 jj=1,ng
         si(jj)=w(jj)
   75    continue
      end if
!
! solve linear system 
!
      call lsolve(ng,cov,w,iflag)

      if(iflag.ne.0) then
      write(6,*) 'error in lsolve for indicator kriging ...return'
      return
      endif
! 
! compute conditional probability and estimation variance 
!
      varik=vari
      cprob=(1.-f0)
      do 80 jj=1,ng
      if(z(ilist(jj)).lt.rainmingg) then
      zi=1.
      else
      zi=0.
      endif
      cprob=cprob+w(jj)*(zi-(1.-f0))
      if(ifvar.eq.1) varik=varik-w(jj)*si(jj)
   80 continue
      cprob=1.0-cprob
      if(cprob.gt.1.) cprob=1.
      if(cprob.lt.0.) cprob=0.
!
! ----------------------------------------------
! ---------- Step 2 of Double Kriging ----------
! ----------------------------------------------
!
! construct conditional correlation coefficient matrix
!
      call covmatgg(ng,distmingg,cor0pc,cor0c,rangec,1.,cov)
!
! retain conditional correlation coefficient matrix
!
      do 81 jj=1,ng
      do 82 ii=1,ng
      cor(ii,jj)=cov(ii,jj)
   82 continue
   81 continue
!   
! construct conditional covariance matrix
!
      do  90 jj=1,ng
      if(f0.lt.1.) then
      cpj0=(1.-f0)*wri(jj)+f0
      else
      cpj0=1.
      endif
      do 100 ii=1,ng
      if(f0.lt.1.) then
      cpi0=(1.-f0)*wri(ii)+f0
      else
      cpi0=1.
      endif
      if(ii.eq.jj) then
      cpij0=cpi0
      go to 95
      endif
      if(f0.lt.1.) then
      cpij=(1.-f0)*cori(ii,jj)+f0
      denom=1.-cori(ii,jj)*cori(ii,jj)
      wght1=wri(ii)-cori(ii,jj)*wri(jj)
      wght1=wght1/denom
      wght2=wri(jj)-cori(ii,jj)*wri(ii)
      wght2=wght2/denom
      cp0ij=f0+(1.-f0)*(wght1+wght2)
      if(cp0ij.gt.1.) then
      write(6,*) '***** cp0ij gt 1...set to 1 *****'
      cp0ij=1.
      endif
      if(cp0ij.lt.0.) then
      write(6,*) '***** cp0ij lt 0...set to 0 *****'
      cp0ij=0.
      endif
      cpij0=cp0ij*cpij
      else
      cpij0=1.
      endif
   95 cov(ii,jj)=cvari*cor(ii,jj)+cmean*cmean
      cov(ii,jj)=cov(ii,jj)*cpij0-cmean*cmean*cpi0*cpj0
  100 continue
   90 continue
!
! construct conditional correlation coefficient vector
!
      call covvecgg(ng,distmingg,cor0pc,cor0c,rangec,1.,u0,v0,w)
!
! retain conditional correlation coefficient vector for future use
!
      do 113 jj=1,ng
      wr(jj)=w(jj)
  113 continue
!
! construct conditional covariance vector
!
      do 110 jj=1,ng
      if(f0.lt.1.) then
      w(jj)=cvari*wr(jj)*((1.-f0)*wri(jj)+f0)
      else
      w(jj)=cvari*wr(jj)
      endif
  110 continue
!
! copy covariance vector before calling lsolve 
!
      if(ifvar.eq.1) then
         do 120 jj=1,ng
         s(jj)=w(jj)
  120    continue
      end if
!
! solve linear system 
!
      call lsolve(ng,cov,w,iflag)
! 
! check error flag here 
! 
      if(iflag.ne.0) then
      write(6,*) 'error in lsolve in conditional estimation...return'
      return
      endif
!
! compute conditional kriging estimate
!
      vari2=cvari
      esti2=cmean
      do 130 k=1,ng
      esti2=esti2+w(k)*(z(ilist(k))-cmean*((1.-f0)*wri(k)+f0))
      if(ifvar.eq.1) vari2=vari2-w(k)*s(k)
  130 continue
!
! check nonnegativity of estimation variance and estimate
!
      if(ifvar.eq.1.and.vari2.lt.-0.0001) then
         write(6,*) 'cond. kriging variance negative...',vari2,f0,cpij0
         write(6,*) 'cmean,cvari ',cmean,cvari
!
         do 311 jj=1,ng
         write(6,*) 'jj,rlist,w,s ',jj,rlist(jj),w(jj),s(jj)
  311    continue
         iflag=1
         return
      endif

      if(esti2.lt.0.) esti2=0.
! 
! compute the unconditional estimate and estimation variance 
!
      estim=cprob*esti2
      if(ifvar.eq.1) varia=vari2*cprob+(esti2**2)*cprob*(1.-cprob)
!
! check bounds for varia
! 
      if(ifvar.eq.1.and.varia.lt.-0.0001) then
         write(6,*) 'varia negative...',varia,f0
         iflag=1
         return
      endif

      iflag=0
      return
      end
