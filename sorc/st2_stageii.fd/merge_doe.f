!***************************************************************
      subroutine merge_doe(f0,vari,cave,cvar,ng,                              &
                           rad,i,j,u0,v0,estim,varia)
!***************************************************************
! 
! subroutine performs radar-gage merging via Double Optimal
! Estimation
! Version Dec 27, 1995 by DJ Seo at NWS/HRL
!
      parameter (ndim=20,ndat=1000,ngmax=20,nx=131,ny=131)
      include 'mltparam'
      include 'flags'
      common/inform/iactrv(2),nactrv,iordrv(2)
      common/big1/u(ndat),v(ndat),z(ndat)
      common/srtord/ilist(ngmax)
      common/srtdst/rlist(ngmax)
      common/big3/irv(ndat)
      dimension cov(ndim,ndim),cori(ndim,ndim),cor(ndim,ndim)
      dimension w(ndim),wri(ndim),si(ndim),wr(ndim),s(ndim)
      dimension rad(nx,ny)
!
! perform the following operations only  when rainfall coverage is 
! fractional
!
      if(f0.lt.1.) then
!
! construct indicator covariance matrix
!
      call covmatml(u0,v0,vari,vari,ng,distminml,                             &
     &              cor0pig,cor0ig,rangeig,cor0pic,cor0ic,rangeic,cov)
!
! retain indicator correlation matrix
!
      do 51 ii=1,ng+1
      do 61 jj=1,ng+1
      cori(ii,jj)=cov(ii,jj)/vari
   61 continue
   51 continue
!
! construct indicator covariance vector
!
      call covvecml(u0,v0,vari,vari,ng,distminml,                             &
     &              cor0pig,cor0ig,rangeig,cor0ic,w)
!
! retain indicator covariance vector
!
      do 71 jj=1,ng+1
      wri(jj)=w(jj)/vari
   71 continue
!
! copy w to si
!
      if(ifvar.eq.1) then
         do 76 jj=1,ng+1
         si(jj)=w(jj)
   76    continue
      end if
!
! solve linear system
!
      call lsolve(ng+1,cov,w,iflag)

      if(iflag.ne.0) then
      write(*,*) 'terminal error in lsolve...stop'
      stop
      endif
!
! compute conditional probability and estimation variance
!
      varik=vari
      cprob=(1.-f0)
      do 80 jj=1,ng
      if(z(ilist(jj)).lt.rainminml) then
      zi=1.
      else
      zi=0.
      endif
      cprob=cprob+w(jj)*(zi-(1.-f0))
      if(ifvar.eq.1) varik=varik-w(jj)*si(jj)
   80 continue
      if(rad(i,j).eq.0.) then
      zi=1.
      else
      zi=0.
      endif
      cprob=cprob+w(ng+1)*(zi-(1.-f0))
      if(ifvar.eq.1) varik=varik-w(ng+1)*si(ng+1)
      cprob=1.0-cprob
      if(cprob.gt.1.0) cprob=1.
      if(cprob.lt.0.0) cprob=0.
      else

      cprob=1.

      endif
!
! construct conditional correlation coefficient matrix
!
      call covmatml(u0,v0,1.,1.,ng,distminml,                                 &
                    cor0pcg,cor0cg,rangecg,cor0pcc,cor0cc,rangecc,cov)
!
! retain conditional correlation coefficient matrix
!
      do 881 jj=1,ng+1
      do 882 ii=1,ng+1
      cor(ii,jj)=cov(ii,jj)
  882 continue
  881 continue
!
! construct conditional covariance matrix
!
      do  90 jj=1,ng+1
      if(f0.lt.1.) then
      cpj0=(1.-f0)*wri(jj)+f0
      else
      cpj0=1.
      endif
      do 100 ii=1,ng+1
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
   95 cov(ii,jj)=cvar*cor(ii,jj)+cave*cave
      cov(ii,jj)=cov(ii,jj)*cpij0-cave*cave*cpi0*cpj0
  100 continue
   90 continue
!
! construct conditional correlation vector
!
      call covvecml(u0,v0,1.,1.,ng,distminml,                                 &
                    cor0pcg,cor0cg,rangecg,cor0cc,w)
!
! retain conditional correlation coefficient vector for future use
!
      do 1113 jj=1,ng+1
      wr(jj)=w(jj)
 1113 continue
!
! construct conditional covariance vector
!
      do 310 jj=1,ng+1
      if(f0.lt.1.) then
      w(jj)=cvar*wr(jj)*((1.-f0)*wri(jj)+f0)
      else
      w(jj)=cvar*wr(jj)
      endif
  310 continue
!
! copy covariance vector before calling lsolve
!
      if(ifvar.eq.1) then
         do 320 jj=1,ng+1
         s(jj)=w(jj)
  320    continue
      end if
!
! solve linear system
!
      call lsolve(ng+1,cov,w,iflag)
!
! check error flag here
!
      if(iflag.ne.0) then
      write(6,*) 'terminal error in lsolve...stop'
      stop
      endif
!
! compute conditional kriging estimate
!
      vari2=cvar
      esti2=cave
      do 330 k=1,ng
      esti2=esti2+w(k)*(z(ilist(k))-cave*((1.-f0)*wri(k)+f0))
      vari2=vari2-w(k)*s(k)
  330 continue
      esti2=esti2+w(ng+1)*(rad(i,j)-cave*((1.-f0)*wri(ng+1)+f0))
      if(ifvar.eq.1) vari2=vari2-w(ng+1)*s(ng+1)
!
! check nonnegativity of estimation variance and estimate
!
      if(ifvar.eq.1.and.vari2.lt.-0.0001) then
         write(6,*) 'cond. kriging variance negative...stop ',                &
                     vari2,f0,cpij0,numu,npos,cave,cvar
         stop
      endif

      if(esti2.lt.0.) esti2=0.
!
! compute the unconditional estimate and estimation variance
!
      estim=cprob*esti2
      if(ifvar.eq.1) varia=vari2*cprob+esti2*esti2*cprob*(1.-cprob)
!
! check bounds for varia
!
      if(ifvar.eq.1.and.varia.lt.-0.0001) then
         write(6,*) 'varia negative...stop ',varia,f0
         stop
      endif

      return
      end
