!****************************************************************
       subroutine merge_soe(uave,uvar,f0,vari,cave,cvar,ng,                   &
                            rad,i,j,u0,v0,estim,varia)
!****************************************************************
!
! subroutine performs radar-gage merging via Single Optimal
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
      dimension cov(ndim,ndim),covi(ndim,ndim),cor(ndim,ndim)
      dimension w(ndim),wri(ndim),bi(ndim),wr(ndim),b(ndim)
      dimension rad(nx,ny)
!
! construct indicator covariance matrix
!
      if(f0.lt.1.) call covmatml(u0,v0,vari,vari,ng,distminml,                &
                     cor0pig,cor0ig,rangeig,cor0pic,cor0ic,rangeic,covi)
!
! construct indicator covariance vector
!
      if(f0.lt.1.) call covvecml(u0,v0,vari,vari,ng,distminml,                &
                     cor0pig,cor0ig,rangeig,cor0ic,bi)
!
! construct conditional covariance matrix
!
      call covmatml(u0,v0,cvar,cvar,ng,distminml,                             &
        cor0pcg,cor0cg,rangecg,cor0pcc,cor0cc,rangecc,cov)
!
! construct conditional covariance vector
!
      call covvecml(u0,v0,cvar,cvar,ng,distminml,                             &
        cor0pcg,cor0cg,rangecg,cor0cc,b)
!
! compute unconditional gage covariance matrix 
!
      do 105 j1=1,ng+1
      do 115 j2=1,j1
      if(f0.lt.1.) cov(j1,j2)=(cov(j1,j2)+cave*cave)*(covi(j1,j2)+f0*f0)      &
                               -cave*cave*f0*f0
  115 continue
  105 continue
!
! fill using symmetry
!
      do 70 j1= 1,ng+1
      do 80 j2=j1,ng+1
      cov(j1,j2)=cov(j2,j1)
   80 continue
   70 continue
!
! compute unconditional covariance vector
!
      do 125 j1=1,ng+1
      if(f0.lt.1.) b(j1)=(b(j1)+cave*cave)*(bi(j1)+f0*f0)-cave*cave*f0*f0
  125 continue
!
! copy before calling lsolve
!
      if(ifvar.eq.1) then
         do 185 j1=1,ng+1
         w(j1)=b(j1)
  185    continue
      end if
!
! solve linear system 
!
      call lsolve(ng+1,cov,b,iflag)
! 
! check error flag here 
! 
      if(iflag.ne.0) then
      write(6,*) 'error in lsolve...stop'
      stop
      endif
!
! compute conditional kriging estimate
!
      varia=uvar
      estim=uave
      do 135 k=1,ng
      estim=estim+b(k)*(z(ilist(k))-uave)
      if(ifvar.eq.1) varia=varia-b(k)*w(k)
  135 continue
!
      estim=estim+b(ng+1)*(rad(i,j)-uave)
      if(ifvar.eq.1) varia=varia-b(ng+1)*w(ng+1)
!
! check nonnegativity of estimation variance and estimate
!
      if(ifvar.eq.1.and.varia.lt.-0.0001) then
      write(6,*) 'negative estimation variance...stop ',varia
      stop
      endif

      if(estim.lt.0.) estim=0.

      return
      end
