!******************************************************************
      subroutine local_stat(u0,v0,radi,rad,iinc,jinc,numu,uave,uvar,npos)
!******************************************************************
!
! subroutine computes local statistics of radar rainfall
!
      parameter (nx=131,ny=131)
      dimension rad(nx,ny)

      if(u0-radi.lt.1.) then
      ibeg=1
      else
      ibeg=u0-radi
      endif

      if(u0+radi.gt.nx) then
      iend=nx
      else
      iend=u0+radi
      endif

      if(v0-radi.lt.1.) then
      jbeg=1
      else
      jbeg=v0-radi
      endif

      if(v0+radi.gt.ny) then
      jend=ny
      else
      jend=v0+radi
      endif

      numu=0
      uave=0.
      uvar=0.
      npos=0

      do 10 j=jbeg,jend,jinc
      ydis=j-v0
      do 20 i=ibeg,iend,iinc
      xdis=i-u0
      dist=xdis**2+ydis**2
      dist=sqrt(dist)
      if(dist.gt.radi) go to 20
      if(rad(i,j).ge.0.) then
      numu=numu+1
      uave=uave+rad(i,j)
      uvar=uvar+rad(i,j)*rad(i,j)
      if(rad(i,j).gt.0.) npos=npos+1
      endif
   20 continue
   10 continue
!
      return
      end
