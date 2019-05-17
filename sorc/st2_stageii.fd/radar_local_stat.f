!*********************************************************************
      subroutine radar_local_stat(radi,rad,ndata,nposi,summ1,summ2)
!*********************************************************************
!
! subroutine compute local statistics of radar rainfall by successive
! addition and subtraction
! Version Jan 2, 1996 by DJ Seo at NWS/HRL
!
      parameter (nx=131,ny=131)
      common/gag2rad/istart(nx),itermi(nx)
      integer*2 il(nx),jl(nx),ir(nx),jr(nx)
      integer*2 ndata(nx,ny),nposi(nx,ny)
      dimension summ1(nx,ny),summ2(nx,ny)
      dimension rad(nx,ny)
!
! initialize arrays
!
      do 1 i=1,nx
      do 2 j=1,ny
      ndata(i,j)=0
      nposi(i,j)=0
    2 continue
    1 continue
!
! determine the zone of influence at the current bin
!
      iradi=radi
      if(radi-iradi.ge.0.5) iradi=iradi+1
      iside=2*iradi+1
      do 10 j=1,iside
      ydis=j-(iradi+1)
      do 20 i=1,iside+1
      xdis=i-(iradi+1)
      dist=xdis*xdis+ydis*ydis
      dist=sqrt(dist)
      if(dist.le.iradi) then
      ndata(i,j)=1
      else
      ndata(i,j)=0
      endif
   20 continue
   10 continue
!
! determine the zone of influence at the bin immediately to the
! right of the current bin
!
      do 30 j=1,iside
      ydis=j-(iradi+1)
      do 40 i=2,iside+2
      xdis=i-(iradi+2)
      dist=xdis*xdis+ydis*ydis
      dist=sqrt(dist)
      if(dist.le.iradi) then
      nposi(i,j)=2
      else
     nposi(i,j)=0
      endif
   40 continue
   30 continue
!
! current bin is at (iradi+2,iradi+1)....determine additive
! and subtractive crescents with respect to the current bin
!
      imid=iradi+1
      jmid=iradi+1
      nl=0
      nr=0
      do 100 j=1,iside
      do 110 i=1,iside+2
      if(nposi(i,j)-ndata(i,j).eq.-1) then
      nl=nl+1
      il(nl)=i-imid
      jl(nl)=j-jmid
      go to 110
      endif
      if(nposi(i,j)-ndata(i,j).eq. 2) then
      nr=nr+1
      ir(nr)=i-imid
      jr(nr)=j-jmid
      go to 110
      endif
  110 continue
  100 continue
!
! initialize
!
      do 105 j=1,ny
      do 115 i=1,nx
      ndata(i,j)=-99
  115 continue
  105 continue
!
! start computing local statisics at every bin
!
      do 140 j0=1,ny
      if(istart(j0).eq.0) go to 140
      if(istart(j0).gt.itermi(j0)) then
         write(6,*) 'istart > itermi', istart(j0),itermi(j0)
         go to 140
      end if
!
      if(istart(j0).le.0) write(6,*) 'istart(j0) ',j0,istart(j0)
      do 150 i0=istart(j0),itermi(j0)

      if(i0.eq.istart(j0)) then
!
! determine the square that circumscribes the radius of influence
!
      if(i0-iradi.lt.1) then
      ibeg=1
      else
      ibeg=i0-iradi
      endif

      if(i0+iradi.gt.nx) then
      iend=nx
      else
      iend=i0+iradi
      endif

      if(j0-iradi.lt.1) then
      jbeg=1
      else
      jbeg=j0-iradi
      endif

      if(j0+iradi.gt.ny) then
      jend=ny
      else
      jend=j0+iradi
      endif

      ndat=0
      sum1=0.
      sum2=0.
      npos=0
      do 160 j=jbeg,jend
      iydis=j-j0
      do 170 i=ibeg,iend
      if(rad(i,j).lt.0.) go to 170
      ixdis=i-i0
      idist=ixdis**2+iydis**2
      if(idist.le.iradi**2) then
      ndat=ndat+1
      sum1=sum1+rad(i,j)
      sum2=sum2+rad(i,j)*rad(i,j)
      if(rad(i,j).gt.0.) npos=npos+1
      endif
  170 continue
  160 continue

      else
!
! current bin is at (i0,j0)...subtract and add
!
      ndatl=0
      nposl=0
      do 190 k=1,nl
      i=(i0-1)+il(k)
      if(i.lt. 1) go to 190
      if(i.gt.nx) go to 190
      j=j0+jl(k)
      if(j.lt. 1) go to 190
      if(j.gt.ny) go to 190
      if(rad(i,j).lt.0.) go to 190
      ndatl=ndatl+1
      ndat=ndat-1
      sum1=sum1-rad(i,j)
      sum2=sum2-rad(i,j)*rad(i,j)
      if(rad(i,j).gt.0.) then
      nposl=nposl+1
      npos=npos-1
      endif
  190 continue

      ndatr=0
      nposr=0
      do 180 k=1,nr
      i=(i0-1)+ir(k)
      if(i.lt. 1) go to 180
      if(i.gt.nx) go to 180
      j=j0+jr(k)
      if(j.lt. 1) go to 180
      if(j.gt.ny) go to 180
      if(rad(i,j).lt.0.) go to 180
      ndat=ndat+1
      ndatr=ndatr+1
      sum1=sum1+rad(i,j)
      sum2=sum2+rad(i,j)*rad(i,j)
      if(rad(i,j).gt.0.) then
      npos=npos+1
      nposr=nposr+1
      endif
  180 continue

      endif
!
!     if(ndat.le.0) write(6,*) 'ndat le 0 ',ndat
!     if(npos.lt.0) write(6,*) 'npos lt 0 ',npos
      if(sum1.lt.0) sum1=0.
      if(sum2.lt.0) sum2=0.
!
      ndata(i0,j0)=ndat
      nposi(i0,j0)=npos
      summ1(i0,j0)=sum1
      summ2(i0,j0)=sum2

  150 continue
  140 continue

      return
      end
