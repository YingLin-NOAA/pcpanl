!*********************************************************************
      subroutine ludcmp(a,n,indx,d,iflag)
!**********************************************************************
      parameter (ndim=20,tiny=1.e-20)
      dimension a(ndim,n),indx(n),vv(ndim)
      d=1.
      do 12 i=1,n
      aamax=0.
      do 11 j=1,n
      if(abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
   11 continue
      if(aamax.eq.0.) go to 22
      go to 33
! check if matrix is singular
   22 iflag=1
      write(*,*) 'maxtrix is singular...skip'
      return
   33 continue
      vv(i)=1./aamax
   12 continue
      do 19 j=1,n
      jm1=j-1
      if(jm1.lt.1) go to 888
      do 14 i=1,jm1
      sum=a(i,j)
      im1=i-1
      if(im1.lt.1) go to 892
      do 13 k=1,im1
      sum=sum-a(i,k)*a(k,j)
   13 continue
  892 continue
      a(i,j)=sum
   14 continue
  888 continue
      aamax=0.
      do 16 i=j,n
      sum=a(i,j)
      jm1=j-1
      if(jm1.lt.1) go to 904
      do 15 k=1,jm1
      sum=sum-a(i,k)*a(k,j)
   15 continue
  904 continue
      a(i,j)=sum
      dum=vv(i)*abs(sum)
      if(dum.ge.aamax) go to 44
      go to 55
   44 imax=i
      aamax=dum
   55 continue
   16 continue
      if(j.ne.imax) go to 66
      go to 77
   66 do 17 k=1,n
      dum=a(imax,k)
      a(imax,k)=a(j,k)
      a(j,k)=dum
   17 continue
      d=-d
      vv(imax)=vv(j)
   77 continue
      indx(j)=imax
      if(a(j,j).eq.0.) a(j,j)=tiny
      if(j.ne.n) go to 88
      go to 99
   88 dum=1./a(j,j)
      jp1=j+1
      if(jp1.gt.n) go to 933
      do 18 i=jp1,n
      a(i,j)=a(i,j)*dum
   18 continue
  933 continue
   99 continue
   19 continue
      iflag=0
      return
      end
