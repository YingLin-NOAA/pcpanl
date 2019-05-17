!**********************************************************************
      subroutine lubksb(a,n,indx,b)
!**********************************************************************
      parameter (ndim=20)
      dimension a(ndim,n),indx(n),b(n)
      ii=0
      do 12 i=1,n
      ll=indx(i)
      sum=b(ll)
      b(ll)=b(i)
      if(ii.ne.0) go to 22
      go to 33
   22 im1=i-1
      if(im1.lt.ii.or.im1.gt.n) go to 957
      do 11 j=ii,im1
      sum=sum-a(i,j)*b(j)
   11 continue
  957 continue
      go to 44
   33 continue
      if(sum.ne.0.) go to 55
      go to 66
   55 ii=i
   66 continue
   44 continue
      b(i)=sum
   12 continue
      do 14 itemp=1,n
      i=n+1-itemp
      sum=b(i)
      if(i.lt.n) go to 77
      go to 88
   77 ip1=i+1
      if(ip1.gt.n) go to 977
      do 13 j=ip1,n
      sum=sum-a(i,j)*b(j)
   13 continue
  977 continue
   88 continue
      b(i)=sum/a(i,i)
   14 continue
      return
      end
