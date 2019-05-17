!**********************************************************************
      subroutine lsolve(n,cov,b,iflag)
!**********************************************************************
! subroutine solves linear system via LU decomposition of Press et al.
      parameter (ndim=20)
      dimension cov(ndim,ndim),b(n),indx(ndim)
      call ludcmp(cov,n,indx,d,iflag)
      if(iflag.ne.0) then
      write(*,*) 'error in ludcmp...skip'
      return
      endif
      call lubksb(cov,n,indx,b)
      return
      end
