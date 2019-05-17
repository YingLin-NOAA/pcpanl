!****************************************************************
      subroutine calggo1(ng,estim)
!****************************************************************
!
! subroutine computes gage-only estimate via inverse distance
! weighting
! Version Oct 28, 1995 by D.-J. Seo at HRL/NWS
!
      parameter (ndat=1000,ngmax=20)
      common/big1/u(ndat),v(ndat),z(ndat)
      common/srtord/ilist(ngmax)
      common/srtdst/rlist(ngmax)
      include 'ggoparam'
      include 'flags'
!
! perform inverse distance weighting
!
      if(rlist(1).eq.0.) then
         estim=z(ilist(1))
         return
      endif

      sum=0.
      do 170 ii=1,ng
      sum=sum+1./(rlist(ii)*rlist(ii))
  170 continue

      estim=0.
      do 180 k=1,ng
      estim=estim+z(ilist(k))/(rlist(k)*rlist(k))
  180 continue
      estim=estim/sum

      return
      end
