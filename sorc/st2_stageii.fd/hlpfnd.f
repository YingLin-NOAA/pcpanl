!**********************************************************************
      subroutine hlpfnd(n1,n2)
!**********************************************************************
      parameter (ngmax=20)
      common/srtdst/rlist(ngmax)
      common/srtord/ilist(ngmax)
      imax=n2-1
 10   continue
      iflag=0
      do 40 i=n1,imax
      if(rlist(i).gt.rlist(i+1)) go to 20
      go to 40
 20   continue
      itemp=ilist(i)
      rtemp=rlist(i)
      ilist(i)=ilist(i+1)
      rlist(i)=rlist(i+1)
      ilist(i+1)=itemp
      rlist(i+1)=rtemp
      iflag=1
 40   continue
      if(iflag.eq.0) return
      imax=imax-1
      go to 10
      end
