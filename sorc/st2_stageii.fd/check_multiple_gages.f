!***********************************************************
      subroutine check_multiple_gages(nval,iu,iv,zz)
!***********************************************************
!
! check for multiple gages within a single HRAP bin: if multiple
! gages exist within a single HRAP bin, average them
!
      parameter (ndat=1000)
      dimension iu(nval),iv(nval),zz(nval),ndupl(ndat)

      do 2 i=1,nval
      ndupl(i)=1
    2 continue

      do 4 l=1,100000
      do 5 i=1,nval-1
      do 6 j=i+1,nval
      if(iu(i).eq.iu(j).and.iv(i).eq.iv(j)) then
      ndupl(i)=ndupl(i)+1
      zz(i)=zz(i)+zz(j)
      do 7 k=j,nval-1
      ndupl(k)=ndupl(k+1)
      iu(k)=iu(k+1)
      iv(k)=iv(k+1)
      zz(k)=zz(k+1)
    7 continue
      nval=nval-1
      go to 4
      endif
    6 continue
    5 continue
      go to 8
    4 continue

    8 do 9 i=1,nval
      if(ndupl(i).gt.1) zz(i)=zz(i)/ndupl(i)
    9 continue

      return
      end
