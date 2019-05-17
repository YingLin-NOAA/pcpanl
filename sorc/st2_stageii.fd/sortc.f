!**********************************************************************
          subroutine sortc(ng)
!**********************************************************************
      parameter (ndat=1000)
          common/big1/u(ndat),v(ndat),z(ndat)
          common/big3/irv(ndat)
          imax=ng-1
 10       continue
          iflag=0
          do 40 i=1,imax
          if(u(i).gt.u(i+1)) go to 20
          go to 40
 20       continue
          zz=z(i)
          uu=u(i)
          vv=v(i)
          ii=irv(i)
          z(i)=z(i+1)
          u(i)=u(i+1)
          v(i)=v(i+1)
          irv(i)=irv(i+1)
          z(i+1)=zz
          u(i+1)=uu
          v(i+1)=vv
          irv(i+1)=ii
          iflag=1
 40       continue
          if(iflag.eq.0) return
          imax=imax-1
          go to 10
          end
