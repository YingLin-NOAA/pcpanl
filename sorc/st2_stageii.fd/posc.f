!**********************************************************************
          subroutine posc(u,v,irv,ndata,u0,in)
!**********************************************************************
         dimension u(ndata),v(ndata),irv(ndata)
          do 4 i=1,ndata
          if(u0.le.u(i)) go to 5
 4        continue
          in=ndata
          return
 5        continue
          in=i
          return
          end
