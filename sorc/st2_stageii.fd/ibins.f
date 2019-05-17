      subroutine ibins
!
!   This subroutine fills iok and jok arrays with all coordinates
!     of boxes inside a circle of radius equal to 66 boxes
!   Calling subroutine: main program
!
      include 'bins'
!
      nok=0
!
      do 100 i=1,131
      do 100 j=1,131
!                                  4356=66*66
        if(((i-66)*(i-66)+(j-66)*(j-66)).le.4356) then 
          nok=nok+1
          iok(nok)=i
          jok(nok)=j
        end if
!
  100 continue
      return
      end
