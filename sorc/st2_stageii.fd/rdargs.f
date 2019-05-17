      subroutine rdargs(iflend)
!
!     this subroutine reads the command line argument list
!
      include 'ppinp'
      include 'raddat'
!
      read(11,10,end=100) radid,rlat,rlon
      read(11,*,end=100) 
!
 10   format(a3,2f10.3)
      iflend=0
      return
 100  iflend=1
      return
      end
