      subroutine rdmisc
!
!   This subroutine reads miscellaneous input data
!   Calling subroutine: main program
!
      dimension xmptim(131,131)
!
      include 'luqual'
      include 'mis'
      include 'lugage'
      include 'ppinp'
      include 'flags'
      common/xmp/xmptim
!
!   Read other flags
!
! luflg=16 (set in stageii.f)
      read(luflg,*) ifapr
      read(luflg,*) ifvar
      read(luflg,*) ifltd
      read(luflg,*) ifltyp
!
      return
      end
