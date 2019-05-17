      subroutine rdparam
!
!   This subroutine reads the parameters for the quality control,
!     summary statistics, gage only field and multisensor field
!     analyses.
!   Calling subroutine: main program
!
      include 'cnqu'
      include 'cnst'
      include 'ggoparam'
      include 'mltparam'
      include 'luqual'
      include 'lustat'
      include 'lugage'
      include 'lumult'
!
! Read from 'qual.con':
      read(luqlcn,*) tmin
      read(luqlcn,*) rmax
!
! Read from 'stat.con':
      read(lustcn,*) thr1
      read(lustcn,*) thr2
      read(lustcn,*) thr3
      read(lustcn,*) rcli
!
! Read from 'gageo.con':
      read(lugacn,*) idmax
      read(lugacn,*) iwind
      read(lugacn,*) grmin
      read(lugacn,*) idrmin
      read(lugacn,*) distmingg
      read(lugacn,*) itygg
      read(lugacn,*) nbrsgg
      read(lugacn,*) rainmingg
      read(lugacn,*) rangei
      read(lugacn,*) cor0i
      read(lugacn,*) cor0pi
      read(lugacn,*) rangec
      read(lugacn,*) cor0c
      read(lugacn,*) cor0pc
      read(lugacn,*) radius
!
! Read from 'mult.con'
      read(lumtcn,*) gmin
      read(lumtcn,*) vmin
      read(lumtcn,*) b1
      read(lumtcn,*) b2
      read(lumtcn,*) b3
      read(lumtcn,*) b4
      read(lumtcn,*) sc
      read(lumtcn,*) kmax
      read(lumtcn,*) errmax
      read(lumtcn,*) ityml
      read(lumtcn,*) nbrsml
      read(lumtcn,*) distminml
      read(lumtcn,*) rainminml
      read(lumtcn,*) crscori
      read(lumtcn,*) crscorc
      read(lumtcn,*) scali_def
      read(lumtcn,*) scale_def
      read(lumtcn,*) scali_max
      read(lumtcn,*) scale_max
      read(lumtcn,*) threshmin
!
      return
      end
