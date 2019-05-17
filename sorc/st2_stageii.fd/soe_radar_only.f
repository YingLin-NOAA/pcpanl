!*****************************************************************
      subroutine soe_radar_only(crscori,crscorc,uave,uvar,f0,vari,            &
                   cave,cvar,rad_rain,estim,ifvar,varia)
!*****************************************************************
!
! subroutine performs radar-only Single Optimal Estimation
!
      denum=cvar*vari*crscorc*crscori                                         &
           +cave*cave*vari*crscori                                            &
           +cvar*f0*f0*crscorc
      denom=cvar*f0+cave*cave*vari
      wgt=denum/denom
      estim=uave+wgt*(rad_rain-uave)
      if(ifvar.eq.1) varia=uvar-wgt*denom
      return
      end
