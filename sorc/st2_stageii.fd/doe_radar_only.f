!*******************************************************************
      subroutine doe_radar_only(crscori,crscorc,rainminml,f0,vari,            &
                                cave,cvar,rad_rain,estim,ifvar,varia)
!*******************************************************************
!
! subroutine performs radar-only estimation 
!
      if(rad_rain.ge.rainminml) then
      zi=1.
      else
      zi=0.
      endif
!
! estimate probability of rainfall
!
      cprob=(1.-f0)+crscori*(zi-(1.-f0))
!
! estimate rainfall given raining
!
      cpr=(1.-f0)*crscori+f0
      crs_cov=cvar*crscorc*cpr
      con_cov=(cvar+cave*cave*(1.-cpr))*cpr
      wgt=crs_cov/con_cov
      cex=cave*cpr
      esti2=cave+wgt*(rad_rain-cex)
      if(ifvar.eq.1) vari2=cvar-wgt*crs_cov
!
      if(ifvar.eq.1.and.vari2.lt.0.) then
      write(6,*) 'vari2 < 0 in smooth transition operations',vari2
      stop
      endif
!
      estim=cprob*esti2
      if(ifvar.eq.1) varia=vari2*cprob+esti2*esti2*cprob*(1.-cprob)
      return
      end
