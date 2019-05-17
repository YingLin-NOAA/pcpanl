!*********************************************************
      subroutine cor_scale(rad,scali,scale,rmesh,iflag)
!*********************************************************
!
! estimate spatial correlation coefficients
!
      parameter (nx=131,ny=131)
      dimension rad(nx,ny)

      numbi=0
      sumi1=0.
      sumih=0.
      sumiv=0.
      numbc=0
      sumc1=0.
      sumc2=0.
      sumch=0.
      sumcv=0.

      do 10 i=1,nx-1
      do 20 j=1,ny-1
      rain0=rad(i,j)
      rainh=rad(i+1,j)
      rainv=rad(i,j+1)
      if(rain0.lt.0.) go to 20
      if(rainh.lt.0.) go to 20
      if(rainv.lt.0.) go to 20
      if(rain0.gt.0.) then
      irain0=1
      else
      irain0=0
      endif
      if(rainh.gt.0.) then
      irainh=1
      else
      irainh=0
      endif
      if(rainv.gt.0.) then
      irainv=1
      else
      irainv=0
      endif
      numbi=numbi+1
      sumi1=sumi1+irain0
      sumih=sumih+irain0*irainh
      sumiv=sumiv+irain0*irainv
      if(rain0.eq.0.) go to 20
      if(rainh.eq.0.) go to 20
      if(rainv.eq.0.) go to 20
      numbc=numbc+1
      sumc1=sumc1+rain0
      sumc2=sumc2+rain0*rain0
      sumch=sumch+rain0*rainh
      sumcv=sumcv+rain0*rainv
   20 continue
   10 continue
      if(numbi.eq.0) go to 999
      if(numbc.le.1) go to 999
!
! compute lag-1 indicator correlation coefficient
!
      avei=sumi1/numbi
      vari=avei*(1.-avei)

      if(vari.le.0.) go to 999

      rhoi=0.5*(sumih+sumiv)/numbi-avei*avei
      rhoi=rhoi/vari

      if(rhoi.lt.0.) go to 999
      if(rhoi.gt.1.) go to 999
!
! compute lag-1 conditional correlation coefficient
!
      avec=sumc1/numbc
      varc=sumc2/(numbc-1)-sumc1*sumc1/(numbc*(numbc-1))

      if(varc.le.0.) go to 999

      rhoc=0.5*(sumch+sumcv)/numbc-avec*avec
      rhoc=rhoc/varc

      if(rhoc.lt.0.) go to 999
      if(rhoc.gt.1.) go to 999
!
! assuming exponential model
!
      scali=-rmesh/alog(rhoi)
      scale=-rmesh/alog(rhoc)

      iflag=0
      return

  999 iflag=1
      return
      end
