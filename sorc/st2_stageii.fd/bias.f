      subroutine bias(date,bpps,b)
!
!   this subroutine calculates the radar mean field bias.  because
!     it uses data from hours in the future and past, gage/radar tables
!     must be searched and hrap coordinates of the gages must be
!     generated.
!   a scatter plot of radar value vs gage value for the current hour
!     is also generated.
!MEB or maybe not...
!
!   calling subroutine: mnmult
!   subroutines called: rdgrt,bfbias
!
!   b = mean field bias
!   bl = sample log bias
!   bpps = bias calculated in pps program
!   ngag = number of gage values > gmin for each hour
!   gag  = values for each gage at hour = k
!   rad  = radar values corresponding to gag
!   ng1hr = number of gages found in gagrad table for current
!            date and hour with duration = 1 hour
!           if no gages are found, then do not calculate a bias
!
      character*10 date, date1
      character*8 gid(300)
      dimension rad(300),gag(300),error(300)
      dimension bl(0:23),ngag(0:23),bppk(0:23)
      dimension ih1(300),jh1(300), idat(8), idat1(8), rinc(5)
!
      include 'ppinp'
      include 'raddat'
      include 'cnst'
      include 'mltparam'
      include 'gagloc'
      include 'mis'
!
!   *** determine the rainy period before the current hour and compute
!     the sample bias ***
!      k = number of hour(s) back
!      read xmhat,phat and pps bias from k hour(s) before current hour
!      if not available, then set kfor and jump out of loop
!
      read(date,10) idat(1), idat(2), idat(3), idat(5)
 10   format(i4,3i2.2)
!
      write(6,*) 'in BIAS, kmax=', kmax
      do 100 k=0,kmax
        rinc(2)=float(-k)
        call w3movdat(rinc,idat,idat1)
        write(date1,10) idat1(1), idat1(2), idat1(3), idat1(5)
!
!       read gage values and corresponding radar values
        call rdgrt(date1,ng1hr,xmhat,phat,bpps,gag,rad,gid,ih1,jh1,iflavl)
        if(iflavl.eq.1) then
          bppk(k)=bpps
          if(xmhat.le.thr1.and.phat.ge.thr2) then
            kfor=k-1
            go to 150
          end if
        else
          kfor=k-1
          go to 150
        end if
!
        if(ng1hr.gt.0) then
!
! perform linear regression between radar and gage data points
! passing through the origin
!
          npair=0
          sumgr=0.
          sumr2=0.
          slope=1.
!
          do 110 i=1,ng1hr
            if(gag(i).le.gmin) go to 110
            if(rad(i).le.gmin) go to 110
            npair=npair+1
            sumgr=sumgr+gag(i)*rad(i)
            sumr2=sumr2+rad(i)*rad(i)
  110     continue
!
          if (sumr2.gt.0.) slope=sumgr/sumr2
!
! compute error statistics
!
          sume1=0.
          sume2=0.
          do 120 i=1,ng1hr
            if(gag(i).le.gmin) go to 120
            if(rad(i).le.gmin) go to 120
            error(i)=gag(i)-slope*rad(i)
            sume1=sume1+error(i)
            sume2=sume2+error(i)*error(i)
  120     continue
!
          if(npair.le.1) then
            bl(k)=-99.
            go to 100
          endif
!
          ave=sume1/npair
          std=sume2/(npair-1)-sume1*sume1/(npair*(npair-1))
!
          if(std.le.0.) then
            bl(k)=-99.
            go to 100
          endif
          std=sqrt(std)
!
! standardize errors
!
          do 130 i=1,ng1hr
          if(gag(i).le.gmin.or.rad(i).le.gmin) then
            error(i)=-999.
          else
            error(i)=(error(i)-ave)/std
          end if
  130   continue
!
!
        ngag(k)=0
        ggsum=0.0
        rdsum=0.0
!
        do 50 ii=1,ng1hr
          ih=ih1(ii)
          jh=jh1(ii)
!
          if(gag(ii).gt.gmin.and.misbin(ih,jh).eq.1) then
            if(rad(ii).gt.gmin) then
              if(abs(error(ii)).le.errmax) then
                ngag(k)=ngag(k)+1
                ggsum=ggsum+gag(ii)
                rdsum=rdsum+rad(ii)
              endif
            endif
          end if
   50   continue
!
        if(ngag(k).eq.0.or.rdsum.lt.0.0001) then
          bl(k)=-99.
        else
          bl(k)=alog(ggsum/rdsum/bpps)
        end if
!
      else
        ngag(k)=0
        bl(k)=-99.
      end if
!
  100 continue
      kfor=kmax
!
!   *** compute forward log bias and variance ***
!
  150 continue
      bf=0.0
      sf=b2
      call bfbias(bf,sf,kfor,ngag,bl)
!
      if(idebug.gt.0) then
        if(kfor.ge.0) then
          write(6,4)
          do 160 k=0,kfor
            write(6,1) k,ngag(k),bppk(k),bl(k)
  160     continue
          write(6,2) bf,sf
        end if
      end if
!
!   *** Determine the rainy period after current hour and compute
!         sample bias ***
!      k = number of hour(s) forward
!      read xmhat,phat and pps bias from k hour(s) after current hour
!      if not available, then set kbac and jump out of loop
!
      do 200 k=0,kmax
        rinc(2)=float(k)
        call w3movdat(rinc,idat,idat1)
        write(date1,10) idat1(1), idat1(2), idat1(3), idat1(5)
!
!     read gage values and corresponding radar values
      write(6,*) 'k=',k,' date1=', date1
      call rdgrt(date1,ng1hr,xmhat,phat,bpps,gag,rad,gid,ih1,jh1,iflavl)
!
        if(iflavl.eq.1) then
          bppk(k)=bpps
          if(xmhat.le.thr1.and.phat.ge.thr2) then
            kbac=k-1
            go to 250
          end if
        else
          kbac=k-1
          go to 250
        end if
!
        if(ng1hr.gt.0) then
!
! Perform linear regression between radar and gage data points
! passing through the origin
!
          npair=0
          sumgr=0.
          sumr2=0.
          slope=1.
!
          do 210 i=1,ng1hr
            if(gag(i).le.gmin) go to 210
            if(rad(i).le.gmin) go to 210
            npair=npair+1
            sumgr=sumgr+gag(i)*rad(i)
            sumr2=sumr2+rad(i)*rad(i)
  210     continue
          if (sumr2.gt.0.) slope=sumgr/sumr2
!
! compute error statistics
!
          sume1=0.
          sume2=0.
          do 220 i=1,ng1hr
            if(gag(i).le.gmin) go to 220
            if(rad(i).le.gmin) go to 220
            error(i)=gag(i)-slope*rad(i)
            sume1=sume1+error(i)
            sume2=sume2+error(i)*error(i)
  220     continue
!
          if(npair.le.1) then
            bl(k)=-99.
            go to 200
          endif
!
          ave=sume1/npair
          std=sume2/(npair-1)-sume1*sume1/(npair*(npair-1))
          if(std.le.0.) then
            bl(k)=-99.
            go to 200
          endif
          std=sqrt(std)
!
! standardize errors
!
          do 230 i=1,ng1hr
            if(gag(i).le.gmin) go to 230
            if(rad(i).le.gmin) go to 230
            error(i)=(error(i)-ave)/std
  230     continue
!
          ngag(k)=0
          ggsum=0.0
          rdsum=0.0
!
          do 170 ii=1,ng1hr
            ih=ih1(ii)
            jh=jh1(ii)
!
            if(gag(ii).gt.gmin.and.misbin(ih,jh).eq.1) then
              if(rad(ii).gt.gmin) then
                if(abs(error(ii)).le.errmax) then
                  ngag(k)=ngag(k)+1
                  ggsum=ggsum+gag(ii)
                  rdsum=rdsum+rad(ii)
                endif
              end if
            end if
  170     continue
!
          if(ngag(k).eq.0.or.rdsum.lt.0.0001) then
            bl(k)=-99.
          else
            bl(k)=alog(ggsum/rdsum/bpps)
          end if
!       else
          ngag(k)=0
          bl(k)=-99.
        end if
! The above concludes the BIG 'if' block ["if(ng1hr.gt.0)" - just before
!  'do 210')
!
  200 continue
      kbac=kmax
!
!   *** COMPUTE BACKWARD LOG BIAS AND VARIANCE ***
!
  250 continue
      br=0.0
      sr=b2
      call bfbias(br,sr,kbac,ngag,bl)
!
      if(idebug.gt.0) then
        if(kbac.gt.0) then
          write(6,4)
          do 275 k=0,kbac
            write(6,1) k,ngag(k),bppk(k),bl(k)
  275     continue
          write(6,3) br,sr
        end if
      end if
!
!   *** Compute log bias and variance ***
!
      c1=sr/(sf+sr)
      bb=c1*bf+(1.0-c1)*br
      ss=sf*sr/(sf+sr)
!
!   *** Compute bias and variance ***
!
      b=exp(bb+0.5*ss)
      v=(b*b)*(exp(ss)-1.0)
      if(idebug.gt.0) write(6,6) bb,ss,c1,v,b
!
!   *** If coefficient of variation of bias estimator >= vmin, then
!         set bias to stagei bias value ***
!
      if(((v**(0.5))/b).ge.vmin) then
        b=bpps
        write(6,9)
      end if
!
      RETURN
    1 FORMAT(' ',I2,4X,I2,6X,F5.2,4X,F6.2)
    2 FORMAT(' BF=',F5.2,3X,'SF=',F5.2)
    3 FORMAT(' BR=',F5.2,3X,'SR=',F5.2)
    4 FORMAT(/,1X,'K',3X,'NGAG(K)',3X,'BPPS(K)',3X,'BL(K)')
    6 FORMAT(/,2X,'BB=',F6.3,2X,'SS=',F6.3,2X,'C1=',F6.3,2X,'V=',             &
         F6.3,2X,'BIAS=',F6.3)
    7 FORMAT(/,2X,'BACKWARD LOG BIAS CALC :  GGSUM=',2X,'RDSUM=',F8.3,        &
         F8.3,2X,'BPPS=',F8.3)
    8 FORMAT(/,2X,'FORWARD LOG BIAS CALC :  GGSUM=',2X,'RDSUM=',F8.3,         &
         F8.3,2X,'BPPS=',F8.3)
    9 format(/,2x,'*** StageII bias set equal to StageI bias ***')
      END

