      program mosaic
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! MAIN PROGRAM: MOSAIC      program to combine the Stage II precipitation
!   analysis at each radar site into a national product.
!   Programmer: Ying lin           ORG: NP22        Date: 2001-05-31  
!
! ABSTRACT: MOSAIC combines the Stage II precipitation analyses created 
!   on individual radar sites (on local 131x131 grids) into a national 
!   product, on the Office of Hydrology National HRAP grid (grid 240)
!
! I/O units
!   Input:
!     fort.5:   yyyymmddhh to be processed
!     fort.11:  list of DPA files ($ST2_PARAM/cards.parm)
!     fort.20:  The gg/ml/un/rd (i.e. dpa) data under each radar umbrella
!               (this is 'lunin', each file is opened within mosaic.f)
!   Output:
!     fort.51:  ST2gg${date0}.Grb
!     fort.52:  ST2ml${date0}.Grb
!     fort.53:  ST2un${date0}.Grb
!     fort.54:  ST2rd${date0}.Grb
!
! PROGRAM HISTORY LOG:
!
!    5/09/96
!        program to combine all radars into one grid
!        grib version
!    SP port: 21 Jan 2000 MEB
!    YL 2001/1/9   Use 4 digit year, yyyymmddhh in output file names.
!    YL 2001/1/23  Changed to official national HRAP grid
!    YL 2003/10/23 Corrected error in weights calculation for ST2rd
!                  (radar-only analysis)
!    YL 2004/1/7   Changed grid number from 240 to 255.  The grid we are 
!                  using is physically the same as the official 'HRAP grid', 
!                  but the grid specs for GRIB were supplied by hydrologists,
!                  who define grid co-ordinates  using the 'lower-left'
!                  corner of the grid box, while we (the meteorologist) 
!                  use the center of the grid box for grid co-ordinates.
!                  To keep the physical grid the same, our definition of 
!                  the corner points need to be shifted by half of a grid
!                  increment.  
!    YL 2012/07/11 set pds(23)=4 [PDS octet 26, for subcenter=EMC]
!    YL 2012/08/01 Convert to WCOSS use
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!$$$
      parameter (im=1121,jm=881)
      parameter(alat1=23.117,alon1=-119.023,dx=4762.5,alonv=-105.)
!
      dimension test(im,jm)
      dimension sow(im,jm)
      dimension prec(131,131)
!
      integer         kgds(200),kpds(200)
      integer         idat(8), idat1(8)
      real            rinc(5)
!
      character * 200  fname
      character *   7  oname
      character * 200  outdir,out2
      character       date*10,radid*3,radnam*3
      logical*1 ibitm(im,jm)
      real*8 timef
!
      CALL W3TAGB('MOSAIC ',2001,0151,0060,'NP22   ')
!
!  Keep track of CPU time:
      btim=timef()
      cpu_time=0.
!
      raddeg=3.14159265/180.
      deg=180./3.14159265
      rmin=-6.
      xinc=0.125
!
!  set up grid
!
      ilat1=nint(alat1*1000.)
      ilon1=nint(alon1*1000.)
      idx=nint(dx)
      ilonv=nint(alonv*1000.)
!
!  open input/output files
!
      lunin =20
      out2 = ' '
      call getenv('ST2_ST1',out2)
      len2 = index(out2,' ') -1
      if (len2.le.0) len2 = len(out2)
      outdir = ' '
      call getenv('ST2_OUT',outdir)
      lendir = index(outdir,' ') -1
      if (lendir.le.0) lendir = len(outdir)
!
      rinc=0.
      rinc(2)=-1.
!
      read(5,'(a10)') date
!
!  Name of output file (fort.51/52/53/54)
      oname(1:5)='fort.'
      do 100 nmo=1,4
        write(6,*) 'loop 100, nmo=', nmo
        krad=0
!  These are arrays:
        test=0.
        sow=0.
        ibitm=.false.
!
 10     read(11,20,end=50) radid,alatc,alonc
        read(11,*,end=50)
 20     format(a3,2f10.3)
!
        if (nmo.eq.1) then
          fname=outdir(1:lendir)//'/'//radid//'gg'//date//'z'
! Mosaicked output: ST2gg
        else if (nmo.eq.2) then
          fname=outdir(1:lendir)//'/'//radid//'ml'//date//'z'
! Mosaicked output: ST2ml
        else if (nmo.eq.4) then
          fname=outdir(1:lendir)//'/'//radid//'un'//date//'z'
! Mosaicked output: ST2un
        else
          fname=out2(1:len2)//'/'//radid//date//'Z'
! Mosaicked output: ST2rd
        endif
!
!  open input file
!
        close(lunin)
        write(6,*) 'Opening ', fname
        open(lunin,file=fname,form='unformatted')
        rewind(lunin)
!
!   read data
!
        read(lunin,err=10,end=10) radnam,rlat,rlon
        read(lunin,err=10) date
        write(6,*) 'fname= ', fname
        read(date,40) idat(1),idat(2),idat(3),idat(5)
 40     format(i4,3i2)
        read(lunin,err=10) prec
!
        krad=krad+1
        rlon=360.-rlon
        write(6,*) ' read ',radnam,rlat,rlon,krad
!
!  open grib file
!
        if (krad.eq.1) then
          write(oname(6:7),'(i2)') 50+nmo
          call baopen(50+nmo,oname,ierr)
          write(6,*) 'opening ', oname, ' ierr=', ierr
          call w3movdat(rinc,idat,idat1)
        endif
!
!  add to mosaic 
!
        call w3fb06(rlat,rlon,alat1,alon1,dx,alonv,xi,xj)
        istrt=nint(xi)-65
        jstrt=nint(xj)-65
        do ii=1,131
          do jj=1,131
            i1=ii+istrt
            j1=jj+jstrt
            if (i1.ge.1.and.j1.ge.1.and.                                      &
              i1.le.im.and.j1.le.jm.and.prec(ii,jj).ge.0.) then
!
!  Compute dist from radar using 1/dist for weight for now
!
              xx=float(ii-65)
              yy=float(jj-65)
              wgt=1.0
!original     if (nmo.eq.3.and.xx.ne.0.0.and.yy.ne.0.0)
              if (nmo.eq.3.and..not.(ii.eq.65.and.jj.eq.65))                  &
                 wgt=1.0/sqrt(xx*xx+yy*yy)
              if (wgt.gt.1.0) wgt=1.0
              test(i1,j1)=test(i1,j1)+prec(ii,jj)*wgt
              sow(i1,j1)=sow(i1,j1)+wgt
            endif
          enddo
        enddo
        goto 10
 50     continue
!
!  divide by the sum of the weights
!
        close(lunin)
        write(6,*) ' divide by sow'
        rmin=0.
        rmax=0.
        do i=1,im
          do j=1,jm
            if (sow(i,j).gt.0.) then
              test(i,j)=test(i,j)/sow(i,j)
              ibitm(i,j)=.true.
              if (test(i,j).gt.rmax) rmax=test(i,j)
            endif
          enddo
        enddo
!
!  set up kpds
!
!  pack to 0.1mm precision
!
        iscale=1
!
        kpds=0
!           (1)   - ID of center
        kpds(1)=7
!           (2)   - Generating process ID number
        kpds(2)=152
!           (3)   - Grid definition
        kpds(3)=255
!           (4)   - GDS/BMS flag (right adj copy of octet 8)
        kpds(4)=192
!           (5)   - Indicator of parameter
        kpds(5)=61
!           (6)   - Type of level
        kpds(6)=1
!           (7)   - Height/pressure, etc of level
        kpds(7)=0
!           (8)   - Year of century (2000=100, 2001=1)
        kpds(8)= mod(idat1(1),100)
        if (kpds(8).eq.0) kpds(8) = 100
!           (9)   - Month of year
        kpds(9)= idat1(2)
!           (10)  - Day of month
        kpds(10)=idat1(3)
!           (11)  - Hour of day
        kpds(11)=idat1(5)
!           (12)  - Minute of hour
        kpds(12)=0
!           (13)  - Indicator of forecast time unit
        kpds(13)=1
!           (14)  - Time range 1
        kpds(14)=0
!           (15)  - Time range 2
        kpds(15)=1
!           (16)  - Time range flag
        kpds(16)=4
!           (17)  - Number included in average
        kpds(17)=0
!           (18)  - Version nr of grib specification
        kpds(18)=1
!           (19)  - Version nr of parameter table
        kpds(19)=2
!           (20)  - NR missing from average/accumulation
        kpds(20)=0
!           (21)  - Century of reference time of data
        kpds(21)=idat1(1)/100 + 1 - kpds(8)/100
!           (22)  - Units decimal scale factor
        kpds(22)=iscale
!           (23)  - ID of NCEP Subcenter (EMC)
        kpds(23)=4
!           (25)  - used to distinguish the 3 runs (early; 6h and 18h later)
!  set up kgds
!
        kgds=0
!  Data rep type
        kgds( 1)=5
!  No pts x dir
        kgds( 2)=im
!  No pts y dir
        kgds( 3)=jm
!  Lat of 1st point in millideg
        kgds( 4)=ilat1
!  Lon of 1st point in millideg
        kgds( 5)=ilon1
!  Res and comp flag
        kgds( 6)=8
!  Orient of grid in millideg
        kgds( 7)=ilonv
!  X dir grid length in meters
        kgds( 8)=idx
!  Y dir grid length in meters
        kgds( 9)=idx
!
        kgds(10)=0
!  Scanning mode
        kgds(11)=64
!
        kgds(20)=255
!
        npts   = im*jm
!
!  Pack into grib
!
        call putgb(50+nmo,npts,kpds,kgds,ibitm,test,iret)
        write(6,*) 'putgb iret=', iret, ' kpds 21, 8, 9, 10, 11=',            &
          kpds(21), kpds(8), kpds(9), kpds(10), kpds(11)
!
        rewind(11)
 100  continue
      cpu_time=cpu_time+timef()-btim
      write(6,*) 'Time spent in MOSAIC (sec)=', cpu_time*0.001
      write(6,*) ' All done - bye'
      CALL W3TAGE('MOSAIC ')
      stop
      end
