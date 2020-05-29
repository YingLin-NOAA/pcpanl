      subroutine grib2_wrt_g2func_hrap(nx,ny,alat1,alon1,                     &
                     fld,bmap,vdate,acc,iptable,lugb,ierr)
!-----------------------------------------------------------------
! ABSTRACT: This routine is to write out a new grib2 file
!   March 2013:     J.Wang
!   23 September 2013: Revised by Youlong Xia for NLDAS grib2 output   
!   21 March 2016: Xia Y.,Revised for NDAS forcing output
!   30 Oct 2017 Y Lin modified for precip URMA
!     This program is hardwired to write out an 8km cmorph file:
!     grid spec is fixed.  It can be called to write out either the hourly 
!     total, or the sum of 6h total (use the iacc argument)
!   05 Aug 2019 Y Lin modified for converting ST4 output to GRIB2
!     HRAP grid (national or regional): Polar-stereographic
!     
!     Add input nx, ny, alat, alon
!     Fixed fields: dx, dy, alonv (LOV grid orientation)
! Arguments:
!   fld      the (APCP) field
!   bmap     bitmap of fld
!   vdate    valid time yyyymmddhh in character*10
!   acc     accumulating hours (1.0,6.0 etc.)
!   iptable  GRIB2 look-up table to APCP
!   lugb     unit for the grib2 output
!   ierr     error code
!-----------------------------------------------------------------
!
      implicit none
      integer, parameter   :: max_bytes=100000000
      real, parameter :: dx= 4.7625, dy= 4.7625, alonv=255., alad=60.
! 
! From http://www2.nco.ncep.noaa.gov/sib/grib2/examples/nesdis/HEtoGrib2.f90:
      real, parameter :: res_comp_flag=8 
      integer nx, ny
      real(4) alat1, alon1
      integer iptable(13) ! ipds table
      integer scan_mode
      character vdate*10
      real acc
!
! valid time (idat), reference time (idat0: starting time of accumulation)
!   idat(1)=yyyy
!   idat(2)=mm
!   idat(3)=dd
!   idat(5)=hh
!
      integer idat(8), idat0(8)
! Use w3movdat (rinc(2)=-acc) to compute idat0 from idat. 
      real    rinc(5)
!
      integer listsec0(2)
      integer listsec1(13)
      integer igds(5)
      integer igdstmpllen
      integer ipdstmpllen
      integer idrstmpllen
      integer idrsnum,ibmap,numcoord,ipdsnum,idefnum    
 
      integer,dimension(100) :: igdstmpl
      integer,dimension(100) :: ipdstmpl
      integer,dimension(100) :: idrstmpl
!
      integer ideflist(1)
      real(4) coordlist(1)
!
      character*1 cgrib(max_bytes)
      logical*1 bmap(nx,ny)
!
      real(4),dimension(nx*ny) :: fld
      integer ifilw,i,j,lengrib,lon1,lon2,lat1,lat2,idx,idy,ierr
      integer yyyy,mm,dd,hh,lugb
!
      write(6,*) 'in grib2_wrt_g2func_hrap, alat1, alon1=', alat1, alon1
!
! code start
!-----------------------------------------------------------------
!
! Set scan_mode to 64 so that the order would be WE:SN:
      scan_mode=64
!-- set file unit
      ifilw=lugb
!
      idat=0
      rinc=0.
      read(vdate,'(i4,3i2)') idat(1), idat(2), idat(3), idat(5)
      rinc(2)=-acc
      call w3movdat(rinc,idat,idat0)
      write(6,*) 'idat=', idat
      write(6,*) 'idat0=', idat0

! -------------- add field  -----------------------------------------
      cgrib=''
!
!-- section 0: indicator section 
      listsec0(1)=iptable(1) !Discipline: table 0.0 
!---- (0:Meteorological;1: Hydrlogical; 2:Land) ----------------------
      listsec0(2)=2         ! grib edition number (2:grib2)
!
!-- section 1: identification section
      listsec1(1)=7  ! Identification of orginating center (Table 0)  (7:ncep)
      listsec1(2)=4  ! Identification of orginating subcenter (ON388-Table C) (4:emc)
      listsec1(3)=2         ! GRIB master tables version number (Table 1.0)  (2: Nov 2003)
      listsec1(4)=1         ! Version number of GRIB local tables used to augment Master Tables (Table 1.1)
      listsec1(5)=0         ! Significance of reference time (Table 1.2) (0:ana 1:start of fcst 2:vrfy 3:obs)
      listsec1(6)=idat0(1)  ! Reference time - Year (4 digits)
      listsec1(7)=idat0(2)  ! Reference time - Month
      listsec1(8)=idat0(3)  ! Reference time - Day
      listsec1(9)=idat0(5)  ! Reference time - Hour
      listsec1(10)=0        ! Reference time - Minute
      listsec1(11)=0        ! Reference time - Second
      listsec1(12)=0        ! Production status of data (Table 1.3) (0:opn products 1:opn test products)
      listsec1(13)=0        ! Type of processed data (Table 1.4) (0:ana products 1:fcst products 2:ana & fcst 3: cntl fcst)

       call gribcreate(cgrib,max_bytes,listsec0,listsec1,ierr)
      print*,'gribcreate status=',ierr
!
!-- section 3: grid definition section
      igds(1)=0             ! Source of grid definition (Table 3.0) (0:specified in the code)
      igds(2)=nx*ny         ! Number of grid points in the defined grid
      igds(3)=0             ! Number of octets for optional list of numbers defining number of points 
      igds(4)=0             ! Interpretation of list of numbers defining number of points 
!-- example: Lat/lon
      igds(5)=20            ! Grid definition template number (Table 3.1) 
                            !   (20: polarstereographic)
      igdstmpl=0           
      if( igds(5)==20) then
! Imitating 
!   http://www2.nco.ncep.noaa.gov/sib/grib2/examples/nesdis/HEtoGrib2.f90
!       call apply_template_3dot20_nesdis(igdstmpl,igdstmplen,nx,ny) 
!       subroutine apply_template_3dot20_nesdis(ifield3,len3,nx,ny) 
!   
        igdstmpllen=18        
!
!-- set up grid definition template 3.0
        igdstmpl=0

!       igdstmpl(1) = 0 !Earth assumed spherical with radius of 6,367,470.0m
!       igdstmpl(1) = 8 !Earth assumed spherical with radius of 6,371,200.0m
!       igdstmpl(2) = 0
!       igdstmpl(3) = 0
        igdstmpl(1) = 1 !Earth assumed spherical with radius specified by data producer
        igdstmpl(2) = 0 !that is, no change here
        igdstmpl(3) = 6371200
        igdstmpl(4) = 0
        igdstmpl(5) = 0
        igdstmpl(6) = 0
        igdstmpl(7) = 0
        igdstmpl(8) = nx
        igdstmpl(9) = ny
        igdstmpl(10) = alat1*1000000
        igdstmpl(11) = alon1*1000000
        igdstmpl(12) = res_comp_flag 
        igdstmpl(13) = alad*1000000  ! latitude where dx/dy are specified 
        igdstmpl(14) = alonv*1000000 ! orientation of the grid
        igdstmpl(15) = dx*1000000
        igdstmpl(16) = dy*1000000
        igdstmpl(17) = 0   !projection center flag
        igdstmpl(18) = scan_mode
      endif 
!
      idefnum=1             ! Used if igds(3) .ne. 0. The number of entries in array ideflist
      ideflist=0            ! Used if igds(3) .ne. 0. number of grid points contained in each row ( or column ), Dummy array otherwise
      call addgrid(cgrib,max_bytes,igds,igdstmpl,igdstmpllen,ideflist,        &
                   idefnum,ierr)
!      print*,'addgrid status=',ierr
!
!-- section 4: product definition section
      ipdstmpl=0
! ------------ product definition for simultaneous variable ----------
! YL 2019/08/06: removed if block for iptable(2).eq.0 (instantaneous variables)
!   Should always have iptables(2).eq.8 (average or accumulating variable)
! ----- product difinition for average or accumulation variable -----
      if(iptable(2).eq.8) then   
      ipdsnum=iptable(2)    ! Product Definition Template
!     Number (Table 4.8) (0: at a point in time; 8 for average or accumulation)
      ipdstmpllen=iptable(3)  ! pdt template length
      ipdstmpl(1)=iptable(4)  ! catogory
      ipdstmpl(2)=iptable(5)  ! parameter (8: total precip)
      ipdstmpl(3)=2           ! Type of generating process (Table 4.3)
                              ! (0:ana, 1:ic, 2:fcst) - choose 2 to conform 
                              ! to WMO header for tocgrib2 for pre-v2.8 pcpurma
      ipdstmpl(4)=0           ! Background generating process identifier
      ipdstmpl(5)=182         ! River Forecast Center Quantitative Precipitation estimate mosaic generated by NCEP (https://www.nco.ncep.noaa.gov/pmb/docs/on388/tablea.html)
      ipdstmpl(6)=0            ! Hours of observational data cutoff after reference time
      ipdstmpl(7)=0            ! Minutes of observational data cutoff after reference time
      ipdstmpl(8)=1            ! Indicator of unit of time range (Table4.4) (0:minute, 1:hour 2:day)
      ipdstmpl(9)=0            ! Forecast time in units defined by ipdstmpl(8)
      ipdstmpl(10)=iptable(6)  ! Type of first fixed surface (see Code table 4.5) (100:isobaric level)
      ipdstmpl(11)=iptable(7)  ! Scale factor of first fixed surface
      ipdstmpl(12)=iptable(8)  ! Scaled value of first fixed surface
      ipdstmpl(13)=iptable(9)  ! Type of first second surface (see Code table 4.5)
      ipdstmpl(14)=iptable(10) ! Scale factor of second fixed surface
      ipdstmpl(15)=iptable(11) ! Scaled value of second fixed surface
      ipdstmpl(16)=idat(1)     ! End year of overall time interval 
      ipdstmpl(17)=idat(2)     ! End month
      ipdstmpl(18)=idat(3)     ! End day
      ipdstmpl(19)=idat(5)     ! End hour
      ipdstmpl(20)=0           ! End minute
      ipdstmpl(21)=0           ! End second
      ipdstmpl(22)=1           ! number of time ranges
      ipdstmpl(23)=255         ! total number of data values missing
      ipdstmpl(24)=1           ! accumulation
      ipdstmpl(25)=2           ! 1: analysis, 2: forecast from table 4.11
      ipdstmpl(26)=1
      ipdstmpl(27)=acc
      ipdstmpl(28)=255
      ipdstmpl(29)=0
      ipdstmpl(30)=0
      endif

      numcoord=0               ! Number of coordinate values after template 
      coordlist=0.             ! Optional list of coordinate values
! ----------- end of Section 4 -----------------------------------------    
!-- section 5: Data Representation Section
      idrstmpl=0
      idrsnum=3             ! Data representation section template number (Table 5.0) (3:Grid Point Data - Complex Packing and Spatial Differencing)
      idrstmpllen=18            ! Length of Data representation section
      idrstmpl(2)=0             ! Binary scale factor
      idrstmpl(3)=iptable(13)   ! Decimal scale factor
      idrstmpl(7)=0             ! Missing value management used (see Code Table 5.5)
      idrstmpl(8)=0             ! Primary missing value substitute
      idrstmpl(9)=0             ! Secondary missing value substitute 
      idrstmpl(17)=1            ! Order of spatial difference (see Code Table 5.6, 2 does not work for Noah output and get segmetation fault) 
!
!-- section 6:       
      ibmap=0  ! Bit-map indicator (Table 6.0) (0:A bit map applies, 255:A bit map doesn't apply)
!
      call addfield(cgrib,max_bytes,ipdsnum,ipdstmpl,ipdstmpllen,             &
                    coordlist,numcoord,idrsnum,idrstmpl,idrstmpllen,          &
                    fld,nx*ny,ibmap,bmap,ierr)
      if(ierr.ne.0) then
      print*,'addfield fails and status=',ierr
      endif  ! if(iptable(2).eq.8)
!      print*,'addfield status=',ierr

!-- finalize  GRIB message after all section
!-- adds the End Section ( "7777" )
      call gribend(cgrib,max_bytes,lengrib,ierr)
!      print*,'gribend status=',ierr
!      print*,'length of the final GRIB2 message in octets =',lengrib
!
      call wryte(ifilw, lengrib, cgrib)

      end subroutine grib2_wrt_g2func_hrap

