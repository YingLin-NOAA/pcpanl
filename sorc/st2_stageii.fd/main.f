      program stageii
!
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! MAIN PROGRAM: STAGEII      Program to perform Stage II analysis
!   Programmer: Ying lin           ORG: NP22        Date: 2001-06-01  
!
! ABSTRACT: MOSAIC combines the Stage II precipitation analyses created 
!   on individual radar sites (on local 131x131 grids) into a national 
!   product, on the Office of Hydrology National HRAP grid (grid 240)
!
! PROGRAM HISTORY LOG:
!
!    1996/??/?? MEB Got this code from OH
!    2001/1/23 YL   Use 4 digit year, yyyymmddhh in output file names.
!                   Changed to official national HRAP grid
!                   Streamlined the code, use w3lib to process date
!    2012/08/01 YL  Convert for WCOSS
! ABSTRACT:
!    
!   Stage ii processing main program
!   Subroutines called: rdargs,rdparam,wrinpt,rdmisc,radll2hr,
!                       ibins,rdprcp,savrg,mnqual,mndist,
!                       mnstat,mngage_new,mnmult_new,wrgrfl
!
!   date = date of data being processed (automatic/manual mode)
!
!   ifinrd,ifingg,ifinst = input data availability flags
!     ifinrd -- radar data
!     ifingg -- gage data
!     ifinst -- satellite data
!
!     ifinrd = 0 -- no radar data available
!            = 1 -- radar data available (some values > 0.0)
!            = 2 -- radar data available (all values = 0.0)
!     ifingg = 0 -- no gage data available
!            = 1 -- gage data available
!     ifinst = 0 -- no satellite data available
!            = 1 -- satellite data available
!            note that if there is no temperature data, then ifinst is
!              is reset to 0 even if satellite data is available
!
!     ifapr = ap removal test on/off flag
!           = 0 -- no ap removal test
!           = 1 -- ap removal test done as part of qc algorithm
!
!     iflign = ignore radar data flag set in stageiii
!                and read from fld table in database
!            = 0 - use radar data
!            = 1 - ignore radar data
!     bias = StageII bias calculated as part of multi sensor field
!              generation
!            initialized to StageI value in main
!     igg/iml = gage only/multisensor field calculation flag
!             = 0 -- no calculation
!             = 1 -- calculated
!             = 2 -- zero field
!....................................................................
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!$$$
      character*2 fldtyp,gg,ml
      character*10 date
      character type*5
      logical*4 qctest,tdtest,sttest,ggtest,mltest
!
      dimension ictt(15,15)
!
      include 'luprod'
      include 'ppinp'
      include 'prdflg'
      include 'gagloc'
      include 'raddat'
      include 'ggoparam'
      include 'mltparam'
      include 'flags'
!
      data ichr/0/,ngrep/0/
      data istat/0/
      data type/'stgii'/,gg/'gg'/,ml/'ml'/
!
!  Keep track of CPU time:
      real*8 timef
      CALL W3TAGB('STAGEII ',2001,0152,0060,'NP22   ')
      btim=timef()
      cpu_time=0.
!...........................................................................
!
!   Read input parameters
!
      call rdparam
!
!   Read miscellaneous input data including flags
!   print input summary
!
      call rdmisc
!
! Change the units of range parameters from km to HRAP
!
      rmesh=4.
      distmingg=distmingg/rmesh
      rangei=rangei/rmesh
      rangec=rangec/rmesh
      radius=radius/rmesh
      distminml=distminml/rmesh
!
      scali_def=scali_def/rmesh
      scale_def=scale_def/rmesh
!
!   Generate tables of coordinates of bins within radar umbrella
!
      call ibins
!.............................................................................
!   Calculate current hour of run and change date if necessary
!   Read radar, satellite, hourly/multiple hourly gage precip
!     and minimum surface temperature data corresponding to
!     current hour
!   Initialize bias to Stage I value
!
      read(5,'(a10)') date
!
!yl 
! 20120928 ihour was never set in the original code.  This means array 
! initialization in readgg's loop 10 was done incorrectly in F77 (ihour=0)
! In F90/95, the code blew up in readgg because the un-set ihour had a 
! very large (9-digit)
!
      read(date,'(8x,i2)') ihour
! 20121002 iflign, ifingg and ifinst are all uninitialized, any where.  ifingg
! is not used at all in the program.  In my F90/95 test (eddy) iflign had a 
! very large (9-digit) value, ifingg and ifinst were both zero.  We'll set both
! iflign and ifinst to zero, and leave ifingg alone.
      iflign = 0
      ifinst = 0
! 20121220: iflend is also uninitialized.  Had a 5-digit value on Tide.
      iflend = 0
!
   10 continue
      call rdargs(iflend)
!
!   convert radar lat/lon to National HRAP grid coordinates
!
      call radll2hr
!
      call wrinpt(date)
!
      if(iflend.eq.1) go to 20
      igg=0
      iml=0
!
      write(6,*) 'about to call rdprcp from main, date=', date
      call rdprcp(ngaghr,ngagmh,npseudo,                                      &
     &            ngrep,ihour,ngage,nhrtd,date,ictt,tsurf,bpps)
      bias=bpps
!
!   Save radar values corresponding to gage locations
!   update gagrad and pseudo tables with radar values
!   print out gage/radar table to output file
!
      if(ngage.gt.0) then
        call savrg(ihour,ngage,ihloc,jhloc,iflign)
      end if
!
!   Calculate algorithm execution tests
!
      qctest=(ifinrd.gt.0.and.iflign.eq.0) !radar present; no 'ignore rad' flag
      tdtest=(ifltd.eq.1.and.ngagmh.gt.0)  !
      sttest=(ngaghr.gt.0.or.ifinrd.gt.0)
      ggtest=(ngaghr.gt.0)
      mltest=(ifinrd.gt.0.and.iflign.eq.0)
!.......................................................................
!   Execute algorithm main programs
!
      if(qctest) call mnqual(ngage,ihour,ictt,tsurf,ihloc,jhloc)
!
      if(sttest) call mnstat(xmhat,vhat,phat,rhat,xmxr,xmgag,                 &
                             xmxg,ngaghr,ihour,iflign)
!
!  Write out stuff needed for bias removal
!
      if(ngaghr.gt.0) call wrgrfl(ihour,date,ngage,xmhat,phat,bpps)
!
      if(ifltyp.eq.1) then
         if(ggtest) call mngage_new(ngage,ihloc,jhloc,date,ihour,             &
            igg,xmhat,xmgag,phat,xmxr,xmxg,ngaghr)
!
         if(mltest) call mnmult_new(ngage,ihloc,jhloc,date,ihour,             &
            rmesh,bpps,bias,iml,xmhat,xmgag,phat,xmxr,xmxg,rhat)
      end if
!
      fldtyp="  "
      if(ggtest) fldtyp=gg
      if(mltest) fldtyp=ml
!
      GO TO 10
!..............................................................................
!   Final processing
!
   20 continue
!
    1 FORMAT(/,2x,'*** Informix error #',I8,' attempting to open',            &
         1x,'database -- program stopping ***')
    2 FORMAT(/,2x,'*** Informix error #',I8,' attempting to close',           &
         1x,'database ***')
    3 format(/,2x,'*** Informix error #',i8,' attempting to write',           &
         1x,'to siilog table ***')
    4 format(/,4x,'cpu time used=',f6.2,' sec -- elapsed time=',              &
         i3,' sec')
    6 format(/,2x,'*** ERROR -- ',a3,'  NOT FOUND IN radarloc table')
    7 format(/,3x,'*** radar data ignored ***')
    8 format(/,2x,'*** Informix error #',i8,' attempting to write',           &
         1x,'to siistat table ***')
!
      cpu_time=cpu_time+timef()-btim
      write(6,*) 'Time spent in STAGEII (sec)=', cpu_time*0.001
!
      CALL W3TAGE('STAGEII ')
      stop
      end
