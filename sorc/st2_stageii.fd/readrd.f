      subroutine readrd(lurad,date,bpps)
!
!   This subroutine reads the hourly radar precip accumulation data
!     from stagei 
!   It also reads the stagei bias from the database.
!
!   calling subroutine: rdprcp
!
!   ifinrd = radar availability flag
!          = 0 -- no radar data available
!          = 1 -- radar data available (some values > 0.0)
!          = 2 -- radar data available (all values = 0.0)
!
      character date*10,outdir*180,radnam*3
!
      dimension radar(131,131)
      common/radcom/radar
      include 'ppinp'
      include 'bins'
      include 'mis'
      include 'raddat'
!
!   if no radar data available, then Stage I bias set to 1.0
!
      ifinrd=0
      bpps=1.0
!
!   get radar data pathname
!
      outdir = ' '
      call getenv('ST2_ST1',outdir)
      lendir = index(outdir,' ') -1
      if (lendir.le.0) lendir = len(outdir)
!
      filrad=outdir(1:lendir)//'/'//radid//date//'Z'
!
      open(lurad,file=filrad,form='unformatted')
!
!   read radar data file
!   (divide out stagei bias which has previously been applied)
!MEB  not sure about this  ccc
!
      read(lurad,end=100,err=90) radnam,rlat,rlon,bias
      read(lurad,end=100,err=90) date
      read(lurad,end=100,err=90) radar
      close(lurad)
!
!  Make up misbin array
!    
      do i=1,131
        do j=1,131
          misbin(i,j)=1
          if (radar(i,j).lt.0.0) misbin(i,j)=0
        enddo
      enddo
      iflmis=1
      nbin=0
      nbin1=0
!
      do i=1,131
        do j=1,131
          nbin=nbin+misbin(i,j)
          if(j.ne.131) nbin1=nbin1+misbin(i,j+1)
        enddo
      enddo
!
      ntot=0
      do 20 ibin=1,nok
        i=iok(ibin)
        j=jok(ibin)
        if(radar(i,j).le.azhmm) then
          radar(i,j)=0.0
          ntot=ntot+1
        endif
   20 continue
      ifinrd=1
      bpps=bias
      if (ntot.eq.nok) then
        ifinrd=2
        bpps=1.0
        write(6,4) filrad
      endif
!
      return
!
 90   continue
      write(6,3) filrad
      return
!
 100  continue
      write(6,2) filrad
      return
    2 format(' *** eof encountered reading radar file=',1x,a200)
    3 format(' *** error encountered reading radar file=',1x,a200)
    4 format(' *** radar data all zero for current hour ***',1x,a200)
      end
