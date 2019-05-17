      program decode
!$$$  main program documentation block
!
! MAIN PROGRAM: DECODE      program to decode DPA files
!   Programmer: Ying lin           ORG: NP22        Date: 2001-06-01  
!
! PROGRAM HISTORY LOG:
!   sp port: 21 jan 2000 meb
!   yl: dec 2000
!       1) Made superficial modifications to my liking
!       2) Change all date stamps to yyyymmddhh
!       3) Use w3difdat to throw out data outside of the 20 minute window,
!          rather than using the original torturous logic
!       4) Added option to simply inventory a DPA file
!
!   Y Lin, 2009-09-17: do not apply 'rbias' to precip values. 
!   Y Lin, 2012-08-01: modify for WCOSS.
!
! I/O units
!   Input:
!     fort.5:  yyyymmddhh to be processed
!     fort.11: DPA file for this hour (output from combdpa)
!     fort.17: /nwprod/fix/nam_master_radar_list
!   Output:
!     fort.51: Inventory of a DPA file (only used when the program is run in
!               'inventory' mode)
!     fort.52: DPA output (opened inside this program with 
!     fort.71: wrkdir/erly/cmp/cards.parm (list of DPA output from this program

! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : IBM SP
!
!$$$
!
! ibuf: maximum size of combined two-hour DPA file.  As of 2006/7/26 the file
!       is usually around 20,000,000.  The 23Z 24 Jul and 00Z 25 Jul DPA files
!       on 'white' were twice the normal size (it appears that there was a 
!       dataflow issue around that time).  To guard against a recurrence of 
!       such a data problem, we are increasing IBUF from 20M to 60M.
! nrad : maximum number of radars (right now we have around 150)
! msize: maximum size of each record (radar obs) in bytes.  Right now the 
!        maximum is a bit over 13000 bytes
! nobs: number of reports in the DPA file.
!
      parameter (msize=50000, nrad=1000, nobs=22000)
!
!     set ibuf bigger than the largest file you may read
!
      character * 1     ibuf(60000000)
      character * 10    nfile
      character * 200   master
      character * 1     msga(msize)
      character * 50000 msgb
      character * 200   filnam
      character * 150    prefx
      character * 3     staid(nrad),aid
      character * 10    adate
      character * 200   cardsparm(nrad)
      integer cardindx(nrad)
!
! 'cardsparm' contains info to be printed out to 'cards.parm'.  We're doing
! this in order to eliminate duplicate entries in 'cards.parm'.
! chars  1-40: 1st line of the output for this station
! chars 41-80: 2nd line of the output   "   "   "   
! ----:----|----:----|----:----|----:----|----:----|----:----|----:----|----:
! MRX    36.168    83.402                   1200101022256               
! 
! We use integer array 'cardindx' to keep track of whether a given station
! in the master_radar_list has already been written into 'cardsparm'.  If so,
! over-write instead of writing out a new one.
!
      integer           rcbyte(nobs)
      integer           istart(nobs)
      integer           jstat(1250),icoun(nrad)
      integer           stat
!
      character*1         invent
!
! idat   - target analysis time
! jdat70 - Julian date/time read in from DPA - end of 1h accumulation period
! jdat   - jdat70 converted to the regular date
      integer           idat(8),jdat70(8),jdat(8)
      integer*2         nrows,ibyte,irep,ival
      dimension         precip(131,131),slat(nrad),slon(nrad)
      dimension         rinc(5)
      real*8 timef
!
      save
!
      equivalence       (msga(1),msgb)
!
      data jdat70/1969,12,31,0,0,0,0,0/
!
!***********************************************************************
!
      CALL W3TAGB('DECODE ',2001,0152,0060,'NP22   ')
!  Keep track of CPU time:
      btim=timef()
      cpu_time=0.
!
!  read in data cards
!    input file
!    output dir
!    hour to get (yyyymmddhh)
!
      jdat=0
      ncard = 0
      cardindx = -999
!
      call getenv("INVENTORY", invent)
      call getenv("ST2_ST1", prefx)
      read(5,20) idat(1),idat(2),idat(3),idat(5)
 20   format(i4,3i2)
!
 25   continue
      istat=0
      icoun=99
 30   read(17,40,end=50) aid,alat,alon
 40   format(1x,a3,2f9.3)
      istat=istat+1
      staid(istat)=aid
      slat(istat)=alat
      slon(istat)=alon
      goto 30
 50   continue
!
      nfile='fort.11'
      print *,' looking for date ',idat(1),idat(2),idat(3),idat(5)
      prefx=trim(prefx)//"/"
      kdatp = index(prefx,' ') -1
      if (kdatp.le.0) kdatp=len(prefx)
      nfile=trim(nfile)//char(0)
!
!     call function stat to find number of bytes in file
!     word 8 of array jstat has the number of bytes in file
!     if your computer does not have the c function stat
!     comment out the next 7 lines
!

      if (stat(nfile,jstat).ne.0) then
        print *,'error in function stat getting file stats'
        stop 99
      else
#ifdef XLF
! Use this for XLF Fortran on the CCS:
        kbytes = jstat(11)
#else
! Use this for Intel Fortran on WCOSS:
        kbytes = jstat(8)
#endif
        print *,'number of bytes in file   = ',kbytes
      end if
!
!     open file 
!
      open (unit=11,status='old',access='direct',                             &
         form='unformatted',iostat=merr,recl=kbytes)
      if (merr.ne.0) then
        print *,'open input file error on file = ', nfile
        print *,'error = ',merr
        stop 20
      end if
      write(6,60) 
 60   format(' rad rec  lat      lon yr mn da hr mn ok (1=ok 0=err)')
!
!     read the entire file into array ibuf
!
      read (11,rec=1,err=1000) (ibuf(i),i=1,kbytes)
      close (unit=11)
!
!  -------Each DPA record starts with 'SDUS'.  Find  all 'SDUS' in file,
!  -------To obtain length of each message.
      jstart = 1
      nn = 0
      do 70 i = jstart, kbytes
        if(ibuf(i)//ibuf(i+1)//ibuf(i+2)//ibuf(i+3).ne.'SDUS') go to 70
        nn = nn + 1
        istart(nn) = i
 70   continue
!
      istart(nn+1) = kbytes+1
!  The above line means that all records are accounted for.  Thanks Mike.
!
!     Compute the length of record (in bytes).  Actually this is the
!     space from the beginning of one file to the end of the next.  
!     Without unpacking the record, we do not know the actual
!     length of the record. This value will be
!     equal to the record length or greater,
!     but never less.
!
      do i = 1,nn
        rcbyte(i) = istart(i+1) - istart(i)
      end do
!
      print *,'number of records in file = ',nn
      numrec = nn
!
      if (invent.eq.'T' .or. invent.eq.'t') then
        open(51,file='dpa.inventory')
        write(51,*) 'number of bytes in file   = ',kbytes
        write(51,*) 'number of records in file = ',nn
      endif
!
!     read and unpack all records in the file
!
      nrec = 0
      print *,'write all records in the file'
!
      do 200 k =1,numrec
        mbytes = rcbyte(k)
        kstart = istart(k)
        iunp = 0
        rlat = 0.
        rlon = 0.
!
        do ii=1,mbytes
          msga(ii)=ibuf(kstart+ii-1)
        enddo
!
!  ending date of accum (days since 1/1/70)
        idays70=mova2i(msga(129))*256+mova2i(msga(130))
!  ending time of accum (minutes UTC)
        imins=mova2i(msga(131))*256+mova2i(msga(132))
!  
        rinc(1)=float(idays70)
        rinc(2)=imins/60
        rinc(3)=mod(imins,60)
        rinc(4)=0.
        rinc(5)=0.
        call w3movdat(rinc,jdat70,jdat)
           
        jyr=jdat(1)
        jmn=jdat(2)
        jda=jdat(3)
        jhr=jdat(5)
        jmin=jdat(6) 

!  latitude
        ilatt=mova2i(msga(51))*256*256*256 + mova2i(msga(52))*256*256         &
             +mova2i(msga(53))*256         + mova2i(msga(54))
        rlat=ilatt*0.001
!  longitude
        ilong=-256*256*256*256+mova2i(msga(55))*256*256*256                   &
                              +mova2i(msga(56))*256*256                       &
                              +mova2i(msga(57))*256+mova2i(msga(58))
        rlon=-ilong*0.001
!  rmin
        irmin=-256*256 + mova2i(msga(91))*256+mova2i(msga(92))
!  xinc
        ixinc=mova2i(msga(93))*256+mova2i(msga(94))
!  bias
        ibias=mova2i(msga(125))*256+mova2i(msga(126))
        rmin=irmin*0.1
        xinc=ixinc*0.001
        bias=ibias*0.01
!
        if (invent.eq.'T' .or. invent.eq.'t') then
!  height:
           ihgt=mova2i(msga(59))*256+mova2i(msga(60))

           write(51,75) msgb(1:18), msgb(22:27), mbytes, rlat, rlon,          &
               ihgt, jyr,jmn,jda, jhr, jmin, rmin, xinc, bias
 75        format(a18,x,a6,x,i5,f7.3,1x,f8.3,i6,2x,i4,2i2.2,1x,i2.2,          &
               ':',i2.2,4f7.3)
           go to 200
        endif
!
        if (bias.ne.0.) rbias=1.0/bias

!  number of rows cols
        irows=mova2i(msga(173))*256+mova2i(msga(174))
        icols=mova2i(msga(175))*256+mova2i(msga(176))
!
!  skip this obs if the ending time of accum period is more than 10min 
!  before or after the 'top of the hour' given in 'idat':
! 
        call w3difdat(jdat,idat,3,rinc)
!
        if (abs(rinc(3)).gt.10.) go to 200
!
        write(6,80) msgb(25:27),rlat,rlon,jyr,jmn,jda,jhr,jmin
 80     format(1x,a3,2f9.3,i5,4i3,' read')
!
!  open output file
!
        write(adate,90) idat(1),idat(2),idat(3),idat(5)
 90     format(i4.4,4(i2.2))
        filnam=prefx(1:kdatp)//msgb(25:27)//adate//'Z'
!
!  unpack, check for bad reports along the way
!
        if (irmin.eq.0) then
          write(6,*) ' error minimum is zero ',mbytes
          goto 170
        endif
        if (ixinc.eq.0) then
          write(6,*) ' error incr is zero ',mbytes
          goto 170
        endif
        if (bias.eq.0.) then
          write(6,*) ' error bias is zero ',mbytes
          goto 170
        endif
!
        iptr=177
        do 130 j=1,irows
          do i=1,131
            precip(i,132-j)=-9.99
          enddo
!
          ibyte=mova2i(msga(iptr))*256+mova2i(msga(iptr+1))
!
          inext=iptr+ibyte+2
          nruns=ibyte/2
!
          if (nruns.le.0) then
            write(6,*) ' error ibyte nruns j',ibyte,nruns,j,mbytes
            goto 120
          endif
          iptr=iptr+1
          i=0
!
          do 110 mk=1,nruns
            irep=mova2i(msga(iptr+1))
            ival=mova2i(msga(iptr+2))
            if (ival.gt.255.or.ival.lt.0) then
              write(6,*) ' error valu rep ptr',ival,irep,iptr,mbytes
              goto 120
            endif
!
            do 100 l=1,irep
              i=i+1
              if (i.gt.131) then
                write(6,*) ' error i out of bounds',ival,irep,iptr,mbytes
                goto 120
              endif
              if (ival.eq.0) then
                precip(i,132-j)=0.0
              else if (ival.lt.255.and.ival.gt.0) then
                value=rmin+(ival-1)*xinc
!yl             precip(i,132-j)=(10.**(value*0.1))*rbias
                precip(i,132-j)=(10.**(value*0.1))
              endif
 100        continue
!
            iptr=iptr+2
 110      continue
 120      iptr=inext
 130    continue
!
!  successful write out to output file
!
        nhr1=1
        jfound=0
        icloser= 0
        do 140 j=1,istat
          if (msgb(25:27).eq.staid(j)) then
             imrlist=j
             igap=abs(rinc(3))
             jfound=1
             if(igap.lt.icoun(j)) then
              icloser=1
!  some stations report several times per hour
!  write out the report that is closest to the top of the hour
              icoun(j)=igap
              close(52)
              open(unit=52,file=filnam,form='unformatted')
              write(52) msgb(25:27),rlat,rlon,bias
              write(52) adate
              write(52) precip
              iunp=1
            endif
          endif
 140    continue
!
        if (jfound.eq.0) then
          istat=istat+1
          imrlist=istat
          igap=abs(rinc(3))
          slat(istat)=rlat
          slon(istat)=rlon
          staid(istat)=msgb(25:27)
          write(6,*) ' radar not in master list ',msgb(25:27)
          if(igap.lt.icoun(istat)) then
            icloser=1
            icoun(istat)=igap
            close(52)
            open (unit=52,file=filnam,form='unformatted')
            write(52) msgb(25:27),rlat,rlon,bias
            write(52) adate
            write(52) precip
            iunp=1
          endif
        endif
!
! Add this station to cardsparm.  If station is already on cardsparm,
! but this one is closer to the top of the hour, over-write.
! 
        if (cardindx(imrlist) .lt. 0 .and. icloser.eq.1) then
           ncard = ncard + 1
           cardindx(imrlist) = ncard 
           ic = ncard
        else
           ic = cardindx(imrlist)
        endif
        write(cardsparm(ic) (1:40),150) msgb(25:27),rlat,rlon
        write(cardsparm(ic)(41:80),160) nhr1,jyr,jmn,jda,jhr,jmin
!
 150    format(a3,2f10.3)
 160    format(i3,i4.4,4i2.2)
!
!  write out info
!
 170    continue
        if (iunp.eq.1) write(6,180) msgb(25:27),rlat,rlon,                    &
            jyr,jmn,jda,jhr,jmin
 180    format(1x,a3,2f9.3,i5,4i3,' success')
!
 200  continue
!
! Write out radar stations that have no radar data available within
! the time window (20 min centered on the top of the hour).  Why do
! we need to do this?)
!
! Early on I was outputting the 'true' date stamp (i.e. end of obs
! period), but since that include minutes, the yyyymmddhh sometimes 
! differ from the yyyymmddhh we want (e.g. 00:58Z 16 Jan 2001 was
! written out as 200101160058.  Then prephads and stageii read them
! in as 2001011600, and trouble starts.  So now we'll output the
! original 'date0' (input to decode).  We'll also output the true
! date stamp on the obs (just so I can see when it was taken) but it
! won't be used by any program.
!
      do ic = 1, ncard
        write(71,'(a40)') cardsparm(ic)(1:40)
        write(71,"(a3,a10,3x,'obs: ',a37)")                                   &
             cardsparm(ic)(41:43), adate, cardsparm(ic)(44:80)
      enddo
!
      nhr1=1
      do j=1,istat
        if (icoun(j).eq.99) then
          write(71,150) staid(j),slat(j),-1.0*slon(j)
          write(71,220) nhr1, adate
 220      format(i3,a10,': no data within +/- 10 min window')
        endif
      enddo
!
      cpu_time=cpu_time+timef()-btim
      write(6,*) 'Time spent in DECODE (sec)=', cpu_time*0.001
!
 900  close (unit=52)
      CALL W3TAGE('DECODE ')
      stop
!
 1000 continue
      print *,'Error reading file, error No. = ',jerr
      stop 1000
      end


