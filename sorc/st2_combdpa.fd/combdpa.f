      program combdpa
!$$$  main program documentation block
!
! MAIN PROGRAM: COMBDPA      program to combine two consecutive hourly DPA 
!   files into one single DPA file.
!   Programmer: Ying lin           ORG: NP22        Date: 2001-06-01  
!
! ABSTRACT: This code reads in DPA files from two consecutive hours, and 
!   combine them into one single DPA file to be used as input for the DPA 
!   decoder (for example, in order to obtain radar precip for the hour 
!   ending 2001040317, we combine the two files
!         /dcom/us007003/20010403/wtxtbul/dpa_16
!   and   /dcom/us007003/20010403/wtxtbul/dpa_17
!   and call the resulting file dpa_2001040317 (since we use files that 
!   come in within a 20 minute window centered arounbd 17:00Z, we'd need 
!   radar report from both dpa_16 and dpa_17)
!
! PROGRAM HISTORY LOG:
!   YL: 3 Apr 2001 combdpa created, because the daily DPA files are getting
!   too large, we requested that NCO split the daily DPAs into hourly DPAs.
!
!   YL: 2012/08/01: convert for WCOSS
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE : CCS/WCOSS
!
!$$$
!  set ibuf bigger than the largest file you may read
!
      character*1     ibuf(50000000)
      character*10    infile(2)
      integer         jstat(1250), kbytes(2)
      integer         stat
!
!
      CALL W3TAGB('COMBDPA ',2001,0152,0060,'NP22   ')
!
!  read in data cards
!    input file hour1
!    input file hour2
!    output file
!
      infile(1)='fort.11'
      infile(2)='fort.12'
!
      do 20 ifl = 1, 2
        infile(ifl)=trim(infile(ifl))//char(0)
!
!     call function stat to find number of bytes in file
!     word 8 of array jstat has the number of bytes in file
!     if your computer does not have the ! function stat
!     comment out the next 7 lines
!
        if (stat(infile(ifl),jstat).ne.0) then
          print *,'error in function stat getting stats for', infile(ifl)
          stop 99
        else
#ifdef XLF
! Use this for XLF Fortran on the CCS:
          kbytes(ifl) = jstat(11)
#else
! Use this for Intel Fortran on WCOSS:
          kbytes(ifl) = jstat(8)
#endif
          print *,'number of bytes in ',infile(ifl),' = ',kbytes(ifl)
        end if
!
!
!       open file 
!
        open (unit=10+ifl,status='old',access='direct',                       &
             form='unformatted',iostat=merr,recl=kbytes(ifl))
          if (merr.ne.0) then
          print *,'open input file error on ', infile(ifl)
          print *,'error = ',merr
          stop 20
        end if
 20   continue
!
!     read the two files into array ibuf
!
      read (11,rec=1,err=900) (ibuf(i),i=1,kbytes(1))
      read (12,rec=1,err=1000)                                                &
                    (ibuf(i),i=kbytes(1)+1,kbytes(1)+kbytes(2))
!
      open (unit=51,access='direct',form='unformatted',                       &
          iostat=merr,recl=kbytes(1)+kbytes(2))
      write(51,rec=1,err=1100) (ibuf(i),i=1,kbytes(1)+kbytes(2))
      print *, 'Writing ', kbytes(1)+kbytes(2), ' bytes to combined DPA file.'
!
      CALL W3TAGE('COMBDPA ')
      stop
!
 800  stop 'Incomplete input card!'
!
 900  continue
      print *, 'Error reading file ', infile(1)
      stop 900
 1000 continue
      print *, 'Error reading file ', infile(2)
 1100 continue
      print *, 'Error writting to combined DPA file.'
      stop 1000
      end


