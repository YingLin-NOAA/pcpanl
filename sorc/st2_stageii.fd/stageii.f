      BLOCK DATA STAGEII
!
      DIMENSION FLDMLT(131,131),FLDGGO(131,131)
      DIMENSION RADAR(131,131)
      COMMON/RADCOM/RADAR
      COMMON/FLDM/FLDMLT
      COMMON/FLDG/FLDGGO
      INCLUDE 'luqual'
      INCLUDE 'lustat'
      INCLUDE 'lugage'
      INCLUDE 'lumult'
      INCLUDE 'luprod'

      include 'raddat'
      include 'ppinp'
      INCLUDE 'ppout'
      INCLUDE 'prdflg'
      INCLUDE 'flags'
!yl
      include 'hrap'
!yl
!
      DATA LUQLCN/12/,LURAD/31/
      DATA LUSTCN/13/
      DATA LUGACN/14/,LUGAFD/51/,LUGAG/32/
      DATA LUMTCN/15/,LUMTFD/52/,LUBIAS/53/
      DATA LUFLG/16/
      DATA LUGRTB/82/
!
      DATA RADID/'   '/
      DATA FILRAD                                                             &
       /'                                                            '/
      DATA FILGAG                                                             &
       /'                                                            '/
      DATA FILSAT                                                             &
       /'                                                            '/
      DATA FILGAO                                                             &
       /'                                                            '/
      DATA FILMUL                                                             &
       /'                                                            '/
      DATA FILGRT                                                             &
       /'                                                            '/
      DATA FILFLG                                                             &
       /'                                                            '/
!
      DATA AZH/-6.0/,AZHMM/0.251/
!
      data idebug/1/
      data fldggo/17161*-99./,fldmlt/17161*-99./
      data radar/17161*-99./
!
!yl
      data alat1,alon1,dx,alonv/23.117,240.977,4762.5,255./
!yl
      END
