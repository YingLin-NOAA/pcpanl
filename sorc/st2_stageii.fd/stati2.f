      subroutine stati2(ng,z,umean,uvari)
!
! subroutine computes conditional sample statistics of gage rainfall 
! input variiables 
! n     - number of entries in array z() 
! z     - array containing rain gage measurements 
! output variables Ô
! cmean - conditional mean of rain gage rainfall 
! cvari - conditional cvariiance of rain gage rainfall 
! iflag - error flag; 1 = npos too small or ccvarii nonpositive,
!                     0 = OK 
      parameter (ngmax=20)
      common/srtord/ilist(ngmax)
      dimension z(ng)
! 
      sum1=0.
      sum2=0.
! 
      do 10 i=1,ng
      j=ilist(i)
      sum1=sum1+z(j)
      sum2=sum2+z(j)**2
   10 continue
! 
      umean=sum1/ng
      uvari=sum2/(ng-1)-sum1**2/(ng*(ng-1))
      return
      end
