!**********************************************************************
          subroutine find1c(ndata,i0,u0,v0,n0,ier)
!**********************************************************************
       parameter (ndim=20,ndat=1000,ngmax=20)
          common/inform/iactrv(2),nactrv,iordrv(2)
          common/big1/u(ndat),v(ndat),z(ndat)
          common/srtord/ilist(ngmax)
          common/srtdst/rlist(ngmax)
          common/big3/irv(ndat)
          nbors=n0*nactrv
          do 200 ir=1,2
          if(iactrv(ir).eq.0) go to 200
          j=(iordrv(ir)-1)*n0
          if(i0.eq.0) go to 5
          u0=u(i0)
          v0=v(i0)
          ip=i0
          im=i0
          go to 6
 5        continue
          call posc(u,v,irv,ndata,u0,in)
          ip=in-1
          im=in
 6        continue
          i=0
 10       continue
          ip=ip+1
          if(ip.gt.ndata.and.im.le.0.and.i.lt.n0) go to 300
          if(ip.gt.ndata) go to 15
          if(irv(ip).ne.ir) go to 10
          i=i+1
          ilist(i+j)=ip
          rlist(i+j)=(u0-u(ip))**2+(v0-v(ip))**2
          rlist(i+j)=sqrt(rlist(i+j))
          if(i.eq.n0) go to 20
 15       continue
          im=im-1
          if(im.le.0) go to 10
          if(irv(im).ne.ir) go to 15
          i=i+1
          ilist(i+j)=im
          rlist(i+j)=(u0-u(im))**2+(v0-v(im))**2
          rlist(i+j)=sqrt(rlist(i+j))
          if(i.eq.n0) go to 20
          go to 10
 20       continue
          call hlpfnd(1+j,n0+j)
          ifp=0
          ifm=0
 30       continue
          ip=ip+1
          if(ip.gt.ndata) go to 40
          if(irv(ip).ne.ir) go to 30
          if(abs(u(ip)-u0).ge.rlist(n0+j))go to 40
          r=(u0-u(ip))**2+(v0-v(ip))**2
          r=sqrt(r)
          if(r.lt.rlist(n0+j)) go to 35
          go to 50
 35       continue
          ilist(n0+j)=ip
          rlist(n0+j)=r
          call hlpfnd(1+j,n0+j)
          go to 50
 40       continue
          ifp=1
          if(ifp.eq.1.and.ifm.eq.1) go to 200
 50       continue
          im=im-1
          if(im.le.0) go to 140
          if(irv(im).ne.ir) goto 50
          if(abs(u(im)-u0).ge.rlist(n0+j)) go to 140
          r=(u0-u(im))**2+(v0-v(im))**2
          r=sqrt(r)
          if(r.lt.rlist(n0+j)) go to 135
          go to 30
 135      continue
          ilist(n0+j)=im
          rlist(n0+j)=r
          call hlpfnd(1+j,n0+j)
          go to 30
 140      continue
          ifm=1
          if(ifp.eq.1.and.ifm.eq.1) go to 200
          go to 30
 200      continue
!*** this last call to hlpfnd must be omitted
!         call hlpfnd(1,nbors) **** skip ****
      ier=0
      return
  300 continue
      ier=1
      write(6,1) ip,ndata,im,i,n0
      return
    1 format(/,2x,'find1c:  ip=',i3,1x,'ndata=',i3,1x,'im=',i3,               &
             1x,'i=',i3,1x,'n0=',i3)
      end
