!========================================================================
      SUBROUTINE DxSPb3_77 (dia,fdia,xspars,foff,fwrk,zero,work,iwork,
     &                   iwork2,iwork3,ixsub,ixvec1,ixvec2,nsrow,
     &                   maxlnz,maxsub)
!========================================================================

      real*8 work(nsrow),fwrk(nsrow),fdia(nsrow),dia(nsrow),
     &        xspars(maxlnz), foff(maxlnz)
      integer iwork(nsrow), iwork2(nsrow), iwork3(nsrow),ixsub(maxsub),
     &        ixvec1(nsrow+1),ixvec2(nsrow+1)
      integer nn,krow,ksub,j,jrow,i,jj,ii,nsrow,maxlnz,maxsub
      real*8  zero,ff

      do krow=nsrow,1,-1

      if(mod(krow,1000).eq.0)print *,'dxspb3',krow,nn

c     ... pick out non-zero elements K+1 to n of k-th row
      nn=0
      ksub=ixvec2(krow)
      do j=ixvec1(krow),ixvec1(krow+1)-1
      jrow=ixsub(ksub)
      nn=nn+1
      iwork3(nn)=jrow
      iwork(nn)=j
      iwork2(jrow)=nn
      work(nn)=xspars(j)
      fwrk(nn)=foff(j)
      ksub=ksub+1
      end do

      do jj=1,nn
      jrow=iwork3(jj)
      fwrk(jj)=fwrk(jj)-2.d0*fdia(jrow)*work(jj)
      ksub=ixvec2(jrow)
      do i=ixvec1(jrow),ixvec1(jrow+1)-1
      ii=iwork2(ixsub(ksub))
      if( ii.gt.0 .and.foff(i).ne.0 )then
         fwrk(ii)=fwrk(ii)-foff(i)*work(jj)
         fwrk(jj)=fwrk(jj)-foff(i)*work(ii)
      end if
      ksub=ksub+1
      end do
      end do
    
c     ... adjust lead row
      do ii=1,nn
      ff=fwrk(ii)/dia(krow)
      if(dabs(ff).gt.zero)then
         foff( iwork(ii) )=ff
         fdia(krow)=fdia(krow)-work(ii)*ff
      end if
      iwork2(iwork3(ii))=0
      end do 

c     ... pivot
      fdia(krow)=0.5d0*fdia(krow)/dia(krow)
      end do

      return
      END 












