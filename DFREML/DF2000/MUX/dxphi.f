C     Last change:  KM   17 Nov 97   10:58 am
c=============================================================================
      subroutine phimat (nq,nfit)
c=============================================================================

c     purpose : routine to set up "phi" matrix as in Kirkpatrick et al.'s
c               paper (1990) -> symmetric coefficients

c--------------------------------------------------------------km 7/95-------

      use legendre
      use phimatrix
      use ages

      integer, intent(in)    :: nq,nfit

      integer                :: i
      real(8)                :: xlow,xupp,aamin,aamax,aa,pp
      real(8), dimension(nq) :: astar

      call all_phimatrix (nq)

c     set up vector of standardised ages;  assume ages are in order
      xlow=-1.d0
      xupp=1.d0
      aamin=iiage(1)
      aamax=iiage(nq)
      aa=(xupp-xlow)/(aamax-aamin)
      astar(:nq)=xlow+aa*(iiage(:nq)-aamin)

c     evaluate legendre polynomials at given ages
      phi=0.d0
      do ifit=1,nfit
      do i=1,nq
      aa=astar(i)
      pp=clgndr(1,ifit)
      if(aa.ne.0.d0)then
         do j=2,ifit
         if(clgndr(j,ifit).ne.0)pp=pp+clgndr(j,ifit)*aa**(j-1)
         end do
      end if
      phi(i,ifit)=pp
      end do
      end do

      print *,'phi',phi
      return
      end subroutine phimat

c============================================================================
      subroutine phisym (nq,nfit)
c============================================================================

c     purpose : routine to set up "phi x phi" matrix longhand, as
c               described by kirkpatrick et al 1994

c               symmetric coefficients
c---------------------------------------------------------------------------

      use legendre
      use phimatrix

      integer, intent(in) :: nq,nfit

      integer, dimension(:,:),allocatable :: i2,i3,i4,i5

c     set up index functions
      nqq=nq*(nq+1)/2
      allocate(i2(nqq,nq),i3(nqq,nq),i4(nqq,nq),i5(nqq,nq),
     *         stat=ii)
      if(ii>0)stop 'phisym : alloc'

      call jjndex (nfit)
      call jjndex (nq)
      phi2=0.d0
      
      ij=0
      do i=1,nq
      do j=i,nq
      ij=ij+1
      kl=0
      kl1=0
      do k=1,nq
      do l=1,nq+1-k
      kl=kl+1
      if(k+l.le.nfit+1)then
        kl1=kl1+1
        tt=1.d0
        if( i2(kl1,nfit).eq.i5(kl1,nfit) )tt=0.5d0
        phi2(ij,kl)= (phi( i3(ij,nq),i5(kl1,nfit)+1 ) * 
     *              phi( i4(ij,nq),i2(kl1,nfit)+1 )
     *          +   phi( i3(ij,nq),i2(kl1,nfit)+1 ) * 
     *              phi( i4(ij,nq),i5(kl1,nfit)+1 ) ) *tt
      end if
      end do
      end do
      end do
      end do

      deallocate(i2,i3,i4,i5)

      return

      contains

c     =====================
      subroutine jjndex (k)
c     =====================

c     purpose : routine to set up index functions described by
c               kirkpatrick et al 1994


c     index 1 & 2
      ii=0
      do i=0,k-1
      do j=0,k-1-i
      ii=ii+1
!      i1(ii,k)=j
      i2(ii,k)=i
      i4(ii,k)=i2(ii,k)+1
      end do
      end do

c     index 3
      ii=0
      do i=1,k
      do j=i,k
      ii=ii+1
      i3(ii,k)=j
      i5(ii,k)=i3(ii,k)-1
      end do
      end do

      return
      end subroutine jjndex

      end subroutine phisym









