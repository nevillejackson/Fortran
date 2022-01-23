c==========================================================================
      subroutine     dxkdv3 (nparm,dnorm2)
c==========================================================================

c     purpose :      transform derivatives w.r.t. variance components to
c                    derivatives w.r.t. to elements of K-matrices
c------------------------------------------------------------km--8/95-----

      use params
      use derivs
      use phimatrix
      use parmap
      use numbers

      integer, intent(in) :: nparm
      real(8), intent(out) :: dnorm2

      integer :: i,j,ij,m,n,mn,ii
      integer, external :: ihmii,ihmssf
      real(8) :: xx,tt
      real(8), dimension (:,:), allocatable :: xjacob,hh
      real(8), dimension (:), allocatable ::  row
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c     set up jacobian matrix ...
      allocate(xjacob(kparm,nparm),hh(nparm,nparm),row(kparm),stat=ii)
      if(ii>0)stop 'dxkdv3 : alloc '
      xjacob=0.d0

c     additive genetic ...
      ij=0
      do i=1,kfit(1)
      do j=i,kfit(1)
      ij=ij+1
      tt=1.d0
      if(i.eq.j)tt=0.5d0
      mn=0
      do m=1,nq
      do n=m,nq
      mn=mn+1
      xjacob(ij,mn)=tt*(phi(m,i)*phi(n,j)+phi(m,j)*phi(n,i))
      end do
      end do
      end do
      end do

c     maternal genetic
      if(ioprn2.eq.1)then
         do i=1,kfit(2)
         do j=i,kfit(2)
         ij=ij+1
         tt=1.d0
         if(i.eq.j)tt=0.5d0
         mn=istrt(1)
         do m=1,nq
         do n=m,nq
         mn=mn+1
         xjacob(ij,mn)=tt*(phi(m,i)*phi(n,j)+phi(m,j)*phi(n,i))
         end do
         end do
         end do
         end do
      end if

c     additional random effect
      if(ioprn1.eq.1)then
         do i=1,kfit(4)
         do j=i,kfit(4)
         ij=ij+1
         tt=1.d0
         if(i.eq.j)tt=0.5d0
         mn=istrt(3)
         do m=1,nq
         do n=m,nq
         mn=mn+1
         xjacob(ij,mn)=tt*(phi(m,i)*phi(n,j)+phi(m,j)*phi(n,i))
         end do
         end do
         end do
         end do
      end if
      if(ioprn3.eq.1)then
         do i=1,kfit(7)
         do j=i,kfit(7)
         ij=ij+1
         tt=1.d0
         if(i.eq.j)tt=0.5d0
         mn=istrt(7)
         do m=1,nq
         do n=m,nq
         mn=mn+1
         xjacob(ij,mn)=tt*(phi(m,i)*phi(n,j)+phi(m,j)*phi(n,i))
         end do
         end do
         end do
         end do
      end if

c     error ..
      do i=1,kfit(5)
      do j=i,kfit(5)
      ij=ij+1
      tt=1.d0
      if(i.eq.j)tt=0.5d0
      mn=istrt(4)
      do m=1,nq
      do n=m,nq
      mn=mn+1
      xjacob(ij,mn)=tt*(phi(m,i)*phi(n,j)+phi(m,j)*phi(n,i))
      end do
      end do
      end do
      end do

c     measurement errors
      if(iomease.eq.1)then
         do i=1,nq
         ij=ij+1
         ii=istrt(4)+ihmii(i,nq)
         xjacob(ij,ii)=1.d0
         end do
      end if

c     pre-multiply gradient vector
      row=matmul(xjacob,devl1)
      dnorm2=dsqrt( dot_product(row,row ) )
      devl1(:kparm)=row

c     pre- and post-multiply average information matrix
      do i=1,kparm
      do j=1,nparm
      xx=0.d0
      do k=1,nparm
      xx=xx+xjacob(i,k)*devl2(ihmssf(k,j,nparm))
      end do
      hh(i,j)=xx
      end do
      end do
      ij=0
      do i=1,kparm
      do j=i,kparm
      ij=ij+1     
      devl2(ij)=dot_product( hh(i,:nparm),xjacob(j,:nparm) )
      end do
      end do

      deallocate (xjacob,hh,row)

      return
      end





