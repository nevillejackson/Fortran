!==========================================================================
      subroutine dxkdv3 (dnorm,xvec)
!==============================================================km===8/97===

      use parameters, only : mxparm
      use derivs
      use parmap
      use numbers
      use eigen_decomp

      real(8), dimension(mxparm), intent(in) :: xvec
      real(8), intent(out)                   :: dnorm

      integer                                :: i,j,ij,ibegin,ii
      integer, external                      :: ihmssf
      real(8)                                :: xx
      real(8), dimension (:,:), allocatable  :: xjacob,hh,d2
      real(8), dimension (:), allocatable    :: xkk

      allocate(xjacob(kparm,kparm),hh(kparm,kparm),d2(kparm,kparm),
     &         xkk(kfitmx*(kfitmx+1)/2),stat=ii)
      if(ii>0)stop 'dxkdv3 : alloc '

c     set up jacobian matrix
      xjacob=0.d0
      ibegin=0
      call build_jac(1)                     ! add. gen.
      if(ksfit(2)>0)call build_jac(2)        ! mat.
      if(ksfit(4)>0)call build_jac(4)        ! perm. env.
      if(ksfit(5)>0)call build_jac(5)        ! 2nd perm. env.
      ij=ibegin
      if(kfit(7,1)>0)then
         do i=1,nrparm                         ! meas. err.
         ij=ij+1
         if(kopt.eq.1 .or.kopt.eq.3)then
            xjacob(ij,ij)=2.d0*xvec(ij)
         else if(kopt.eq.2)then
            xx=dexp( xvec(ij) )
            xjacob(ij,ij)=2.d0*xx*xx
         end if
         end do 
      else                                     ! VF
         do i=1,nrparm -1
         ij=ij+1
         xjacob(ij,ij)=1.d0                    ! ... reg coeff
         end do
         ij=ij+1      ! i=nrparm               ! ... variance
         if(kopt.eq.1 .or.kopt.eq.3)then
            xjacob(ij,ij)=2.d0*xvec(ij)
         else if(kopt.eq.2)then
            xx=dexp( xvec(ij) )
            xjacob(ij,ij)=2.d0*xx*xx
         end if
      end if

c     pre-multiply gradient vector
      dnorm=0.d0
      do i=1,kparm
      xx=0.d0
      do j=1,kparm
      xx=xx+xjacob(i,j)*devl1(j)
      end do
      dnorm=dnorm+xx*xx
      der1(i)=xx
      end do
      dnorm=sqrt(dnorm)

c     pre- and post-multiply average information matrix
      do i=1,kparm
      do j=1,kparm
      d2(j,i)=devl2(ihmssf(i,j,kparm))
      end do     
      end do     
      hh=matmul(xjacob,d2)
      d2=matmul(hh,transpose(xjacob))
 
!     terms due to non-zero derivatives of jacobian
      ibegin=0
      call deriv_jac(1)
      if(ksfit(2)>0)call deriv_jac(2) 
      if(ksfit(4)>0)call deriv_jac(4)
      if(ksfit(5)>0)call deriv_jac(5)
      ij=ibegin
      if(kfit(7,1)>0)then
         do i=1,nrparm                 
         ij=ij+1
         if(kopt.eq.1 .or. kopt.eq.3)then
            d2(ij,ij)=d2(ij,ij)+2.d0*devl1(ij)
         else if(kopt.eq.2)then
            xx=dexp(xvec(ij))
            d2(ij,ij)=d2(ij,ij)+4.d0*xx*xx*devl1(ij)
         end if
         end do 
      else
         do i=1,nrparm-1
         ij=ij+1
         end do
         ij=ij+1                       ! sigeps is only parm transformed
         if(kopt.eq.1 .or. kopt.eq.3)then
            d2(ij,ij)=d2(ij,ij)+2.d0*devl1(ij)
         else if(kopt.eq.2)then
            xx=dexp(xvec(ij))
            d2(ij,ij)=d2(ij,ij)+4.d0*xx*xx*devl1(ij)
         end if
      end if

      ij=0
      do i=1,kparm
      do j=i,kparm
      ij=ij+1
      der2(ij)=d2(j,i)
      end do
      end do

      deallocate (d2,xjacob,hh,xkk,stat=ii)
      if(ii>0)stop 'deall dxkdv3'

      return

      contains

!     ========================
      subroutine build_jac(ii)
!     ========================

      integer, intent(in) :: ii
      integer             :: i,j,k,ki,kk
      real(8)             :: tt

      kk=ksfit(ii)*(ksfit(ii)+1)/2
      xkk(:kk)=xvec(ibegin+1:ibegin+kk)
      if(kopt.eq.2)then
         do i=1,ksfit(ii)
         xkk(ihmii(i,ksfit(ii)))=dexp( xkk(ihmii(i,ksfit(ii))) )
         end do
      end if

      do j=1,ksfit(ii)
      do i=j,ksfit(ii)
      ij=ihmssf(kindx(i,ii),kindx(j,ii),ksfit(ii))+ibegin
      do k=j,ksfit(ii)
      ki=ihmssf(kindx(i,ii),kindx(k,ii),ksfit(ii))+ibegin
      tt=1.d0
      if(i.eq.k)tt=2.d0
      xjacob(ij,ki)=tt*xkk( ihmssf(kindx(k,ii),kindx(j,ii),ksfit(ii)) )
      end do
      if(kopt.eq.2.and.i.eq.j)xjacob(ij,:)=xjacob(ij,:)*xkk(ij-ibegin)
      end do
      end do
      ibegin=ibegin+ksfit(ii)*(ksfit(ii)+1)/2
      return
      end subroutine build_jac

!     ========================
      subroutine deriv_jac(ii)
!     ========================

      integer, intent(in) :: ii
      integer             :: i,j,k,ki,kj
      real(8)             :: tt

      do j=1,ksfit(ii)
      do i=j,ksfit(ii)
      ij=ihmssf(kindx(i,ii),kindx(j,ii),ksfit(ii))+ibegin
      do k=j,ksfit(ii)
      ki=ihmssf(kindx(i,ii),kindx(k,ii),ksfit(ii))+ibegin
      kj=ihmssf(kindx(j,ii),kindx(k,ii),ksfit(ii))+ibegin
      tt=1.d0
      if(i.eq.k)tt=2.d0
      d2(kj,ij)=d2(kj,ij)+tt*devl1(ki)
      end do
      end do
      end do
      ibegin=ibegin+ksfit(ii)*(ksfit(ii)+1)/2
      return
      end subroutine deriv_jac     

      end subroutine dxkdv3





