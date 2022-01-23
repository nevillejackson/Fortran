!===========================================================================
      SUBROUTINE  DxREZ3 (varvec,fvalue,iout,iopt)
!=================================================================km==7/99==

!     residual cov matrices for multiple traits -> allow for
!      temporary environmental covariances

      use parameters, only : mxparm
      use units
      use parmap
      use combinations
      use numbers
      use traces
      use dmatrices
      use like_components, only : dete
      use residuals
      use ages
      use eigen_wrk

!     arguments
      real(8), dimension(mxparm), intent(inout) :: varvec
      real(8), intent(out)                      :: fvalue
      integer, intent(in)                       :: iopt
      integer, intent(out)                      :: iout

!     local variables
      real(8),dimension(:), allocatable         :: sigeps
      real(8)                                   :: es

      iout=0 
      nqq=nq*(nq+1)/2
      call all_eigen(nq)
      allocate (sigeps(nqq),stat=ii)
      if(ii>0)stop 'alloc sigeps'

!     check error covariance matrices
      ibegin=istrt(4)
      do i=1,kfit(7,1)
      sigeps=varvec(ibegin+1:ibegin+nqq)
      call eigem(sigeps,zero,es,nq,ineg)

      if(ineg>0 )then
         print *,'dxres3 : sigeps',i,sigeps
         if(iabs(iosrch).ne.3)then
            fvalue=big
            iout=1
            go to 99
         end if
         varvec(istrt(4)+i)=10.d0*dsqrt(eigzer)
      end if
      ibegin=ibegin+nqq
      end do

!     pick out matrices for all ages
      do i=1,nage(1)
      k=(meage(i)-1)*nqq
      eevec(i,:)=varvec(istrt(4)+k+1:istrt(4)+k+nqq)
      end do

 99   call deall_eigen
      deallocate (sigeps,stat=ii)
      if(ii>0)stop 'deall sigeps'
      RETURN
      END subroutine dxrez3

!=========================================================================
      subroutine ff_inverse (xvec,iopt,ip,nobs,nna,nnq)
!=========================================================================

!     set up e-inverse & derivatives for one animal with nobs records
!     & ages at recording/traits as given in nna and nnq

      use combinations
      use ages
      use parameters, only : mxparm
      use parmap
      use numbers
      use residuals
      use dmatrices
      use units
      use like_components, only : dete
      use traces

      real(8), dimension(mxparm), intent(in)    :: xvec
      integer, intent(in)                       :: nobs,iopt,ip
      integer, dimension(mobs),intent(in)       :: nnq
      integer, dimension(mobs,nmeta),intent(in) :: nna
      real(8), dimension(mobs)                  :: w1,w2
      integer, dimension(mobs)                  :: iw
      real(8)                                   :: det
      integer                                   :: k,i,j,nrank

!     inverse of error covariance matrix -> not diagonal but block-diag !
      work=0.d0
      do iobs=1,nobs
      iq=nnq(iobs)
      iage=nna(iobs,1)
      work(ihmii(iobs,nobs))=eevec(iage,ihmii(iq,nq))
!     ... identify blocks
      jq=iq
      do i=1,nq-1
      i1=iobs+i
      if(i1>nobs)exit
      if(nna(i1,1).eq.iage.and. nnq(i1)>jq)then
      jq=nnq(i1)
      work(ihmssf(iobs,i1,nobs))= eevec(iage,ihmssf(iq,jq,nq))
      end if
      end do
      end do ! iobs
!     invert matrix -> use that max. bandwith is nq
!     call dkmbnd (work, w1,w2,det,zero,iw,nrank,nobs,nq)
      call dkmwhf(work,w1,w2,det,zero,iw,nrank,nobs,0)

      ij=0
      do i=1,nobs
      do j=i,nobs
      ij=ij+1
      sig(i,j)=work(ij)
      sig(j,i)=work(ij)
      end do
      end do
      if(iopt.eq.0)then
         dete(1)=dete(1)+det
         return   ! require e-inverse only
      end if

c     set up d-matrices  
      de=.false.
      do iobs=1,nobs
      iq=nnq(iobs)
      iage=nna(iobs,1)
      ij=(meage(iage)-1)*nqq+ihmii(iq,nq)       ! diagonal element
      de(ihmii(iobs,nobs),ij)=.true.
!     ... identify blocks
      jq=iq
      do i=1,nq-1
      i1=iobs+i
      if(i1>nobs)exit
      if(nna(i1,1).eq.iage.and. nnq(i1)>jq)then
        jq=nnq(i1)
        ij=nqq*(meage(iage)-1)+ihmssf(iq,jq,nq)
        de(ihmssf(iobs,i1,nobs),ij)= .true.
      end if
      end do
      end do ! iobs
      
c     premultiply d-matrices with inverse residual cov. matrix
      rd1=0.d0
      if(iopt.eq.1)then       ! need RD for specific parameter only
         l1=ip                !      (dxmmd3 in dxspa3)
         l2=ip
      else  if(iopt.ge.2)then ! all matrices R**(-1)D required (for dxypy3)
         l1=1
         l2=nrparm
      end if

      do ll=l1,l2
      do k=1,nobs
      do j=1,nobs
      if(de(ihmssf(k,j,nobs),ll))rd1(:nobs,j,ll)=rd1(:nobs,j,ll)+
     &                                                   sig(:nobs,k)
      end do
      end do

c     calculate trace r(-1)d(ij) ! check option !
      if(iopt.eq.2)trres1(ngparm+ll,1)=trres1(ngparm+ll,1)+
     &                             sum( (/ (rd1(i,i,ll),i=1,nobs) /) )
      end do ! ll=1,nrparm
      if(iopt.ne.1)return

c     post-multiply with inverse residual cov. matrix
      work=0.d0
      do  k=1,nobs
      do  i=1,nobs
      dd=rd1(i,k,ip)
      if(dd.ne.0)then
         do j=i,nobs
         ij=ihmssf(i,j,nobs)
         work(ij)=work(ij)+dd*sig(j,k)
         end do
      end if
      end do
      end do
      ij=0
      do i=1,nobs
      do j=i,nobs
      ij=ij+1
      sig(i,j)=-work(ij)
      sig(j,i)=-work(ij)
      end do
      end do

      return
      end subroutine ff_inverse


