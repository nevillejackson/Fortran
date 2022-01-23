c===========================================================================
      subroutine       dxsca3 (kopt,xvec)
c===========================================================================
c                      steve smith's routine to reverse parameterization
c--------------------------------------------------------------------------

      use parameters, only : mxparm
      use iindex
      use sps_lists

!     arguments
      integer, intent(in)                       :: kopt
      real(8), dimension (mxparm),intent(inout) :: xvec

!     variables
      integer                                   :: ivar

c     reform parameters
      do ivar=1,nvars
      if(nvv(ivar)>0)cycle ! skip reg coefficients for var function
      call reform(kopt,ivar,xvec,xvec)
      end do

      return

      contains

c     =====================================
      subroutine reform (kopt,ivar,xvec,zz)
c     =====================================
      
      integer, intent(in)                        :: kopt,ivar
      real(8), dimension (mxparm), intent(inout) :: xvec
      real(8), dimension (mxparm), intent(out)   :: zz

      real(8)           :: pivot,x
      integer           :: i,ii,iiii,kk,n,nrank,j,jj,k,l
      integer, external :: nij,mij 

      n=nvec(ivar)
      if(n.eq.0)return
      nrank=nrvec(ivar)

c     inverse log transformation
      if(kopt.eq.2)then
         do i=1, nrank
         ii=index(i,ivar)
         kk=nij(ii,ii)
         pivot=xvec(kk)
         if(pivot.le.logzero)then
            xvec(kk)=0.d0
         else
            xvec(kk)=dexp(pivot)
         end if
         end do
      end if

c     reformation
      do i=n, 1, -1
      ii=index(i,ivar)
      iiii=nij(ii,ii)
      pivot=xvec(iiii)
      do j=i, 1, -1
      jj=index(j,ivar)
      
      x=0.d0
      l=j
      if(l.gt.nrank)l=nrank
      do k=1, l
      kk=index(k,ivar)
      x=x+xvec(nij(kk,ii))*xvec(nij(kk,jj))
      end do
      zz(nij(ii,jj))=x
      end do
      if(pivot.lt.zero)zz(iiii)=zz(iiii)+small
      end do

      return
      end subroutine reform

      end subroutine dxsca3

c===========================================================================
      subroutine dxsca33 (xvec)
c===========================================================================

      use parameters, only : mxparm
      use numbers
      use parmap
      use eigen_decomp
      use phimatrix, only : irropt
      use units, only : zero, eigzer

      real(8), dimension (mxparm),intent(inout) :: xvec
      real(8),dimension(kfitmx*(kfitmx+1)/2)    :: xkk
      integer                                   :: ibegin

      ibegin=0

!     covariance functions
      if(nanim>0)call reconstitute(1)
      if(ksfit(2)>0)call reconstitute(2)
      if(ksfit(4)>0.and.irropt(4,1)<4)then
         call reconstitute(4)
      else
         if(kvfpe+kfoupe.ge.0)then
            ibegin=ibegin+kvfpe+2*kfoupe+1   ! skip regression coefficients
            if(kopt.eq.2)xvec(ibegin)=dexp( xvec(ibegin) ) ! variance at
            xvec(ibegin)=xvec(ibegin)*xvec(ibegin)         ! intercept
            ibegin=ibegin+ncpno     ! skip correlation parameters
         else
            ibegin=ibegin+kfit(4,1) ! skip all params
         end if
      end if    
      if(ksfit(5)>0)call reconstitute(5)

!     measurement error variances
      if(kfit(7,1)>0)then
         do i=1,nrparm
         ibegin=ibegin+1
         if(kopt.eq.2)xvec(ibegin)=dexp( xvec(ibegin) )
         xvec(ibegin)=xvec(ibegin)*xvec(ibegin)
         end do
      else  if(kfit(7,1)<0)then
         ibegin=ibegin+nrparm
         if(kopt.eq.2)xvec(ibegin)=dexp( xvec(ibegin) )
         xvec(ibegin)=xvec(ibegin)*xvec(ibegin)
      end if

      return

      contains

c     ============================
      subroutine reconstitute (ii)
c     ============================
      
      integer, intent(in)                        :: ii
      real(8)                                    :: pivot,xx
      integer                                    :: i,idia,kk,j,jj,k
      integer, external                          :: ihmii,ihmssf

      xkk=0.d0
      do i=ksfit(ii), 1, -1
      ii2=kindx(i,ii)
      idia=ihmii(ii2,ksfit(ii))+ibegin
      pivot=xvec(idia)
      if(kopt.eq.2)pivot=dexp(pivot)
      do j=i, 1, -1
      jj=kindx(j,ii)
      xx=0.d0
      do k=1,min0(kfteig(ii),j)
      kk=kindx(k,ii)
      xx=xx+xvec(ihmssf(kk,ii2,ksfit(ii))+ibegin)
     &                            *xvec(ihmssf(kk,jj,ksfit(ii))+ibegin)
      end do
      xkk(ihmssf(ii2,jj,ksfit(ii)))=xx
      end do
      if(pivot<zero)xkk(idia-ibegin)=xkk(idia-ibegin)+eigzer
      end do

      kk=ksfit(ii)*(ksfit(ii)+1)/2
      xvec(ibegin+1:ibegin+kk)=xkk(:kk)
      ibegin=ibegin+kk
      return
      end subroutine reconstitute

      end subroutine dxsca33









