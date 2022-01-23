c===========================================================================
      subroutine       dxsca3 (nparm,kopt,xvec)
c===========================================================================
c                      steve smith's routine to reverse parameterization
c--------------------------------------------------------------------------

      use params
      use iindex
      use sps_lists

      integer, intent(in)                       :: nparm,kopt
      real(8), dimension (nparm),intent(inout)  :: xvec
      integer                                   :: ivar

c     reform parameters
      do ivar=1,nvars
      call reform(kopt,ivar,xvec,xvec)
      end do

      return

      contains

c     =====================================
      subroutine reform (kopt,ivar,xvec,zz)
c     =====================================
      
      integer, intent(in)                        :: kopt,ivar
      real(8), dimension (nparm), intent(inout)  :: xvec
      real(8), dimension (nparm), intent(out)    :: zz

      real(8) :: pivot,x
      integer :: i,ii,iiii,kk,n,nrank,j,jj,k,l
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
















