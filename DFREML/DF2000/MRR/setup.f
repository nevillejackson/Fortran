c=============================================================================
      subroutine       setup 
c=============================================================================
c                      subroutine to initialize pointer vectors, addresser
c                      functions, and operational zeros
c-----------------------------------------------------------------------------
      use parameters, only : mxparm
      use iindex
      use sps_lists
      use units, only : eigzer

      integer             :: ii,k,ivar
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate (list1(mxparm),list2(mxparm),stat=ii)
      if(ii>0)stop 'setup : alloc list'

      zero=eigzer
      small=sqrt(zero)
      logzero=dlog(zero)
      list1=0
      list2=0

      if(nvec(2).lt.0)nvec(2)=0
      k=0
      len1=0
      do ivar=1,nvars
      do i=1,nvec(ivar)
      list1(len1+i)=k
      k=k+nvec(ivar)-i
      index(i,ivar)=len1+i
      end do
      len1=len1+nvec(ivar)
      end do

      k=0
      len2=len1+list1(len1)
      do  i=1,len2
      list2(i)=k
      k=k+len2-i
      end do

      return
      end subroutine setup

c=============================================================================
      subroutine  sindex 
c=============================================================================

c     n1 = size of matrix of sig_(Aij) | for A and M uncorrelated
c     n2 = size of matrix of sig_(Mij) |
c     n1 = size of joint matrix of sig_(Aij) & sig_(Mij) | for A and M 
c     n2 = minus (size of matrix of sig_(Mij))           | correlated
c     n3 = size of matrix of sig_(Cij)
c     n4 = size of matrix of sig_(Eij)
c     n5 = size of matrix of sig_(Qij)
c-------------------------------------------------------------------------  
      use params
      use iindex
      use parmap
      use numbers
      use phimatrix, only : irropt
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if(kfit(7,1)>0)then
         k7=kfit(7,1)
      else
         k7=-kfit(7,1)+1
      end if

      nq4=4+k7
      allocate(nvec(nq4),nrvec(nq4),nvv(nq4),index(kfitmx,nq4),stat=ii)
      if(ii>0)stop 'all_iindex'
      nvec=0
      nvv=0

c     animal effects
      if(nanim.eq.0)then
        nvec(1)=0
        nvec(2)=0
      else if(imodel.le.2)then ! no mat eff
        nvec(1)=ksfit(1)
        nvec(2)=0
      else if(iopcov.eq.2)then
        nvec(1)=ksfit(3)        ! check thoroughly before uncommenting !
        nvec(2)=-ksfit(2)*(ksfit(2)+1)/2
      else
        nvec(1)=ksfit(1)  ! uncorrelated mat eff
        nvec(2)=ksfit(2)
      end if

c     add. random effect
      if(ioprn1.eq.0)then
        nvec(3)=0
      else 
        nvec(3)=ksfit(4)
        if(irropt(4,1).eq.4)nvv(4)=1
      end if

c     add. random effect
      if(ioprn3.eq.0)then
        nvec(4)=0
      else
        nvec(4)=ksfit(5)
      end if

c     errors ! use that measurement errors are uncorrelated here !
      nvars=4
      do i=1,k7
      nvars=nvars+1
      nvec(nvars)=nq ! allow for cov for traits measured at same age
      if(kfit(7,1)<0 .and. i<k7)nvv(nvars)=1
      end do

      return
      end subroutine sindex

c==========================================================================
      function mij(i,j)
c==========================================================================
c     steve smith's Half-stored matrix addresser function.
c--------------------------------------------------------------------------

      use params
      use sps_lists

      ii=min0(i,j)
      jj=max0(i,j)
      mij=list2(ii)+jj

      return
      end function mij

c============================================================================
      function nij(i,j)
c============================================================================
c     steve smith's Block-stored matrix addresser function.
c----------------------------------------------------------------------------

      use params
      use sps_lists

      ii=min0(i,j)
      jj=max0(i,j)
      nij=list1(ii)+jj

      return
      end function nij






















