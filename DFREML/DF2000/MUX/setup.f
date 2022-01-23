c=============================================================================
      subroutine       setup (mparm)
c=============================================================================

c                      subroutine to initialize pointer vectors, addresser
c                      functions, and operational zeros
c-----------------------------------------------------------------------------
      use params
      use iindex
      use sps_lists

      integer, intent(in) :: mparm

      integer :: ii,k,ivar
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate (list1(mparm),list2(mparm),stat=ii)
      if(ii>0)stop 'setup : alloc list'

      zero=.1d-4
      small=.1d-2
      zero=.1d-7
      small=.1d-5
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
      end

c=============================================================================
      subroutine       sindex 
c=============================================================================

c     n1 = size of matrix of sig_(Aij) | for A and M uncorrelated
c     n2 = size of matrix of sig_(Mij) |
c     n1 = size of joint matrix of sig_(Aij) & sig_(Mij) | for A and M 
c     n2 = minus (size of matrix of sig_(Mij))           | correlated
c     n3 = size of matrix of sig_(Cij)
c     n4 = size of matrix of sig_(Eij)
c-------------------------------------------------------------------------  
      use params
      use iindex
      use parmap
      use numbers
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      call all_iindex(nq)
      nvec=0

c     animal effects
      if(imodel.le.2)then
        nvec(1)=kfit(1)
      else if(iopcov.eq.2)then
        nvec(1)=kfit(3)        ! check thoroughly before uncommenting !
        nvec(2)=-kfit(2)*(kfit(2)+1)/2
        print *, nvec(1),nvec(2)
      else
        nvec(1)=kfit(1)
        nvec(2)=kfit(2)
      end if

c     add. random effect
      if(ioprn1.eq.1)nvec(3)=kfit(4)
      if(ioprn3.eq.1)nvec(4)=kfit(7)

c     errors
      nvec(5)=kfit(5)
      nvars=5

      if(iomease.eq.1)then
        do i=1,nq
        nvars=nvars+1
        nvec(nvars)=1
        end do
      end if

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







