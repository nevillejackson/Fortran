!===========================================================================
      subroutine dxchn3 (nparm,kopt,xvec)
!===========================================================================

      use params
      use derivs
      use iindex
      use parmap

      integer,intent(in)                      :: nparm,kopt
      real(8),dimension(nparm),intent(inout)  :: xvec

      integer                                 :: ivar,jvar
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      call dxsca3(nparm,kopt,xvec)

c     apply reparameterization
      do ivar=1,nvars
      call cholsky(nparm,ivar,xvec)
      end do

c     transform 1st derivatives
      der1=0.d0
      do ivar=1,nvars
      call first (nparm,ivar,xvec)
      end do

c     transform second derivatives
      der2=0.d0
c     ... diagonal blocks
      do ivar=1,nvars
      call secdia (nparm,ivar,xvec)
      end do
c     ... off-diagonal blocks
      do ivar=1,nvars
      do jvar=ivar+1,nvars
      call secoff(nparm,ivar,jvar,xvec)
      end do
      end do

c     apply log transformation to diagonals
      if(kopt.eq.2)then
         do ivar=1,nvars
         do jvar=ivar+1,nvars
         call logoff(nparm,ivar,jvar,xvec)
         end do
         end do
         do ivar=1,nvars
         call logdiag(nparm,ivar,xvec)
         end do
      end if
      return
      end subroutine dxchn3

c===========================================================================
      subroutine cholsky (nparm,ivar,xvec)
c===========================================================================
c     Subroutine for directed Cholesky decomposition;
c     operation applied only to n rows and columns contained in index.
c--------------------------------------------------------------------------

      use params
      use iindex
      use sps_lists

      integer,intent(in)                       :: nparm,ivar
      real(8), dimension(nparm),intent(inout)  :: xvec

      integer                                  :: n,kp,i,k,kk,kj,j,ij,ik
      real(8)                                  :: p,pivot

      n=nvec(ivar)
      nrvec(ivar)=0

      do 50 k=1, n

c     select largest pivot
      pivot=0.d0
      do i=k, n
      p=xvec(nij(index(i,ivar),index(i,ivar)))
      if(p.gt.pivot)then
         pivot=p
         kp=i
      end if
      end do
      if(pivot.lt.zero)return
      nrvec(ivar)=nrvec(ivar)+1

c     exchange rows and columns
      i=index(k,ivar)
      index(k,ivar)=index(kp,ivar)
      index(kp,ivar)=i

c     adjust pivot element
      pivot=dsqrt(pivot)
      kk=nij(index(k,ivar),index(k,ivar))
      xvec(kk)=pivot
      do 45 j=k+1, n

c     adjust lead row elements
      kj=nij(index(k,ivar),index(j,ivar))
      xvec(kj)=xvec(kj)/pivot

c     row operations
      do 40 i=k+1, j 
      ik=nij(index(i,ivar),index(k,ivar))
      ij=nij(index(i,ivar),index(j,ivar))
   40 xvec(ij)=xvec(ij)-xvec(kj)*xvec(ik)
   45 continue
   50 continue

      return
      end subroutine cholsky

c===========================================================================
      subroutine first (nparm,ivar,xvec)
c===========================================================================
c     Subroutine to calculate first derivatives of
c     the log-likelihood with respect to elements
c     of the Cholesky decomposition.
c--------------------------------------------------------------------------
      use params
      use derivs
      use iindex

      integer,intent(in)                      :: nparm,ivar
      real(8), dimension(nparm),intent(inout) :: xvec

      integer                                 :: n,i,ii,j,jj,k,kk,kj,ki
      real(8)                                 :: c

c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      n=nvec(ivar)
      nrank=nrvec(ivar)

c     calculate first derivatives
      do i=1, n
      ii=index(i,ivar)
      do j=1, min(i,nrank)
      jj=index(j,ivar)
      c=devl1(nij(ii,jj))

      do k=1, j
      kk=index(k,ivar)
      kj=nij(kk,jj)
      der1(kj)=der1(kj)+c*xvec(nij(kk,ii))
      ki=nij(kk,ii)
      der1(ki)=der1(ki)+c*xvec(nij(kk,jj))
      end do

      end do
      end do

      return
      end subroutine first

c===========================================================================
      subroutine secdia (nparm,ivar,xvec)
c===========================================================================
c     Subroutine to calculate second derivatives of
c     the log-likelihood with respect to elements
c     of the Cholesky decomposition.
c----------------------------------------------------------------------------
      use params
      use derivs
      use iindex

      integer,intent(in)                      :: nparm,ivar
      real(8), dimension(nparm),intent(inout) :: xvec

      integer :: n,i,ii,j,jj,k,kk,kj,ki,nrank,l,ll,m,mm,km,kl
      real(8) :: c,s

      n=nvec(ivar)
      nrank=nrvec(ivar)

c     calculate second derivatives
      do 50 i=1, n
      ii=index(i,ivar)
      do 49 j=1, min(i,nrank)
      jj=index(j,ivar)
      c=devl1(nij(ii,jj))

      do 5 k=1, j
      ki=nij(index(k,ivar),index(i,ivar))
      kj=nij(index(k,ivar),index(j,ivar))

      if(kj.le.ki)then
         kk=mij(kj,ki)
         der2(kk)=der2(kk)+c
      end if
      if(ki.le.kj)then
         kk=mij(ki,kj)
         der2(kk)=der2(kk)+c
      end if

    5 continue

      do 48 l=1, n
      ll=index(l,ivar)
      do 47 m=1, min(l,nrank)
      mm=index(m,ivar)

      nn=mij(nij(ll,mm),nij(ii,jj))
      s=devl2(nn)

      do 11 k1=1, j
      do 10 k2=1, m
       kj=nij(index(k1,ivar),jj)
       ki=nij(index(k1,ivar),ii)
       km=nij(index(k2,ivar),mm)
       kl=nij(index(k2,ivar),ll)

      if(kj.le.km)then
         kk=mij(kj,km)
         der2(kk)=der2(kk)+s*xvec(ki)*xvec(kl)
      end if

      if(ki.le.kl)then
         kk=mij(ki,kl)
         der2(kk)=der2(kk)+s*xvec(kj)*xvec(km)
      end if

      if(km.le.ki)then
         kk=mij(km,ki)
         der2(kk)=der2(kk)+s*xvec(kl)*xvec(kj)
      end if

      if(kl.le.kj)then
         kk=mij(kl,kj)
         der2(kk)=der2(kk)+s*xvec(km)*xvec(ki)
      end if

   10 continue
   11 continue

   47 continue
   48 continue

   49 continue
   50 continue

      return
      end subroutine secdia

c===========================================================================
      subroutine secoff (nparm,ivar,jvar,xvec)
c===========================================================================
c     Subroutine to calculate second derivatives of
c     the log-likelihood with respect to elements
c     of two different Cholesky decompositions.
c--------------------------------------------------------------------------
      use params
      use derivs
      use iindex

      integer,intent(in)                   :: nparm,ivar,jvar
      real(8), dimension(nparm),intent(in) :: xvec

      integer :: n1,n2,i,ii,j,jj,kk,nr1,nr2,l,ll,m,mm,ki,kj,km,kl
      real(8) :: s

      n1=nvec(ivar)
      n2=nvec(jvar)
      nr1=nrvec(ivar)
      nr2=nrvec(jvar)

c     calculate second derivatives
      do 50 i=1, n1
      ii=index(i,ivar)
      do 49 j=1, min(i,nr1)
      jj=index(j,ivar)

      do 48 l=1, n2
      ll=index(l,jvar)
      do 47 m=1, min(l,nr2)
      mm=index(m,jvar)

      nn=mij(nij(ll,mm),nij(ii,jj))
      s=devl2(nn)

      do 11 k1=1, j
      do 10 k2=1, m
       kj=nij(index(k1,ivar),jj)
       ki=nij(index(k1,ivar),ii)
       km=nij(index(k2,jvar),mm)
       kl=nij(index(k2,jvar),ll)

       kk=mij(kj,km)
       der2(kk)=der2(kk)+s*xvec(ki)*xvec(kl)

       kk=mij(ki,kl)
       der2(kk)=der2(kk)+s*xvec(kj)*xvec(km)

       kk=mij(km,ki)
       der2(kk)=der2(kk)+s*xvec(kl)*xvec(kj)

       kk=mij(kl,kj)
       der2(kk)=der2(kk)+s*xvec(km)*xvec(ki)

   10 continue
   11 continue

   47 continue
   48 continue

   49 continue
   50 continue

      return
      end

c===========================================================================
      subroutine logdiag (nparm,ivar,xvec)
c===========================================================================
c     Subroutine to reparameterize the diagonals of
c     Cholesky decompostion to log scale.
c--------------------------------------------------------------------------

      use params
      use derivs
      use iindex

      integer,intent(in)                      :: nparm,ivar
      real(8), dimension(nparm),intent(inout) :: xvec

      integer :: n,nrank,i,ii,kk,iiii,l,ll,m,mm,llmm

      n=nvec(ivar)
      nrank=nrvec(ivar)

c     adjust first derivatives
      do i=1, nrank
      ii=index(i,ivar)
      kk=nij(ii,ii)
      der1(kk)=der1(kk)*xvec(kk)
      end do

c     adjust second derivatives and cholesky
      do i=1, nrank
      ii=index(i,ivar)
      iiii=nij(ii,ii)
      do l=1, n
      ll=index(l,ivar)
      do m=1, min(nrank,l)
         mm=index(m,ivar)
         llmm=nij(ll,mm)
         kk=mij(iiii,llmm)
         der2(kk)=xvec(iiii)*der2(kk)
      end do
      end do
      kk=mij(iiii,iiii)
      der2(kk)=der2(kk)*xvec(iiii)+der1(iiii)
      xvec(iiii)=dlog(xvec(iiii))
      end do

      return
      end subroutine logdiag

c===========================================================================
      subroutine logoff (nparm,ivar,jvar,xvec)
c===========================================================================
c     Subroutine to reparameterize the diagonals of two
c     Cholesky decompostions to log scale, but adjustements
c     are restricted to off diagonal blocks between Cholesky factors.

c     always call this routine before logdiag
c     because xvec changes in logdiag

c     adjustments needed only for 2nd derivative in the off diagonal blocks
c----------------------------------------------------------------------------

      use params
      use derivs
      use iindex

      integer,intent(in)                   :: nparm,ivar,jvar
      real(8), dimension(nparm),intent(in) :: xvec

      integer :: n1,n2,nr1,nr2,i,ii,iiii,l,ll,m,mm,llmm,kk

      n1=nvec(ivar)
      n2=nvec(jvar)
      nr1=nrvec(ivar)
      nr2=nrvec(jvar)

c     adjustemts for the first Cholesky factor
      do i=1, nr1
      ii=index(i,ivar)
      iiii=nij(ii,ii)
      do l=1, n2
      ll=index(l,jvar)
      do m=1, min(nr2,l)
         mm=index(m,jvar)
         llmm=nij(ll,mm)
         kk=mij(iiii,llmm)
         der2(kk)=xvec(iiii)*der2(kk)
      end do
      end do
      end do

c     adjustemts for the second Cholesky factor
      do i=1, nr2
      ii=index(i,jvar)
      iiii=nij(ii,ii)
      do l=1, n1
      ll=index(l,ivar)
      do m=1, min(nr1,l)
         mm=index(m,ivar)
         llmm=nij(ll,mm)
         kk=mij(iiii,llmm)
         der2(kk)=xvec(iiii)*der2(kk)
      end do
      end do
      end do

      return
      end subroutine logoff

c===========================================================================
      subroutine dxkhn3 (nparm,kopt,xvec)
c===========================================================================

      use params
      use iindex
      use parmap

      integer,intent(in)                       :: nparm,kopt
      real(8), dimension(nparm),intent(inout)  :: xvec

      integer                                  :: ivar

c     apply reparameterization
      do ivar=1,nvars
      call cholsky(nparm,ivar,xvec)
      end do

c     apply log transformation to diagonals
      if(kopt.eq.2)then
         do ivar=1,nvars
         call logdiak
         end do
      end if
      return

      contains

c     ==================
      subroutine logdiak 
c     ==================

      integer                                  :: i,ii,iiii

      do i=1,nrvec(ivar)
      ii=index(i,ivar)
      iiii=nij(ii,ii)
      xvec(iiii)=dlog(xvec(iiii))
      end do

      return
      end subroutine logdiak

      end subroutine dxkhn3













