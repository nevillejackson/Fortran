!===========================================================================
      SUBROUTINE dxypy3 (varvec)
!===========================================================================

      use units
      use sparse
      use xsprse
      use derivs
      use parmap
      use combinations
      use levels
      use numbers
      use order
      use dmatrices
      use residuals
      use solutions
      use phimatrix
      use parameters, only : mxparm
      use read_iun52

      real(8),dimension(mxparm), intent(in)  :: varvec
      real(8), dimension (:),allocatable     :: wwrk
      real(8), dimension (:,:),allocatable   :: bmat,b,brx,rb
      real(8)                                :: bb, xx, ddd
      integer                                :: nq2,irow,ksub,jrow,ian,
     &                                          i,j,iparm,ii,ks1,ks2,
     &                                          iobs,lim0=1,ks4,ks5
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!     initialize
      mobs=sum(nxobs)
      ks1=ksfit(1)      
      ks2=ksfit(2)      
      nq2=ks1+ks2
      ks4=ksfit(4)
      ks5=ksfit(5)

      allocate (brx(nsrow,nparm),bmat(neqns,ngparm),b(mobs,nparm),
     *     rb(mobs,nparm),wwrk(nq2),stat=ii)
      if(ii>0)stop 'dxypy3: alloc bmatrix'

      dypy2=0.d0
      brx=0.d0
      bmat=0.d0

c     pick out vector of solutions for each animal
      if(nanim>0)call pick_solns (lim3, nq2, nanim, ngpar1, 0)

c     same for additional random effect(s)
      if(ioprn1.eq.1 .and. ieqmod.eq.0)
     &               call pick_solns (lim2, ks4, nrand1, ngpar2, ngpar1)

      if(ioprn3.eq.1)call pick_solns (lim3a, ks5, nrand3, ngparm, 
     &                                                           ngpar2)

c     ---------------
c     re-process data
c     ---------------
      
      ndd=0
      REWIND(IUN52)
50    READ(IUN52,END=99)ICOMB,NOBS,NR,NNA(:NOBS,:),NNQ(:NOBS),
     *   IICOL(:NR),( (IEQ(K,L) ,K=1,NEFF(NNQ(L)) ),L=1,NOBS),
     *   ( (XCOV(K,L), K=KFTFIX(NNQ(L))+1,NFR(NNQ(L)) ),L=1,NOBS),
     &    YVEC(:NOBS)
      ndd=ndd+1

      if(ncomb.le.max_comb .and. nq.eq.1.and. ieqmod.eq.0)then  
         sig(:nobs,:nobs)=eei(:nobs,:nobs,icomb)
         rd1(:nobs,:nobs,:nrparm)=rd(:nobs,:nobs,:nrparm,icomb)
      else if(nq.eq.1 .and. ieqmod.eq.0)then
         call ee_inverse(varvec,icomb,2,99)
      else if(nq.eq.1 .and. ieqmod.eq.1)then
         call gg_inverse(varvec,2,99,nobs,nna,nnq)
      else if(nq>1)then
         call ff_inverse (varvec,2,99,nobs,nna,nnq)
      end if
      b=0.d0

c     calculate vector of residuals
      do iobs=1,nobs
      iq=nnq(iobs)
      call kk_values(iq)
      xx=0.d0
c     ... FE
      do i=1,nfix(iq)
      irow=ieq(i,iobs)-lim1
      if(irow.gt.0)xx=xx+solns(iicol(irow))
      end do
c     ... perm. env. effects
      if(kfit(4,iq)>0 .and. ieqmod.eq.0)then
         iage=nna(iobs,irrmet(4,iq))
         do i=1,kfit(4,iq)
         irow=ieq(nfix(iq)+i,iobs)-lim1
         xx=xx+phi(iage,i,4,iq)*solns(iicol(irow))
         end do
      end if
      if(kfit(5,iq)>0)then
         iage=nna(iobs,irrmet(5,iq))
         do i=1,kfit(5,iq)
         irow=ieq(kk1a+i,iobs)-lim1
         xx=xx+phi(iage,i,5,iq)*solns(iicol(irow))
         end do
      end if
c     ... mat. genetic
      if(kfit(2,iq)>0)then
         iage=nna(iobs,irrmet(2,iq))
         do i=1,kfit(2,iq)
         irow=ieq(kk1+i,iobs)-lim1
         xx=xx+phi(iage,i,2,iq)*solns(iicol(irow))
         end do
      end if
c     ... additive genetic
      if(kfit(1,iq)>0)then
         iage=nna(iobs,irrmet(1,iq))
         do i=1,kfit(1,iq)
         irow=ieq(kk2+i,iobs)-lim1
         xx=xx+phi(iage,i,1,iq)*solns(iicol(irow))
         end do
      end if
c     ... covariables
      if(kftfix(iq)>0)then
         iage=nna(iobs,irrmet(0,iq))
         xcov(:kftfix(iq),iobs)=phi(iage,:kftfix(iq),0,iq)
      end if
      irow=nfrst(iq)
      do i=1,nfr(iq)
      irow=irow+1
      xx=xx+xcov(i,iobs)*solns(ieqnew(irow))
      end do
      yvec(iobs)=yvec(iobs)-xx
      end  do ! iobs

c     premultiply residuals with D(ij)E**(-1) for each residual var com
      do iparm=1,nrparm
      do i=1,nobs
      bb=0.d0
      do j=1,nobs
      if(rd1(j,i,iparm).ne.0)bb=bb+rd1(j,i,iparm)*yvec(j)
      end do
      b(i,ngparm+iparm)=bb
      end do ! i
      end do ! iparm

!     same for p.e. var comps
      if(ieqmod.eq.1)then
         do iparm=ngpar1+1,ngpar2
         do i=1,nobs
         bb=0.d0
         do j=1,nobs
         bb=bb+rd2(j,i,iparm-ngpar1)*yvec(j)
         end do
         b(i,iparm)=bb  
         end do ! i
         end do ! iparm
      end if

c     set-up Z(D(ij)T**(-1))u for this animal
      if(nanim>0)then
         do iparm=1,ngpar1
         do iobs=1,nobs
         iq=nnq(iobs)
         call kk_values(iq)
         iage=nna(iobs,irrmet(1,iq))
         bb=0.d0
         do k=1,kfit(1,iq)
         ian=ieq( kk2+k,iobs )-lim1
         bb=bb+ phi(iage,k,1,iq )* bmat( iicol(ian),iparm )
         end do
         if(ioprn2.eq.1)then
            iage=nna(iobs,irrmet(2,iq))
            do k=1,kfit(2,iq)
            ian=ieq( kk1+k,iobs )-lim1
            bb=bb+ phi(iage,k,2,iq)*bmat( iicol(ian),iparm )
            end do
         end if
         b(iobs,iparm)=bb
         end do
         end do
      end if
      if(ioprn1.eq.1 .and. ieqmod.eq.0 )then
         do iparm=ngpar1+1,ngpar2
         do iobs=1,nobs
         iq=nnq(iobs)
         iage=nna(iobs,irrmet(4,iq))
         do k=1,kfit(4,iq)
         ii=ieq( nfix(iq)+k,iobs )-lim1
         b(iobs,iparm)=b(iobs,iparm)+phi(iage,k,4,iq)*
     &                                    bmat(iicol(ii),iparm)
         end do
         end do
         end do
      end if      

      if(ioprn3.eq.1)then
         do iparm=ngpar2+1,ngparm
         do iobs=1,nobs
         iq=nnq(iobs)
         call kk_values(iq)
         iage=nna(iobs,irrmet(5,iq))
         do k=1,kfit(5,iq)
         ii=ieq( kk1a+k,iobs )-lim1
         b(iobs,iparm)=b(iobs,iparm)+phi(iage,k,5,iq)*
     &                                          bmat(iicol(ii),iparm)
         end do
         end do
         end do
      end if      

! at the end of this step b is the same for ieqmod=1 or 0 !!

c     accumulate contributions for this animal

c     ... premultiply with submatrix of R**(-1)
      rb=0.d0
      do i=1,nobs
      do j=1,nparm
      do k=1,nobs
      rb(i,j)=rb(i,j)+sig(i,k)*b(k,j)  ! no assumptions about sig here
      end do
      end do
      end do
! rb differs because r=phi K_r phi'+diag(sigeps) and diag(sigeps), respectively

      do iparm=1,nparm
      DO IOBS=1,NOBS
      bb=rb(iobs,iparm)
      if(dabs(bb)<zero)cycle
      iq=nnq(iobs)
      call kk_values(iq)

c     ... covariables
      II=NFRST(IQ)
      DO  i=1,NFR(iQ)
      ii=ii+1
      brx(ieqnew(ii),iparm)=brx(ieqnew(ii),iparm)+xcov(i,iobs)*bb
      end do ! i

c     ... fixed effects
      DO  i=1,nfix(iq)
      irow=iicol( ieq(i,iobs)-lim1)
      if(irow>0)brx(irow,iparm)=brx(irow,iparm)+bb
      end do

c     ... perm. env. effects
      if(kfit(4,iq)>0 .and. ieqmod.eq.0 )call accum_brx( 4, nfix(iq) )
      if(kfit(5,iq)>0)call accum_brx( 5, kk1a )

c     ... mat. genetic
      if(kfit(2,iq)>0)call accum_brx( 2, kk1 )

c     ... additive genetic
      if(kfit(1,iq)>0)call accum_brx( 1, kk2 )

!     ... matrix of cross-products
      do jparm=iparm,nparm
      ij=ihmssf(iparm,jparm,nparm)
      dypy2(ij)=dypy2(ij)+bb*b(iobs,jparm)
      end do !JPARM

      end do ! iobs
      end do ! iparm

      go to 50 ! next animal

!     -------------------------------------------
!     adjust for previous cholesky decompositions
!     -------------------------------------------

 99   do irow=1,nsrow-1
      ddd=1.d0/dia(irow)

c     process current colums
      brx(irow,:nparm)=brx(irow,:nparm)*ddd

c     adjustments to remaining part
      ksub=ixvec2(irow)
      do j=ixvec1(irow),ixvec1(irow+1)-1
      jrow=ixsub(ksub)
      brx(jrow,:nparm)=brx(jrow,:nparm)-xspars(j)*brx(irow,:nparm)
      ksub=ksub+1
      end do
      end do ! irow

c     adjustments  to B'R**(-1)B
      ij=0
      do iparm=1,nparm
      do jparm=iparm,nparm
      ij=ij+1
      bb=0.d0
      do k=1,nsrow-1
      bb=bb+brx(k,iparm)*brx(k,jparm)
      end do
      dypy2(ij)=dypy2(ij)-bb
      end do
      end do

      deallocate(wwrk,bmat,b,rb,brx,stat=ii)
      if(ii>0)stop 'dxypy3 : dealloc'

      RETURN

      contains

!     =========================
      subroutine kk_values (iq)
!     =========================
      integer, intent(in) :: iq

      kk1a=nfix(iq)
      if(ieqmod.eq.0)kk1a=kk1a+kfit(4,iq)
      kk1=kk1a+kfit(5,iq)
      kk2=kk1+kfit(2,iq)
      return
      end subroutine kk_values

!     ========================================================
      subroutine pick_solns (istart, nql, nlevls, npar, npadd)
!     ========================================================
      integer, intent(in) :: istart, nql, nlevls, npar, npadd
      integer             :: irow, ii, i, k, iparm
      real(8)             :: bb

      irow=istart
      do ii=1,nlevls
!     ... pick out solutions
      wwrk(:nql)=solns( ieqnew(irow+1:irow+nql) )
!     ... pre-multiply with D(ij)T**(-1) for each parameter
      do iparm=npadd+1,npar
      do i=1,nql
      bb=0.d0
      do k=1,nql
      bb=bb+td(k,i,iparm)*wwrk(k)
      end do
      bmat(ieqnew(irow+i),iparm)=bb
      end do
      end do ! iparm
      irow=irow+nql
      end do ! loop for ii

      return
      END subroutine pick_solns

!     ====================================
      subroutine accum_brx (ifact, ibegin)
!     ====================================
      integer, intent(in) :: ifact, ibegin
      integer             :: iage, i, irow

      iage=nna(iobs,irrmet(ifact,iq))
      do i=1,kfit(ifact,iq)
      irow=iicol( ieq(ibegin+i,iobs)-lim1 )
      if(irow>0)brx(irow,iparm)=brx(irow,iparm)+phi(iage,i,ifact,iq)*bb
      end do

      return
      END subroutine accum_brx

      END subroutine dxypy3

!===========================================================================
      subroutine back_solve
!===========================================================================

      use sparse
      use xsprse
      use solutions

      real(8) :: adj, rhs
      integer :: irow, jrow, ksub, j

      solns=0.d0

      do irow=nsrow-1,1,-1
      ksub=ixvec2(irow)
c     ... adjust for previous solutions
      rhs=0.d0
      adj=0.d0
      do  j=ixvec1(irow),ixvec1(irow+1)-1
      jrow=ixsub(ksub)
      if(jrow.eq.nsrow)then
        rhs=xspars(j)
      else
        adj=adj+xspars(j)*solns(jrow)
      end if
      ksub=ksub+1
      end do
c     ... solution
      solns(irow)=(rhs-adj)/dia(irow)
      end do
      return

      end subroutine back_solve










