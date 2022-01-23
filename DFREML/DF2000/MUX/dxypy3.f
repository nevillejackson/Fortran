c===========================================================================
      SUBROUTINE   dxypy3 (nparm)
C===========================================================================

      use params
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

!     arguments
      integer, intent(in)                    :: nparm

!     local variables
      integer, dimension (:),allocatable     :: iwork
      real(8), dimension (:),allocatable     :: work
      real(8), dimension (:,:),allocatable   :: bmat,b,brx
      real(8), dimension (:,:),allocatable   :: xcov
      real(8), dimension (:), allocatable    :: yvec
      integer, dimension (:), allocatable    :: nnq
      integer, dimension (:,:), allocatable  :: ieq
      integer :: nq2, irow, ksub, jrow, ian, jjrow, i, j, iparm, ii, iq,
     &           iobs
      real(8) ::  bb, xx,dd
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      nq2=nq+nq222
      mobs=sum(mxobs)

      allocate (xcov(mnfr,mobs),yvec(mobs),nnq(mobs),
     *          ieq(mfix+3,mobs),stat=ii)
      if(ii>0)stop 'dxypy3 : alloc 2'

      allocate (bmat(neqns,ngparm),stat=ii)
      if(ii>0)stop 'dxypy3 : alloc bmat'

      allocate (brx(nsrow,nparm),stat=ii)
      if(ii>0)stop 'dxypy3 : alloc brx'

      allocate (b(mobs,nparm), iwork(mqhq), work(nq2), stat=ii)
      if(ii>0)stop 'dxypy3: alloc '

      dypy2=0.d0
      brx=0.d0

c     pick out vector of solutions for each animal
      irow=lim3
      do ian=1,nanim
      jjrow=irow
      work(:nq2)=solns(ieqnew(jjrow+1:jjrow+nq2))
      irow=irow+nq2      

c     ... pre-multiply with D(ij)T**(-1) for each parameter
      do iparm=1,ngpar1
      jrow=jjrow
      do i=1,nq2
      jrow=jrow+1
      bb=0.d0
      do k=1,nq2
      bb=bb+td(k,i,iparm)*work(k)
      end do
      bmat(ieqnew(jrow),iparm)=bb
      end do
      end do
      end do ! loop for ian

c     same for additional random effect
      if(ioprn1.eq.1)then 
         irow=lim2
         do ii=1,nrand1
         jjrow=irow
         work(:nq)=0.d0
         do iq=1,nq
         if(nfix1(iq).gt.nfix(iq))then
            irow=irow+1
            work(iq)=solns( ieqnew(irow) )
         end if
         end do ! iq
         do iparm=ngpar1+1,ngpar2
         jrow=jjrow
         do iq=1,nq
         if(nfix1(iq).gt.nfix(iq))then
            jrow=jrow+1
            bb=0.d0
            do jq=1,nq
            if(td(jq,iq,iparm).ne.0)bb=bb+td(jq,iq,iparm)*work(jq)
            end do
            bmat(ieqnew(jrow),iparm)=bb
         end if
         end do ! iq
         end do ! iparm
         end do ! ii

         if(ioprn3.eq.1)then
            irow=lim3a
            do ii=1,nrand3
            jjrow=irow
            work(:nq)=0.d0
            do iq=1,nq
            if(nfix3(iq).gt.nfix1(iq))then
               irow=irow+1
               work(iq)=solns( ieqnew(irow) )
            end if
            end do ! iq
            do iparm=ngpar2+1,ngparm
            jrow=jjrow
            do iq=1,nq
            if(nfix3(iq).gt.nfix1(iq))then
               jrow=jrow+1
               bb=0.d0
               do jq=1,nq
               if(td(jq,iq,iparm).ne.0)bb=bb+td(jq,iq,iparm)*work(jq)
               end do
               bmat(ieqnew(jrow),iparm)=bb
            end if
            end do
            end do
            end do
         end if
      end if

c     ---------------
c     re-process data
c     ---------------

      REWIND(IUN52)
50    READ(IUN52,END=99)ICOMB,NOBS,NR,NNQ(:NOBS),
     *      IWORK(:NR),( (IEQ(K,L),K=1,NEFF(NNQ(L)) ),L=1,NOBS),
     *    ( (XCOV(K,L),K=1,NFR(NNQ(L)) ),L=1,NOBS),YVEC(:NOBS)

c     calculate vector of residuals
      do iobs=1,nobs
      iq=nnq(iobs)
      xx=0.d0
c     ... FE/RE
      do i=1,neff(iq)
      irow=ieq(i,iobs)-lim1
      if(irow.gt.0)xx=xx+solns(iwork(irow))
      end do
c     ... covariables
      irow=nfrst(iq)
      do i=1,nfr(iq)
      irow=irow+1
      xx=xx+xcov(i,iobs)*solns(ieqnew(irow))
      end do
      yvec(iobs)=yvec(iobs)-xx
      end  do

c     premultiply residuals with D(ij)E**(-1) for each residual var com
      do iparm=1,nrparm
      do i=1,nobs
      bb=0.d0
      do j=1,nobs
      if(rd(j,i,iparm,icomb).ne.0)bb=bb+rd(j,i,iparm,icomb)*yvec(j)
      end do
      b(i,ngparm+iparm)=bb
      end do
      end do

c     set-up Z(D(ij)T**(-1))u for this animal
      do iparm=1,ngpar1
      do iobs=1,nobs
      iq=nnq(iobs)
      ian=ieq( nfix2(iq)+1,iobs )-lim1         ! anim eff
      bb= bmat( iwork(ian),iparm )
      if(nfix2(iq).gt.nfix3(iq))then
         ian=ieq( nfix2(iq),iobs )-lim1        ! mat gen eff
         bb=bb+ bmat( iwork(ian),iparm )
      end if
      b(iobs,iparm)=bb
      end do
      end do
      
      if(ioprn1.eq.1)then
         do iparm=ngpar1+1,ngpar2
         do iobs=1,nobs
         iq=nnq(iobs)
         if(nfix1(iq).gt.nfix(iq))then
            ii=ieq( nfix1(iq),iobs )-lim1
            b(iobs,iparm)=bmat( iwork(ii),iparm )
         else
            b(iobs,iparm)=0.d0
         end if
         end do
         end do
         if(ioprn3.eq.1)then
            do iparm=ngpar2+1,ngparm
            do iobs=1,nobs
            iq=nnq(iobs)
            if(nfix3(iq).gt.nfix1(iq))then
               ii=ieq( nfix3(iq),iobs )-lim1
               b(iobs,iparm)=bmat( iwork(ii),iparm )
            else
               b(iobs,iparm)=0.d0
            end if
            end do
            end do
         end if      
      end if      

c     accumulate contributions for this animal
      DO IOBS=1,NOBS
      IQ=NNQ(IOBS)

      do iparm=1,nparm

c     ... premultiply with submatrix of R**(-1)
!      bb=dot_product( eei(:nobs,iobs,icomb), b(:nobs,iparm) )
      bb=0.d0
      do i=1,nobs
      bb=bb+eei(i,iobs,icomb)*b(i,iparm)
      end do

c     ... covariables
      II=NFRST(IQ)
      DO  i=1,NFR(iQ)
      ii=ii+1
      brx(ieqnew(ii),iparm)=brx(ieqnew(ii),iparm)+xcov(i,iobs)*bb
      end do

c     ... fixed & random effects
      DO  i=1,neff(iQ)
      irow=iwork( ieq(i,iobs)-lim1)
      if(irow.gt.0)brx(irow,iparm)=brx(irow,iparm)+bb
      end do

c     ... matrix of cross-products
      do jparm=iparm,nparm
      ij=ihmssf(iparm,jparm,nparm)
      dypy2(ij)=dypy2(ij)+bb*b(iobs,jparm)
      end do

      end do
      end do
      go to 50

c     adjust for previous cholesky decompositions
 99   do irow=1,nsrow-1
      dd=1.d0/dia(irow)

c     process current colums
      do iparm=1,nparm
      brx(irow,iparm)=brx(irow,iparm)*dd
      end do

c     adjustments to remaining part
      ksub=ixvec2(irow)
      do j=ixvec1(irow),ixvec1(irow+1)-1
      jrow=ixsub(ksub)
      brx(jrow,:nparm)=brx(jrow,:nparm)-xspars(j)*brx(irow,:nparm)
      ksub=ksub+1
      end do
      
      end do

c     adjustments  to B'R**(-1)B
      ij=0
      do iparm=1,nparm
      do jparm=iparm,nparm
      ij=ij+1
      dypy2(ij)=dypy2(ij) - dot_product(brx(1:nsrow-1,iparm),
     &                                  brx(1:nsrow-1,jparm))
      end do
      end do

      deallocate(iwork,work,bmat,b,brx,xcov,yvec,ieq,nnq,stat=ii)
      if(ii>0)stop 'dxypy3 : dealloc'

      RETURN
      END subroutine dxypy3

!=========================================================================
      subroutine back_solve
!=========================================================================

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




















