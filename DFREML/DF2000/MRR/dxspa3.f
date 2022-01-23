!===========================================================================
      SUBROUTINE DxSPA3 (varvec)
!===========================================================================

      use units
      use sparse
      use xsprse
      use derivs
      use parmap
      use numbers
      use like_components
      use parameters, only : mxparm

      implicit none
      real(8), dimension (mxparm), intent(in)   :: varvec
      real(8), dimension (:), allocatable       :: fdia,foff,fwrk
      real(8), dimension (:), allocatable       :: work
      integer, dimension (:), allocatable       :: iwork,iwork2,iwork3
      integer                                   :: ii,nam1,na,nam2,ll,
     &                                             ipar,ipar1,nc1,nc2,
     &                                             nm1,nm2,nn
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c     ----------------------------------------------
c     first derivatives by backwards differentiation
c     ----------------------------------------------

      allocate (fdia(nsrow),fwrk(nsrow),foff(maxlnz),stat=ii)
      if(ii>0)stop ' dxspa3 : alloc fmatrix'

!     ... initialise f matrix; NB : f(L)=1/2 (log|C|+y'Py) here !
      foff=0.d0
      fdia=1.d0/dia
      fdia(nsrow)=dia(nsrow)
      
c     ... carry out backwards recursions
      allocate(work(nsrow),iwork(nsrow),iwork2(nsrow),iwork3(nsrow),
     *                                                      stat=ii)
      if(ii>0)stop 'alloc work'
      iwork2=0

!      call  dxspb3 
      call  dxspb3_77 (dia,fdia,xspars,foff,fwrk,zero,work,iwork,
     &                   iwork2,iwork3,ixsub,ixvec1,ixvec2,nsrow,
     &                   maxlnz,maxsub)
!     separate routine, compiled with f77 -> save 10+ % cpu on DEC system 
      deallocate(work,iwork,iwork2,iwork3)
      print *,' end of dxspb3'

c     ... evaluate first derivatives
      devc=0.d0

c     additive genetic (direct and maternal ) effects
      if(nanim>0)then
         call iparno (ksfit(1),1,nam1,nam2)
         if(ksfit(2)>0.and.iopcov.le.2)then
            ll=1+iopcov
            call iparno (ksfit(2),ll,na,nam2)
         end if
         do ipar=nam1,nam2
         call dxmmp3(ipar,1)
         call dxspc3(ipar)
         end do

c        other second animal effect components
         if(iopcov.ge.3)then
            call iparno (ksfit(2),2,nm1,nm2)
            do ipar=nm1,nm2
            call dxmmp3(ipar,2)
            call dxspc3(ipar)
            end do
         end if
      end if

!     ADDITIONAL RANDOM EFFECT COMPONENTS
      if(ioprn1.eq.1)then
        call iparno (ksfit(4),4,nc1,nc2)
        do ipar=nc1,nc2
        if(ieqmod.eq.0)then
           call dxmmp3(ipar,3)
        else  ! if(ieqmod.eq.1)then
           ipar1=-ipar
           CALL DxMMD3(VARVEC,1,ipar1)
           write(*,'(1x,a,i5)') 'par no.',ipar
        end if
        call dxspc3(ipar)
        end do
      end if

      IF(IOPRN3.EQ.1)THEN
        call iparno (ksfit(5),5,nc1,nc2)
        do ipar=nc1,nc2
        call dxmmp3(ipar,4)
        call dxspc3(ipar)
        end do
      end if

!     RESIDUAL COMPONENTS
      do ipar=ngparm+1,nparm
      ipar1=ipar-ngparm          ! 1 ... no.reg coeff +1
      CALL DxMMD3(VARVEC,1,ipar1)
      call dxspc3(ipar)
      write(*,'(1x,a,i5)') 'par no.',ipar
      end do

      deallocate(fdia,fwrk,foff,stat=ii)
      if(ii>0)stop 'dxspa3 : de-alloc'

      return

      contains

C     ======================
      SUBROUTINE  DxSPb3 
C     ======================

      integer                             :: nn,krow,ksub,j,jrow,i,
     &                                       jj,ii
      real(8)                             :: ff

      do krow=nsrow,1,-1

      if(mod(krow,1000).eq.0)write(*,'(1x,a,2i8)') 'dxspb3',krow,nn

c     ... pick out non-zero elements K+1 to n of k-th row
      nn=0
      ksub=ixvec2(krow)
      do j=ixvec1(krow),ixvec1(krow+1)-1
      jrow=ixsub(ksub)
      nn=nn+1
      iwork3(nn)=jrow
      iwork(nn)=j
      iwork2(jrow)=nn
      work(nn)=xspars(j)
      fwrk(nn)=foff(j)
      ksub=ksub+1
      end do

      do jj=1,nn
      jrow=iwork3(jj)
      fwrk(jj)=fwrk(jj)-2.d0*fdia(jrow)*work(jj)
      ksub=ixvec2(jrow)
      do i=ixvec1(jrow),ixvec1(jrow+1)-1
      ii=iwork2(ixsub(ksub))
      if( ii.gt.0 .and.foff(i).ne.0 )then
         fwrk(ii)=fwrk(ii)-foff(i)*work(jj)
         fwrk(jj)=fwrk(jj)-foff(i)*work(ii)
      end if
      ksub=ksub+1
      end do
      end do
    
c     ... adjust lead row
      fwrk(:nn)=fwrk(:nn)/dia(krow)
      fdia(krow)=fdia(krow) - dot_product( fwrk(:nn),work(:nn))
      foff( (/ (iwork(ii),ii=1,nn) /) ) = fwrk(:nn)
      iwork2( (/ (iwork3(ii),ii=1,nn) /) ) =0

c     ... pivot
      fdia(krow)=0.5d0*fdia(krow)/dia(krow)
      end do

      return
      END subroutine dxspb3

!     =========================
      SUBROUTINE  DXSPC3 (ipar)
!     =========================
 
      integer, intent(in) :: ipar
      real(8)             :: dd
      integer             :: krow,j

!     devc(ipar)=2.d0*(dot_product(dia,fdia)+dot_product(xspars,foff))
                     ! factor of 2 here added to make f(L)=log|C|+y'Py
      dd=0.d0
      do  krow=1,nsrow
      dd=dd+dia(krow)*fdia(krow)
      end do
      do j=1,maxlnz
      dd=dd+xspars(j)*foff(j)
      end do       
      devc(ipar)=dd+dd

      return 
      end subroutine dxspc3

!     ===============================
      subroutine iparno (nq,ll,n1,n2)
!     ===============================

      integer, intent(in)   :: nq,ll
      integer, intent(out)  :: n1,n2
      integer               :: i,j

      n1=0
      n2=0
      do  i=1,nq
      do  j=1,nq
      nn=nparno(i,j,ll)
      if(nn.gt.0)then
         if(n1.eq.0)n1=nn
         if(nn.gt.n2)n2=nn
      end if
      end do
      end do
      return
      end subroutine iparno

      end subroutine dxspa3








