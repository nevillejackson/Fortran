!===========================================================================
      SUBROUTINE dfcor3 
!===========================================================================

      use parameters
      use names
      use units
      use parmap
      use sigmas
      use correlations
      use numbers
      use ages
      use phimatrix
      use eigen_wrk

!     local variables
      real(8), dimension(:), allocatable   :: work
      real(8)                              :: es
      integer                              :: i,j,ii,nneg
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      call all_eigen(nanq)
      allocate (work(nanq*(nanq+1)/2),stat=ii)
      if(ii>0)stop 'dfcor3 : all vv'
     
C     -------------------
C     VARIANCE COMPONENTS
C     -------------------

C     ... ADDITIV-GENETIC (DIRECT)
      ibegin=0
      ipar=1
      call cov_ages(ksfit(ipar),ipar,ibegin,nanq,siga,eiga)

C     ... SECOND ANIMAL EFFECT (MATERNAL)
      sigm=0.d0
      ipar=2
      IF(IOPRN2.EQ.1)call cov_ages(ksfit(ipar),ipar,ibegin,nanq,
     &                                                     sigm,eigm)

c     ... additional random effect
      sigc=0.d0
      ipar=4
      if(ioprn1.eq.1)call cov_ages(ksfit(ipar),ipar,ibegin,nanq,
     &                                                    sigc,eigc)

      sigq=0.d0
      ipar=5
      if(ioprn3.eq.1)call cov_ages(ksfit(ipar),ipar,ibegin,nanq,
     &                                                    sigq,eigq)

!     measurement errors 
      sige=0.d0
      do i=1,nage(1)
      ij=istrt(4)+nq*(nq+1)/2*(meage(i)-1)
      do iq=1,nq
      i1=(iq-1)*nage(1)+i
      do jq=iq,nq
      j1=(jq-1)*nage(1)+i
      ij=ij+1
      sige(i1,j1)=xvec( ij )
      sige(j1,i1)=xvec( ij )
      end do
      end do
      end do
      work = (/ ((sige(j,i),j=i,nanq ),i=1,nanq) /) 
      CALL EIGEm(WORK,ZERO,ES,nanq,NNEG)
      eige(:nanq)=eig(:nanq)

!     phenotypic variance
      sigp=sige
      if(irrmet(1,iq).eq.1)sigp=sigp+siga
      if(irrmet(2,iq).eq.1)sigp=sigp+sigm
      if(irrmet(4,iq).eq.1)sigp=sigp+sigc
      if(irrmet(5,iq).eq.1)sigp=sigp+sigq
      work = (/ ((sigp(j,i),j=i,manq),i=1,manq ) /) 
      CALL EIGEm(WORK,ZERO,ES,manq,NNEG)
      eigp(:nanq)=eig(:nanq)
      call deall_eigen

C     ----------------------
C     CALCULATE CORRELATIONS
C     ----------------------

      naa=nanq*(nanq+1)/2
      allocate(rra(naa),rrm(naa),rram(mage,mage),rrr(naa),rrc(naa), 
     &         rrq(naa),rre(naa),rrp(naa),stat=ii)
      if(ii>0)stop 'alloc : correlations'

c     check that phenotypic variances are sensible
      do i=1,nanq
      if(sigp(i,i).lt.zero .or. sigp(i,i).gt.(1.d0/zero))then
         print *,'routine DFCOR3 : invalid phenotypic variance !'
         print *,i,iiage(i,1),sigp(i,i)
         stop
      end if
      end do

      call correl_ages (rra,siga,irrmet(1,iq))
      if(ioprn2.eq.1)call correl_ages (rrm,sigm,irrmet(2,iq))
      if(ioprn1.eq.1)call correl_ages (rrc,sigc,irrmet(4,iq))
      if(ioprn3.eq.1)call correl_ages (rrq,sigq,irrmet(5,iq))
      call correl_ages (rre,sige,1)
      call correl_ages (rrp,sigp,1)

      RETURN

      contains

!     =================================================
      subroutine cov_ages(kk,ipar,ibegin,isig,sig,eigg)
!     =================================================

      integer, intent(in)                        :: kk,ipar,isig
      integer, intent(inout)                     :: ibegin
      real(8), dimension(isig,isig), intent(out) :: sig
      real(8), dimension(manq),intent(out)       :: eigg
      real(8), dimension(:,:), allocatable       :: hh,ww

      sig=0.d0
      im=irrmet(ipar,1)
      if(im.ne.1)return                 ! must regress on first meta-meter
      allocate(hh(nage(im),kk),ww(kk,kk),stat=ii)
      if(ii>0)stop 'dfcor3 : alloc hh'

!     pick out coefficient matrix
      ww(:kk,:kk)=reshape(xvec( (/ (( ibegin+ihmssf(i,j,kk),i=1,kk),
     &                                 j=1,kk) /)), shape=(/ kk,kk /) )
      ibegin=ibegin+kk*(kk+1)/2

!     pre- and postmultiply with phi and phi'
      ki=0
      do iq=1,nq
      ni=(iq-1)*nage(im)
      kj=0
      do jq=1,nq
      nj=(jq-1)*nage(im)
      hh(:,:kfit(ipar,jq))=matmul(phi(:nage(im),:kfit(ipar,iq),ipar,iq),
     &                 ww(ki+1:ki+kfit(ipar,iq),kj+1:kj+kfit(ipar,jq) ))
      sig(ni+1:ni+nage(im),nj+1:nj+nage(im))=matmul(hh(:,:kfit(ipar,jq))
     &,              transpose( phi(:nage(im),:kfit(ipar,jq),ipar,jq) ))
      kj=kj+kfit(ipar,jq)
      end do
      ki=ki+kfit(ipar,iq)
      end do

!     calculate eigenvalues
      work = (/ ((sig(j,i),j=i,nage(im)*nq),i=1,nage(im)*nq) /) 
      call eigem(work,zero,es,nage(im)*nq,nneg)
      eigg(:nage(im)*nq)=eig(:nage(im)*nq)

      deallocate(hh,ww,stat=ii)
      if(ii>0)stop 'dfcor3 : de-alloc hh'
      return
      end subroutine cov_ages

!     ==================================
      subroutine correl_ages (rr,sig,im)
!     ==================================

      real(8),dimension (manq*(manq+1)/2), intent(out) :: rr
      real(8),dimension (manq,manq), intent(in)        :: sig
      integer, intent(in)                              :: im

      ij=0
      do i=1,manq
      ij=ij+1
      if(im.eq.1)then
         rr(ij)=sig(i,i)/sigp(i,i)
      else
         rr(ij)=1.d0
      end if
      do j=i+1,manq
      ij=ij+1
      rr(ij)=sig(j,i)/dsqrt( sig(i,i)*sig(j,j) )
      end do
      end do

      return
      end subroutine correl_ages

      END subroutine dfcor3










