!==========================================================================
      program kstart
!==========================================================================

!     purpose : obtain starting values for K-matrices in random
!               regression models
!              a) have estimate(s) for different order of fit, want
!                 corresponding matrix for higher/lower value of k
!                 -> set up cov matrix for 'ma' standardised ages
!                    for each K-matrix
!                 -> get estimate of K-matrix for new order of fit
!                    using LSQ (future : update to ML)
!  not yet     b) have covariance matrices among observations
!  implemented   (not necessarily all age in the data)
!--------------------------------------------------------------km 2/2000---

      integer, parameter              :: mk=22     ! max. order of fit
      integer, parameter              :: ma=21     ! no. of ages

      character(len=20)               :: fin='                    ',
     &                                   fout
      real(8), dimension(mk*(mk+1)/2) :: xkk
      real(8), dimension(mk,mk)       :: clgnd,ww
      real(8), dimension(ma)          :: astar
      real(8), dimension(ma*(ma+1)/2) :: cov
      real(8), dimension(ma,ma)       :: sig	
      real(8), dimension(ma,mk)       :: phi,hh
      real(8)                         :: xupp=1.d0, xlow=-1.d0, aa,
     &                                   aamin, pp, zero=1.d-8

!     ACQUIRE NAME OF INPUT FILE & OPEN
      write(*,'(a)')'Input file ? (<return> for DF17#DAT) '
      read(*,'(a)')fin
      if(fin(1:6).eq.'     ')fin='DF17#DAT'
      open(1,file=fin,status='old')

!     OUTPUT FILE IS "DF18#DAT"
      fout='DF18#DAT'
      open(2,file=fout,status='unknown')

!     SET UP VECTOR OF STANDARDISED AGES
      astar(:ma)=(/ (i,i=1,ma) /)
      aa=(xupp-xlow)/(astar(ma)-astar(1))
      aamin=astar(1)
      astar(:ma)=xlow+aa*(astar(:na)-aamin)

!     SET UP PHIMATRIX
      call legendre (mk)
      do ifit=1,mk
      do i=1,ma
      aa=astar(i)
      pp=clgnd(1,ifit)
      do j=2,ifit
      pp=pp+clgnd(j,ifit)*aa**(j-1)
      end do
      phi(i,ifit)=pp
      end do
      end do

!     INTERACTIVE INPUT
      write(*,'(a)')'Number of K- matrices ?'
      call option(ncovm,1,10)
      write(*,'(a)')'Number of parameters for meas.error variances ?'
      call option(me,1,999)

!     FOR EACH K-MATRIX ...
      do ic=1,ncovm
      write(*,'(a,i3,a)')'Previous order of fit for k-matrix no.',
     &                                                       ic,'  ?'
      call option(kold,1,mk)
      write(*,'(a)')'New order of fit for this matrix ?'
      call option(knew,1,mk)

!     ...READ OLD K-MATRIX
      kk1=kold*(kold+1)/2
      do i=1,kk1
      read(1,*)xkk(i)
      end do

      if(kold.ne.knew)then
!        set up variance matrix among observations
!        ... pick out coefficient matrix
         ww(:kold,:kold)=reshape(xkk( (/ (( ihmssf(i,j,kold),i=1,kold),
     &                          j=1,kold) /)), shape=(/ kold,kold /) )

!        ... pre- and postmultiply with phi and phi'
         hh(:ma,:kold)=matmul( phi(:ma,:kold), ww(:kold,:kold) )
         sig(:ma,:ma)=matmul( hh(:ma,:kold),transpose(phi(:ma,:kold)) )
         ij=0
         do i=1,ma
         if(knew>kold)sig(i,i)=sig(i,i)*1.05d0  ! augment diagonals to make
         do j=i,ma                              ! up for rank deficiencies
         ij=ij+1
         cov(ij)=sig(j,i)
         end do
         end do

!        obtain least squares estimate
         call kmat_lsq (knew, ma)
      end if

!     write out new matrix
      ij=0
      do i=1,knew
      do j=i,knew
      ij=ij+1
      write(2,'(g20.8,10x,a,i1,4x,2i3)')xkk(ij),'   K',ic,i,j
      end do
      end do
      
      end do  ! ic
 
!     read & write out m.e. variance parameters
      if(me<0)me=-me+1
      do i=1,me
      read(1,*)xx
      write(2,'(g20.8,10x a)')xx,'m.e. var parameter'
      end do

      write(*,*)'Done ! -> Output file is ',fout
      contains

!     =============================
      subroutine kmat_lsq (kfit,na)
!     =============================

      integer, intent(in)                  :: kfit,na
      real(8), dimension(:,:), allocatable :: xmat
      real(8), dimension(:), allocatable   :: work,w1,w2
      integer, dimension(:), allocatable   :: iwork
      real(8)                              :: detx

      kk=kfit*(kfit+1)/2
      allocate (xmat(na*(na+1)/2,kk),work(kk*(kk+1)/2),w1(kk),
     &          w2(kk), iwork(kk), stat=ii)
      if(ii>0)stop 'alloc kmat_lsq'
      
!     design matrix X
      xmat=0.d0
      ij=0
      do i=1,na
      do j=i,na
      ij=ij+1
      do m=1,kfit
      do n=1,kfit
      mn=ihmssf(m,n,kfit)
      xmat(ij,mn)=xmat(ij,mn)+phi(i,m)*phi(j,n)
      end do
      end do
      end do
      end do
!     ... coefficient matrix X'X
      ij=0
      do i=1,kk
      do j=i,kk
      ij=ij+1
      xx=0.d0
      do k=1,na*(na+1)/2
      xx=xx+xmat(k,i)*xmat(k,j)
      end do
      work(ij)=xx
      end do
       end do
!     ... inverse
      call dkmwhf(work,w1,w2,detx,zero,iwork,irank,kk,0)
!     ... RHS X'vec(V)
      do i=1,kk
      xx=0.d0
      do k=1,na*(na+1)/2
      xx=xx+xmat(k,i)*cov(k)
      end do
      w1(i)=xx
      end do
!     ... lsq-estimate
      ij=0
      do ij=1,kk
      xx=0.d0
      do m=1,kk
      xx=xx+work(ihmssf(ij,m,kk))*w1(m)
      end do
      xkk(ij)=xx
      end do
      print *,'lsq estimate of K'
      do i=1,kfit
      print '(i4,(6f10.4))',i,(xkk(ihmssf(i,j,kfit)),j=1,i)
      end do
      deallocate (xmat,work,w1, w2, iwork, stat=ii)
      if(ii>0)stop 'deall kmat_lsq'
      return
      end subroutine kmat_lsq
     
!     ========================
      subroutine legendre (nc)
!     ========================

      integer, intent(in) :: nc
      real(8)             :: c1, c2, cc, binfac
      integer             :: iord,jj,i,j,kk,m
      clgnd=0.d0

      do iord=0,nc-1
      if(iord.eq.0)then
         c1=1.d0
         c2=0.5d0
      else
         c1=2.d0**iord
         c2=iord+0.5d0
      end if
      cc=dsqrt(c2)/c1
      jj=iord/2
      do m=jj,0,-1
      i=1+iord-2*m
      j=2*(iord-m)
      kk=1
      if(m.gt.0)kk=(-1)**m
      clgnd(i,iord+1)=kk*cc*binfac(iord,m)*binfac(j,iord)
      end do
      end do

      return
      end subroutine legendre

      end program kstart

!     ======================================
      double precision function binfac (n,m)
!     ======================================

      integer, intent(in) :: n,m
      real(8)             :: factrl,b1,b2,b3
      integer             :: n1

      if(m.eq.0.or.m.eq.n)then
         binfac=1.d0
      else
         b1=factrl(n)
         n1=n-m
         b2=factrl(n1)
         b3=factrl(m)
         binfac=b1/(b2*b3)
      end if
      return
      end function binfac

!     ====================================
      double precision function factrl (n)
!     =====================================

      integer, intent(in) :: n
      real(8)             :: ff
      integer             :: i

      ff=1.d0
      do i=2,n
      ff=ff*i
      end do
      factrl=ff
      return
      end function factrl














