c========================================================================
      subroutine      dxkfg3 (xvec,nparm)
c========================================================================

c     purpose :       replace vector of covariance components by elements
c                     of corresponding coefficient matrices
c------------------------------------------------------------------------

      use params
      use units
      use phimatrix
      use parmap
      use numbers

!     arguments
      integer,  intent(in)                      :: nparm
      real(8), dimension(mxparm), intent(inout) :: xvec

!     local variables
      integer                                   :: i,j
      integer, dimension(:), allocatable        :: iwrk
      real(8), dimension(:), allocatable        :: eig,xkk,h2,work,sig
      real(8), dimension(:,:), allocatable      :: vv
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate (ntok(nparm+nq),kton(nparm+nq),sig(nqq),stat=ii)
      if(ii>0)stop 'dxkfg3 : alloc'
      allocate(vv(nq,nq),eig(nq),xkk(nqq),h2(nqq),work(nqq*(nqq+1)/2),
     &         iwrk(nq), stat=ii)
      if(ii>0)stop 'alloc KFROMG'

c     additive genetic
      sig=xvec(:nqq)
      call kfromg(kfit(1))
      xvec(:nqq)=sig

c     maternal genetic
      if( nosvec(15) .eq.1 )then
         sig=xvec(istrt(1)+1:istrt(1)+nqq)
         call kfromg(kfit(2))
         xvec(istrt(1)+1:istrt(1)+nqq)=sig
      end if

c     worry about models with sig am some other time ...
      
c     additional random effect
      if(nosvec(14).eq.1)then
         sig=xvec(istrt(3)+1:istrt(3)+nqq)
         call kfromg(kfit(4))
         xvec(istrt(3)+1:istrt(3)+nqq)=sig
      end if

c     error components
      sig=0.d0
      kk=0
      do  i=1,nq
      do  j=i,nq
      kk=kk+1
      k=nparno(j,i,5)
      if(k.gt.0)sig(kk)=xvec(k)
      end do
      end do
      call kfromg(kfit(5))
      xvec(istrt(4)+1:istrt(4)+nqq)=sig

c     leave out models with correlated residuals too for the moment

c     NB : this subroutine assume all nqq var comp. are fitted for 
c          each random factor !

      call cndnse

      ipm(:kparm)=0
      ipm(kparm+1:nparm)=1

      deallocate(vv,eig,xkk,h2,work,iwrk,sig,stat=ii)
      if(ii>0)stop 'de-alloc KFROMG'

      return

      contains

c     =======================
      SUBROUTINE       CNDNSE 
c     =======================
 
c     purpose :        "condense" vector of parameters (potentially for a 
c                      full fit) to those actually estimated in rewduced fit
c----------------------------------------------------------------------------

      use names

c     establish mapping (do this once)
      ntok=0
      lparm=0

c     sig a
      jj=1
      do i=1,kfit(jj)
      do j=i,kfit(jj)     
      lparm=lparm+1
      ii=nparno(i,j,jj)
      ntok(ii)=lparm
      kton(lparm)=ii
      end do
      end do

c     sig m
      if(ioprn2.eq.1)then
         jj=2
         do i=1,kfit(jj)
         do j=i,kfit(jj)     
         lparm=lparm+1
         ii=nparno(i,j,jj)
         ntok(ii)=lparm
         kton(lparm)=ii
         end do
         end do
      end if

c     have not accommodated  models with sig am here !

c     sig c
      if(ioprn1.eq.1)then
         jj=4
         do i=1,kfit(jj)
         do j=i,kfit(jj)     
         lparm=lparm+1
         ii=nparno(i,j,jj)
         ntok(ii)=lparm
         kton(lparm)=ii
         end do
         end do
      end if
      if(ioprn3.eq.1)then
         jj=7
         do i=1,kfit(jj)
         do j=i,kfit(jj)     
         lparm=lparm+1
         ii=nparno(i,j,jj)
         ntok(ii)=lparm
         kton(lparm)=ii
         end do
         end do
      end if

c     sig e
      jj=5
      do i=1,kfit(jj)
      do j=i,kfit(jj)     
      lparm=lparm+1
      ii=nparno(i,j,jj)
      ntok(ii)=lparm
      kton(lparm)=ii
      end do
      end do

c     sig me
      if(iomease.eq.1)then
        do i=1,nq
        lparm=lparm+1
        ii=nparm+i
        ntok(ii)=lparm
        kton(lparm)=ii
        end do
      end if

c     condense parameter vector 
      do i=1,lparm
      ii=kton(i)
      xvec(i)=xvec(ii)
      param(i)=param(ii)
      end do

      kparm=lparm
      return
      end subroutine cndnse

c========================================================================
      SUBROUTINE      KFROMG (kq)
c========================================================================

      use phimatrix

!     arguments
      integer, intent(in)                           :: kq

!     local variables
      integer                                       :: ij,i,j,k,irnk
      real(8)                                       :: xx, det, es

c     set up matrix phi2 long-hand ...
      call phisym(nq,kq)

c     set up "X'X"
 1000 ij=0
      do i=1,nqq
      do j=i,nqq
      ij=ij+1
!      work(ij)=dot_product( phi2(:nqq,i),phi2(:nqq,j))
      xx=0.d0
      do k=1,nqq
      xx=xx+phi2(k,i)*phi2(k,j)
      end do
      work(ij)=xx
      end do
      end do

c     invert ...
      call dkmwhf(work,xkk,h2,det,zero,iwrk,irnk,nqq,0)

c     "X'y"
      do i=1,nqq
      xx=0.d0
      do j=1,nqq
      xx=xx+phi2(j,i)*sig(j)
      end do
      h2(i)=xx
      end do
!      h2=matmul( transpose(phi2(:nqq,:nqq)),sig(:nqq))

c     ... coefficients
      do i=1,nqq
      xx=0.d0
      do j=1,nqq
      xx=xx+work(ihmssf(i,j,nqq))*h2(j)
      end do
!      xx=dot_product( work((/(ihmssf(i,j,nqq),j=1,nqq)/)),h2(:nqq))
      if(dabs(xx).lt.zero)xx=0.d0
      sig(i)=xx
      end do

c     re-generate cov. matrix and check for p.(s.)d.
      call ktog(sig,h2,nq,kq)

      call eigem(h2,eig,zero,es,nq,vv,nq,ineg)
      if(ineg.gt.0)then
c        make negative eigenvalues positive and try again 
c        (for symmetric coeff.s, could also check matrix K directly)
         do i=1,nq
         print *,'"DXKFG3" : eig',i,eig(i)
         if(eig(i).lt.zero)eig(i)=.001d0
         end do
         sig=0.d0
         sig( (/ (ihmii(i,nq),i=1,nq)/))=eig(:nq)
         call inverrt(vv,nq,nq)
         call matrba(vv,sig,nq,nq)
         go to 1000
      end if

      return
      end subroutine kfromg

      end subroutine dxkfg3

c=========================================================================
      SUBROUTINE KTOG(xkk,sigs,nq,kq)
c=========================================================================

c     purpose :       calculate covariance matrix from coefficients matrix
c--------------------------------------------------------------km--7/95---

      use phimatrix

!     arguments
      real(8), dimension(nq*(nq+1)/2), intent(in)  :: xkk
      real(8), dimension(nq*(nq+1)/2), intent(out) :: sigs
      integer, intent(in)                          :: kq,nq

!     local variables
      integer, external                            :: ihmssf
      integer                                      :: i,j,ij
      real(8)                                      :: gg
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      ij=0
      do i=1,nq
      do j=i,nq
      gg=0.d0
      do k=1,kq
      do l=1,kq
      gg=gg+phi(i,k)*phi(j,l)*xkk(ihmssf(k,l,nq))
      end do
      end do
      ij=ij+1
      sigs(ij)=gg
      end do
      end do
      return
      end subroutine ktog

C=======================================================================
      SUBROUTINE INVerRT( A, IA, N)
C=======================================================================

      INTEGER, INTENT(IN)                      :: n,ia
      REAL(8), DIMENSION(ia,ia), INTENT(INOUT) :: a
      REAL(8), DIMENSION(:), ALLOCATABLE       :: vec
      INTEGER, DIMENSION(:), ALLOCATABLE       :: iflag
      REAL(8) :: zave, zero=1.d-12, xx, zz
      INTEGER :: ii,imax,izave,i,j,k

      ALLOCATE(vec(n),iflag(n),STAT=ii)
      IF(ii>0)STOP 'alloc inverrt'

      IFLAG(:n)=(/ (i,i=1,n) /)

      DO I=1,N

C     FIND MAXIMUM ELEMENT IN THE COLUMN (START AT I-TH EL. ONLY)
      XX=DABS(A(I,I))
      IMAX=I
      DO J=I+1,N
      ZZ=DABS(A(J,I))
      IF(ZZ.GT.XX)THEN
         XX=ZZ
         IMAX=J
      END IF
      END do

C     CHECK FOR SINGULARITY
      IF(XX.LT.ZERO)THEN
          WRITE(*,*)'"INVRT" : MATRIX IS SINGULAR'
          STOP
      END IF

C     INTERCHANGE ROW I AND ROW WITH MAX. ELEMENT IN THE COLUMN
      IF(IMAX.GT.I)THEN
         DO K=1,N
         zave=A(I,K)
         A(I,K)=A(IMAX,K)
         A(IMAX,K)=zave
         END do
         Izave=IFLAG(I)
         IFLAG(I)=IFLAG(IMAX)
         IFLAG(IMAX)=Izave
      END IF

C     TRANSFORM THE MATRIX
      zave=1.D0/A(I,I)
      A(:n,I)=A(:n,I)*zave
      A(I,I)=zave
      DO K=1,N
      IF(K.EQ.I)cycle
      DO J=1,N
      IF(J.NE.I)A(J,K)=A(J,K)-A(J,I)*A(I,K)
      END do
      A(I,K)=-A(I,K)*zave
      END do

      END do

C     INTERCHANGE COLUMNS (ANALOGOUS TO PREVIOUS ROW CHANGES )
      DO I=1,N
      DO K=1,N
      J=IFLAG(K)
      VEC(J)=A(I,K)
      END do
      A(I,:n)=VEC(:n)
      END do

      RETURN
      END subroutine inverrt
