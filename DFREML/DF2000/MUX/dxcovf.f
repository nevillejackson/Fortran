C     Last change:  KM   17 Nov 97   10:34 am
c===========================================================================
      SUBROUTINE      DXCOVF (xvec,kparm)
c===========================================================================

c     purpose :       routine to write out results (CF estimates) to
c                     unit "66"
c-------------------------------------------------------------km--8/95------

      use params
      use names
      use units
      use ages
      use numbers

!     arguments
      real(8), dimension(mxparm), intent(in) :: xvec
      integer, intent(in) :: kparm

!     local variables
      character(len=25)            :: UNKN,FORMA,FSTAND
      real(8), dimension(maxnqq,6) :: ckoeff
!      real(8), dimension(maxnqq,6) :: xkoeff,ckoeff
!      real(8), dimension(maxnq,6)  ::  ceigs
      real(8), dimension(maxnqq)   :: XKK
      real(8), dimension(maxnq)    :: astar
      real(8) :: xlow,xupp,aa,aamin,aamax
      integer :: i,j,ij,jj,kk
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c     write out estimates to unit "17"
      UNKN='UNKNOWN'
      FORMA='FORMATTED'
      FSTAND='DF17#DAT'
      CALL FCONCT(IUN17,FSTAND,FORMA,UNKN)
      do i=1,kparm
      write(iun17,900)xvec(i),param(i)
      end do

      nq=nosvec(3)
      aamin=iiage(1)
      aamax=iiage(nq)
      xlow=-1.d0
      xupp=1.d0
      aa=(xupp-xlow)/(aamax-aamin)
      do i=1,nq
      astar(i)=xlow+aa*(iiage(i)-aamin)
      end do


      WRITE(IUN66,908)'---------------------------------'
      WRITE(IUN66,*)  'ESTIMATES OF COVARIANCE FUNCTIONS'
      WRITE(IUN66,*)  '---------------------------------'
      WRITE(IUN66,*)' '

c     additive genetic
      jj=1
      kk=kfit(jj)
      ij=0
      do i=1,kk
      do j=i,kk
      ij=ij+1
      xkk(ij)=xvec(ij)
!      xkoeff(ij,jj)=xkk(ij)
      end do
      end do
      WRITE(IUN66,908)'ADDITIV-GENETIC (DIRECT) COVARIANCE MATRIX'
      call cofunc  (kk,jj)

c     maternal genetic
      if(nosvec(15).eq.1)then
         jj=2
         kk=kfit(jj)
         mm=ij
         do i=1,kk
         do j=i,kk
         ij=ij+1
         xkk(ij-mm)=xvec(ij)
!         xkoeff(ij-mm,jj)=xkk(ij-mm)
         end do
         end do
         WRITE(IUN66,908)'SECOND "ANIMAL" COVARIANCE MATRIX'
         call cofunc  (kk,jj)
      end if

c     add. random effect
      if(nosvec(14).eq.1)then
         jj=4
         kk=kfit(jj)
         mm=ij
         do i=1,kk
         do j=i,kk
         ij=ij+1
         xkk(ij-mm)=xvec(ij)
!         xkoeff(ij-mm,jj)=xkk(ij-mm)
         end do
         end do
         WRITE(IUN66,908)'COVARIANCE MATRIX FOR ADD. RANDOM EFFECT'
         call cofunc  (kk,jj)
      end if

c     error
      jj=5
      kk=kfit(jj)
      mm=ij
      do i=1,kk
      do j=i,kk
      ij=ij+1
      xkk(ij-mm)=xvec(ij)
!      xkoeff(ij-mm,jj)=xkk(ij-mm)
      end do
      end do
      WRITE(IUN66,908)'ERROR COVARIANCE MATRIX'
      call cofunc  (kk,jj)
908   FORMAT(/1X,A)
900   format(g16.8,20x,a)
      return

      contains

c     ====================================
      SUBROUTINE      COFUNC (kkfit,iparm)
c     ====================================

      use legendre

      integer, intent(in) :: kkfit, iparm

      real(8), dimension(maxnq,maxnq) :: vv,vvi
      real(8), dimension(maxnq) :: a1,a2,eig
      real(8) :: es,aa,aaa,xx
      integer :: i,j,ij,nneg,ifit,k,m
      integer, external :: ihmssf

      write(iun66,*)' '
      write(iun66,901)'order of polynomial fit',kkfit
      write(iun66,*)' '
      write(iun66,*)'Estimated coefficient matrix'
      do i=1,kkfit
      write(iun66,902)i,(xkk(ihmssf(i,j,kkfit)),j=1,i)
      end do

c     eigenvalue decomposition of coefficient matrix
      call eigem(xkk,eig,zero,es,maxnq,vv,kkfit,nneg)
      write(iun66,*)' '
      write(iun66,*)'Eigenvalues of coefficient matrix & cov. function'
      do i=1,kkfit
      write(iun66,902)i,eig(i)
!      ceigs(i,iparm)=eig(i)
      end do
      
c     transform eigenvectors to those of the covariance function
      vvi=0.d0
      do ifit=1,kkfit
      do j=1,kkfit
      vvi(:kkfit,ifit)=vvi(:kkfit,ifit)+vv(j,:kkfit)*clgndr(ifit,j)
      end do
      end do
      write(iun66,*)' '
      write(iun66,*)'Eigenfunctions of covariance function'
      do i=1,kkfit
      write(iun66,902)i,(vvi(i,j),j=1,kkfit)
      end do

c     covariance function
      vv=0.d0
      do ifit=1,kkfit
      do jfit=1,kkfit
      do i=1,kkfit
      do j=1,kkfit
      vv(i,j)=vv(i,j)+clgndr(i,ifit)*clgndr(j,jfit)*
     *                              xkk(ihmssf(ifit,jfit,kkfit))
      end do
      end do
      end do
      end do
      write(iun66,*)' '
      write(iun66,*)'Coefficients of covariance function'
      do i=1,kkfit
      write(iun66,902)i-1,(vv(i,j),j=1,kkfit)
      do j=1,i
      ckoeff(ihmssf(i,j,kkfit),iparm)=vv(i,j)
      end do
      end do

c     calculate (regenerated)  covariance matrix
      ij=0
      do i=1,nq
      aa=astar(i)
      aaa=1.d0
      do m=1,kkfit
      a1(m)=aaa
      aaa=aaa*aa
      end do
      do j=i,nq
      ij=ij+1
      aa=astar(j)
      aaa=1.d0
      do m=1,kkfit
      a2(m)=aaa
      aaa=aaa*aa
      end do
      xx=0.d0
      do k=1,kkfit
      do m=1,kkfit
      xx=xx+a1(k)*a2(m)*ckoeff(ihmssf(k,m,kkfit),iparm)
      end do
      end do
!      sig(ij)=xx
      end do
      end do

      return
 901  format(1x,a,t30,' =',i6)
 902  format(i4,(t6,8g13.6))
      end subroutine cofunc

      end subroutine dxcovf










