c=============================================================================
      subroutine dxlgnd (nq)
c=============================================================================

c     purpose : routine to evaluate coefficients of Legendre polynomials

c     matrix of coefficients :
c          -> row i for order i-1
c          -> column j for coefficient to power j-1
c             (1 = scalar, 2=linear, 3=quadratic, 4=cubic, etc.)
c--------------------------------------------------------------km 7/95-------

      use legendre

      integer, intent(in) :: nq

      integer             :: iord,jj,m,i,j,mm
      real(8)             :: c1,c2,cc,binfac

      clgndr=0.d0

      do iord=0,nq-1
      c1=2.d0**iord
      c2=dble(iord)+0.5d0
      cc=dsqrt(c2)/c1
      jj=iord/2
      do m=jj,0,-1
      i=1+iord-2*m
      j=2*(iord-m)
      mm=1
      if(m>0)mm=(-1)**m             ! some compilers spew if exponent=0
      clgndr(i,iord+1)=mm*cc*binfac(iord,m)*binfac(j,iord)
      end do
      end do

      return
      end subroutine dxlgnd

!     ======================================  ! evaluate binomial
      double precision function binfac (n,m)  ! n over m
!     ======================================

      integer,intent(in) :: n,m
      integer            :: n1
      real(8)            :: factrl,b1,b2,b3

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

!     ====================================   ! evaluate factorial
      double precision function factrl (n)   ! of n
!     ====================================

      integer, intent(in) :: n
      integer             :: i
      real(8)             :: ff

      ff=1.d0
      do i=2,n
      ff=ff*dble(i)
      end do
      factrl=ff
      return
      end function factrl
