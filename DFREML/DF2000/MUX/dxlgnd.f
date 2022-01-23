C     Last change:  KM   17 Nov 97   10:21 am
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

      integer :: iord,jj,m,i,j,mm
      real(8) :: c1,c2,cc,binfac

      call all_legendre (nq)
      clgndr=0.d0

      do iord=0,nq-1
      c1=2.d0**iord
      c2=iord+0.5d0
      cc=dsqrt(c2)/c1
      jj=iord/2
      do m=jj,0,-1
      i=1+iord-2*m
      j=2*(iord-m)
      mm=1
      if(m>0)mm=(-1)**m
      clgndr(i,iord+1)=mm*cc*binfac(iord,m)*binfac(j,iord)
      end do
      end do

      return
      end subroutine dxlgnd

c===========================================================================
      double precision function binfac (n,m)
c===========================================================================

      integer, intent(in) :: n,m
      integer :: n1
      real(8) :: factrl,b1,b2,b3


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

c===========================================================================
      double precision function factrl (n)
c===========================================================================

      integer, intent(in) :: n
      integer :: i
      real(8) :: ff

      ff=1.d0
      do i=2,n
      ff=ff*i
      end do
      factrl=ff
      return
      end function factrl





