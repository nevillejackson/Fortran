c===========================================================================
      double precision function vartio ( x, y, vxx, vxy, vyy )
c===========================================================================

c     function to work out approx. variance of a ratio x/y
c     (use first order taylor series expansion)
      real(8), intent(in) :: x,y,vxx,vxy,vyy
      real(8)             :: y2,vv

      y2=y*y
      vv=vxx*y2+vyy*x*x-2.d0*x*y*vxy
      vv=vv/y2
      vartio=vv/y2
      end function vartio

c===========================================================================
      double precision function wratio ( x, y, vxx, vxy, vyy )
c===========================================================================

c     function to work out approx. variance of a ratio x/(x+y)

      real(8), intent(in) :: x,y,vxx,vxy,vyy
      real(8)             :: z,vzz,vxz,z2

      z=x+y
      vzz=vyy+vxx+2*vxy     !  var of x+y
      vxz=vxx+vxy           !  cov of x and x+y
      z2=z*z
      wratio=(vxx*z2+vzz*x*x-2.d0*x*z*vxz)/(z2*z2)
      end function wratio
