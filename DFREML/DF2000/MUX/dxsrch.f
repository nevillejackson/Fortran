!=============================================================================
      SUBROUTINE search (nparm,xlik0,fvalue,save,xvec)
!=============================================================================
c     modification of steve smith's subroutine to determine opt. step size
c-----------------------------------------------------------------------------

      use params
      use units
      use grid

!     arguments
      integer, intent(in)               :: nparm
      real(8), intent(in)               :: xlik0
      real(8), intent(in)               :: fvalue
      real(8) ,dimension(nparm)         :: xvec,save

      real(8) :: x,deth,fval,xm,size1,size2,tm,choice,diff,p
      integer :: maxiter,iter,koff,move

      xlik1=-.5d0*fvalue
      tau1=tau2
      tau3=tau2-1.0d0
      maxiter=3
      diff=.1
      iter=0

      tau2=tau2+1.0d0
    1 continue
      iter=iter+1
      x=tau2
      call dxest3 (nparm,tau2,deth,save,xvec,fval)
      if(dabs(fval-big)<zz0)then
        tau2=tau2+tau2
        if(tau2.gt.100)then
           print *,'abandon search for tau'
           tau2=tau2+1.d0
           return
        end if
        go to 1
      end if
      xlik2=-0.5d0*fval
      if(xlik2.gt.xlik0)return

      if(xlik2.lt.xlik1)then
         tau3=tau2
         xlik3=xlik2
         tau2=tau2/(1.0d0+iter)
         if(iter.gt.maxiter)then
            tau2=0.d0
            xlik2=-.5d0*fvalue
            return
         end if
         go to 1
      end if
      if(tau3.gt.0.0d0)go to 10

      tau3=1.0d0
    2 tau3=tau3+1.0d0+iter
      iter=iter+1
      x=tau3
      call dxest3 (nparm,x,deth,save,xvec,fval)
      xlik3=-0.5d0*fval
      if(xlik3.lt.xlik2)go to 10

      tau1=tau2
      xlik1=xlik2
      tau2=tau3
      xlik2=xlik3
      if(iter.gt.maxiter)return
      go to 2
   10 continue
      p=0.1d0
      koff=0

   11 continue
      if(koff.eq.0)then
         xm=choice(p,move)
      else
         size1=tau2-tau1
         size2=tau3-tau2
         if(size1.lt.size2)then
            xm=(tau2+tau3)/2.0d0
         else
            xm=(tau2+tau1)/2.0d0
         end if
      end if
      iter=iter+1
      if(xm.lt.0.or.xm.gt.1.d6)return
      call dxest3 (nparm,xm,deth,save,xvec,fval)
      tm=-0.5d0*fval

      if(koff.eq.0)then
         if((move.eq.1).and.(tm.lt.xlik2))koff=1
         if((move.eq.2).and.(tm.gt.xlik2))koff=1
      else
         koff=0
      end if

      if(xm.lt.tau2)then
         if(tm.gt.xlik2)then
           tau3=tau2
           xlik3=xlik2
           tau2=xm
           xlik2=tm
         else 
           tau1=xm
           xlik1=tm
         end if
      else
         if(tm.gt.xlik2)then
           tau1=tau2
           xlik1=xlik2
           tau2=xm
           xlik2=tm
         else 
           tau3=xm
           xlik3=tm
         end if
      end if
      if( (iter.gt.maxiter) .or. (tau2-tau1.lt.diff) .or. 
     *                           (tau3-tau1.lt.diff) )return
      if( (xlik2-xlik1.lt.diff) .or. (xlik3-xlik1.lt.diff) )return
      go to 11

      end subroutine search

c==========================================================================  
      function choice(p,move)
c==========================================================================  

c     function to estimate optimal step size based on  quadratic fit
 
      use grid
      USE units

      real(8), intent(in) :: p
      integer, intent(out) :: move

      real(8) :: xvec1,xvec2,xvec3,choice,d1,d2,xm,size1,size2,xo
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      xvec1=tau1*tau1
      xvec2=tau2*tau2
      xvec3=tau3*tau3

      d1=xlik1*(xvec2-xvec3)+xlik2*(xvec3-xvec1)+xlik3*(xvec1-xvec2)
      d2=xlik1*(tau2-tau3)+xlik2*(tau3-tau1)+xlik3*(tau1-tau2)

      xm=(d1/d2)/2.0d0
      if(dabs(xm-tau2)<zz0)then
         move=2
         size1=tau3-tau2
         size2=tau2-tau1
         if(size1.lt.size2)then
            choice=p*tau1+(1.0d0-p)*xm
         else
            choice=p*tau3+(1.0d0-p)*xm
         end if
         return
      end if

      if(xm.lt.tau2)then
         xo=p*tau1+(1.0d0-p)*(xm+xm-tau2)
         size1=tau2-tau1
         size2=tau3-xo
      else
         xo=p*tau3+(1.0d0-p)*(xm+xm-tau2)
         size1=tau3-tau2
         size2=xo-tau1
      end if
      if(size1.lt.size2)then
         move=1 
         choice=xm
      else
         move=2
         choice=xo
      end if

      return
      end function choice












