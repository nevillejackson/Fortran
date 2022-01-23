c===========================================================================
      subroutine dxest3 (tau,alpha,deth,fval,xlike)
c===========================================================================

      use parameters
      use units
      use derivs
      use parmap
      use numbers
      use hess_wrk

!     arguments
      real(8), intent(in)                       :: tau,alpha
      real(8), intent(out)                      :: deth,fval,xlike
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c     set up hessian matrix for given "tau" ...
      hess=0.d0
      ij=0
      lparm=0
      do i=1,kparm
      if(ipm(i).eq.0)then
         ij=ij+1
         hess(ij)=-der2(ij)+tau*dabs(der2(ij))   ! x_t+1 =x_t - H_t ^(-1) g_t
         do j=i+1,kparm
         ij=ij+1
         if(ipm(j).eq.0)hess(ij)=-der2(ij)
         end do
         lparm=lparm+1
      else                                       ! allow for fixed parameters
         ij=ij+kparm-i+1
      end if
      end do

!     ... and invert
      call dkmwhf(hess,w1,w2,deth,zero,iw,nrank,kparm,0)

      if(nrank.lt.lparm)then
         write(*,'(a)')' Hessian matrix not of full rank !'
         write(*,'(a,2i6)')' no. of parameters =',kparm,lparm
         write(*,'(a,2i6)')' rank              =',nrank
         write(*,'(a,g12.5)')' tau               =',tau
      end if

c     multiply inverse hessian with vector of gradients
      do i=1,kparm
      if(ipm(i).ne.0)cycle
      xx=0.d0
      do j=1,kparm
      xx=xx+hess(ihmssf(i,j,kparm))*der1(j)
      end do
      if(iopeps.eq.1.and.i.eq.kparm)then
         xvec(i)=save1(i) ! estimate sigeps directly
      else
         xvec(i)=save1(i)+xx*alpha
      end if
      end do

c     calculate likelihood for estimates
      w1(:kparm)=xvec(:kparm)
      xlike=-0.5d0*big
      call dxlik3(w1,fval,xlike,0)
      write(*,'(2(a,g12.5),a,f16.8)')' tau=', tau,'alpha=',alpha,
     &                             '  log L =',xlike
      return
      end subroutine dxest3





