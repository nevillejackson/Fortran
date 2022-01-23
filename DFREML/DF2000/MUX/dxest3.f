c===========================================================================
      subroutine dxest3 (nparm,tau,deth,save,xvec,fval)
c===========================================================================

c     purpose : routine to obtain estimates and likelihood for given "tau"

c               save = vector of starting values (reparm. scale; input)
c               xvec = estimates (reparm. scale) for "tau" (output)
c               fval = -2 log L for current estimates
c--------------------------------------km---4/95-----converted to f90--5/96---

      use params
      use units
      use derivs
      use parmap
      use numbers

!     arguments
      integer, intent(in)                      :: nparm
      real(8), intent(in)                      :: tau      
      real(8), intent(out)                     :: deth,fval
      real(8), dimension(nparm), intent(in)    :: save
      real(8), dimension(nparm), intent(inout) :: xvec

!     local variables
      integer, dimension (:),allocatable       :: iw
      real(8), dimension (:), allocatable      :: hess, w1,w2
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate(hess(kparm*(kparm+1)/2),w1(kparm),w2(kparm),iw(kparm),
     &                                                        stat=ii)
      if(ii>0)stop 'dxest3 : alloc'

c     set up hessian matrix for given "tau" & invert ...
      hess=0.d0
      ij=0
      lparm=0
      do i=1,kparm
      if(ipm(i).eq.0)then
         ij=ij+1
         hess(ij)=-der2(ij)+tau*dabs(der2(ij))
!                  minus sign here because NR alg is x_t+1 =x_t - H_t ^(-1) g_t
         do j=i+1,kparm
         ij=ij+1
         if(ipm(j).eq.0)hess(ij)=-der2(ij)
         end do
         lparm=lparm+1
      else
         ij=ij+kparm-i+1
      end if
      end do
      call dkmwhf(hess,w1,w2,deth,zero,iw,nrank,kparm,0)

      if(nrank.lt.lparm)then
         print *,'Hessian matrix not of full rank !'
         print *,'no. of parameters =',kparm,lparm
         print *,'rank              =',nrank
         print *,'tau               =',tau
      end if

c     multiply inverse hessian with vector of gradients
      do i=1,kparm
      if(ipm(i).eq.0)then
         xx=0.d0
         do j=1,kparm
         xx=xx+hess(ihmssf(i,j,kparm))*der1(j)
         end do
         xvec(i)=save(i)+xx
      end if
      end do

c     calculate values for parameters not estimated
      if(nosvec(22).eq.1.and.lparm.lt.kparm)then
         
         nq=nosvec(3)
         do ll=1,6
         do i=1,nq
         do j=i,nq
         ij=nparno(i,j,ll)
         if(ij.gt.0 .and. ipm(ij).eq.1)then
            xx=0.d0
            do k=1,i-1
            xx=xx+xvec(nparno(k,i,ll))*xvec(nparno(k,j,ll))
            end do
            xx=-xx/xvec(nparno(i,i,ll))
            xvec(ij)=xx
         end if
         end do
         end do
         end do
      end if

c     calculate likelihood for estimates
      call dxlik3(nparm,xvec,fval,0)
      print *,'tau =', tau,'   log L =',-0.5d0*fval

      deallocate(hess,w1,w2,iw,stat=ii)
      if(ii>0)stop 'dxest3 : de-alloc'

      return
      end subroutine dxest3
