!=========================================================================
      SUBROUTINE  DXNRP3 (ioprun,isilen,fmin)
!====================================================================km===

      use parameters
      use names
      use units
      use grid
      use times
      use derivs
      use iindex
      use parmap
      use combinations
      use likelihoods
      use numbers
      use dmatrices
      use like_components
      use current
      use solutions
      use hess_wrk

!     arguments
      integer,intent(in)                    :: ioprun,isilen
      real(8), intent(out)                  :: fmin

!     local variables
      real(8)                               :: cc=0.d0,dnorm,dnorm1
     &,                                        fvalue,xlik00,xconv,deth
     &,                                        alpha,fval00,dnmpre
     &,                                        taupre=0.d0
      integer                               :: ntry,iostep
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      call all_hessian(kparm)
      NREUSE=0
      NROUND=0
      NCALL=LCALL

      call conv_crit

!     start of round of iteration
 222  FVALUE=0.d0

C     EVALUATE LIKELIHOOD AND DERIVATIVES FOR STARTING VALUES
      CALL DXLIK3(XVEC,fvalue,xlik00,1)

      IF(NROUND.EQ.0)call write_lik28(nround,xlik00,0.d0,0.d0,0.d0,
     &                                                0.d0,0.d0)
      nround=nround+1
      fval00=fvalue
      write(*, '(1x,a,i4,g20.10)') 'start',nround,xlik00

C     PARAMETER VECTOR OUT OF BOUNDS ...
      IF(dabs(FVALUE-BIG)<zz0)then
         CALL DFERR4(IOPRUN,kPARM,XVEC)
         GO TO 222
      END IF

c     save starting values 
      save(:kparm)=xvec(:kparm)
      dnorm=dsqrt( dot_product(devl1,devl1) )

!     transform derivatives
      if(kopt.eq.3.or.kopt.eq.1)then
         call dxkdv3(dnorm1,xvec)
      else if(kopt.gt.0)then
         call dxchn3(kopt,xvec)
      else
         der1=devl1
         der2=devl2
      end if

c     now we are on the  scale where estimation takes place
      save1(:kparm)=xvec(:kparm)
      dnorm1=dsqrt( dot_product(der1,der1) )
      if(nround.eq.1)dnmpre=dnorm1
      write(*,*)' '
      write(iun66,*)' '

      write(iun66,*)'Newton-Raphson iteration : round no. =',nround
      write(iun66,*)'norm of gradient vector (orig. scale) =',dnorm
      write(iun66,*)'norm of gradient vector (new scale)   =',dnorm1
      write(*,919)'NR : round no. =',nround,' norm =',dnorm,dnorm1
 919  format(1x,a,i4,a,2g15.6)
      if(dnmpre*100.d0<dnorm1)then
         write(*,*)'Caution : Norm has increased more than 100-fold ',
     *             ' from last iterate !  '
         write(*,*)'current',dnorm1,'   previous',dnmpre
         write(*,*)'This is likely to be due to numerical problems !'
         write(*,*)'Do you want to proceed ? '
         call yndef(ii,0)
         if(ii.eq.0)stop
      end if

c     estimates and log L for tau=0
      tau2=0.d0
      alpha=1.d0
      if(iostep.eq.1)tau2=taupre*0.5d0
      if(iostep.eq.2.and.dnorm1>100)alpha=0.5d0
      if(iostep.eq.2.and.dnorm1>1000)alpha=0.25d0
      tau2b=tau2
      nt50=1
      ntry=0
 333  call dxest3 (tau2,alpha,deth,fvalue,xlik2)

c     likelihood has improved, end of iterate
      if(xlik00.lt.xlik2) go to 20

c     likelihood not improved, search for step size which does
      if(iostep.eq.2)then
         alpha=alpha*0.5d0
         if(alpha>0.01)go to 333
         write(*,*)' failed to improve log L !',xlik2,xlik00,alpha
         write(*,*)' norm',dnorm,dnorm1
         write(*,*)' switch to marquardt''s procedure ?'
         call yndef(ii,1)
         if(ii.eq.1)then
            iostep=1
            alpha=1.d0
            xvec(:kparm)=save1(:kparm)
            go to 333
         end if
         write(iun66,*)'failed to improve likelihood ! alpha =',alpha
         do i=1,kparm
         write(*,'(i4,3g16.8)')i,xvec(i),der1(i),devl1(i)
         end do
         xvec(:kparm)=save(:kparm)
         fvalue=fval00
         go to 9999

      else if(ntry.eq.0 .and. dabs(fvalue-big)>zz0)then
         call search(xlik00)
         ntry=ntry+1
         go to 333

c     likelihood not improved, search for step size which does
      else if(ntry.lt.2 .and. dabs(fvalue-big)>zz0)then
         tau2=tau2b+nt50*5.d0
         tau2b=tau2
         nt50=nt50+1
         call search(xlik00)
         ntry=ntry+1
         go to 333

c     search did not yield better step size
      else if(dabs(fvalue-big)>zz0)then
         if(tau2.le.150.d0)then
             tau2=tau2b+nt50*5.d0
             tau2b=tau2
             nt50=nt50+1
             go to 333
         end if
         write(iun66,*)'failed to improve likelihood !'
         write(*,*)' failed to improve log L !',xlik2,xlik00,tau2
         xvec(:kparm)=save(:kparm)
         do i=1,kparm
         write(*,'(i4,3g16.8)')i,xvec(i),der1(i),devl1(i)
         end do
         fvalue=fval00
         go to 9999

c     invalid estimates
      else if(dabs(fvalue-big)<zz0)then
         tau2=tau2b+nt50
         tau2b=tau2
         nt50=nt50+1
         if(tau2.le.150.d0) go to 333
         write(iun66,*)'failed to find valid estimates !'
         write(iun66,*)'"tau" value =',tau2
         write( *,'(a,g13.6)')' failed to obtain valid estimates !',tau2
         xvec(:kparm)=save(:kparm)
         fvalue=fval00
         go to 9999
      end if

c     estimates for this round of iteration
 200  format(1x,a,i4,a,f8.3,a,f6.3,a,2g16.6,a,g12.5)
 20   write(*,200)'It.No. =', nround,' tau =',tau2,' alpha=',
     & alpha,' log L =',xlik2,xlik00 !,' norm =',dnorm1
      write(iun66,*)'optimal value for "tau" =',tau2
      write(iun66,*)'current max. log L      =',xlik2,xlik00

      do i=1,kparm
      if(ipm(i).ne.0)cycle
      write(iun66,603)i,param(i),save(i),xvec(i),der1(i)
      write(*,603)i,param(i),save(i),xvec(i),der1(i)
      end do
      dnmpre=dnorm1
      taupre=tau2
      cc=xlik2-xlik00
      call write_lik28 (nround,xlik2,tau2,alpha,dnorm,dnorm1,cc)
 603  format(i4,4x,a,2g16.8,' 1st d',g16.8)

c     check for convergence 
c     ... norm of gradient vector less than 10**(-2)
      if(dnorm.lt.1.d-2)then
          write( *,*)' convergence reached :'
          write(*, *)' norm of gradient vector less than 10**(-2)'
          write(iun28,*)' norm of gradient vector less than 10**(-2)'
          go to 9999
c     ... increase in likelihood less than 10**(iic)
      else if(dabs(cc).lt.xconv)then
          write( *,*)' convergence reached :'
          write(*,*)' change in log L less than',xconv
          write(iun28,*)' change in log L less than',xconv
          go to 9999
      else if(nround.lt.mround)then
          go to 222
      end if
      write(*,'(a,i5)')' stop iterating : max. no. of rounds reached',
     &                                                         mround
      write(iun28,'(2a,i5)')' stop iterating : max. no. of rounds',
     &                                              ' reached',mround

 9999 fmin=fvalue

 1988 format(i4,f20.6,f9.3,f5.2,3g12.4,i5)
      write(iun28,1988)nround,xlik2,tau2,alpha,dnorm,dnorm1,cc,ncalls(1)
      write(*,'(a,t40,i10)')' no. of new-raphson iterates =',nround
!      write(*,'(a,t40,i10)')' no. of calls to "DXLIKE"    =',ncalls(2)
!      write(*,'(a,t40,i10)')' no. of calls to "DFLIKE"    =',ncalls(1)
      write(*,'(a,t40,i10)')' no. of "re-uses" of fact.    =',nreuse
!      write(iun28,'(a,t40,i10)')' no. of calls to "DXLIKE"    =',ncalls(2)
!      write(iun28,'(a,t40,i10)')' no. of calls to "DFLIKE"    =',ncalls(1)
      write(iun28,'(a,t40,i10)')' no. of "re-uses" of fact.    =',nreuse
      close(iun28)
!      write(iun66,'(a,t40,i10)')' no. of calls to "DXLIKE"    =',ncalls(2)
!      write(iun66,'(a,t40,i10)')' no. of calls to "DFLIKE"    =',ncalls(1)
      write(iun66,'(a,t40,i10)')' no. of "re-uses" of fact.    =',nreuse

      call deall_hessian(kparm)
      return

      contains

!     ====================
      subroutine conv_crit
!     ====================

      integer :: iic

      if(isilen.eq.1 .or.iosrch>0)then  ! use 'default' values
         iostep=1
         xconv=1.d-4
         mround=kparm+4
         return
      end if

      WRITE(*,*)' '
      WRITE(*,*)' Modified Newton-Raphson algorithm : '
      write(*,9)' 1  ...  Marquard; search for optimum step size'
      write(*,9)' 2  ...  Step Halving '
      call optdef(iostep,1,2,2)
9     FORMAT(7X,A)

      WRITE(*,*)' '
      WRITE(*,*)'CONVERGENCE CRITERION : MAX. CHANGE IN LOG L ?'
      IIC=4                          ! default value 10**(-iic)
      CALL DFCONV(XCONV,IIC)
      WRITE(*,*)' '
      WRITE(*,*)'MAXIMUM NO. OF NEWTON-RAPHSON ITERATES ALLOWED ?'
      CALL OPTDEF(MROUND,1,300,kparm+4)
      return
      end subroutine conv_crit

!     ====================================================
      subroutine write_lik28(n,xlike,tau,alpha,dn,dn1,chn)
!     ====================================================
      integer, intent(in) :: n
      real(8), intent(in) :: xlike, tau, alpha, dn, dn1, chn
      logical             :: lopen

      INQUIRE(unit=iun28,opened=lopen)
      if(.not.lopen)open(iun28,file='Iterates',status='unknown',
     &                      form='formatted',position='append')

      write(iun28,1988)n,xlike,tau,alpha,dn,dn1,chn
      close(iun28)
      open(iun28,file='Iterates',status='unknown',
     &                      form='formatted',position='append')

      return
 1988 format(i4,f20.6,f9.3,f5.2,3g12.4,i5)
      end subroutine write_lik28

      END subroutine dxnrp3
