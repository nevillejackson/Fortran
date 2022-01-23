C=========================================================================
      SUBROUTINE DXNRP3 (ioprun,nparm,iconv,xvec,fmin)
C=========================================================================

      use params
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

!     arguments
      integer,intent(in)                     :: ioprun,nparm
      integer,intent(out)                    :: iconv
      real(8), dimension(nparm), intent(out) :: xvec
      real(8), intent(out)                   :: fmin

!     local variables
      real(8) :: cc=0.d0,dnorm,dnorm1,fvalue,xlik00,xconv,deth
      real(8) ,dimension(:), allocatable     :: save,save1
      integer                                :: ntry,ii
      LOGICAL                                :: lopen
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate(save(nparm),save1(nparm),stat=ii)
      if(ii>0)stop 'alloc dxnrp3'

      NREUSE=0
      NROUND=0
      NCALL=0

      call conv_crit

C     START ITERATIONS
 222  FVALUE=0.d0
      INQUIRE(unit=28,opened=lopen)
      if(lopen) close(28)
      open(28,file='Iterates',status='unknown',form='formatted',
     *                                         position='append')

C     EVALUATE LIKELIHOOD AND DERIVATIVES FOR STARTING VALUES
      CALL DXLIK3(NPARM,XVEC,FVALUE,1)
      XLIK00=-0.5D0*FVALUE

      IF(NROUND.EQ.0)write(28,1988)nround,xlik00,0.d0,0.d0,0.d0,0
      nround=nround+1
      print *,'start',nround,xlik00

C     PARAMETER VECTOR OUT OF BOUNDS ...
      IF(dabs(FVALUE-BIG)<zz0)then
         CALL DFERR4(IOPRUN,kPARM,XVEC)
         GO TO 222
      END IF

c     save starting values (K-matrix scale)
      save(:kparm)=xvec(:kparm)
!      dnorm=dsqrt( dot_product(devl1(:kparm),devl1(:kparm) ) )
      dnorm=0.d0
      do i=1,kparm
      dnorm=dnorm+devl1(i)*devl1(i)
      end do
      dnorm=sqrt(dnorm)

c     reparameterise to impose bounds on the parameter space ...
      if(kopt.gt.0)then
         call dxchn3(kparm,kopt,xvec)
      else
         der1=devl1
         der2=devl2
      end if

c     now we are on the  scale where estimation takes place
      save1(:kparm)=xvec(:kparm)
!      dnorm1=dsqrt( dot_product(der1(:kparm),der1(:kparm) ) )
      dnorm1=0.d0
      do i=1,kparm
      dnorm1=dnorm1+der1(i)*der1(i)
      end do
      dnorm1=sqrt(dnorm1)

      write(*,*)' '
      write(iun66,*)' '

      write(iun66,*)'Newton-Raphson iteration : round no. =',nround
      write(iun66,*)'norm of gradient vector (orig. scale) =',dnorm
      write(iun66,*)'norm of gradient vector (new scale)   =',dnorm1
      write(*,919)'NR : round no. =',nround,' norm =',dnorm,dnorm1
 919  format(1x,a,i4,a,2g15.6)

c     estimates and log L for tau=0
      tau2=0.d0
      nt50=1
      ntry=0
 333  call dxest3 (nparm,tau2,deth,save1,xvec,fvalue)
      xlik2=-0.5d0*fvalue

c     likelihood has improved, skip search for optimal step size
      if(xlik00.lt.xlik2)then
         go to 20

c     likelihood not improved, search for step size which does
      else if(ntry.eq.0 .and. dabs(fvalue-big)>zz0)then
         call search(nparm,xlik00,fvalue,save1,xvec)
         ntry=ntry+1
         go to 333

c     likelihood not improved, search for step size which does
      else if(ntry.lt.2 .and. dabs(fvalue-big)>zz0)then
         tau2=tau2+nt50*5.d0
         nt50=nt50+1
         call search(nparm,xlik00,fvalue,save1,xvec)
         ntry=ntry+1
         go to 333

c     search did not yield better step size
      else if(dabs(fvalue-big)>zz0)then
         if(tau2.le.150.d0)then
             tau2=tau2+nt50*5.d0
             nt50=nt50+1
             go to 333
         end if
         write(iun66,*)'failed to improve likelihood !'
         write(*,*)'failed to improve log L !',xlik2,xlik00,tau2
         xvec(:kparm)=save(:kparm)
         go to 9999

c     invalid estimates
      else if(dabs(fvalue-big)>zz0)then
         tau2=tau2+nt50
         nt50=nt50+1
         if(tau2.le.150.d0) go to 333
         write(iun66,*)'failed to find valid estimates !'
         write(iun66,*)'"tau" value =',tau2
         print *,'fail to obtain valid estimates !',tau2
         xvec(:kparm)=save(:kparm)
         go to 9999

      end if

c     estimates for this round of iteration
 20   write(*,21)'Iterate No. =',nround,'  tau =',tau2,'  log L =',
     *                                                  xlik2,xlik00
 21   format(1x,a,i4,a,f8.3,a,2g16.8)
      write(iun66,*)'optimal value for "tau" =',tau2
      write(iun66,*)'current max. log L      =',xlik2,xlik00

      do i=1,kparm
      write(iun66,603)i,param(i),save(i),xvec(i)
 603  format(i4,4x,a,4g16.8)
      print 603,i,param(i),save(i),xvec(i)
      end do
      cc=xlik2-xlik00
      write(28,1988)nround,xlik2,tau2,dnorm,cc, ncalls(1)
 1988 format(i4,f20.10,f12.6,g13.5,g16.7,i5)

c     check for convergence 
c     ... norm of gradient vector less than 10**(-4)
      if(dnorm.lt.1.d-4)then
          print *,'convergence reached :'
          print *,'norm of gradient vector less than 10**(-4)'
          write(28,*)'norm of gradient vector less than 10**(-4)'
          go to 9999
c     ... increase in likelihood less than 10**(iic)
      else if(dabs(cc).lt.xconv)then
          print *,'convergence reached :'
          print *,'change in log L less',xconv
          write(28,*)'change in log L less than',xconv
          go to 9999
      else if(nround.lt.mround)then
          go to 222
      end if
      print *,'stop iterating : max. no. of rounds reached',mround
      write(28,*)'stop iterating : max. no. of rounds reached',mround

 9999 fmin=fvalue
      write(28,1988)nround,xlik2,tau2,dnorm,cc, ncalls(1)
      print *,'no. of new-raphson iterates =',nround
      print *,'no. of calls to "DXLIK3"    =',ncalls(2)
      print *,'no. of calls to "DFLIKE"    =',ncalls(1)
      write(*,*)'no. of "re-uses" of fact.    =',nreuse
      write(28,*)'no. of calls to "DXLIK3"    =',ncalls(2)
      write(28,*)'no. of calls to "DFLIKE"    =',ncalls(1)
      write(28,*)'no. of "re-uses" of fact.    =',nreuse
      write(iun66,*)'no. of calls to "DXLIK3"    =',ncalls(2)
      write(iun66,*)'no. of calls to "DFLIKE"    =',ncalls(1)
      write(iun66,*)'no. of "re-uses" of fact.    =',nreuse
      return

      contains

!     ====================
      subroutine conv_crit
!     ====================

      integer :: iic

      if(iosrch>0)then  ! use 'default' values
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

      END subroutine dxnrp3





