!=======================================================================
      SUBROUTINE DFSPX3 (IOPRUN,ISILEN,IMIN,ICONV,FMIN)
!=======================================================================

      use parameters, only : mxparm, xvec
      use names
      use units
      use parmap
      use likelihoods
      use numbers
      use like_components

!     arguments
      implicit none
      integer, intent(in)                       :: ioprun,isilen
      integer, intent(out)                      :: imin,iconv
      real(8), intent(inout)                    :: fmin

!     CONSTANTS FOR SIMPLEX OPERATIONS
      real(8)                    :: alpha=1.d0  ! REFLECTION COEFFICIENT
      real(8)                    :: beta=0.5d0  ! CONTRACTION COEFFICIENT
      real(8)                    :: gamma=2.d0  ! EXPANSION COEFFICIENT

!     local variables
      real(8) :: zz,zz1,fvalue,fss,fmax,ff,xlike,save,
     *           fmax1,xconv,delta,fexpnd,fcontr,fadd,fsum,fstep
      real(8), dimension (:), allocatable   :: step,center,tryvec,fcvec
      real(8), dimension (:,:), allocatable :: poly
      integer                               :: mparm,ii,imax,i,jj,iic,
     &                                         mparm1, ibound
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      if(iopeps.eq.1)then
         mparm=kparm-1 ! no. of parameters (on reparameterised scale)
      else
         mparm=kparm
      end if

      allocate(step(mparm),center(kparm),tryvec(kparm),fcvec(mparm+1),
     *         poly(kparm,mparm+1),stat=ii)
      if(ii>0)stop 'dfspx3 : alloc '

      IF(ISILEN.EQ.0 .and. iosrch<0 )THEN
         WRITE(*,*)' '
         WRITE(*,*)'CONVERGENCE CRITERION : MINIMUM VAR (-2 LOG L) ?'
         IIC=8
         CALL DFCONV(XCONV,IIC)
         WRITE(*,*)' '
         WRITE(*,*)'MAXIMUM NO. OF SIMPLEX ITERATES ALLOWED ?'
         CALL OPTdef(MROUND,1,9999,200)
      else
         mround=500
         xconv=1.d-8
         FSTEP=0.1D0
      END IF

      NROUND=0
      MPARM1=MPARM+1

      lparm=count(mask=(ipm(:mparm).eq.0))
      lparm=0
      do i=1,mparm
      if(ipm(i).eq.0)lparm=lparm+1
      end do
      ZZ=1.D0/LPARM
      ZZ1=1.D0/(LPARM+1)

C     ----------------------
C     SET UP INITIAL SIMPLEX
C     ----------------------

      call in_steps

      NCALL=LCALL
      FVALUE=0.D0

!     vector of starting values
      if(fmin.eq.big)then
 222     CALL DxLIK3(XVEC,FVALUE,xlike,0)
         if(kparm>44)write(*,*)ncall,xlike

         IF(dabs(FVALUE-BIG)<zz0)then
            xvec(:kparm)=xvec(:kparm)*ffvec(:kparm)
            call dferr4(ioprun,kparm,xvec)
            fvalue=0.d0
            write(*,*)xvec(:kparm)
            where(ffvec(:kparm).ne.0)xvec(:kparm)=
     &                                     xvec(:kparm)/ffvec(:kparm)
            go to 222
         end if
      else
         fvalue=fmin
         WRITE(*,'(A)')' Starting point recovered from DF59#DAT '
         XLIKE=-0.5d0*fvalue -DETL*(KsFIT(1)+ksfit(2))
         WRITE(*,'(A,G20.10)')' Log L = ',xlike
      end if

      FCVEC(MPARM1)=FVALUE
      POLY(:,MPARM1)=XVEC(:KPARM)

!     other points of polytope
      DO II=1,MPARM
      IF(IPM(II).ne.0)cycle
      SAVE=XVEC(II)
      DELTA=STEP(II)
      XVEC(II)=SAVE+DELTA
      IBOUND=0
21    CALL DxLIK3(XVEC,FVALUE,xlike,0)
      if(kparm>44)write(*,*)ncall,xlike
      IF(dabs(FVALUE-BIG)<zero)then
         call point_out 
         GO TO 21
      END IF
      FCVEC(II)=FVALUE
      POLY(:,II)=XVEC(:kparm)
      XVEC(II)=SAVE
      end do

C     ------------------------------
C     DETERMINE BEST AND WORST POINT
C     ------------------------------

100   NROUND=NROUND+1

      call points (imin,imax,fmin,fmax,fmax1)

C     ------------------
C     DETERMINE CENTROID
C     ------------------

      CENTER=0.D0
      DO II=1,MPARM
      IF(II.EQ.IMAX .OR. IPM(II)>0 )CYCLE
      CENTER=CENTER+POLY(:,II)
      END DO
      if(imax<mparm1)CENTER=CENTER+POLY(:,mparm1)
      CENTER=CENTER*ZZ

C     ---------------
C     REFLECTION STEP
C     ---------------

      where(ipm(:kparm).eq.0)xvec(:kparm)=center+alpha*(center-
     &                                                  poly(:,imax))
      CALL DxLIK3(XVEC,FVALUE,xlike,0)
         if(kparm>44)write(*,*)ncall,xlike

      IF( FVALUE.GE.FMIN .AND. FVALUE.LE.FMAX1 )THEN
C        NEW POINT IS NEITHER BEST NOR WORSE : RETAIN & START NEW ITERATE
         FCVEC(IMAX)=FVALUE
         POLY(:,IMAX)=XVEC(:kparm)

      ELSE IF(FVALUE.LT.FMIN)THEN
C        NEW BEST POINT

C        --------------
C        EXPANSION STEP
C        --------------

         TRYVEC=CENTER+GAMMA*(XVEC(:kparm)-CENTER)
         CALL DxLIK3(TRYVEC,FEXPND,xlike,0)
         if(kparm>44)write(*,*)ncall,xlike

         IF(FEXPND.LT.FVALUE)THEN
C           EXPANSION SUCCESSFUL : NEW BEST POINT
            FCVEC(IMAX)=FEXPND
            POLY(:,IMAX)=TRYVEC
         ELSE
C           RETAIN REFLECTION POINT
            FCVEC(IMAX)=FVALUE
            POLY(:,IMAX)=XVEC(:kparm)
         END IF

      ELSE

C        ----------------
C        CONTRACTION STEP
C        ----------------

         IF(FVALUE.LT.FMAX)THEN
C           REFLECTION POINT IS BETTER THAN WORST POINT
            XVEC(:kparm)=CENTER+BETA*(XVEC(:kparm)-CENTER)
            FF=FVALUE
         ELSE
            XVEC(:kparm)=CENTER+BETA*(POLY(:,IMAX)-CENTER)
            FF=FMAX
         END IF
         CALL DxLIK3(XVEC,FCONTR,xlike,0)
         if(kparm>44)write(*,*)ncall,xlike
         IF(FCONTR.LT.FF)THEN
C           CONTRACTION SUCCESSFUL : RETAIN POINT
            FCVEC(IMAX)=FCONTR
            POLY(:,IMAX)=XVEC(:kparm)
         ELSE
C           FURTHER CONTRACTION : SHRINK COMPLETE POLYTOPE
            DO II=1,MPARM1
            jj=0
            if(ii<mparm1)jj=ipm(ii)
            IF(II.EQ.IMIN .or. jj.ne.0 )cycle
            where(ipm(:kparm).eq.0)xvec(:kparm)=0.5d0*(poly(:,ii)
     &                                                +poly(:,imin))
            poly(:,ii)=xvec(:kparm)          
            CALL DxLIK3(XVEC,FVALUE,xlike,0)
            if(kparm>44)write(*,*)ncall,xlike
            FCVEC(II)=FVALUE
            end do
         END IF
      END IF

C     ---------------------
C     CHECK FOR CONVERGENCE
C     ---------------------

      FADD=-fcvec(mparm1)
      FSUM=FCVEC(MPARM1)+FADD
      FSS=FSUM*FSUM
      DO I=1,MPARM
      IF(IPM(I).ne.0)cycle
      FF=FCVEC(I)+FADD
      FSUM=FSUM+FF
      FSS=FSS+FF*FF
      end do
      FSS=(FSS-FSUM*FSUM*ZZ1)*ZZ
         
C     FIND LOWEST POINT IN SIMPLEX = ESTIMATES
      FMIN=fcvec(mparm1)
      IMIN=mparm1
      DO  I=1,MPARM
      IF(IPM(I).eq.0 .and. FCVEC(I).LT.FMIN)THEN
         FMIN=FCVEC(I)
         IMIN=I
      END IF
      end do
      XLIKE=-0.5d0*fmin -DETL*(KsFIT(1)+ksfit(2))

      IF(ISILEN.NE.1.AND.MOD(NROUND,5).EQ.0)WRITE(*,333)NROUND,FSS,Xlike
333   FORMAT(11X,'IT =',I6,3X,'VAR =',G12.4,4X,'MAX =',f16.4)
      if(mod(nround,10).eq.0)call write_lik28

!     continue iterating
      IF(NROUND < MROUND .AND. FSS > XCONV) GO TO 100

      if(imin.eq.0)stop 'simplex : min not found !'
      XVEC(:kparm)=POLY(:,IMIN)

      IF(ISILEN.EQ.1)RETURN
      IF(FSS.LE.XCONV)ICONV=1

      WRITE(*,*)'TOTAL NO. OF ITERATES FOR THIS ANALYSIS =',LROUND
      WRITE(*,*)'VARIANCE OF FUNCTION VALUES             =',FSS
      WRITE(*,*)'CONVERGENCE CRITERION                   =',XCONV

      deallocate(step,center,tryvec,fcvec,poly)

      RETURN

      contains

!     ==================================================
      subroutine points (imin,imax,fmin,fmax,fmax1)
!     ==================================================

      integer, intent(out) :: imin,imax
      real(8),intent(out)  :: fmin,fmax,fmax1
      integer              :: imax1,i

c     best point
      fmin=fcvec(mparm1)
      imin=mparm1
      do i=1,mparm
      if(ipm(i).eq.0 .and. fcvec(i).lt.fmin)then
         fmin=fcvec(i)
         imin=i
      end if
      end do
      if(imin.eq.0.or.dabs(fmin-big)<zero)stop 'simplex : imin=0'

c     find worst point
      fmax=fcvec(mparm1)
      imax=mparm1
      do i=1,mparm
      if(ipm(i).eq.0 .and. fcvec(i).gt.fmax)then
         fmax=fcvec(i)
         imax=i
      end if
      end do
      if(imax.eq.0)stop 'simplex : imax=0'

c     next to worst point
      if(imax.eq.mparm1)then
         fmax1=fmin
         imax1=imin
      else
         fmax1=fcvec(mparm1)
         imax1=mparm1
      end if
      do i=1,mparm
      if(i.eq.imax)cycle
      if(ipm(i).eq.0 .and. fcvec(i).gt.fmax1)then
         fmax1=fcvec(i)
         imax1=i
      end if
      end do
      if(imax1.eq.0)stop 'simplex : imax1=0'
      return
      end subroutine points

!     ===================
      subroutine in_steps
!     ===================

      IF(ISILEN.EQ.0 .and. iosrch<0 )THEN
         WRITE(*,*)' '
         WRITE(*,*)'STEP SIZE FOR SETTING UP INITIAL SIMPLEX ?'
         CALL DFSTEP(FSTEP,0.D0,10.D0)
      END IF

      step=0.d0
      DO  I=1,mPARM
      IF(IPM(I).ne.0)cycle
      IF(FSTEP.GT.0)THEN
         STEP(I)=XVEC(I)*FSTEP
         if(dabs(step(i)).lt.0.01 .and. xvec(i).ge.0)then
            step(i)=0.01d0
            write(*,*)'force step size',i,step(i),xvec(i)
         else if(dabs(step(i)).lt.0.01)then
            step(i)=-0.01d0
            write(*,*)'force step size',i,step(i),xvec(i)
         end if
      ELSE
         WRITE(*,*)'STEP SIZE FOR PARAMETER NO.',I,'   ',PARAM(I)
         WRITE(*,*)'WITH STARTING VALUE       =',XVEC(I)
         READ(*,*)STEP(I)
      END IF
      end do
      return
      end subroutine in_steps

!     ====================
      subroutine point_out
!     ====================

      IF(IBOUND.EQ.0)THEN
         DELTA=DELTA*0.5D0
      ELSE IF(IBOUND.EQ.1)THEN
         DELTA=-2.d0*DELTA
      ELSE if(ibound.lt.25.and.dabs(delta).gt.1.d-6)then
         delta=-0.25d0*delta
      else
         WRITE(*,*)'  '
         WRITE(*,*)'FAILED TO ASCERTAIN VALID PARAMETER VECTOR ',
     *             'IN INITIAL SIMPLEX  :'
         WRITE(*,*)'    PARAMETER NO.  =',II,'   ',PARAM(II)
         WRITE(*,*)'    STARTING VALUE =',SAVE
         WRITE(*,*)'    STEP SIZE      =',delta
         STOP 'CHECK STARTING VALUES/STEP SIZES !'
      END IF
      IBOUND=IBOUND+1
      XVEC(II)=SAVE+DELTA
      return
      end subroutine point_out

!     ======================
      subroutine write_lik28
!     ======================
      logical :: lopen

      INQUIRE(unit=iun28,opened=lopen)
      if(.not.lopen)open(iun28,file='Iterates',status='unknown',
     &                      form='formatted',position='append')

      write(iun28,'(i6,i8,g16.8,f19.9)')nround,ncall,fss,xlike
      close(iun28)
      open(iun28,file='Iterates',status='unknown',
     &                      form='formatted',position='append')

      return
      end subroutine write_lik28

      END subroutine dfspx3



















