C=======================================================================
      SUBROUTINE DFSPX3(IOPRUN,NPARM,IMIN,ICONV,XVEC,FMIN)
C=======================================================================

      use params
      use names
      use units
      use parmap
      use likelihoods
      use numbers
      use like_components

!     arguments
      integer, intent(in)                       :: ioprun,nparm
      integer, intent(out)                      :: imin,iconv
      real(8), dimension(mxparm), intent(inout) :: xvec
      real(8), intent(inout)                    :: fmin

!     CONSTANTS FOR SIMPLEX OPERATIONS
      real(8) :: alpha=1.d0  ! REFLECTION COEFFICIENT
      real(8) :: beta=0.5d0  ! CONTRACTION COEFFICIENT
      real(8) :: gamma=2.d0  ! EXPANSION COEFFICIENT

!     local variables
      real(8) :: zz,zz1,fstep,fvalue,fss,fmax,ff,
     *           fmax1,xconv,delta,fexpnd,fcontr,fadd,fsum
      real(8), dimension (:), allocatable   :: step,center,tryvec,fcvec
      real(8), dimension (:,:), allocatable :: poly
      LOGICAL                               :: lopen
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate(step(nparm),center(nparm),tryvec(nparm),fcvec(nparm+1),
     *         poly(nparm,nparm+1),stat=ii)
      if(ii>0)stop 'dfspx3 : alloc '

      if(iosrch<0)then
         WRITE(*,*)' '
         WRITE(*,*)'CONVERGENCE CRITERION : MINIMUM VAR (-2 LOG L) ?'
         IIC=8
         CALL DFCONV(XCONV,IIC)
         WRITE(*,*)' '
         WRITE(*,*)'MAXIMUM NO. OF SIMPLEX ITERATES ALLOWED ?'
         CALL OPTION(MROUND,1,MXFUNC)
         WRITE(*,*)' '
         WRITE(*,*)'STEP SIZE FOR SETTING UP INITIAL SIMPLEX ?'
         CALL DFSTEP(FSTEP,0.D0,10.D0)
      else
         xconv=1.d-8
         mround=mxfunc
         fstep=0.2d0
      end if
      

      NROUND=0
      NPARM1=NPARM+1
      lparm=0
      do i=1,nparm
      if(ipm(i).eq.0)lparm=lparm+1
      end do
      ZZ=1.D0/LPARM
      ZZ1=1.D0/(LPARM+1)

C     ----------------------
C     SET UP INITIAL SIMPLEX
C     ----------------------

      call in_steps

      FSS=0.D0
      FVALUE=fmin

      if(fmin.eq.big)then
 222     CALL DXLIK3(NPARM,XVEC,FVALUE,0)
         IF(dabs(FVALUE-BIG)<zz0)then
            xvec(:nparm)=xvec(:nparm)*ffvec(:nparm)
            call dferr4(ioprun,nparm,xvec)
            print *,xvec(:nparm)
            xvec(:nparm)=xvec(:nparm)/ffvec(:nparm)
            go to 222
         end if
      else
         WRITE(*,'(A)')' Starting point recovered from DF59#DAT '
         XLIKE=-0.5d0*fvalue 
         WRITE(*,'(A,G20.10)')' Log L = ',xlike
      end if

      FCVEC(NPARM1)=FVALUE
      POLY(:,NPARM1)=XVEC(:nparm)

      DO II=1,NPARM
      IF(IPM(II).NE.0)cycle
      SAVE=XVEC(II)
      DELTA=STEP(II)
      XVEC(II)=SAVE+DELTA
      IBOUND=0
21    CALL DXLIK3(NPARM,XVEC,FVALUE,0)
      IF(dabs(FVALUE-BIG)<zz0)THEN
         call point_out
         GO TO 21
      END IF
      FCVEC(II)=FVALUE
      POLY(:,II)=XVEC(:nparm)
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

      center=0.d0
      DO II=1,NPARM1
      jj=0
      if(ii<nparm1)jj=ipm(ii)
      if(ii.eq.imax .or. jj>0)cycle
      CENTER=CENTER+POLY(:,II)
      end do
      CENTER=CENTER*ZZ

C     ---------------
C     REFLECTION STEP
C     ---------------

      where(IPM(:nparm).EQ.0)XVEC(:nparm)=CENTER+ALPHA*(CENTER-
     &                                                   POLY(:,IMAX))
      CALL DXLIK3(NPARM,XVEC,FVALUE,0)

      IF( FVALUE.GE.FMIN .AND. FVALUE.LE.FMAX1 )THEN
C        NEW POINT IS NEITHER BEST NOR WORSE : RETAIN & START NEW ITERATE
         FCVEC(IMAX)=FVALUE
         POLY(:,IMAX)=XVEC(:nparm)

      ELSE IF(FVALUE.LT.FMIN)THEN
C        NEW BEST POINT

C        --------------
C        EXPANSION STEP
C        --------------

         TRYVEC=CENTER+GAMMA*(XVEC(:nparm)-CENTER)
         CALL DXLIK3(NPARM,TRYVEC,FEXPND,0)

         IF(FEXPND.LT.FVALUE)THEN
C           EXPANSION SUCCESSFUL : NEW BEST POINT
            FCVEC(IMAX)=FEXPND
            POLY(:,IMAX)=TRYVEC
         ELSE
C           RETAIN REFLECTION POINT
            FCVEC(IMAX)=FVALUE
            POLY(:,IMAX)=XVEC(:nparm)
         END IF

      ELSE

C        ----------------
C        CONTRACTION STEP
C        ----------------

         IF(FVALUE.LT.FMAX)THEN
C           REFLECTION POINT IS BETTER THAN WORST POINT
            XVEC(:nparm)=CENTER+BETA*(XVEC(:nparm)-CENTER)
            FF=FVALUE
         ELSE
            XVEC(:nparm)=CENTER+BETA*(POLY(:,IMAX)-CENTER)
            FF=FMAX
         END IF
         CALL DXLIK3(NPARM,XVEC,FCONTR,0)
         IF(FCONTR.LT.FF)THEN
C           CONTRACTION SUCCESSFUL : RETAIN POINT
            FCVEC(IMAX)=FCONTR
            POLY(:,IMAX)=XVEC(:nparm)
         ELSE
C           FURTHER CONTRACTION : SHRINK COMPLETE POLYTOPE
            DO II=1,NPARM1
            jj=0
            if(ii<nparm1)jj=ipm(ii)
            if(ii.eq.imin .or. jj>0 )cycle
            where(ipm(:nparm).eq.0)XVEC(:nparm)=(POLY(:,II)+
     &                                           POLY(:,IMIN))*0.5D0
            POLY(:,II)=Xvec(:nparm)
            CALL DXLIK3(NPARM,XVEC,FVALUE,0)
            FCVEC(II)=FVALUE
            end do
         END IF
      END IF

C     ---------------------
C     CHECK FOR CONVERGENCE
C     ---------------------

      FADD=-fcvec(nparm1)
      FSUM=FCVEC(NPARM1)+FADD
      FSS=FSUM*FSUM
      DO I=1,NPARM
      IF(IPM(I).eq.0)then
         FF=FCVEC(I)+FADD
         FSUM=FSUM+FF
         FSS=FSS+FF*FF
      end if
      end do
      FSS=(FSS-FSUM*FSUM*ZZ1)*ZZ
         
      IF(MOD(NROUND,5).EQ.0)then
         WRITE(*,333)NROUND,FSS,XCONV
333      FORMAT(11X,'IT =',I6,3X,'VAR =',G14.4,4X,'CONV =',G14.4)
         INQUIRE(unit=28,opened=lopen)
         if(lopen) close(28)
         open(28,file='Iterates',status='unknown',form='formatted',
     *                                            position='append')
         WRITE(28,333)NROUND,FSS,XCONV
         CLOSE(28)
      END if
      IF(NROUND.LT.MROUND .AND. FSS.GT.XCONV)GO TO 100

C     FIND LOWEST POINT IN SIMPLEX = ESTIMATES
      FMIN=fcvec(nparm1)
      IMIN=nparm1
      DO  I=1,NPARM
      IF(IPM(I).eq.0 .and. FCVEC(I).LT.FMIN)THEN
         FMIN=FCVEC(I)
         IMIN=I
      END IF
      end do
      if(imin.eq.0)stop 'simplex : min not found !'
      XVEC(:nparm)=POLY(:,IMIN)
      IF(FSS.LE.XCONV)ICONV=1

      WRITE(*,*)'TOTAL NO. OF ITERATES FOR THIS ANALYSIS =',NROUND
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
      fmin=fcvec(nparm1)
      imin=nparm1
      do i=1,nparm
      if(ipm(i).eq.0 .and. fcvec(i).lt.fmin)then
         fmin=fcvec(i)
         imin=i
      end if
      end do
      if(imin.eq.0)stop 'simplex : imin=0'

c     find worst point
      fmax=fcvec(nparm1)
      imax=nparm1
      do i=1,nparm
      if(ipm(i).eq.0 .and. fcvec(i).gt.fmax)then
         fmax=fcvec(i)
         imax=i
      end if
      end do
      if(imax.eq.0)stop 'simplex : imax=0'

c     next to worst point
      if(imax.eq.nparm1)then
         fmax1=fmin
         imax1=imin
      else
         fmax1=fcvec(nparm1)
         imax1=nparm1
      end if
      do i=1,nparm
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

      real(8) :: fstep

      IF(iosrch<0 )THEN
         WRITE(*,*)' '
         WRITE(*,*)'STEP SIZE FOR SETTING UP INITIAL SIMPLEX ?'
         CALL DFSTEP(FSTEP,0.D0,10.D0)
      ELSE
         FSTEP=0.1D0
      END IF

      step=0.d0
      DO  I=1,nPARM
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

      END subroutine dfspx3






