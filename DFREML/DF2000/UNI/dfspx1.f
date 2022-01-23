C======================================================================
      SUBROUTINE DFSPX1(iopt,NPARM,KPARM,ICONV,FMIN,EVALUE)
C======================================================================

      use likelihoods
      use names, only : param
      use simplex
      use spasol
      use like
      use parameters
      use iterates
      use constants
      use numbers
      use units

      implicit double precision (a-h,o-z)

      integer, intent(in)                :: nparm, kparm,iopt
      integer, intent(out)               :: iconv
      real(8), intent(out)               :: fmin, evalue
      real(8), dimension(:), allocatable :: step,tryvec,center,eevec,
     &                                      fcvec
      real(8),dimension(:,:),allocatable :: poly

      real(8)                             :: ALPHA=1.D0,
     &                                       BETA=0.5D0, 
     &                                       GAMMA=2.D0,
     &                                       time, xsecs

      allocate(poly(nparm,nparm+1),fcvec(nparm+1),eevec(nparm+1),
     &           step(nparm),tryvec(nparm),center(nparm), stat=ii)
      if(ii>0)stop 'alloc simplex'

      if(iopt<0)then      ! read in values determining convergence
         WRITE(*,*)' '
         WRITE(*,*)'CONVERGENCE CRITERION : MINIMUM VAR (-2 LOG L) ?'
         CALL DFCONV(XCONV,8)
         WRITE(*,*)' '
         WRITE(*,*)'MAXIMUM NO. OF SIMPLEX ITERATES ALLOWED ?'
         CALL OPTDEF(MROUND,1,999,MXFUNC)
         WRITE(*,*)' '
         WRITE(*,*)'STEP SIZE FOR SETTING UP INITIAL SIMPLEX ?'
         CALL DFSTEP(FSTEP,0.D0,10.D0)
      else                 ! use default values
         xconv=1.d-8
         mround=500
         fstep=0.2d0
      end if

      NROUND=0
      ZZ=1.D0/KPARM
      ZZ1=1.D0/(KPARM+1)

C     ----------------------
C     SET UP INITIAL SIMPLEX
C     ----------------------

      step=0.d0
      IF(FSTEP.GT.0)THEN
         where(ipm.eq.0)STEP=XVEC(:nparm)*FSTEP
      ELSE
         DO I=1,NPARM
         IF(IPM(I)>0)cycle
         WRITE(*,*)'STEP SIZE FOR PARAMETER NO.',I,'   ',PARAM(I)
         WRITE(*,*)'WITH STARTING VALUE       =',XVEC(I)
         READ(*,*)STEP(I)
         end do
      END IF
      FSS=0.D0
      FVALUE=0.D0
      CALL DFTIME(time,xsecs,isecs,0,'abcdef')
      CALL DFLIK1(NPARM,XVEC,FVALUE,EVALUE)
      IF(dabs(FVALUE-BIG)<10.d-9)stop 'invalid starting point !!!'
      CALL DFTIME(time,xsecs,isecs,10,'abcdef')
      WRITE(*,*)'CPU TIME (SEC.S) REQUIRED PER LOG L =',ISECS
      write(iun28,'(a,i12)')' Time per "DXLIK1" ',isecs
      FCVEC(NPARM+1)=FVALUE
      EEVEC(NPARM+1)=EVALUE
      POLY(:,NPARM+1)=XVEC(:nparm)

      DO II=1,NPARM
      IF(IPM(II).NE.0)cycle
      SAVE=XVEC(II)
      DELTA=STEP(II)
      XVEC(II)=SAVE+DELTA
      IBOUND=0
21    CALL DFLIK1(NPARM,XVEC,FVALUE,EVALUE)
      IF(dabs(FVALUE-BIG)<10.d-9)THEN 
        IF(IBOUND.EQ.0)THEN
            DELTA=DELTA*0.5D0
        ELSE IF(IBOUND.EQ.1)THEN
            DELTA=-DELTA
        ELSE IF(IBOUND.EQ.2)THEN
            DELTA=DELTA+DELTA
        ELSE
            WRITE(*,*)'  '
            WRITE(*,*)'FAILED TO ASCERTAIN VALID PARAMETER VECTOR ',
     *                'IN INITIAL SIMPLEX  :'
            WRITE(*,*)'    PARAMETER NO.  =',II,'   ',PARAM(II)
            WRITE(*,*)'    STARTING VALUE =',SAVE
            WRITE(*,*)'    STEP SIZE      =',STEP(II)
            STOP 'CHECK STARTING VALUES/STEP SIZES !'
         END IF
         IBOUND=IBOUND+1
         XVEC(II)=SAVE+DELTA
         GO TO 21
      END IF
      FCVEC(II)=FVALUE
      EEVEC(II)=EVALUE
      POLY(:,II)=XVEC(:nparm)
      XVEC(II)=SAVE
      end do

C     ------------------------------
C     DETERMINE BEST AND WORST POINT
C     ------------------------------

100   NROUND=NROUND+1
c     ... find best point
      fmin=fcvec(nparm+1)
      imin=nparm+1
      do i=1,nparm
      if(ipm(i).eq.0 .and. fcvec(i).lt.fmin)then
         fmin=fcvec(i)
         imin=i
      end if
      end do
      if(imin.eq.0)stop 'simplex : imin=0'
c     ... find worst point
      fmax=fcvec(nparm+1)
      imax=nparm+1
      do i=1,nparm
      if(ipm(i).eq.0 .and. fcvec(i).gt.fmax)then
         fmax=fcvec(i)
         imax=i
      end if
      end do
      if(imax.eq.0)stop 'simplex : imax=0'
c     ... find next to worst point
      fmax1=fmin
      imax1=0
      do i=1,nparm
      if((ipm(i).eq.0.and.i.ne.imax) .and. fcvec(i).gt.fmax1)then
         fmax1=fcvec(i)
         imax1=i
      end if
      end do
      if(nparm+1.ne.imax .and. fcvec(nparm+1).gt.fmax1)then
         fmax1=fcvec(nparm+1)
         imax1=nparm+1
      end if
      if(imax1.eq.0)stop 'simplex : imax1=0'

C     ------------------
C     DETERMINE CENTROID
C     ------------------

      center=0.d0
      if(nparm+1.ne.imax)CENTER=POLY(:,nparm+1)
      DO II=1,NPARM
      if(ii.ne.imax .and. ipm(ii).eq.0)CENTER=CENTER+POLY(:,II)
      end do
      CENTER=CENTER*ZZ

C     ---------------
C     REFLECTION STEP
C     ---------------

      where(ipm.eq.0)xvec(:nparm)=center+alpha*(center-poly(:,imax))
      CALL DFLIK1(NPARM,XVEC,FVALUE,EVALUE)

      IF( FVALUE.GE.FMIN .AND. FVALUE.LE.FMAX1 )THEN
C        NEW POINT IS NEITHER BEST NOR WORSE : RETAIN & START NEW ITERATE
         FCVEC(IMAX)=FVALUE
         EEVEC(IMAX)=EVALUE
         POLY(:,IMAX)=XVEC(:nparm)

      ELSE IF(FVALUE.LT.FMIN)THEN

C        --------------
C        EXPANSION STEP
C        --------------

         TRYVEC=CENTER+GAMMA*(XVEC(:nparm)-CENTER)
         EEXPND=EVALUE
         CALL DFLIK1(NPARM,TRYVEC,FEXPND,EEXPND)

         IF(FEXPND.LT.FVALUE)THEN
C           EXPANSION SUCCESSFUL : NEW BEST POINT
            FCVEC(IMAX)=FEXPND
            EEVEC(IMAX)=EEXPND
            POLY(:,IMAX)=TRYVEC
         ELSE
C           RETAIN REFLECTION POINT
            FCVEC(IMAX)=FVALUE
            EEVEC(IMAX)=EVALUE
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
            ECONTR=EVALUE
         ELSE
            XVEC(:nparm)=CENTER+BETA*(POLY(:,IMAX)-CENTER)
            FF=FMAX
            ECONTR=EEVEC(IMAX)
         END IF
         CALL DFLIK1(NPARM,XVEC,FCONTR,ECONTR)
         IF(FCONTR.LT.FF)THEN
C           CONTRACTION SUCCESSFUL : RETAIN POINT
            FCVEC(IMAX)=FCONTR
            EEVEC(IMAX)=ECONTR
            POLY(:,IMAX)=XVEC(:nparm)
         ELSE
C           FURTHER CONTRACTION : SHRINK COMPLETE POLYTOPE
            DO II=1,NPARM+1
            if(ii.eq.imin)cycle
            if(ii.eq.nparm+1)then
              where(IPM.eq.0)XVEC(:nparm)=(POLY(:,II)+POLY(:,IMIN))/2.d0
            else if(ipm(ii).eq.0)then
              where(IPM.eq.0)XVEC(:nparm)=(POLY(:,II)+POLY(:,IMIN))/2.d0
            end if
            POLY(:,II)=Xvec(:nparm)
            CALL DFLIK1(NPARM,XVEC,FVALUE,EVALUE)
            FCVEC(II)=FVALUE
            EEVEC(II)=EVALUE
            end do
         END IF
      END IF

C     ---------------------
C     CHECK FOR CONVERGENCE
C     ---------------------

      FADD=-FMIN
      FSUM=FCVEC(NPARM+1)+FADD
      FSS=FSUM*FSUM
      DO I=1,NPARM
      IF(IPM(I).NE.0)cycle
      FF=FCVEC(I)+FADD
      FSUM=FSUM+FF
      FSS=FSS+FF*FF
      end do
      FSS=(FSS-FSUM*FSUM*ZZ1)*ZZ
      IF(MOD(NROUND,1).EQ.0)WRITE(*,333)NROUND,FSS,XCONV
333   FORMAT(11X,'IT =',I6,3X,'VAR =',G14.4,4X,'CONV =',G14.4)
      IF(NROUND.LT.MROUND .AND. FSS.GT.XCONV)GO TO 100

C     FIND LOWEST POINT IN SIMPLEX = ESTIMATES
      FMIN=fcvec(nparm+1)
      IMIN=nparm+1
      DO  I=1,nparm
      IF(IPM(I).eq.0 .and. FCVEC(I).LT.FMIN)THEN
         FMIN=FCVEC(I)
         IMIN=I
      END IF
      end do
      if(imin.eq.0)stop 'simplex : min not found !'
      XVEC(:nparm)=POLY(:,IMIN)
      EVALUE=EEVEC(IMIN)
      IF(FSS.LE.XCONV)ICONV=1

      WRITE(*,*)'NO. OF ITERATES FOR THIS ANALYSIS =',NROUND
      WRITE(*,*)'VARIANCE OF FUNCTION VALUES       =',FSS
      WRITE(*,*)'CONVERGENCE CRITERION             =',XCONV
      RETURN
      END subroutine dfspx1



















