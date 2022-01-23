C======================================================================
      SUBROUTINE DFQUA1(iopt,NPARM,KPARM,IVAR,ICONV,FMIN,SIGE)
C======================================================================

      use likelihoods
      use names, only : param
      use spasol
      use like
      use parameters
      use iterates
      use units
      use constants
      use numbers
      USE platform

      implicit double precision (a-h,o-z)

      integer, intent(in)                :: nparm,kparm,iopt
      integer, intent(out)               :: iconv,ivar
      real(8), intent(out)               :: sige,fmin
      real(8)                            :: GOLD=1.618034
      real(8), dimension(3)              :: zvec,fvec
      integer, dimension(3)              :: kvec
      REAL(8)                            :: hh,hsq22,hsq33,xconv,hsq1,
     *                                      ff1,ff2,ee, time,xsecs

      integer, dimension(:),allocatable  :: ifvald

      allocate( ifvald(mxfunc), stat=ii)
      if(ii>0)stop 'alloc dfqua'

      DO I=1,NPARM
      IF(IPM(I).EQ.0)IVAR=I
      end do

      if(iopt<0)then
         WRITE(*,*)'  '
         WRITE(*,*)'* * QUADRATIC APPROXIMATION OF LIKELIHOOD * *'
         WRITE(*,*)'CONVERGENCE CRIT. : MAX. CHANGE IN PARAMETER ?'
         WRITE(*,*)'GIVE EXPONENT "N" FOR 10**(-N), FOR EXAMPLE '
         WRITE(*,*)'        3  ...  0.001         '
         WRITE(*,*)'        4  ...  0.0001        '
         WRITE(*,*)'        5  ...  0.00001       '
         WRITE(*,*)'        6  ...  0.000001      '
         WRITE(*,*)'        7  ...  0.0000001     '
         CALL OPTDEF(IEXP,0,16,IIC)
      else
         iexp=5
      end if
      XCONV=10.D0**(-IEXP)

      MROUND=MXFUNC
      ifvald=0
      IF(KPARM.LT.NPARM)THEN
         NVALID=0
         DO 400 I=1,NCALL
         DO  J=1,NPARM
         IF(IPM(J).NE.0 .AND.dabs(FEVAL(J,I)-XVEC(J))>10.d-9)THEN
            IFVALD(I)=1
            GO TO 400
         END IF
         end do
         NVALID=NVALID+1
 400     continue
      ELSE
         NVALID=NCALL
      END IF

C--------------------------------------------------------------------------
C     FIND MAXIMUM OF THE LIKELIHOOD
C--------------------------------------------------------------------------

C     SET UP PHASE : FIND TRIPLETT OF HERITABILITIES
      IF(NVALID.GE.3)THEN
         CALL FNDTRI
         IF(ISUCCS.EQ.3)GO TO 698
      END IF

      HSQ1=XVEC(IVAR)
      if(iopt<0)then
         WRITE(*,*)'STARTING VALUE FOR OPTIMUM GIVEN =',HSQ1
         WRITE(*,FMT(ipltf))'STARTING VALUE FOR LEFT BRACKET ?'
         nhh=hsq1-0.001d0
         CALL RVALUE(HSQ33,zero,hh)
         WRITE(*,FMT(ipltf))'STARTING VALUE FOR RIGHT BRACKET ?'
         hh=hsq1+0.001d0
         CALL RVALUE(HSQ22,hh,1.D0)
      else
         hsq33=0.75*hsq1
         if(hsq33<0.0001d0)hsq33=0.0001d0
         hsq22=1.25*hsq1
         if(hsq22>0.999d0)hsq22=0.999d0
      end if
      STEP1=HSQ22-HSQ1
      STEP2=HSQ33-HSQ1
      WRITE(*,*)' '

      CALL DFTIME(time,xsecs,isecs,0,'abcdef')
      CALL DFLIK1(NPARM,XVEC,FF1,EE)
      IF(dabs(FF1-BIG)<10.d-9)stop 'DFQUA : invalid starting point'
      CALL DFTIME(time,xsecs,isecs,10,'abcdef')
      PRINT *,'CPU TIME (SEC.S) REQUIRED PER LOG L =',ISECS

 608  HSQ2=HSQ1+STEP1
      NB2=0
6081  XVEC(IVAR)=HSQ2
      CALL DFLIK1(NPARM,XVEC,FF2,EE)
      IF(dabs(FF2-BIG)<10.d-9)THEN
         PRINT *,'HSQ2',XVEC(IVAR),FF2
         HSQ2=HSQ1+ (HSQ2-HSQ1)/GOLD
         NB2=NB2+1
         IF(NB2.GT.3)STOP 'HSQ 2'
         GO TO 6081
      END IF

      IF( FF2.GE.FF1 )THEN
         HSQ3=HSQ1+STEP2
      ELSE
         HSQ3=HSQ2+STEP1
      END IF
      NB3=0
6971  XVEC(IVAR)=HSQ3
697   CALL DFLIK1(NPARM,XVEC,FF,EE)
      IF(dabs(FF-BIG)<10.d-9)THEN
         PRINT *,'HSQ3',XVEC(IVAR),FF2,ee
         HSQ3=HSQ1 + (HSQ3-HSQ1)/GOLD
         NB3=NB3+1
         IF(NB3.GT.3)then
            print *,'zval',zvec
            print *,'fval',fvec
            STOP 'HSQ 3'
         end if
         GO TO 6971
      END IF
      CALL FNDTRI

695   IF(ISUCCS.EQ.3)THEN
         XVEC(IVAR)=ZVEC(2)
         GO TO 698
      ELSE IF(ISUCCS.EQ.1)THEN
C        LEFT BRACKET FOUND, LOOK FOR RIGHT
         IF(DABS(ZVEC(2)-ZVEC(1)).LT.ZERO)GO TO 666
         HSQ3=ZVEC(2)+STEP1
      ELSE IF(ISUCCS.EQ.2)THEN
C        RIGHT BRACKET FOUND, LOOK FOR LEFT	
         IF( DABS(ZVEC(3)-ZVEC(2)).LT.ZERO)GO TO 666
         HSQ3=ZVEC(2)+STEP2
      ELSE
         PRINT *,'ISUCCS=0'
         STOP
      END IF
      HSQ1=ZVEC(2)
      NB3=0
      GO TO 6971

C     QUADRATIC APPROXIMATION OF LOG L
 698  A=(ZVEC(2)-ZVEC(3))*FVEC(1)
      B=(ZVEC(3)-ZVEC(1))*FVEC(2)
      C=(ZVEC(1)-ZVEC(2))*FVEC(3)
      D=(ZVEC(2)*ZVEC(2)-ZVEC(3)*ZVEC(3))*FVEC(1)
      E=(ZVEC(3)*ZVEC(3)-ZVEC(1)*ZVEC(1))*FVEC(2)
      F=(ZVEC(1)*ZVEC(1)-ZVEC(2)*ZVEC(2))*FVEC(3)
      HSQ=0.5D0*(D+E+F)/(A+B+C)
      Ifail=0
      IF(dabs(HSQ-XVEC(IVAR))<10.d-9)THEN
         WRITE(*,*)'last function evaluation did not yield an ',
     *             '"improved" triplett !'
6980     IF(IFAIL.EQ.0)THEN
            WRITE(*,*)'... 1st try : attempt to replace left bracket ',
     *                'with value further away'
            ILEFT=KVEC(1)
            IFVALD(ILEFT)=1
            CALL FNDTRI
            IF(ISUCCS.EQ.3)then
               A=(ZVEC(2)-ZVEC(3))*FVEC(1)
               B=(ZVEC(3)-ZVEC(1))*FVEC(2)
               C=(ZVEC(1)-ZVEC(2))*FVEC(3)
               D=(ZVEC(2)*ZVEC(2)-ZVEC(3)*ZVEC(3))*FVEC(1)
               E=(ZVEC(3)*ZVEC(3)-ZVEC(1)*ZVEC(1))*FVEC(2)
               F=(ZVEC(1)*ZVEC(1)-ZVEC(2)*ZVEC(2))*FVEC(3)
               HSQ=0.5D0*(D+E+F)/(A+B+C)
               if(dabs(hsq-xvec(ivar))<10.d-9)go to 6980
               go to 698
            end if
            IFAIL=1
            IFVALD(ILEFT)=0
         ELSE IF(IFAIL.EQ.1)THEN
            WRITE(*,*)'... 2nd try : attempt to replace right bracket ',
     *                'with value further away'
            IRIGHT=KVEC(3)
            IFVALD(IRIGHT)=1
            CALL FNDTRI
            IF(ISUCCS.EQ.3)GO TO 698
            IFAIL=2
         ELSE IF(IFAIL.EQ.2)THEN
            write(*,*)'could not improve quadratic fit of likelihood'
            write(*,*)'function by extending interval !'
            write(*,*)'try all over again ... program to be improved to'
     *,               ' handle this at some stage '
            STOP 'bye for now ......! '
         END IF
         go to 6980
      END IF
      XVEC(IVAR)=HSQ
      CALL DFLIK1(NPARM,XVEC,FF,EE)
      IF(dabs(FF-BIG)<10.d-9)THEN
         Write(*,*)'quadratic approximation of log L yielded predicted',
     *             ' maximum out of bounds !'
         write(*,*)'last triplett :'
         do i=1,3
         write(*,*)kvec(i),zvec(i),fvec(i)
         END do
         write(*,*)'predicted optimum =',hsq
         write(*,*)'current vector of parameters :'
         do i=1,nparm
         write(*,*)i,'   ',param(i),xvec(i),ipm(i)
         END do
         STOP 
      END IF
      CALL FNDTRI

C     CHECK FOR CONVERGENCE
      IF(ZVEC(2)-ZVEC(1).LT.XCONV.OR.ZVEC(3)-ZVEC(2).LT.XCONV)THEN
         ICONV=1
      ELSE IF(NCALL.LT.MROUND)THEN
         GO TO 698
      END IF

666   FMIN=-2.D0*FVEC(2)
      IMIN=KVEC(2)
      SIGE=feval(nparm+2,imin)

      RETURN

      contains

C     =================
      SUBROUTINE FNDTRI
C     ==================

C     MINIMUM VALUE FOR -2 LOG L
      FFMIN=BIG
      DO  I=nCALL,1,-1
      IF(IFVALD(I).EQ.0 .AND. FEVAL(NPARM+1,I).LT.FFMIN)THEN
         FFMIN=FEVAL(NPARM+1,I)
         IIMIN=I
      END IF
      end do
      HMIN=FEVAL(IVAR,IIMIN)

C     TRY AND FIND VALUE TO THE 'LEFT'
      FLEFT=BIG
      ILEFT=0
      DO I=nCALL,1,-1
      if(ifvald(i)>0)cycle
      IF(FEVAL(IVAR,I)<HMIN .AND. FEVAL(NPARM+1,I).LT.FLEFT)THEN
         FLEFT=FEVAL(NPARM+1,I)
         ILEFT=I
      END IF
      end do

C     TRY AND FIND VALUE TO THE 'RIGHT'
      FRIGHT=BIG
      IRIGHT=0
      DO I=nCALL,1,-1
      if(ifvald(i)>0)cycle
      IF( FEVAL(IVAR,I)>HMIN .AND. FEVAL(NPARM+1,I).LT.FRIGHT)THEN
         FRIGHT=FEVAL(NPARM+1,I)
         IRIGHT=I
      END IF
      end do

      ZVEC(2)=HMIN
      KVEC(2)=IIMIN
      FVEC(2)=-0.5D0*FFMIN

      IF(ILEFT.GT.0)THEN
         ZVEC(1)=FEVAL(IVAR,ILEFT)
         FVEC(1)=-0.5D0*FLEFT
         KVEC(1)=ILEFT
      END IF

      IF(IRIGHT.GT.0)THEN
         ZVEC(3)=FEVAL(IVAR,IRIGHT)
         KVEC(3)=IRIGHT
         FVEC(3)=-0.5D0*FRIGHT
      END IF
      IF(ILEFT.GT.0.AND.IRIGHT.GT.0)THEN
         ISUCCS=3
      ELSE IF(ILEFT.GT.0.AND.IRIGHT.EQ.0)THEN
         ISUCCS=1
      ELSE IF(ILEFT.EQ.0.AND.IRIGHT.GT.0)THEN
         ISUCCS=2
      ELSE
         ISUCCS=0
      END IF
      RETURN
      END subroutine fndtri

      END subroutine dfqua1
