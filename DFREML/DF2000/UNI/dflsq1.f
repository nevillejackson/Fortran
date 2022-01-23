C============================================================================
      SUBROUTINE  DFLSQ1
C============================================================================

      use lnk
      use names
      use diagonal
      use rows
      use works
      use like
      use solve
      use means
      use levels
      use constants
      use units
      use numbers
      use order

      real(8), dimension(:,:), allocatable :: rhsvec
      real(8)                              :: xx,dd,rhs

C     ----------------------------------------
C     ASSEMBLE LSQ EQUATIONS FOR FIXED EFFECTS
C     ----------------------------------------

      maxzhz=lim2*25
      write(*,*)'give value for "MAXZHZ" '
      call optdef(mzhz,1,222222,maxzhz)

      maxrec=mzhz
      allocate(ifirst(lim2+1),inext(mzhz),ivcol(mzhz),zhz(mzhz),
     &     iflag(lim2),dia(lim2),row(lim2),nnvec(lim2),
     &     work(lim2),rhsvec(lim2,nq),stat=ii)
      if(ii>0)stop 'alloc dflsq1 '

      REWIND(IUN52)
      NTRAIT=1
      NABS=LIM1b+count(nlev(:nfix)>0)+1
      NABS0=LIM1b+count(nlev(:nfix2)>0)+1
      n=max0(nabs0,lim2)
      LIMXHX=lim1
      allocate(xhx(limxhx,limxhx),ivec(n),stat=ii)
      if(ii>0)stop 'alloc limxhx'

      IVEC(:lim1a)=(/ (i,i=1,lim1a) /)
      ROW=1.D0
      MXROW=lim2
      NRCZHZ=0
      XHX=0.D0
      ifirst=0
      DIA=0.D0
      nsize1=0

50    READ(IUN52,END=99)IVEC(LIM1a+1:NABS0),ROW(:LIM1b)
C     ... SS/CP OF COVARIABLES
      DO I=1,LIM1b
      XX=ROW(I)
      DO  J=1,I
      XHX(ivec(J),ivec(I))=XHX(ivec(J),ivec(I))+XX*ROW(J)
      end do
      end do
C     ... FIXED EFFECTS
      DO IABS=LIM1b+1,NABS
      IROW=IVEC(IABS)
      IF(IROW.GT.LIM2)cycle
      IF(IROW.LE.LIMXHX)THEN
         XHX(:lim1a,IROW)=XHX(:lim1a,IROW)+ROW(:lim1a)
         DO J=LIM1a+1,lim1b
         XHX(ivec(J),IROW)=XHX(ivec(J),IROW)+ROW(J)
         end do
         DO J=LIM1b+1,IABS
         XHX(IVEC(J),IROW)=XHX(IVEC(J),IROW)+1.D0
         end do
      ELSE
         CALL LNKROW(IROW,IABS-1)
         DIA(IROW)=DIA(IROW)+1.D0
      END IF
      end do
      GO TO 50

 99   DIA(:limxhx)=(/ (XHX(I,I),i=1,limxhx) /)
      nnvec=0
      NNVEC(lim1+1:lim2)=(/ (INT(DIA(i)),i=lim1+1,lim2) /)
      rhsvec=0.d0
      DO I=LIM1+1,LIMXHX
      RHSVEC(i,:nq)=YBAR(:nq)+ XHX(:nq,i)/DIA(I)
      end do

      DO I=LIMXHX+1,LIM2
      IJ=IFIRST(I)
      do while(ij>0)
      IQ=IVCOL(IJ)
      IF(IQ>NQ)exit
      RHSVEC(I,IQ)=YBAR(IQ)+ZHZ(IJ)/DIA(I)
      IJ=INEXT(IJ)
      end do
      end do

C     WRITE OUT NO.S PER SUBCLASS & CLASS MEANS
      WRITE(IUN13)LIM2
      write(iun13)nnvec(:lim2)
      do i=1,nq
      write(iun13)RHSVEC(:lim2,i)
      end do

C     --------------------------------------------------
C     CHECK FOR RANK DEFICIENCIES & OBTAIN LSQ SOLUTIONS
C     --------------------------------------------------

C     CARRY OUT GAUSSIAN ELIMINATION
      IF(NRZERO.GT.0)CALL DFZER1
      CALL OPZERO(ZERO,0)
      IFLAG=0
      IEMPTY=-1
      IROW=LIM2+1
      KRANK=0
100   IROW=IROW-1
      IF(IROW.EQ.NQ)GO TO 199
      DD=DIA(IROW)
      IF(DABS(DD).LT.ZERO)GO TO 100
      KRANK=KRANK+1
      IFLAG(IROW)=KRANK
C     ABSORB THE ROW INTO REMAINING IROW-1 ROWS AND COLUMNS
      CALL DFABRW
      GO TO 100

 199  WRITE(*,*)'"DFLSQ1" : NO. OF FIXED EFFECTS EQUATIONS =',LIM2-NQ
      WRITE(*,*)'           RANK OF COEFFICIENT MATRIX     =',KRANK
      LL=LIM2-NQ-NRZERO
      WRITE(*,*)'           ... EXPECTED VALUE             =',LL
      IF(KRANK.NE.LL)THEN
C        ... ADD ROWS TO BE ZERO-ED OUT TO ACCOUNT FOR ADD. DEPEND. FOUND
         NR1=NRZERO
         DO 200 I=NQ+1,LIM2
         IF(IFLAG(I).GT.0)GO TO 200
         DO J=1,NRZERO
         IF(KRZERO(J).EQ.I)GO TO 200
         end do
         NRZERO=NRZERO+1
         KRZERO(NRZERO)=I
         WRITE(*,*)'ADD. ROW TO BE ZERO-ED OUT : EQUATION NO. =',I
 200     CONTINUE
         IF(NRZERO.GT.NR1)CALL DFKZER(NRZERO,KRZERO)
      END IF

C     OBTAIN BACKSOLUTIONS
      ROW=0.d0
      DO IQ=1,NQ
      DO IROW=NQ+1,MIN0(LIMXHX,LIM2)
      IF(IFLAG(IROW).GT.0)THEN
         RHS=XHX(IQ,IROW)
         DO J=NQ+1,IROW-1
         IF(DABS(XHX(J,IROW)).GT.ZERO)RHS=RHS-XHX(J,IROW)*ROW(J)
         END DO
         ROW(IROW)=RHS/DIA(IROW)
      END IF
      END DO
      DO IROW=LIMXHX+1,LIM2
      IJ=IFIRST(IROW)
      IF(IJ.EQ.0 .OR. IFLAG(IROW).EQ.0) cycle
      RHS=0.D0
 402  IF(IVCOL(IJ).LE.NQ)THEN
         IF(IVCOL(IJ).EQ.IQ)RHS=ZHZ(IJ)
         IJ=INEXT(IJ)
         IF(IJ.GT.0)GO TO 402
      END IF
      do while(IJ>0)
         JCOL=IVCOL(IJ)
         RHS=RHS-ZHZ(IJ)*ROW(JCOL)
         IJ=INEXT(IJ)
      END do
      ROW(IROW)=RHS/DIA(IROW)
      end do
      write(iun13)row(:lim2)
      end do ! iq
      deallocate(ifirst,inext,ivcol,zhz,nnvec,rhsvec,row,stat=ii)
      if(ii>0)stop 'de-alloc dflsq1 '
      RETURN

      contains

C     ======================
      SUBROUTINE      DFZER1
C     ======================

      DO 85 I=1,NRZERO
      IROW=IEQNEW( KRZERO(I) )

C     ... "ROW" IN LOWER TRIANGLE OF MMM
      IF(IROW.LE.LIMXHX)THEN
         RHSZER(I,:nq)=XHX(:nq,IROW)
         XHX(:irow,IROW)=0.D0
C        ... PART OF "COLUMN" WHICH IS FULL-STORED
         XHX(IROW,IROW+1:LIMXHX)=0.D0
         LL=LIMXHX

      ELSE
C        ... "ROW" IN SPARSE-STORED PART OF MMM
         IJ=IFIRST(IROW)       
         RHSZER(I,:nq)=0.D0
         IPRE=0
         do while(ij>0)
         ICOL=IVCOL(IJ)
         IF(ICOL.LE.NQ)RHSZER(I,ICOL)=ZHZ(IJ)
         ZHZ(IJ)=0.D0
         IPRE=IJ
         IJ=INEXT(IJ)
         INEXT(IPRE)=0
         end do
         IFIRST(IROW)=0
         LL=IROW
      END IF

C     ... PART OF COLUMN WHICH IS SPARSE-STORED
      DO JROW=LL+1,LIM2
      IJ=IFIRST(JROW)
      IPRE=0
402   IF(IJ.EQ.0)cycle
      IF(IVCOL(IJ).LT.IROW)THEN
         IPRE=IJ
         IJ=INEXT(IJ)
         GO TO 402
      ELSE IF(IVCOL(IJ).EQ.IROW)THEN
         ZHZ(IJ)=0.D0
         IF(IPRE.GT.0)THEN
            INEXT(IPRE)=INEXT(IJ)
         ELSE
            IFIRST(JROW)=INEXT(IJ)
         END IF
      END IF
      end do

      DIAZER(I)=DIA(IROW)
85    DIA(IROW)=0.D0
      RETURN
      END subroutine dfzer1

!     =================
      SUBROUTINE DFABRW
!     =================

      K=0
      TT=-1.D0/DD

C     FULLSTORED PART
      IF(IROW.LE.LIMXHX)THEN
         DO I=1,IROW-1
         IF(DABS(XHX(I,IROW)).GT.ZERO)THEN
            K=K+1
            IVEC(K)=I
            WORK(K)=XHX(I,IROW)
         END IF
         end do

         DO  I=1,K
         JROW=IVEC(I)
         ZZ=TT*WORK(I)
         ROW(:i)=ZZ*WORK(:i)
         DO  J=1,I-1
         XHX(IVEC(J),JROW)=XHX(IVEC(J),JROW)+ROW(J)
         end do
         DIA(JROW)=DIA(JROW)+ROW(I)
         end do

C     SPARSE PART
      ELSE
         IJ=IFIRST(IROW)
         IF(IJ.EQ.0)RETURN
         IPRE=0
         do while(ij>0)
         IF(DABS(Zhz(ij)).GT.ZERO)THEN
            K=K+1
            WORK(K)=zhz(ij)
            IVEC(K)=IVCOL(IJ)
         END IF
         IPRE=IJ
         IJ=INEXT(IJ)
         end do

         IF(IEMPTY.GE.0)THEN
            ISAVE=IEMPTY
            IEMPTY=IFIRST(IROW)
            INEXT(IPRE)=ISAVE
         END IF

         DO I=1,K
         JROW=IVEC(I)
         ZZ=TT*WORK(I)
         IF(JROW.LE.LIMXHX)THEN
            ROW(:i)=ZZ*WORK(:i)
            DO J=1,I-1
            XHX(IVEC(J),JROW)=XHX(IVEC(J),JROW)+ROW(J)
            end do
         ELSE
            ROW(:i)=ZZ*WORK(:i)
            IJ=IFIRST(JROW)
            IPRE=0
            DO  J=1,I-1
            JCOL=IVEC(J)
            do while( (IJ.GT.0) .AND. (IVCOL(IJ).LT.JCOL) )
               IPRE=IJ
               IJ=INEXT(IJ)
            end do
            IF( (IJ.GT.0) .AND. (JCOL.EQ.IVCOL(IJ)) )THEN
               ZHZ(IJ)=ZHZ(IJ)+ROW(J)
            ELSE
               IF(IEMPTY.LE.0)THEN
                  NRCZHZ=NRCZHZ+1
                  IF(NRCZHZ.GT.MAXREC)THEN
                     WRITE(*,*)'ROUTINE "ABSROW" : DIMENSION EXCEEDED'
                     WRITE(*,*)'ABSORPTION STEP  : ROW =',IROW
                     WRITE(*,*)'                   COL =',JCOL
                     STOP 'RESET PARAMETER "MAXZHZ" !'
                  END IF
                  NEWADD=NRCZHZ
              ELSE
                  NEWADD=IEMPTY
                  IEMPTY=INEXT(IEMPTY)
              END IF
              ZHZ(NEWADD)=ROW(J)
              IVCOL(NEWADD)=JCOL
              INEXT(NEWADD)=0
              IF( (IJ.EQ.0) .AND. (IFIRST(JROW).EQ.0) )THEN
                 IFIRST(JROW)=NEWADD
                 INEXT(NEWADD)=0
              ELSE IF( (IJ.EQ.0) .AND. (IFIRST(JROW).GT.0) )THEN
                 INEXT(IPRE)=NEWADD
                 INEXT(NEWADD)=0
              ELSE IF( (IJ.GT.0) .AND. (IPRE.EQ.0) )THEN
                 INEXT(NEWADD)=IFIRST(JROW)
                 IFIRST(JROW)=NEWADD
              ELSE
                 INEXT(NEWADD)=INEXT(IPRE)
                 INEXT(IPRE)=NEWADD
              END IF
              IJ=NEWADD
            END IF
            end do
         END IF
         DIA(JROW)=DIA(JROW)+ROW(I)
         end do
      END IF
      RETURN
      END subroutine dfabrw

C     ==========================
      SUBROUTINE LNKROW(IROW,NN)
C     ==========================

      integer, intent(in) :: irow,nn
      integer             :: kk,ipre,ipcol,iplace,icol

      IF(NN.EQ.0)RETURN
      KK=0
      IPRE=0
      IPCOL=0
      IPLACE=IFIRST(IROW)
      do while(kk<nn)
      KK=KK+1
      ICOL=IVEC(KK)
      IF(ICOL.LE.IPCOL)THEN
        WRITE(*,*)'SUBROUTINE "LNKROW" : COL. NO.S OUT OF ORDER !'
        WRITE(*,*)' ROW NO. =',IROW
        STOP
      END IF

C     GO TO NEXT ELEMENT IN THIS ROW
10    IF( IPLACE.GT.0)then
         if (IVCOL(IPLACE).LT.ICOL )THEN
            IPRE=IPLACE
            IPLACE=INEXT(IPLACE)
            GO TO 10

C        ELEMENT FOUND
         ELSE IF( ICOL.EQ.IVCOL(IPLACE) )THEN
            ZHZ(IPLACE)=ZHZ(IPLACE)+row(KK)
            IPRE=IPLACE
            IPLACE=INEXT(IPLACE)
         end if

      ELSE
         NRCZHZ=NRCZHZ+1
         IF(NRCZHZ.GT.MAXREC)stop ' "LNKROW" : DIMENSIONS EXCEEDED !!'
         ZHZ(NRCZHZ)=row(KK)
         IVCOL(NRCZHZ)=ICOL
         IF(IPLACE.EQ.0.AND.IFIRST(IROW).EQ.0)THEN
            IFIRST(IROW)=NRCZHZ
            INEXT(NRCZHZ)=0
         ELSE IF(IPLACE.EQ.0.AND.IFIRST(IROW).NE.0)THEN
             INEXT(IPRE)=NRCZHZ
             INEXT(NRCZHZ)=0
         ELSE IF(IPLACE.NE.0.AND.IPRE.EQ.0)THEN
             INEXT(NRCZHZ)=IFIRST(IROW)
             IFIRST(IROW)=NRCZHZ
         ELSE
             INEXT(NRCZHZ)=INEXT(IPRE)
             INEXT(IPRE)=NRCZHZ
         END IF
         IPRE=NRCZHZ
         IPLACE=INEXT(NRCZHZ)
      END IF
      IPCOL=ICOL
      end do
      RETURN
      END subroutine lnkrow

      END subroutine dflsq1


C============================================================================
      subroutine DFLSP1
C============================================================================

      use names
      use rows
      use works
      use like
      use solve
      use means
      use levels
      use constants
      use units
      use numbers
      use order

      real(8), dimension(:,:), allocatable :: rhsvec

      REWIND(IUN13)
      READ(IUN13)LIM2
      allocate(nnvec(lim2),row(lim2),rhsvec(lim2,nq),stat=ii)
      if(ii>0)stop 'alloc dflsp1 '

      read(iun13)NNVEC(:LIM2)
      do i=1,nq
      READ(iun13)RHSVEC(:lim2,i)
      end do

      WRITE(IUN66,*)'  '
      WRITE(IUN66,*)'----------------------------------------------'
      WRITE(IUN66,*)'LSQ SOLUTION FOR FIXED EFFECTS AND COVARIABLES'
      WRITE(IUN66,*)'----------------------------------------------'
      DO IQ=1,NQ
      If(nq.GT.1)THEN
         WRITE(IUN66,*)'TRAIT NO. =',IQ
         WRITE(IUN66,*)'------------------------- '
      END IF
      read(iun13)ROW(:lim2)
      K=NQ
      DO I=1,NCOV1
      WRITE(IUN66,909)'COVARIABLE NO.',I,COVAR(I)
      WRITE(IUN66,913)
      DO J=1,NPOW(I)
      K=K+1
      WRITE(IUN66,911)K,'ORDER',0,J,row(K)
      end do
      end do

      DO  I=NCOV1+1,ncov
      WRITE(IUN66,909)'COVARIABLE NO.',I,COVAR(I),' nested in ',
     *                fixed(icnest(i))
      WRITE(IUN66,913)
      DO  ii=1,Nlnest(i)
      DO  J=1,NPOW(I)
      K=K+1
      WRITE(IUN66,911)K,'level/order',ii,J,row(K)
      end do
      end do
      end do

      DO I=1,NFIX
      WRITE(IUN66,909)'FIXED EFFECT NO.',I,FIXED(I)
      WRITE(IUN66,913)
      nn=0
      DO J=1,NLEV(I)
      K=K+1
      if(nnvec(k).eq.1)nn=nn+1
      WRITE(IUN66,912)K,'LEVEL',J,NNVEC(K),RHSVEC(K,IQ),ROW(K)
      end do
      nsize1(i)=nn
      write(iun66,*)'no. of subclasses of size 1 =',nn
      end do
      END DO
      CLOSE(IUN13)
      RETURN
913   FORMAT(/' EQ.NO.',T40,2X,'NREC',14x,'MEAN',10X,'SOLUTION')
911   FORMAT(I5,2X,A,T21,2I6,    T40,24X,2G18.10)
919   FORMAT(I5,2X,A,T21,2I6,    T40,24X,2G18.10)
912   FORMAT(I5,2X,A,T21,I6,    T40,I6,2F18.8)
909   FORMAT(/1X,A,I4,4X,A12,(a))
      end subroutine dflsp1
