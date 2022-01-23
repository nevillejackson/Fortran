c============================================================================
      SUBROUTINE      DFLSQ3(lim0)
C============================================================================

      use params
      use names
      use units
      use means
      use levels
      use numbers
      use order
      use zero_rows

!     arguments
      integer, intent(inout)                :: lim0

!     loacl variables
      integer, dimension(:),  allocatable   :: iflag,ivec,inext,ivcol,
     *                                         ifirst,nnvec,iicol
      real(8), dimension(:),  allocatable   :: dia,xvec,rrow,zhz,row
      real(8), dimension(:,:),allocatable   :: xhx
      real(8), dimension (:,:),allocatable  :: xcov,qhq
      real(8), dimension (:), allocatable   :: yvec
      integer, dimension (:), allocatable   :: nnq
      integer, dimension (:,:), allocatable :: ieq
      integer, parameter                    :: maxxhx =500
      real(8)                               :: dd
      integer                               :: maxrec,nobs,
     &                                         nr,i,j,k,l,limnr1
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      maxzhz=neqns*7
      write(*,*)'give value for "MAXZHZ" '
      call optdef(mzhz,1,2222222,maxzhz)

c     set to 1 if obs have been scaled by phen.sd
      iscale=0
      LIMXHX=min0(maxxhx,lim2)
      LL=MIN0(LIMXHX,LIM2)

c     declare space ...
      allocate(iicol(mqhq),iflag(neqns),dia(neqns),xhx(limxhx,limxhx),
     *                                                        stat=ii)
      if(ii>0)stop 'dflsq3 : allocate'
      allocate(ivec(neqns),nnvec(neqns),xvec(neqns),row(lim2+mqhq),
     *                                    rrow(neqns),stat=ii)
      if(ii>0)stop 'dflsq3 : allocate'

      allocate(ifirst(neqns),inext(mzhz),ivcol(mzhz),zhz(mzhz),
     *                                                       stat=ii)
      print *,'zhz allocated : ',mzhz
      if(ii>0)stop 'dflsq3 : allocate'

      allocate (xcov(mnfr,mobs),yvec(mobs),qhq(lim1+mqhq,lim1+mqhq),
     *          nnq(mobs),ieq(maxfix+3,mobs),stat=ii)
      if(ii>0)stop 'dflsq3 : alloc 2'

C     ----------------------------------------
C     ASSEMBLE LSQ EQUATIONS FOR FIXED EFFECTS
C     ----------------------------------------

C     INITIALIZE
      LIM0=1
      NTRAIT=1
      REWIND(IUN52)
      MAXREC=MZHZ
      NRCZHZ=0
      xhx=0.d0 
      dia=0.d0
      iflag=0
      ifirst=0
      nnvec=0

500   READ(IUN52,END=598)II,NOBS,NR,NNQ(:NOBS),
     *     IICOL(:NR),( (IEQ(K,L),K=1,NEFF(NNQ(L)) ),L=1,NOBS),
     *    ( (XCOV(K,L),K=1,NFR(NNQ(L)) ),L=1,NOBS),YVEC(:NOBS)

      LIMNR1=LIM1+NR
      QHQ=0.D0

C     ACCUMULATE CONTRIBUTIONS TO MMM FOR ONE OBSERVATION AT A TIME ...
C     ... SET UP ROW OF X'R(-1) FOR THIS OBSERVATION
      DO  IOBS=1,NOBS
      IQ=NNQ(IOBS)
      II=NFRST(IQ)
      ROW=0.d0
      DO JOBS=iobs,iOBS
      JQ=NNQ(JOBS)
      IF(JQ.EQ.IQ)then
         ROW(1)=ROW(1)+YVEC(JOBS)
         JJ=NFRST(JQ)
         ROW(jj+1:jj+nfr(jq))=ROW(JJ+1:jj+nfr(jq))+XCOV(:nfr(jq),JOBS)
         DO  J=1,NFIX(JQ)
         ROW(IEQ(J,JOBS))=ROW(IEQ(J,JOBS))+1.D0
         end do
      end if
      end do

C     CONTRIBUTION TO MMM FOR THIS RECORD ...

C     ... COV * COV (LOWER TRIANGLE ONLY)
      DO  K=1,NFR(IQ)
      IROW=II+K
      XHX(:irow,IROW)=XHX(:irow,IROW)+XCOV(K,IOBS)*ROW(:irow)
      end do

C     ... COV * FE/RE
      DO K=1,NFIX(IQ)
      JROW=IEQ(K,IOBS)-LIM1
      QHQ(:limnr1,JROW)=QHQ(:limnr1,JROW)+ROW(:limnr1)
      end do

      end do

C     ALL RECORDS FOR THE ANIMAL PROCESSED : PUT CONTRIBUTIONS FOR FIXED
C                                    AND RANDOM EFFECTS INTO RIGHT PLACE
      DO I=1,NR
      IROW=IICOL(I)
      IF(IROW.LE.LIMXHX)THEN
         XHX(:lim1,IROW)=XHX(:lim1,IROW)+QHQ(:lim1,I)
         DO J=1,NR
         IF(IICOL(J).LE.IROW)XHX(IICOL(J),IROW)=XHX(IICOL(J),IROW)
     *                                                   +QHQ(LIM1+J,I)
         END DO
      ELSE IF(IROW.GT.0)THEN
C        ... RHS 
         NN=1
         IVEC(NN)=1
         XVEC(NN)=QHQ(1,I)
C        ... REGRESSION COEFFICIENTS
         DO J=LIM0+1,LIM1
         IF(QHQ(J,I).NE.0)THEN
            NN=NN+1
            IVEC(NN)= J
            XVEC(NN)=QHQ(J,I)
         END IF
         END DO
c        ... FE/RE codes, lower triangle only
         DO  J=1,NR
         IF((IICOL(J).LE.IROW) .AND. (QHQ(LIM1+J,I).NE.0) )THEN
            NN=NN+1
            IVEC(NN)=IICOL(J)
            XVEC(NN)=QHQ(LIM1+J,I)
         END IF
         end do
         CALL LNKROW(IROW,NN-1)
         DIA(IROW)=DIA(IROW)+XVEC(NN)
      END IF
      end do

      GO TO 500

C     STORE NO.S OF OBSERVATIONS & CELL MEANS
598   dia(lim0+1:limxhx) = (/ (xhx(i,i),i=lim0+1,limxhx) /)
      NNVEC(lim1+1:lim2)=DIA(lim1+1:lim2)

      DO IROW=LIM1+1,MIN0(LIMXHX,LIM2)
      ROW(IROW)=XHX(NTRAIT,IROW) ! raw totals
      end do

      DO  IROW=LIMXHX+1,LIM2
      ROW(IROW)=0.D0
      IJ=IFIRST(IROW)
 553  IF(IJ.EQ.0)cycle
      IF(IVCOL(IJ).EQ.NTRAIT)THEN
         ROW(IROW)=ZHZ(IJ)
      ELSE IF(IVCOL(IJ).LT.NTRAIT)THEN
         IJ=INEXT(IJ)
         GO TO 553
      END IF
      end do

C     ACCOUNT FOR DEPENDENCIES AMONG FIXED EFFECTS
      IF(NRZERO.GT.0)CALL DFZER3

C     --------------------------------------------------
C     CHECK FOR RANK DEFICIENCIES & OBTAIN LSQ SOLUTIONS
C     --------------------------------------------------

C     CARRY OUT GAUSSIAN ELIMINATION
      CALL OPZERO(ZERO,0)
      iflag(:lim2)=0
      IEMPTY=-1
      KRANK=0

      do irow=lim2,lim0+1,-1

      DD=DIA(IROW)
      IF(DABS(DD).LT.ZERO)cycle

      KRANK=KRANK+1
      IFLAG(IROW)=KRANK

C     ABSORB THE ROW INTO REMAINING IROW-1 ROWS AND COLUMNS
      CALL DFABRW(DD,IROW,IEMPTY)

      end do

      WRITE(*,*)'"DFLSQ3" : NO. OF FIXED EFFECTS EQUATIONS =',LIM2-LIM0
      WRITE(*,*)'           RANK OF COEFFICIENT MATRIX     =',KRANK
      WRITE(*,*)'           ... EXPECTED VALUE             =',LIM2-LIM0
     &                                                          -NRZERO
      IF(KRANK.NE.Lim2-lim0-nrzero)THEN
C        ... ADD ROWS TO BE ZERO-ED OUT TO ACCOUNT FOR ADD. DEPEND. FOUND
         NR1=NRZERO
         DO 200 I=LIM0+1,lim2
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
      xvec(:lim2)=0.d0

      DO IROW=LIM0+1,LL
      IF(IFLAG(IROW).GT.ZERO)THEN
         RHS=XHX(NTRAIT,IROW)-dot_product( xhx(lim0+1:irow-1,irow),
     &                                     xvec(lim0+1:irow-1) )
         XVEC(IROW)=RHS/DIA(IROW)
      END IF
      END DO
      DO 400 IROW=LIMXHX+1,LIM2
      IJ=IFIRST(IROW)
      IF(IJ.EQ.0.OR.IFLAG(IROW).EQ.0)GO TO 400
      RHS=0.D0
402   IF(IVCOL(IJ).LE.LIM0)THEN
         IF(IVCOL(IJ).EQ.NTRAIT)RHS=ZHZ(IJ)
         IJ=INEXT(IJ)
         IF(IJ.GT.0)GO TO 402
      END IF
 403  IF(IJ.GT.0)THEN
         JCOL=IVCOL(IJ)
         RHS=RHS-ZHZ(IJ)*XVEC(JCOL)
         IJ=INEXT(IJ)
         GO TO 403
      END IF
      XVEC(IROW)=RHS/DIA(IROW)
400   CONTINUE
C     ... CALCULATE CLASS MEANS & SCALE SOLUTIONS
      sd=1.d0
      K=LIM0
      DO IQ=1,NQ
      DO I=1,NCOV(IQ)
      if(iscale.eq.1)SD=SDEV(iq)/CSDEV(I,IQ)
      DO J=1,NPOW(I,IQ)
      K=K+1
      xvec(k)=xvec(k)*sd
      end do
      end do
      end do
      DO IQ=1,NQ
      if(iscale.eq.1)sd=sdev(iq)
      DO I=1,NFIX(IQ)
      DO J=1,NLEV(I,IQ)
      K=K+1
      IF(NNVEC(K)>0)ROW(K)=YBAR(IQ)+ ((ROW(K)*SD)/DBLE(NNVEC(K)))
      XVEC(K)=XVEC(K)*SD
      end do
      end do
      end do

C     WRITE OUT NO.S PER SUBCLASS & CLASS MEANS
      WRITE(IUN13)LIM2
      write(iun13)nnvec(1:lim2)
      write(iun13)row(1:lim2)
      write(iun13)xvec(1:lim2)

      deallocate(ivec,nnvec,iflag,ifirst,ivcol,inext,zhz,xvec,dia,xhx,
     *           rrow,row,stat=ii)
      if(ii>0)print *,'dfslq3 : de-alloc'

      RETURN

      contains

c     ======================
      SUBROUTINE      DFZER3
c     ======================

      integer :: i,irow,ll,ij,ipre,jrow

      DO 85 I=1,NRZERO
      IROW=IEQNEW( KRZERO(I) )

C     ... "ROW" IN LOWER TRIANGLE OF MMM
      IF(IROW.LE.LIMXHX)THEN
         XHX(:irow,IROW)=0.D0
C        ... PART OF "COLUMN" WHICH IS FULL-STORED
         XHX(IROW,irow+1:limxhx)=0.D0
         LL=LIMXHX

      ELSE
C        ... "ROW" IN SPARSE-STORED PART OF MMM
         IJ=IFIRST(IROW)       
         IF(IJ.EQ.0)GO TO 1599
         IPRE=0
         do while(ij>0)
            ZHZ(IJ)=0.D0
            IPRE=IJ
            IJ=INEXT(IJ)
         end do
         IFIRST(IROW)=0
1599     LL=IROW
      END IF

C     ... PART OF COLUMN WHICH IS SPARSE-STORED
      DO 401 JROW=LL+1,NEQNS
      IJ=IFIRST(JROW)
      IPRE=0
402   IF(IJ.EQ.0)GO TO 401
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
401   CONTINUE
85    DIA(IROW)=0.D0
      RETURN

      END subroutine dfzer3

C     =================================
      SUBROUTINE DFABRW(DD,IROW,IEMPTY)
C     =================================

      integer,intent(in) :: irow
      integer,intent(inout) :: iempty
      real(8), intent(in) :: dd

      integer :: k,i
      real(8) :: tt,zz

      K=0
      TT=-1.D0/DD

C     ---------------
C     FULLSTORED PART
C     ---------------

      IF(IROW.LE.LIMXHX)THEN
C        ... PICK OUT NON-ZERO OFF-DIAGONAL ELEMENTS
         DO I=1,IROW-1
         IF(DABS(XHX(I,IROW)).GT.ZERO)THEN
            K=K+1
            IVEC(K)=I
            RROW(K)=XHX(I,IROW)
         END IF
         end do

C        ABSORB
         DO I=1,K
         JROW=IVEC(I)
         ZZ=TT*RROW(I)
         XVEC(:i)=ZZ*RROW(:i)
         DO J=1,I-1
         XHX(IVEC(J),JROW)=XHX(IVEC(J),JROW)+XVEC(J)
         end do
         DIA(JROW)=DIA(JROW)+XVEC(I)
         end do

C     -----------
C     SPARSE PART
C     -----------

      ELSE
C        ... PICK OUT NON-ZERO OFFDIAGONALS
         IJ=IFIRST(IROW)
         IF(IJ.EQ.0)RETURN
         IPRE=0
         do while(ij>0)
         ZZ=ZHZ(IJ)
         IF(DABS(ZZ).GT.ZERO)THEN
            K=K+1
            RROW(K)=ZZ
            IVEC(K)=IVCOL(IJ)
         END IF
         IPRE=IJ
         IJ=INEXT(IJ)
         end do

C        ...SET INDICATOR TO REUSE PLACES IN CURRENT ROW
         IF(IEMPTY.GE.0)THEN
            ISAVE=IEMPTY
            IEMPTY=IFIRST(IROW)
            INEXT(IPRE)=ISAVE
         END IF

C        ...ABSORB
         DO 102 I=1,K
         JROW=IVEC(I)
         ZZ=TT*RROW(I)
         XVEC(:i)=ZZ*RROW(:i)
         IF(JROW.LE.LIMXHX)THEN
            DO  J=1,I-1
            XHX(IVEC(J),JROW)=XHX(IVEC(J),JROW)+XVEC(J)
            end do
         ELSE
            IJ=IFIRST(JROW)
            IPRE=0
            DO 103 J=1,I-1
            JCOL=IVEC(J)
104         IF( (IJ.GT.0) .AND. (IVCOL(IJ).LT.JCOL) )THEN
C              SKIP OVER ELEMENTS IN ROW WITH COL. LOWER THAN JCOL
               IPRE=IJ
               IJ=INEXT(IJ)
               GO TO 104
            ELSE IF( (IJ.GT.0) .AND. (JCOL.EQ.IVCOL(IJ)) )THEN
C              ELEMENT FOUND, MODIFY
               ZHZ(IJ)=ZHZ(IJ)+XVEC(J)
            ELSE
C              NEW ELEMENT
               IF(IEMPTY.LE.0)THEN
C                 NO EMPTY SPACES WHICH CAN BE FILLED
                  NRCZHZ=NRCZHZ+1
                  IF(NRCZHZ.GT.MZHZ)THEN
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
              ZHZ(NEWADD)=XVEC(J)
              IVCOL(NEWADD)=JCOL
              INEXT(NEWADD)=0
              IF( (IJ.EQ.0) .AND. (IFIRST(JROW).EQ.0) )THEN
C                FIRST ELEMENT IN THIS ROW
                 IFIRST(JROW)=NEWADD
                 INEXT(NEWADD)=0
              ELSE IF( (IJ.EQ.0) .AND. (IFIRST(JROW).GT.0) )THEN
C                ALL PREV. ELEMENTS IN ROW HAVE LOWER COL.NO.
                 INEXT(IPRE)=NEWADD
                 INEXT(NEWADD)=0
              ELSE IF( (IJ.GT.0) .AND. (IPRE.EQ.0) )THEN
C                NEW ELEMENT HAS LOWEST COL NO. IN ROW
                 INEXT(NEWADD)=IFIRST(JROW)
                 IFIRST(JROW)=NEWADD
              ELSE
C                FIT ELEMENT IN 'RIGHT' PLACE
                 INEXT(NEWADD)=INEXT(IPRE)
                 INEXT(IPRE)=NEWADD
              END IF
              IJ=NEWADD
            END IF
103         CONTINUE
         END IF
         DIA(JROW)=DIA(JROW)+XVEC(I)
102      CONTINUE

      END IF
      RETURN

      END subroutine dfabrw

C     ==========================
      SUBROUTINE LNKROW(IROW,NN)
C     ==========================

      integer, intent(in) :: irow,nn
      integer             :: kk,ipre,icol,iplace

      IF(NN.EQ.0)RETURN

      KK=0
      IPRE=0
      IPLACE=IFIRST(IROW)
20    KK=KK+1
      ICOL=IVEC(KK)

C     GO TO NEXT ELEMENT IN THIS ROW
10    IF( (IPLACE.GT.0) .AND. (IVCOL(IPLACE).LT.ICOL) )THEN
         IPRE=IPLACE
         IPLACE=INEXT(IPLACE)
         GO TO 10

C     ELEMENT FOUND
      ELSE IF( (IPLACE.GT.0) .AND. (ICOL.EQ.IVCOL(IPLACE)) )THEN
         ZHZ(IPLACE)=ZHZ(IPLACE)+XVEC(KK)
         IPRE=IPLACE
         IPLACE=INEXT(IPLACE)

C     ELEMENT (IROW,ICOL) HAS NOT BEEN ACCESSED BEFORE
      ELSE
         NRCZHZ=NRCZHZ+1
         IF(NRCZHZ.GT.MAXREC)THEN
            write(*,*)'Subroutine "LNKROW" : DIMENSIONS EXCEEDED !!'
            write(*,*)'max. no. of elements =',maxrec
            write(*,*)'row no.              =',irow
            write(*,*)'no. elements in row  =',nn
            write(*,*)'column no.           =',icol
            write(*,*)'element in row no.   =',kk
            stop 'reset parameter "maxrec" '
         END IF
         ZHZ(NRCZHZ)=XVEC(KK)
         IVCOL(NRCZHZ)=ICOL
         IF(IPLACE.EQ.0.AND.IFIRST(IROW).EQ.0)THEN
C        FIRST ELEMENT IN THIS ROW
            IFIRST(IROW)=NRCZHZ
            INEXT(NRCZHZ)=0
         ELSE IF(IPLACE.EQ.0.AND.IFIRST(IROW).NE.0)THEN
C        ALL PREVIOUS ELEMENTS IN THE ROW HAVE LOWER COLUMN NO.
             INEXT(IPRE)=NRCZHZ
             INEXT(NRCZHZ)=0
C        FIT ELEMENT IN "RIGHT" PLACE, SWAPPING INDICATORS
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
      IF(KK.LT.NN)GO TO 20
      RETURN

      END subroutine lnkrow

      END subroutine dflsq3

C============================================================================
      subroutine DFLSP3 (lim0)
C============================================================================

      use names
      use units
      use means
      use levels
      use numbers

      integer, intent(in)                 :: lim0
      integer, dimension(:),  allocatable :: nnvec
      real(8), dimension(:),  allocatable :: xvec,row

      REWIND(IUN13)
      READ(IUN13)LIM2

      allocate(nnvec(lim2),row(lim2),xvec(lim2),stat=ii)
      if(ii>0)stop 'dflsp3 : alloc'

      read(iun13)nnvec
      read(iun13)row
      read(iun13)xvec

      WRITE(IUN66,*)'  '
      WRITE(IUN66,*)'----------------------------------------------'
      WRITE(IUN66,*)'LSQ SOLUTION FOR FIXED EFFECTS AND COVARIABLES'
      WRITE(IUN66,*)'----------------------------------------------'

      K=LIM0
      DO IQ=1,NQ
      DO I=1,NCOV(IQ)
      WRITE(IUN66,909)'COVARIABLE NO.',I,COVAR(I,IQ)
      WRITE(IUN66,913)
      DO  J=1,NPOW(I,IQ)
      K=K+1
      WRITE(IUN66,911)K,'ORDER',J,IQ,XVEC(K)
      end do
      end do
      end do

      DO IQ=1,NQ
      IF(NFIX(IQ).GT.0)WRITE(IUN66,913)
      DO I=1,NFIX(IQ)
      WRITE(IUN66,909)'FIXED EFFECT NO.',I,FIXED(I,IQ)
      DO J=1,NLEV(I,IQ)
      K=K+1
      WRITE(IUN66,912)K,'LEVEL',J,IQ,NNVEC(K),ROW(K),XVEC(K)
      end do
      end do
      end do

      deallocate(nnvec,row,xvec,stat=ii)
      if(ii>0)stop 'dealloc dflsp3'
      return

913   FORMAT(/' EQ.NO.',T28,'TRAIT',T40,2X,'NREC',14X,'MEAN',10X,
     *                                                'SOLUTION')
911   FORMAT(I5,2X,A,T21,2I6,    T40,24X,G18.10)
912   FORMAT(I5,2X,A,T21,2I6,    T40,I6,2F18.8)
909   FORMAT(/1X,A,I4,4X,A12)

      end subroutine dflsp3


















