C============================================================================
      SUBROUTINE DFMMP1
C============================================================================

      use diagonal
      use order
      use like
      use levels
      use variance_ratios
      use units
      use constants
      use numbers
      use xsprse
   
      integer :: ity
      real(8) :: xx

C     --------------------------------------------------------
C     READ NON-ZERO COEFFICIENTS OF NRM INVERSE FROM UNIT "44"
C     ADD CONTRIBUTIONS FOR ANIMALS
C     --------------------------------------------------------

      REWIND(IUN44)
      READ(IUN44)DETL
      if(nainv>1)detl=detll

50    READ(IUN44,END=99)IC,IR,XX,ity
      if(ic<0)then
        detl=xx
        go to 99
      end if

      if(nrand2.eq.0)then
         ICOL1=IEQNEW(LIM3+IC)
      else
         ICOL1=IEQNEW(LIM3+IC+IC-1)
         ICOL2=IEQNEW(LIM3+IC+IC)
      end if

      IF(IR.EQ.IC)THEN    ! diagonal element
         IROW1=ICOL1
         DIA(IROW1)=DIA(IROW1)+XX*XLAMB(ity)
         IF(iopcov.eq.1.or.IOPCOV.Eq.2)THEN
            IROW2=ICOL2
            DIA(IROW2)=DIA(IROW2)+XX*XKAPPA
         ELSE IF(IOPCOV.EQ.3)THEN
            IROW2=ICOL2
            DIA(IROW2)=DIA(IROW2)+XKAPPA
         END IF

      ELSE               ! off-diagonal element
         if(nrand2.eq.0)then
            IROW1=IEQNEW(LIM3+IR)
         else
            IROW1=IEQNEW(LIM3+IR+IR-1)
            IROW2=IEQNEW(LIM3+IR+IR) 
            IF(iopcov.eq.1.or.IOPCOV.eq.2)THEN    !  ... MAT. * MAT.
               k=isploc(icol2,irow2)
               xspars(k)=xspars(k)+XX*XKAPPA
            END IF
         end if
         k=isploc(icol1,irow1)          !   ... DIRECT * DIRECT
         xspars(k)=xspars(k)+XX*XLAMB(ity)
      END IF

      IF(IOPCOV.EQ.2)THEN    !   ... DIRECT * Mat. (always off-diag)
         K=ISPLOC(ICOL2,IROW1)
         XSPARS(K)=XSPARS(K)+XX*ALPHA
         IF(IR.NE.IC)THEN
            K=ISPLOC(ICOL1,IROW2)
            XSPARS(K)=XSPARS(K)+XX*ALPHA
         END IF
      END IF

      GO TO 50

C     -----------------------------------------------------
C     MODEL 6 OR 10 : READ INVERSE OF COVARIANCE MATRIX FOR
C               2ND ANIMAL EFFECT, ADD CONTRIBUTIONS TO MMM
C     -----------------------------------------------------

99    IF(IOPCOV.EQ.4)THEN
         REWIND(IUN45)
55       READ(IUN45,END=89)IC,IR,XX
         ICOL2=IEQNEW(LIM3+IC+IC)
         IF(IC.EQ.IR)THEN
            IF(ICOL2.GT.0)DIA(ICOL2)=DIA(ICOL2)+XX*XKAPPA
         ELSE
            IROW2=IEQNEW(LIM3+IR+IR)
            k=isploc(icol2,irow2)
            xspars(k)=xspars(k)+XX*XKAPPA
         END IF
         GO TO 55
      END IF

C     --------------------------------------------
C     VARIANCE RATIO FOR ADDITIONAL RANDOM EFFECTS
C     --------------------------------------------

89    LL=LIM2
      DO JJ=1,IOPRN1
      if(nlev(nfix+jj).eq.0)cycle

      IF(JJ.GT.1.OR.JOPCOV.EQ.0)THEN       ! EFFECT IS IID
         DO I=1,NLEV(NFIX+JJ)
         LL=LL+1
         II=IEQNEW(LL)
         IF(II.GT.0)DIA(II)=DIA(II)+GAMMA(JJ)
         END DO

      ELSE IF(JJ.EQ.1.AND.JOPCOV.EQ.1)THEN
         REWIND(IUN47)
45       READ(IUN47,END=79)IC,IR,XX
         IF( (IC<1.OR.IC>NRAND1).or.(ir<1.or.ir>nrand1))stop 'err iun47'
         ICOL2=IEQNEW( LL+IC )
         IF(IC.EQ.IR)THEN
            IF(ICOL2.GT.0)DIA(ICOL2)=DIA(ICOL2)+XX*gamma(1)
         ELSE
            IROW2=IEQNEW(LL +IR)
            K=ISPLOC(ICOL2,IROW2)
            XSPARS(K)=XSPARS(K)+XX*GAMMA(1)
         END IF
         GO TO 45
 79      LL=LL+NLEV(NFIX+1)
      END IF
      END DO

      RETURN
      END subroutine dfmmp1










