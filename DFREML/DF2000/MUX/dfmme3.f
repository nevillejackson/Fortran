C===========================================================================
      SUBROUTINE  DFMME3 (iposfx)
C==============================================================================

      use params
      use units
      use means
      use combinations
      use levels
      use numbers
      use order
      use mme3

!     arguments
      integer, dimension (maxfix,maxnq),intent(in) ::  iposfx

!     local variables
      real(8), dimension (:,:),allocatable   :: xcov
      real(8), dimension (:), allocatable    :: yvec,yy
      integer, dimension (:), allocatable    :: nnq,jvec,ifix,NFSUB1,
     &                                          NFSUB2, nfsub3,MARK
      integer, dimension (:,:), allocatable  :: ieq
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      mnr8=MAXVAL(nr8)
      allocate (xcov(mnfr,mobs),yvec(mobs),yy(mnr8),
     *          nnq(mobs),ieq(maxfix+3,mobs),jvec(neqns),
     &          ifix(maxval(nfix2)),nfsub1(nq),nfsub2(nq),nfsub3(nq),
     &          stat=ii)
      if(ii>0)stop 'dfmme3 : alloc 2'
      if(irpmod.eq.1)then
         allocate(mark(mobs),stat=ii)
      else
         allocate(mark(nq),stat=ii)
      end if
      if(ii>0)stop 'dfmme3 : alloc mark'
     
      ncomb=0
      maxnr=0
      NQ2=NQ+NQ222
      nfsub1=0
      if(ioprn1.eq.1)then
         do iq=1,nq
         do jq=1,iq-1
         if(nfix(jq).eq.nfix1(jq))nfsub1(iq)=nfsub1(iq)+1
         end do
         end do
      end if
      nfsub3=0
      if(ioprn3.eq.1)then
         do iq=1,nq
         do jq=1,iq-1
         if(nfix1(jq).eq.nfix3(jq))nfsub3(iq)=nfsub3(iq)+1
         end do
         end do
      end if

      nfsub2=0
      if(ioprn2.eq.1)then
         do iq=1,nq
         do jq=1,iq-1
         if(nfix3(jq).eq.nfix2(jq))nfsub2(iq)=nfsub2(iq)+1
         end do
         end do
      end if

C     ---------
C     READ DATA
C     ---------

C     FIRST RECORD FOR FIRST ANIMAL
 66   READ(IUN22)IQ,ianim,IFIX(:NFIX2(IQ)),YY(:NR8(IQ))
      if(nquni>0.and.iq.ne.nquni)go to 66

      JANIM=IANIM
      NREC=1
      JQ=1
      NDATA=0
      NOBS=0
      NR=0
      mark=0
      IEND=0
      GO TO 55

50    READ(IUN22,END=99)IQ, ianim,IFIX(:NFIX2(IQ) ), YY(:NR8(IQ) )
      if(nquni>0.and.iq.ne.nquni)go to 50
      CALL KOUNT(NREC,5000)
      IF(NFILL.EQ.0)call check_ranges

C     NEW ANIMAL, WRITE OUT RECORD FOR PREVIOUS ANIMAL
55    IF(JANIM.NE.IANIM)THEN
         NDATA=NDATA+1
         IF(NFILL.EQ.0)call check_maxobs

C        ... DETERMINE COMBINATION NO.
         CALL DXCMB3 (MARK,NQ,ICOMB)

C        ... COLLECT CODES FOR FIXED & RANDOM EFFECTS FOR THIS ANIMAL
         DO I=1,NOBS
         DO 513 J=1,NEFF(NNQ(I))
         II= IEQ(J,I)

         DO K=1,NR
         IF(II.EQ.JVEC(K))THEN
            GO TO 513
         ELSE IF(JVEC(K).GT.II)THEN
            NR=NR+1
            DO L=NR,K+1,-1
            JVEC(L)=JVEC(L-1)
            END DO
            JVEC(K)=II
            GO TO 513
         END IF
         END DO
         NR=NR+1
         JVEC(NR)=II
513      CONTINUE
         END DO

C        ... RECODE FIXED & RANDOM EFFECTS "CONDENSED" FOR THE ANIMAL
         DO I=1,NOBS
         DO 5130 J=1,NEFF(NNQ(I))
         II=IEQ(J,I)
         K1=1
         K2=NR+1
5140     K=K1+(K2-K1)/2
         IF(II.EQ.JVEC(K))THEN
            IEQ(J,I)=K+LIM1
            GO TO 5130
         ELSE IF(II.LT.JVEC(K))THEN
            K2=K
         ELSE
            K1=K
         END IF
         GO TO 5140
 5130    CONTINUE
         end do
         WRITE(IUN52)ICOMB,NOBS,NR,NNQ(:NOBS),JVEC(:NR),
     *               ((IEQ(k,l) ,K=1,NEFF(NNQ(L)) ),l=1,nobs),
     *               ((XCOV(K,l) ,K=1,NFR(NNQ(L)) ),l=1,nobs),
     &               YVEC(:NOBS)
         if(nr.gt.maxnr)maxnr=nr
         
C        RESET FOR NEXT ANIMAL
         IF(IEND.EQ.99)GO TO 199
         JANIM=IANIM
         JQ=1
         NOBS=0
         NR=0
         mark=0
      END IF

C     SAME ANIMAL : ACCUMULATE RECORDS FOR THE ANIMAL
      NOBS=NOBS+1
      IF(NFILL.EQ.0)THEN
         IF(irpmod.eq.2 .and. IQ.LT.JQ)THEN
            WRITE(*,*)'SORTING ERROR IN DATA AT RECORD NO. :',NREC
            WRITE(*,*)'ANIMAL NO. IS                       :',IANIM
            WRITE(*,*)'PREVIOUS RECORD WAS FOR TRAIT NO.   :',JQ
            WRITE(*,*)'THIS RECORD IS FOR TRAIT NO.        :',IQ
            WRITE(*,*)'DATA *MUST* BE SORTED ACCORDING TO TRAITS WITHIN'
     *,               ' ANIMALS !!!'
            STOP 'SORT DATA FILE AND TRY AGAIN ...'
         END IF
      END IF
      NNQ(NOBS)=IQ
      MARK(IQ)=MARK(IQ)+1
      YVEC(NOBS)= YY(ITRAIT(IQ))-YBAR(IQ) 
      K=0
      DO I=1,NCOV(IQ)
      C= YY(I)-CBAR(I,IQ) 
      CC=1.D0
      DO J=1,NPOW(I,IQ)
      CC=CC*C
      K=K+1
      XCOV(K,NOBS)=CC
      end do
      end do

C     FIXED EFFECTS
      IEQ(:nfix(iq),NOBS)= IPOSFX(:nfix(iq),IQ)+IFIX(:nfix(iq))

C     ADDITIONAL RANDOM EFFECT(s)
      if(ioprn1.eq.1)then
         M=NFIX1(IQ)
         IF(M>NFIX(IQ)) IEQ(M,NOBS)=LIM2+(IFIX(M)-1)*NQ111+IQ-nfsub1(iq)
         if(ioprn3.eq.1)then
            M=NFIX3(IQ)
            if(m>nfix1(iq)) IEQ(m,NOBS)=LIM3a+(IFIX(M)-1)*NQ333
     &                                                   +IQ-nfsub3(iq)
         end if
      end if

C     RANDOM EFFECT(S) FOR ANIMALS
      N=NFIX2(IQ)
      IF(N>nfix3(iq))IEQ(N,NOBS)=LIM3+(IFIX(N)-1)*NQ2+NQ+IQ-nfsub2(iq)
      IEQ(N+1,NOBS)=LIM3+(IANIM-1)*NQ2+IQ
      JQ=IQ

C     RECODE IF EQUATIONS HAVE BEEN RE-ORDERED
      IF(NFILL.GT.0)THEN
         DO I=1,N+1
         IEQ(I,NOBS)= IEQNEW( IEQ(I,NOBS) )
         END DO
      END IF
      GO TO 50

C      ------------------
C      ALL DATA PROCESSED
C      ------------------

99    IEND=99
      IANIM=-999
      GO TO 55

 199  WRITE(*,9)'NO. OF RECORDS PROCESSED    =',NREC
      WRITE(*,9)'NO. OF ANIMALS IN THE DATA  =',NDATA
      WRITE(*,9)'TOTAL NO. OF EQUATIONS      =',NEQNS
      mqhq=maxnr

      RETURN
9     FORMAT(8X,A,I8)
      contains

!     =======================
      subroutine check_ranges
!     =======================

      DO L=1,NFIX2(IQ)
      IF(IFIX(L)>NLEV(L,IQ) .OR. IFIX(L)<1)THEN
         WRITE(*,*)'CODE OUT OF SPECIFIED RANGE ENCOUNTERED !'
         WRITE(*,*)'   RECORD NO.            =',NREC
         WRITE(*,*)'   ANIMAL NO.            =',IANIM
         WRITE(*,*)'   TRAIT NO.             =',IQ
         WRITE(*,*)'   EFFECT NO.            =',L
         WRITE(*,*)'   CODE FOUND            =',IFIX(L)
         WRITE(*,*)'   NO. OF LEVELS GIVEN   =',NLEV(L,IQ)
         STOP '"DFMME3" : CHECK MODEL & TRY AGAIN !'
      END IF
      end do
      IF(IANIM>NANIM .OR. IANIM<1)THEN
         WRITE(*,*)'ANIMAL ID OUT OF SPECIFIED RANGE ENCOUNTERED !'
         WRITE(*,*)'   RECORD NO.            =',NREC
         WRITE(*,*)'   ANIMAL ID             =',IANIM
         WRITE(*,*)'   TRAIT NO.             =',IQ
         WRITE(*,*)'   NO. OF LEVELS GIVEN   =',NANIM
         STOP '"DFMME3" : CHECK MODEL & TRY AGAIN !'
      end if

      return
      end subroutine check_ranges

!     =======================
      subroutine check_maxobs
!     =======================

C     CHECK THAT MAX. NO. OF OBSERVATIONS/TRAIT IS NOT EXCEEDED
      DO LQ=1,NQ
      IF(MARK(LQ).GT.MXOBS(LQ))THEN
          WRITE(*,*)'FOUND MORE RECORDS PER TRAIT THAN ALLOWED !'
          WRITE(*,9)'TRAIT NO.                     =',LQ
          WRITE(*,9)'MAX. NO. OF RECORDS SPECIFIED =',MXOBS(LQ)
          WRITE(*,9)'NO. OF RECORDS FOUND          =',MARK(LQ)
          WRITE(*,9)'ANIMAL ID                     =',JANIM
          STOP 'MAKE SURE YOU KNOW YOUR DATA !'
      ELSE IF(MARK(LQ).GT.NXOBS(LQ))THEN
          NXOBS(LQ)=MARK(LQ)
      END IF
      END DO

      return
9     FORMAT(8X,A,I8)
      end subroutine check_maxobs

      END subroutine dfmme3
























