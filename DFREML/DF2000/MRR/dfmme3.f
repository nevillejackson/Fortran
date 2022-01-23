!===========================================================================
      SUBROUTINE DFMME3 (m1,m2,iposfx)
!===========================================================================

      use params
      use units
      use means
      use combinations
      use levels
      use numbers
      use order
      use ages
      use mme3
      use read_iun52

!     arguments
      integer, intent(in)                    :: m1,m2
      integer, dimension (m1,m2),intent(in)  :: iposfx

!     local variables
      real(8), dimension (:), allocatable    :: yy
      integer, dimension (:), allocatable    :: jvec,ifix,mark,jage,ivec
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      mnr8=maxval(nr8)
      nneff=maxval(neff)
      mfix2=maxval(nfix2)
      ncomb=0
      if(nq>1)ncomb=1
      mmr=0
      nnrec=0

      if(iall52.eq.0)call all_read52(nneff,mnfr,mobs,neqns,nmeta)

      allocate (yy(mnr8),ifix(mfix2),jvec(neqns),jage(nmeta),
     &          mark(mage*nq),ivec(nmeta+nfxreg),stat=ii)
      if(ii>0)stop 'dfmme3 : alloc '

      icomb=-1
      if(iopmod.eq.5 .or. iopmod.eq.6)then
         NQ2=ksfit(1)+ksfit(2)
      else
         NQ2=NQ+NQ222
      end if         

      maxnr=0
      nboth=0

C     ---------
C     READ DATA
C     ---------

C     FIRST RECORD FOR FIRST ANIMAL
      READ(IUN22)IQ,jage,ianim,IFIX(:NFIX2(IQ)),YY(:NR8(IQ))
     &,          ivec(:nmeta+nfxreg)
      JANIM=IANIM
      NREC=1
      NDATA=0
      NOBS=0
      NR=0
      mark=0
      JA=1
      JQ=1
      IEND=0
      GO TO 55
                           ! assume same no. meta-meters for all traits
50    READ(IUN22,END=99)IQ,jage,ianim,IFIX(:NFIX2(IQ)),YY(:NR8(IQ))
     &,          ivec(:nmeta+nfxreg)
      CALL KOUNT(NREC,5000)
      IF(NFILL.EQ.0)call check_ranges

!     NEW ANIMAL, WRITE OUT RECORD FOR PREVIOUS ANIMAL
55    IF(JANIM.NE.IANIM)THEN
         NDATA=NDATA+1
         mmr(nobs)=mmr(nobs)+1
         IF(NFILL.EQ.0)call check_maxobs

C        ... DETERMINE COMBINATION NO.
         if(nq>1)then
            call check_orderobs         
         else
            CALL DXCMB3 (MARK,mage,nq,ICOMB)
         end if

!        collect & recode FE and RE
         call collect_codes

!        write out info for this animal to unit 52
         call write_iun52

!        count number of records for pairs of traits
         if(nq>1)call count_pairs

C        RESET FOR NEXT ANIMAL
         IF(IEND.EQ.99)GO TO 199
         JANIM=IANIM
         NOBS=0
         NR=0
         mark=0
      END IF

C     SAME ANIMAL : ACCUMULATE RECORDS FOR THE ANIMAL
      NOBS=NOBS+1
      NNQ(NOBS)=iq
      nna(nobs,:)=jage ! running no. of relevant age(s)
      
      jj=nage(1)*(iq-1)+jage(1)
      MARK(jj)=MARK(jj)+1 ! use 1st metameter for
      nnrec(jage(1),iq)=nnrec(jage(1),iq)+1

      YVEC(NOBS)= YY(ITRAIT(IQ))-YBAR(IQ) 
      K=kftfix(iq)
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

      M=NFIX(IQ)

!     perm. environm. effects due to the animal
      if(ioprn1.eq.1 .and. ieqmod.eq.0 )then
         mm=LIM2+(IFIX( nfix(iq)+1 )-1)*sum(kfit(4,:nq))
     &                                + sum(kfit(4,:iq-1))
         do kq=1,kfit(4,iq)
         m=m+1
         IEQ(M,NOBS)=mm+kQ
         end do
      end if

      if(ioprn3.eq.1)then
         mm=LIM3a+(IFIX( nfix(iq)-ieqmod+ioprn1+1 )-1)*sum(kfit(5,:nq))
         if(iq>1)mm=mm+sum(kfit(5,:iq-1))
         do kq=1,kfit(5,iq)
         m=m+1
         IEQ(M,NOBS)=mm+kQ
         end do
      end if

C     RANDOM EFFECT(S) FOR ANIMALS
      if(ioprn2.eq.1)then
         MM=LIM3+(IFIX( nfix2(iq) )-1)*nq2+sum(kfit(1,:nq))
     &                                    +sum(kfit(2,:iq-1))
         do kq=1,kfit(2,iq)
         m=m+1
         IEQ(M,NOBS)=mm+kQ
         end do
      end if

      if(nanim>0)then
         mm=LIM3+(IANIM-1)*nq2 + sum( kfit(1,:iq-1))
         do kq=1,kfit(1,iq)
         m=m+1
         IEQ(M,NOBS)=mm+kQ
         end do
      end if

C     RECODE IF EQUATIONS HAVE BEEN RE-ORDERED
      IF(NFILL.GT.0)THEN
         DO I=1,NEFF(IQ)
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

      deallocate(yy,ifix,mark,ivec,stat=ii)
      if(ii>0)stop 'dfmme3 : de-alloc 2'
      print *,'max. no. for "nr" =',maxnr
      mqhq=maxnr

      RETURN
9     FORMAT(8X,A,I8)

      contains

!     =======================
      subroutine check_ranges
!     =======================

      DO L=1,NFIX2(IQ)
      IF(IFIX(L).GT.NLEV(L,IQ) .OR. IFIX(L).LT.1)THEN
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
      if(nanim.eq.0)return
      IF(IANIM.GT.NANIM .OR. IANIM.LT.1)THEN
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

      integer  :: n,lq

C     CHECK THAT MAX. NO. OF OBSERVATIONS/TRAIT IS NOT EXCEEDED
      DO LQ=1,NQ
      i=nage(1)*(lq-1)
      n=sum( mark(i+1:i+nage(1)) )
      IF(n.GT.MXOBS(LQ))THEN
          WRITE(*,*)'FOUND MORE RECORDS PER TRAIT THAN ALLOWED !'
          WRITE(*,9)'TRAIT NO.                     =',LQ
          WRITE(*,9)'MAX. NO. OF RECORDS SPECIFIED =',MXOBS(LQ)
          WRITE(*,9)'NO. OF RECORDS FOUND          =',n
          WRITE(*,9)'ANIMAL ID                     =',JANIM
          STOP 'MAKE SURE YOU KNOW YOUR DATA !'
      ELSE IF(n.GT.NXOBS(LQ))THEN
          NXOBS(LQ)=n
      END IF
      END DO

      return
9     FORMAT(8X,A,I8)
      end subroutine check_maxobs

!     ==========================
      subroutine check_orderobs
!     ==========================

!     multiple traits : check that records are ordered according to age
!                       & trait no. within age
      do i=1,nobs-1
      if(nna(i,1)>nna(i+1,1))then
          print *,'Records out of order ?'
          print *,'expect : sorted according to age within animal'
          print *,'order found  =',nna(:nobs,1)
          do j=1,nobs
          print *,j,nnq(j),nna(j,1),iiage(nna(j,1),1),yvec(j)
          end do
          print *,'animal       =',janim
          stop 'check data'
      else if(nna(i,1).eq.nna(i+1,1) .and. nnq(i)>nnq(i+1))then
          print *,'traits within age not sorted properly'
          print *,'age          =',nna(i,1)
          print *,'observations =',i,i+1
          print *,'trait no.s   =',nnq(i:i+1)
          stop 'cannot assign error covariances'
      end if
      end do
      return
      end subroutine check_orderobs

!     ========================
      subroutine collect_codes
!     ========================

!     fixed & random effects for this animal
      DO I=1,NOBS
      DO 1 J=1,neff(nnq(i))
      II= IEQ(J,I)

      DO K=1,NR
      IF(II.EQ.JVEC(K))THEN
         GO TO 1
      ELSE IF(JVEC(K).GT.II)THEN
         NR=NR+1
         DO L=NR,K+1,-1
         JVEC(L)=JVEC(L-1)
         END DO
         JVEC(K)=II
         GO TO 1
      END IF
      END DO
      NR=NR+1
      JVEC(NR)=II
 1    CONTINUE
      END DO  ! iobs
      if(nr.gt.maxnr)maxnr=nr

C     RECODE FIXED & RANDOM EFFECTS "CONDENSED" FOR THE ANIMAL
      DO I=1,NOBS
      DO 5 J=1,NEFF(NNQ(I))
      II=IEQ(J,I)
      K1=1
      K2=NR+1
 6    K=K1+(K2-K1)/2
      IF(II.EQ.JVEC(K))THEN
         IEQ(J,I)=K+LIM1
         GO TO 5
      ELSE IF(II.LT.JVEC(K))THEN
         K2=K
      ELSE
         K1=K
      END IF
      GO TO 6
 5    CONTINUE
      end do

      return
      end subroutine collect_codes

!     ======================
      subroutine write_iun52
!     ======================

      WRITE(IUN52)ICOMB,NOBS,NR,NNA(:NOBS,:),NNQ(:nobs),JVEC(:NR),
     *   ((IEQ(K,L) ,K=1,NEFF(NNQ(L)) ),L=1,NOBS),
     *   ((XCOV(K,L) ,K=kftfix(nnq(l))+1,NFR(NNQ(L)) ),L=1,NOBS),
     *   YVEC(:NOBS)
      return
      end subroutine write_iun52

!     ======================
      subroutine count_pairs
!     ======================
      do  i=2,nobs
      if(nnq(i)>nnq(i-1))nboth(ihmssf(nnq(i-1),nnq(i),nq))=
     &                        nboth(ihmssf(nnq(i-1),nnq(i),nq))+1
      end do
      return
      end subroutine count_pairs

      END subroutine dfmme3


