C==========================================================================
      SUBROUTINE DFCFIX(IOPT,IUN34)
C=====================================================================

      use parameters
      use form_of_inputs
      use c_numbers
      use c_fixed
      use ages

      integer, intent(in)                  :: iopt,iun34
      integer, dimension(5)                :: ivec

C     DETERMINE NO. OF FIXED EFFECTS PLUS ADD. RANDOM EFFECTS
      mfix=maxval(kfix)+nfxreg
      allocate(nl(mfix,nq),idfix(maxnfl,mfix,nq),nnfix(maxnfl,mfix,nq),
     &         xxfix(maxnfl,mfix,nq),stat=ii)
      if(ii>0)stop 'alloc fe levels'
      nl=0
      nnfix=0
      xxfix=0.d0

      mfx1=maxval(kfix1)
      if(mfx1>0)then
         allocate(nlrnd1(mfx1),idrnd1(mxrnd1,mfx1),
     &            nnrnd1(mxrnd1,mfx1,nq),xxrnd1(mxrnd1,mfx1,nq),stat=ii)
         if(ii>0)stop 'alloc fe levels'
         nlrnd1=0
         nnrnd1=0
         xxrnd1=0.d0
         idrnd1=0
      end if
      
      REWIND IUN34
      IQ=1
      NREC=0
      IEND=0
 50   CALL DFRD34 (IUN34,IOFFLD,IOPT,IVEC,IQ,IEND,INFMTD)
      IF(IEND.EQ.99)GO TO 99
      if(iospec.eq.4)then
         if(ivec(2).eq.0)go to 50         ! missing sire ID
         if(iopt.ge.5)jvec(kfix(iq)+1)=ivec(2)  
      end if

      CALL KOUNT(NREC,5000)

!     Fixed effects
      jj=0
      DO 1 II=1,KFIX(IQ)
      jj=jj+1
      DO J=1,NL(II,IQ)
      IF(IDFIX(J,II,IQ).EQ.JVEC(jj))THEN        !code already exists
         GO TO 1
      ELSE IF(IDFIX(J,II,IQ).GT.JVEC(jj))THEN   !new code found, slot in
         NL(II,IQ)=NL(II,IQ)+1
         IF(NL(II,IQ)>MAXNFL)call exceed_fix
         DO K=NL(II,IQ),J+1,-1
         IDFIX(K,II,IQ)=IDFIX(K-1,II,IQ)
         end do
         IDFIX(J,II,IQ)=JVEC(jj)
         GO TO 1
      END IF
      end do
      NL(II,IQ)=NL(II,IQ)+1                     !new code is highest so far
      IF(NL(II,IQ)>MAXNFL)call exceed_fix
      IDFIX(NL(II,IQ),II,IQ)=JVEC(jj)
1     CONTINUE        

!     additional random effects
      DO  2 II=1,KFIX1(IQ)
      jj=jj+1
      if(jvec(jj) ==0)go to 2         ! ignore codes of zero
      DO J=1,NLRND1(II)
      IF(IDRND1(J,II).EQ.JVEC(jj))THEN 
         go to 2
      ELSE IF(IDRND1(J,II).GT.JVEC(jj))THEN 
         NLRND1(II)=NLRND1(II)+1
         IF(NLRND1(II).GT.MXRND1)call exceed_rnd1
         DO K=NLRND1(II),J+1,-1
         IDRND1(K,II)=IDRND1(K-1,II)
         end do
         IDRND1(J,II)=JVEC(jj)
         go to 2
      END IF
      end do
      NLRND1(II)=NLRND1(II)+1 
      IF(NLRND1(II).GT.MXrnd1)call exceed_rnd1
      IDRND1( NLRND1(II),II )=JVEC(jj)
 2    continue

!     RR analyses : levels for Fixed regresiion within classes
      do ii=kfix(iq)+1,kfix(iq)+nfxreg
      jj=jj+1
      DO J=1,NL(II,IQ)
      IF(IDFIX(J,II,IQ).EQ.JVEC(jj))THEN        !code already exists
         go to 11
      ELSE IF(IDFIX(J,II,IQ).GT.JVEC(jj))THEN   !new code found, slot in
         NL(II,IQ)=NL(II,IQ)+1
         IF(NL(II,IQ)>MAXNFL)call exceed_fix
         DO K=NL(II,IQ),J+1,-1
         IDFIX(K,II,IQ)=IDFIX(K-1,II,IQ)
         end do
         IDFIX(J,II,IQ)=JVEC(jj)
         GO TO 11
      END IF
      end do
      NL(II,IQ)=NL(II,IQ)+1                     !new code is highest so far
      IF(NL(II,IQ)>MAXNFL)call exceed_fix
      IDFIX(NL(II,IQ),II,IQ)=JVEC(jj)
 11   continue
      end do  ! ii
      GO TO 50

99    continue
      RETURN

      contains

      subroutine exceed_fix
      WRITE(*,*)'"DFCFIX" : Too many fixed effect levels !'
      WRITE(*,*)'   RECORD NO.              =',NREC
      WRITE(*,*)'   TRAIT NO.               =',IQ
      WRITE(*,*)'   FIXED effect no.        =',II
      write(*,*)'   maxnfl                  =',maxnfl,nl(ii,iq)
      write(*,*)'jvec',jvec(ii),ii
      STOP 'RESET PARAMETER "MAXNFL" !'
      end subroutine exceed_fix

      subroutine exceed_rnd1
      WRITE(*,*)'"DFCFIX" : Too many add. RE levels !'
      WRITE(*,*)'   RECORD NO.              =',NREC
      WRITE(*,*)'   Add. RE no.             =',II
      write(*,*)'   mxrnd1                  =',mxrnd1,nlrnd1(ii)
      write(*,*)'jvec',jvec(kfix(iq)+ii),kfix(iq)+ii
      STOP 'RESET PARAMETER "MXRND1" !'
      end subroutine exceed_rnd1
      END subroutine dfcfix

C==========================================================================
      subroutine DFIFIX(IQ)
C==========================================================================

      use c_numbers
      use c_fixed

C     RECODE FIXED EFFECTS LEVELS, USE HALVING SEARCH
      DO I=1,KFIX(IQ)+nfxreg
      K1=1                                                              
      K2=NL(I,IQ)+1
24    K=K1+(K2-K1)/2                                                    
      IF(JVEC(I).eq.IDFIX(K,I,IQ))then
         JVEC(I)=K
         NNFIX(k,I,IQ)=NNFIX(k,I,IQ)+1
         XXFIX(k,I,IQ)=XXFIX(k,I,IQ)+XVEC(KCOV(IQ)+1)  ! 1st RHS
         cycle
      end if
      IF(JVEC(I)-IDFIX(K,I,IQ).gt.0)go to 22
      K2=K                     
      GO TO 24
22    IF(K1.EQ.K)THEN
         WRITE(*,*)'SEARCH FAILED ?'
         WRITE(*,*)'IQ=',IQ,'  IFIX=',I,'   NLEV=',NL(I,IQ)
         WRITE(*,*)'CODE =',JVEC(I)
         STOP 'SOMETHING IS WRONG SOMEWHERE ...!!!'
      END IF
      K1=K                                                              
      GO TO 24                                                           
      end do

      if(kfix1(iq).eq.0)return

!     recode additional random effects levels
      DO I=1,kfix1(iq)
      if( jvec(kfix(iq)+i) ==0) cycle             ! ignore codes of 0
      K1=1                                                              
      K2=NLRND1(I)+1
34    K=K1+(K2-K1)/2                                                    
      IF(JVEC(kfix(iq)+I).eq.IDRND1(K,I))go to 33
      IF(JVEC(kfix(iq)+I)-IDRND1(K,I)>0)go to 32
      K2=K                     
      GO TO 34
32    IF(K1.EQ.K)THEN
         WRITE(*,*)'SEARCH FAILED ?'
         WRITE(*,*)'IQ=',IQ,'  IRND1=',I,'   NLEV=',NLRND1(I)
         WRITE(*,*)'CODE =',JVEC(kfix(iq)+I)
         STOP 'SOMETHING IS WRONG SOMEWHERE ...!!!'
      END IF
      K1=K                                                              
      GO TO 34                                                           
33    JVEC(kfix(iq)+I)=K
      NNRND1(k,I,IQ)=NNRND1(k,I,IQ)+1
      XXRND1(k,I,IQ)=XXRND1(k,I,IQ)+XVEC(KCOV(IQ)+1)
      end do

      RETURN
      END subroutine dfifix

C==========================================================================
      subroutine DFSFIX(IOPT,IUN11,IUN66)
C==========================================================================

      use c_numbers
      use c_fixed
      use ages
      use ped_nos

      integer, intent(in)                  :: iun11,iun66,iopt
      integer                              :: i,j,k,l,m,ii,iq,nl1

C     WRITE INFO TO UNIT "11" TO BE PICKED UP IN SET-UP & SOLUTION STEPS
      WRITE(IUN11)NNPED
      WRITE(IUN11)IOPT,NQ,MQ
      WRITE(IUN11)KCOV(:nq)
      WRITE(IUN11)KFIX(:nq),nfxreg
      WRITE(IUN11)KFIX1(:nq)
      WRITE(IUN11)KINT(:NQ)
      WRITE(IUN11)KNSERT(:NQ)
      WRITE(IUN11)((NL(L,K),L=1,KFIX(K)+nfxreg), K=1,NQ)
      WRITE(IUN11)(((IDFIX(M,L,K),M=1,NL(L,K)), L=1,KFIX(K)+nfxreg), 
     &                                                       K=1,NQ)
      WRITE(IUN11)(((NNFIX(M,L,K),M=1,NL(L,K)), L=1,KFIX(K)+nfxreg), 
     &                                                       K=1,NQ)

      nrand1=maxval(kfix1)
      write(iun11)nrand1
      if(nrand1>0)then
         write(iun11)nlrnd1(:nrand1)
         write(iun11)((idrnd1(m,l),m=1,nlrnd1(l)),l=1,nrand1)
      end if

      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'------------------------'
      WRITE(IUN66,*)'FIXED EFFECTS SUBCLASSES'
      WRITE(IUN66,*)'------------------------'
      WRITE(IUN66,*)' '
      DO IQ=1,NQ
      If(NQ.GT.1)THEN
         WRITE(IUN66,*)'TRAIT NO. =',IQ
         WRITE(IUN66,*)'------------------------- '
      END IF
      DO I=1,KFIX(IQ)+nfxreg
      WRITE(IUN66,*)'FIXED EFFECT NO. =',I,
     *              '    WITH NO. OF LEVELS =',NL(I,IQ)
      nl1=0
      DO J=1,NL(I,IQ)
      IF(NNFIX(J,I,IQ).GT.0)XXFIX(J,I,IQ)=XXFIX(J,I,IQ)/Nnfix(j,i,iq)
      IF(NNFIX(J,I,IQ).eq.1)nl1=nl1+1
      WRITE(IUN66,39)J,IDFIX(J,I,IQ),NNFIX(J,I,IQ),XXFIX(J,I,IQ)
      end do
      write(iun66,*)'no. of single observation subclasses =',nl1
      WRITE(IUN66,*)'  '
      end do
      end do

      do ii=1,nrand1
      WRITE(IUN66,'(a,i2,a,i8)')'Additional random effect no.',ii,
     &      ':  No. of levels =',nlrnd1(ii)

      do m=1,nlrnd1(ii)
      where(nnrnd1(m,ii,:nq)>0)xxrnd1(m,ii,:nq)=xxrnd1(m,ii,:nq)
     &                                             /nnrnd1(m,ii,:nq)
      WRITE(IUN66,391)m,IDRND1(m,ii),(nnrnd1(m,ii,l),xxrnd1(m,ii,l),
     &                                                       l=1,nq)
      END DO
      END DO

      RETURN
39    FORMAT(I8,I10,I8,G20.8)
 391  FORMAT(I8,I10,(t19,3(I6,G15.5)) )
      END subroutine dfsfix


      







