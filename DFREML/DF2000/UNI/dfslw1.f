!========================================================================
      SUBROUTINE DFSLW1 (iopslv)
!========================================================================

      use names
      use spasol
      use order
      use rows
      use solve
      use means
      use levels
      use constants
      use units
      use numbers
      use platform

      integer, intent(in)                    :: iopslv
      integer, dimension(8)                  :: nnped
      integer, dimension(:), allocatable     :: kfix,nlrnd1,idvec
      integer, dimension(:,:), allocatable   :: idrnd1,nl
      integer, dimension(:,:,:), allocatable :: idfix
      real(8), dimension(:), allocatable     :: uvec
      real(8), dimension(:), allocatable     :: rhsfix
      integer                                :: kanim,i,j,k,ii,jj,kk,
     &                                          iopibr

      allocate(rhsfix(neqns),nl(nfix2,nq),kfix(nq),stat=ii)
      if(ii>0)stop 'alloc soln'

C     INPUT FROM UNIT "11"
      call read_iun11

C     ------------------------------
C     WRITE OUT RESULTS TO UNIT "66"
C     ------------------------------

      WRITE(IUN66,*)'------------------------------------------'
      WRITE(IUN66,*)'SOLUTION FOR FIXED EFFECTS AND COVARIABLES'
      WRITE(IUN66,*)'------------------------------------------'

c     read no.s, class means & lsq solutions
      read(iun13)lim2
      read(iun13)nnvec(:lim2)
      do iq=1,nq
      read(iun13)row(:lim2)
      if(iq.eq.ntrait)rhsfix(:lim2)=row(:lim2)
      end do
      do iq=1,ntrait
      read(iun13)row(:lim2)
      end do

      K=nq
      DO  I=1,ncov
      WRITE(IUN66,909)'COVARIABLE NO.',I,COVAR(I)
      WRITE(IUN66,913)
      do ii=1,nlnest(i)
      DO J=1,NPOW(I)
      K=K+1
      KK=IEQNEW(K)
      WRITE(IUN66,911)K,'level/ORDER',ii,J,SAVSOL(KK),row(k)
      end do
      end do
      end do

      DO I=1,NFIX
      WRITE(IUN66,909)'FIXED EFFECT NO.',I,FIXED(I)
      WRITE(IUN66,913)
      DO J=1,NLEV(I)
      K=K+1
      KK=IEQNEW(K)
      WRITE(IUN66,912)K,'LEVEL',J,IDFIX(J,I,1),NNVEC(k),rhsfix(k),
     &                                              SAVSOL(KK),row(k)
      end do
      end do
      IF(IOPSLV.EQ.1)RETURN

C     COUNT NO. OF RECORDS & ACCUMMULATE RAW TOTALS
      nnvec=0
      rhsfix=0.d0
      REWIND(IUN52)
150   READ(IUN52,END=199)KABS,IVEC(LIM1a+1:KABS),ROW(:LIM1b)
      DO IABS=LIM1b+1,KABS
      IROW=IVEC(IABS)
      RHSFIX(IROW)=RHSFIX(IROW)+ROW(NTRAIT)
      NNVEC(IROW)=NNVEC(IROW)+1
      end do
      GO TO 150

 199  IF(IOPRN1>0)THEN
         WRITE(IUN66,*)'--------------------------------------'
         WRITE(IUN66,*)'SOLUTION FOR ADDITIONAL RANDOM EFFECTS'
         WRITE(IUN66,*)'--------------------------------------'
         WRITE(IUN66,*)' '
         WRITE(IUN66,914)
         do kk=1,krand1
         IF(nlev(nfix+kk)>0)then
            write(iun66,909)'ADD. RE No.',kk,fixed(nfix+kk)
            DO I=1,nlrnd1(kk)
            k=k+1
            II=IEQNEW(k)
            IF(nnvec(ii)>0)RHSFIX(II)=YBAR(NTRAIT)+RHSFIX(II)/nnvec(ii)
            WRITE(IUN66,912)K,FIXED(NFIX1),I,IDRND1(I,kk),NNVEC(II)
     *,                                           rhsfix(ii),SAVSOL(II)
            end do
         END if
         end do
      END IF

      WRITE(IUN66,*)'-----------------------------'
      WRITE(IUN66,*)'SOLUTION FOR ANIMAL EFFECT(S)'
      WRITE(IUN66,*)'-----------------------------'
      WRITE(IUN66,*)' '
      WRITE(IUN66,914)
      DO I=1,NANIM
      k=k+1
      JJ=IEQNEW(k)
      IF(NNVEC(JJ).GT.0)RHSFIX(JJ)=YBAR(NTRAIT)+RHSFIX(JJ)/NNVEC(JJ)
      WRITE(IUN66,912)k,'ADD.GENET.',I,IDVEC(I),NNVEC(JJ),rhsfix(jj),
     &                                                    SAVSOL(JJ)
      IF(IOPRN2.EQ.1.and.nlev(nfix2)>0)THEN
         k=k+1
         JJ=IEQNEW(k)
         IF(NNVEC(JJ)>ZERO)rhsfix(jj)=YBAR(NTRAIT)+RHSFIX(JJ)/NNVEC(JJ)
         WRITE(IUN66,912)k,FIXED(NFIX2),I,IDVEC(I),NNVEC(JJ),
     *                                            rhsfix(jj),SAVSOL(JJ)
      END IF
      end do

C     CALCULATE MEANS WITHIN SPECIFIED SUBCLASSES
      IF(IOPIBR.EQ.1)CALL DFGBAR
      RETURN

913   FORMAT(/' EQ.NO.',T31,'ORIG.ID.',T40,2X,'NREC',6X,'MEAN'
     *,                          6X,'GLS-SOLUTION',6X,'LSQ-SOLUTION')
914   FORMAT(/' EQ.NO.',T31,'ORIG.ID.',T40,2X,'NREC',6X,'MEAN'
     *,                          6X,'GLS-SOLUTION')
911   FORMAT(I5,2X,A,T21,2I6,    T40,16X,         3G18.10)
912   FORMAT(I5,2X,A,T21,I6,I12,T40, I6 ,F10.4, 4F18.8,i6)
909   FORMAT(/1X,A,I4,4X,A12)

      contains

!     =====================
      subroutine read_iun11
!     =====================

      rewind(iun11)
      read(iun11)fped,fdata,cwdir
      read(iun11)nnped
      read(iun11)iopt,llq
      if(iopt.ne.1)stop 'mismatch with "dfprep" !'
      if(llq.gt.nq)stop 'dfslw1 : no. of traits'
      read(iun11)(ii, i=1,llq)
      kfix=0
      read(iun11)kfix(:llq)
      read(iun11)(ii, i=1,llq)
      read(iun11)(ii, i=1,llq)
      read(iun11)(ii, i=1,llq)
      nl=0
      read(iun11)((nl(l,k),l=1,kfix(k)), k=1,llq)
      m=maxval(nl)
      l=maxval(kfix)
      allocate(idfix(m,l,llq),stat=ii)
      if(ii>0)stop 'alloc idfix'
      READ(IUN11)(((IDFIX(M,L,K),M=1,NL(L,K)), L=1,KFIX(K)), K=1,LLQ)
      READ(IUN11)((( ii ,M=1,NL(L,K)), L=1,KFIX(K)), K=1,LLQ)
      read(iun11)krand1
      IF(krand1>0)THEN
         allocate(nlrnd1(krand1),stat=ii)
         read(iun11)nlrnd1
         mm=maxval(nlrnd1)
         allocate(idrnd1(mm,krand1),stat=ii)
         if(ii>0)stop 'read_iun11 : alloc 2'
         read(iun11)((idrnd1(m,l),m=1,nlrnd1(l)),l=1,krand1)
      END IF

      READ(IUN11)KANIM,ii,IOPIBR,nainv,detll
      allocate(uvec(kanim),idvec(kanim),stat=ii)
      if(ii>0)stop 'alloc uvec'
      read(iun11)idvec
      read(iun11)uvec
      return
      end subroutine read_iun11

C     ==================
      SUBROUTINE  DFGBAR 
C     ==================

      use works

      real(8), dimension(:), allocatable :: gfbar,gabar,gpbar,gsol
      integer, dimension(:), allocatable :: ng,ngp
      CHARACTER(len=12)                  :: GNAME

      IF(KANIM.NE.NANIM)STOP 'DFGBAR'

C     READ GROUP CODES AS WRITTEN OUT BY "DFPREP"
      read(iun11)iwork(:nanim)
      READ(IUN11)LLEV,IGFIX,GNAME

C     ALIGN GROUP CODE WITH FIXED EFFECT IN MODEL OF ANALYSIS
      IGADD=0
      IF(IGFIX.EQ.0)THEN
         WRITE(*,*)' '
         WRITE(*,*)'READ FIXED EFFECT CODE FOR ',GNAME,' FROM ',
     *             'PEDIGREE FILE - BUT :'
         WRITE(*,*)'DOES IT CORRESPOND TO AN EFFECT IN THE MODEL',
     *             ' OF ANALYSIS ?'
         CALL YNDEF(MM,1)
         IF(MM.EQ.1)THEN
            WRITE(*,fmt(ipltf) )'GIVE RUNNING NO. OF FIXED EFFECT '
            CALL OPTION( IGFIX,1,KFIX(1) )
            WRITE(*,*)'ALIGN : LEVEL "1" IN DATA FILE IS EQUAL TO   '
            WRITE(*,*)'        *WHICH* LEVEL CODE ON PEDIGREE FILE ?'
            CALL OPTDEF(IC,0,NL(IGFIX,1),1)
            IGADD=1-IC
         END IF
      END IF

C     INITIALIZE MEANS ARRAYS
      allocate(gfbar(llev),gabar(llev),gpbar(llev),gsol(llev),
     &         ng(0:llev),stat=ii)
      if(ii>0)stop 'alloc gbar'
      iq=1
      NG=0
      NGP=0
      GFBAR=0
      GPBAR=0.D0
      GABAR=0.D0

c     pick out fixed effect solutions
      IF(IGFIX.GT.0)THEN
         K=LIM1
         DO I=1,KFIX(IQ)
         IF(I.EQ.IGFIX)THEN
            DO J=1,NL(I,iQ)
            K=K+1
            GSOL(J)=Savsol( IEQNEW(K) )
            end do
         ELSE
            K=K+NL(I,iQ)
         END IF
         end do
      END IF

C     CALCULATE MEAN INBREEDING, BREEDING VALUE & PHENOTYPE
      J=LIM3
      DO I=1,NANIM
      IGEN=IWORK(I)
      NG(IGEN)=NG(IGEN)+1
      GFBAR(IGEN)=GFBAR(IGEN)+UVEC(I)
      J=J+1
      JJ=IEQNEW(J)
      GABAR(IGEN)=GABAR(IGEN)+Savsol(JJ)
      IF(NNVEC(JJ).GT.0)THEN
         NGP(IGEN)=NGP(IGEN)+1
         GPBAR(IGEN)=GPBAR(IGEN)+RHSFIX(JJ)
      END IF
      IF(IOPRN2.EQ.1)J=J+1
      end do

      WRITE(IUN66,*)' '
      WRITE(IUN66,*)'----------------------------------------------'
      WRITE(IUN66,*)'MEANS FOR ANIMALS FOR LEVELS OF : ',GNAME
      WRITE(IUN66,*)'----------------------------------------------'
      WRITE(IUN66,*)' '
      WRITE(IUN66,2090)

C     MEAN INBREEDING COEFFICIENT
      DO L=0,LLEV
      IF(NG(L).GT.0)GFBAR(L)=GFBAR(L)/DBLE(NG(L))
      end do

C     MEAN BREEDING VALUE & PHENOTYPE
      DO  L=0,LLEV
      IF(NG(L).eq.0)cycle
      A=GABAR(L)/DBLE(NG(L))
      P=0.D0
      IF(NGP(L).GT.0)P=GPBAR(L)/DBLE(NGP(L))
      IF(IGFIX.GT.0 .AND. L+IGADD.GT.0)THEN
C      ... ADD FIXED EFFECT SOLUTION IF IN MODEL OF ANALYSIS
         G=GSOL(L+IGADD)
         GA=G+A
         WRITE(IUN66,2009)L,NGP(L),P,NG(L),GFBAR(L),A,G,GA
         WRITE(*,2009)L,NGP(L),P,NG(L),GFBAR(L),A,G,GA
      ELSE
         WRITE(IUN66,2009)L,NGP(L),P,NG(L),GFBAR(L),A
         WRITE(*,2009)L,NGP(L),P,NG(L),GFBAR(L),A
      END IF
      end do

2003  FORMAT(' TRAIT NO. ',I6,' :  ',A)
2009  FORMAT(I5,I8,F12.4,I8,F8.3,3F16.10)
2090  FORMAT(5X,'NO.REC.S',4X,'MEAN',4X,'NO.ANIM.',5X,'1+F',7X,
     *          'MEAN B.V.',5X,'EFFECT SOLN.',5X,'EFFECT+B.V'/)
      RETURN
      END subroutine dfgbar

      END subroutine dfslw1










