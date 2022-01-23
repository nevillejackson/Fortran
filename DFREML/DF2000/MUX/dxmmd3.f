C===========================================================================
      SUBROUTINE  DxMMD3 (ipar)
C===========================================================================

      use params
      use units
      use sparse
      use xsprse
      use combinations
      use levels
      use numbers
      use order
      use dmatrices, only : rdr
      use like_components
      use residuals

!     arguments
      integer, intent(in)                     :: ipar

!     local variables
      real(8), dimension(:), allocatable      :: row
      integer, dimension(:), allocatable      :: icol
      real(8), dimension (:,:), allocatable   :: xcov,qhq
      real(8), dimension (:), allocatable     :: yvec
      integer, dimension (:), allocatable     :: nnq
      integer, dimension (:,:), allocatable   :: ieq
      integer                                 :: lim0=1,nr, limnr1,icomb
     &,                                          nobs,iq,iobs,jobs,ii,jq
     &,                                          jj,irnew,irow,k,l
      real(8)                                 :: eeij,xx
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      allocate(row(neqns),icol(mqhq),stat=ii)
      if(ii>0)stop 'dxmmd3 : alloc'

      allocate (xcov(mnfr,mobs),yvec(mobs),qhq(lim1+mqhq,lim1+mqhq),
     *          nnq(mobs),ieq(mfix+3,mobs),stat=ii)
      if(ii>0)stop 'dxmmd3 : alloc 2'

C     -------------------------------
C     DATA PART OF MIXED MODEL MATRIX
C     -------------------------------

C     INITIALIZE
      dia=0.d0
      xspars=0.d0
      nnrec=0
      REWIND(IUN52)

500   READ(IUN52,END=598)ICOMB,NOBS,NR,NNQ(:NOBS),
     *     ICOL(:NR),( (IEQ(K,L),K=1,NEFF(NNQ(L)) ),L=1,NOBS),
     *    ( (XCOV(K,L),K=1,NFR(NNQ(L)) ),L=1,NOBS),YVEC(:NOBS)

      NNREC=NNREC+1
      LIMNR1=LIM1+NR
      qhq=0.d0

C     ACCUMULATE CONTRIBUTIONS TO MMM FOR ONE OBSERVATION AT A TIME ...
C     ... SET UP ROW OF X'R(-1) FOR THIS OBSERVATION
      DO IOBS=1,NOBS
      IQ=NNQ(IOBS)
      II=NFRST(IQ)
      ROW=0.d0

      DO JOBS=1,NOBS

      if(ipar.eq.0)then
         EEIJ=EEI(JOBS,IOBS,ICOMB)
      else
         eeij=-rdr(ihmssf(iobs,jobs,nobs),ipar,icomb)
      end if

      if(dabs(eeij)>zz0)then
         JQ=NNQ(JOBS)
c        ... right hand side
         ROW(1)=ROW(1)+EEIJ*yvec(jobs)
c        ... covariables
         JJ=NFRST(JQ)
         ROW(jj+1:jj+nfr(jq))=ROW(JJ+1:jj+nfr(jq))+EEIJ*
     &                               xcov(:nfr(jq),jobs)
c        ... fixed & random effects
         DO  J=1,NEFF(JQ)
         ROW(ieq(j,jobs))=ROW(ieq(j,jobs))+EEIJ
         end do
      end if
      end do ! jobs

C     CONTRIBUTION TO MMM FOR THIS RECORD ...

C     ... WEIGHTED SS
      dia(nsrow)=dia(nsrow)+ROW(1)*yvec(iobs)

C     ... COV * COV (LOWER TRIANGLE ONLY)
      DO K=1,NFR(IQ)
      XX=XCOV(K,IOBS)
      IROW=II+K
      irnew=ieqnew(irow)
      ij=isploc(irnew,nsrow)
      xspars(ij)=xspars(ij)+xx*row(1)
      DO JROW=2,IROW-1
      ij=isploc(irnew,ieqnew(jrow))
      xspars(ij)=xspars(ij)+xx*row(jrow)
      end do
      DIA( Irnew ) = DIA( Irnew ) + XX*ROW(IROW)
      END DO

C     ... COV * FE/RE
      DO K=1,NEFF(IQ)
      JROW=IEQ(K,IOBS)-LIM1
      QHQ(:limnr1,JROW)=QHQ(:limnr1,JROW)+ROW(:limnr1)
      end do

      end do ! iobs

C     ALL RECORDS FOR THE ANIMAL PROCESSED : PUT CONTRIBUTIONS FOR FIXED
C                                    AND RANDOM EFFECTS INTO RIGHT PLACE
      DO I=1,NR
      IROW=ICOL(I)
      IF(IROW.GT.0)THEN
C        ... RHS 
         kk=isploc(irow,nsrow)
         xspars(kk)=xspars(kk)+qhq(1,i)
C        ... REGRESSION COEFFICIENTS
         DO J=LIM0+1,LIM1
         IF(QHQ(J,I).NE.0)THEN
             kk=isploc(irow,ieqnew(j))
             xspars(kk)=xspars(kk)+qhq(j,i)
         END IF
         END DO
c        ... FE/RE codes, lower triangle only
         DO J=1,NR
         j1=lim1+j
         IF((icol(j).gt.0.and.ICOL(J).Lt.IROW) .AND. (QHQ(J1,I).NE.0) )
     *                                                            THEN
            kk=isploc(irow,icol(j))
            xspars(kk)=xspars(kk)+qhq(j1,i)
         else IF(ICOL(J).eq.IROW .AND. QHQ(J1,I).NE.0 ) then
            dia(irow)=dia(irow)+qhq(j1,i)
         END IF
         END DO
      END IF
      END DO
      GO TO 500

598   if(ipar.eq.0)yry=dia(nsrow)

      deallocate(row,icol,xcov,yvec,qhq,nnq,ieq,stat=ii)
      if(ii>0)stop 'dxmmd3 : de-alloc '

      RETURN
      END subroutine dxmmd3









