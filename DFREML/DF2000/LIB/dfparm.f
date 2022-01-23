C========================================================================
      SUBROUTINE DFPARM(jparam,NPARM,NQ,NQQ,PARVEC,SAVE,ZERO,BIG,FVALUE,
     *                  param,parsav)
C========================================================================

C     PURPOSE : ROUTINE TO transform vector of parameters for multi-
c               variate analyses to variance/covariance matrices,
c               when a different parameterisation has been chosen

c               currently implemented for model 1 only !

c     jparam=2  TRANSFORM VECTOR OF HERITABILTIES, GENETIC &
C               PHENOTYPIC CORRELATIONS AND PHENOTYPIC VARIANCES TO
C               VECTOR OF ADDITIVE GENETIC & ERROR (CO)VARIANCES

c     jparam=3  parameterisation is to cholesky decompositions of
c               original covariance matrices

c     jparam=4  parameterisation is to elements of the canonical
c               decomposition of additive genetic & error covariances

C-----------------------------------------------------------KM--8/90----

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      parameter( maxnq =8 )
      character*12 parsav(nparm),param(nparm),para
      DIMENSION PARVEC(NPARM),SAVE(NPARM),SMAT(maxnq,maxnq)

c     save original parameter vector
      FVALUE=BIG
      DO 1111 I=1,NPARM
      parsav(i)=param(i)
1111  SAVE(I)=PARVEC(I)
729   FORMAT(A8,2I2)

c     rewrite vector param to cov comp.s for use in dfcovm etc.
      ij=0
      do i=1,nq
      do j=i,nq
      ij=ij+1
      WRITE(PARA,729)'SIG A   ',I,J
      param(ij)=para
      WRITE(PARA,729)'SIG E   ',I,J
      param(nqq+ij)=para
      end do
      end do

c     convert genetic parameters to covariances
      IF(JPARAM.EQ.2)THEN
c        ... calculate additive genetic variances
         DO I=1,NQ
         II=IHMII(I,NQ)
         IF(SAVE(II).LT.ZERO .or. SAVE(NQQ+II).LT.ZERO)RETURN
         PARVEC(II)=SAVE(II)*SAVE(NQQ+II)
         END DO
c        ... calculate covariances
         DO I=1,NQ
         II=IHMII(I,NQ)
         DO J=I+1,NQ
         JJ=IHMII(J,NQ)
         IJ=IHMIJ(I,J,NQ)
         PARVEC(IJ)=SAVE(IJ)*DSQRT( PARVEC(II)*PARVEC(JJ) )
         PARVEC(NQQ+IJ)=SAVE(NQQ+IJ)*DSQRT( PARVEC(NQQ+II)*
     *                                      PARVEC(NQQ+JJ) )
         END DO
         END DO
c        ... calculate error components
         DO IJ=1,NQQ
         PARVEC(nqq+IJ)=PARVEC(NQQ+IJ)-PARVEC(IJ)
         END DO

c     parameters are choleky decomposition of covariance matrices
      else if(jparam.eq.3)then
         ij=0
         do 1 i=1,nq
         do 1 j=i,nq
         ij=ij+1
         p1=0.d0
         p2=0.d0
         do 2 k=1,i
         ik=ihmssf(i,k,nq)
         jk=ihmssf(j,k,nq)
         p1=p1+save(ik)*save(jk)
2        p2=p2+save(nqq+ik)*save(nqq+jk)
         parvec(ij)=p1
1        parvec(nqq+ij)=p2

c     parameterisation to canonical transformation
      else if(jparam.eq.4)then
         if(nq.gt.maxnq)stop 'dfparm'
         ij=nq
         do 3 i=1,nq
         do 3 j=1,nq
         ij=ij+1
3        smat(j,i)=save(ij)
         ij=0
         do 4 i=1,nq
         do 4 j=i,nq
         ij=ij+1
         p1=0.d0
         p2=0.d0
         do 5 k=1,nq
         p1=p1+smat(i,k)*save(k)*smat(j,k)
5        p2=p2+smat(i,k)*smat(j,k)
         parvec(ij)=p1
4        parvec(nqq+ij)=p2-p1
      end if
      FVALUE=0.D0
      RETURN
      END


