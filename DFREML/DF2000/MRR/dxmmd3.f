!===========================================================================
      SUBROUTINE  DxMMD3 (xvec,iopt,ip)
C===========================================================================

      use units
      use sparse
      use xsprse
      use combinations
      use levels
      use numbers
      use order
      use like_components
      use residuals
      use phimatrix
      use dmatrices
      use parameters, only : mxparm
      use parmap, only :kfoume
      use read_iun52

!     arguments
      integer, intent(in)                     :: ip,iopt
      real(8), dimension(mxparm), intent(in)  :: xvec

!     local variables
      real(8), dimension(:),allocatable       :: row
      real(8), dimension (:,:),allocatable    :: qhq,dd
      integer                                 :: nrrec=0
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
      nneff=maxval(neff(:nq))

      allocate (qhq(mqhq+lim1,mqhq+lim1),row(mqhq+lim1),dd(nneff,mobs),
     *  stat=ii)
      if(ii>0)stop 'dfmmd3 : alloc '

C     -------------------------------
C     DATA PART OF MIXED MODEL MATRIX
C     -------------------------------

C     INITIALIZE
      LIM0=1
      REWIND(IUN52)
      dia=0.d0
      xspars=0.d0
      dd=0.d0
      nrrec=0
      if( iopt.eq.0 .and. (nq>1.or.ieqmod.eq.1.or.kfoume>0) )dete=0.d0

500   READ(IUN52,end=598)ICOMB,NOBS,NR,NNA(:NOBS,:),nnq(:nobs),
     *  IICOL(:NR), ( (IEQ(K,L) ,K=1,NEFF(NNQ(L)) ),L=1,NOBS),
     *   ( (XCOV(K,L), K=kftfix(nnq(l))+1,NFR(NNQ(L)) ),L=1,NOBS),
     &    YVEC(:NOBS)
   
      NrREC=NRrEC+1

      qhq=0.d0
!     set up inverse residual cov matrix (or derivative)
      if(ncomb.le.max_comb .and. nq.eq.1.and. ieqmod.eq.0)then  
         if(iopt.eq.0)then
            sig(:nobs,:nobs)=eei(:nobs,:nobs,icomb)
         else if(iopt.eq.1)then
            do i=1,nobs
            do j=1,nobs
            sig(j,i)= rdr( ihmssf(i,j,nobs), ip, icomb )
            end do
            end do
         end if
      else if(nq.eq.1 .and. ieqmod.eq.0 .and.kfoume.eq.0)then
         call ee_inverse(xvec,icomb,iopt,ip)
      else if(nq.eq.1 .and. (ieqmod.eq.1.or.kfoume>0) )then
         call gg_inverse( xvec,iopt,ip,nobs,nna,nnq)
      else if(nq>1)then
         call ff_inverse( xvec,iopt,ip,nobs,nna,nnq)
      end if

      dd=0.d0
      do iobs=1,nobs
      iq=nnq(iobs)
      kk1a=nfix(iq)
      if(ieqmod.eq.0) kk1a=kk1a+kfit(4,iq)
      kk1=kk1a+kfit(5,iq)
      kk2=kk1+kfit(2,iq)
      dd(:nfix(iq),iobs)=1.d0
 
      if(kftfix(iq)>0)then
         iage=nna(iobs,irrmet(0,iq))
         xcov(:kftfix(iq),iobs)=phi(iage,:kftfix(iq),0,iq)!fix reg on orthpol
      end if
      if(kfit(4,iq)>0 .and. ieqmod.eq.0)then
         iage=nna(iobs,irrmet(4,iq))
         dd(nfix(iq)+1:kk1a,iobs)=phi(iage,:kfit(4,iq),4,iq) 
      end if
      if(kfit(5,iq)>0)then
         iage=nna(iobs,irrmet(5,iq))
         dd(kk1a+1:kk1,iobs)=phi(iage,:kfit(5,iq),5,iq) 
      end if
      if(kfit(2,iq)>0)then
         iage=nna(iobs,irrmet(2,iq))
         dd(kk1+1:kk2,iobs)=phi(iage,:kfit(2,iq),2,iq)
      end if
      if(nanim>0)then
         iage=nna(iobs,irrmet(1,iq))
         dd(kk2+1:kk2+kfit(1,iq),iobs)=phi(iage,:kfit(1,iq),1,iq)
      end if
      end do ! iobs

C     ACCUMULATE CONTRIBUTIONS TO MMM FOR ONE OBSERVATION AT A TIME ...
C     ... SET UP ROW OF X'R(-1) FOR THIS OBSERVATION
      DO  IOBS=1,NOBS
      IQ=NNQ(IOBS)
      II=NFRST(IQ)
      ROW=0.d0

      DO JOBS=1,NOBS
      EEIJ=sig(JOBS,IOBS)
      if(eeij.ne.0)then
         jq=nnq(jobs)
c        ... right hand side
         ROW(1)=ROW(1)+EEIJ*yvec(jobs)
c        ... covariables
         JJ=NFRST(JQ)
         do i=1,nfr(jq)
         jj=jj+1
         row(jj)=row(jj)+eeij*xcov(i,jobs)
         end do
c        ... fixed effects
         DO  J=1,NFIX(jq)
         JJ=IEQ(J,JOBS)
         ROW(JJ)=ROW(JJ)+EEIJ *DD(J,JOBS)
         END DO

c        ... perm. environmental effect due to the animal
         if(ioprn1.eq.1 .and. ieqmod.eq.0 )then
            DO m=nfix(jq)+1,nfix(jq)+kfit(4,jq)
            jj=ieq(m,jobs)
            ROW(jj)=ROW(JJ)+EEIJ*dd(m,jobs)
            end do
         end if

!        ... additional random effect
         if(ioprn3.eq.1)then
            mm1a=nfix(jq)
            if(ieqmod.eq.0)mm1a=mm1a+kfit(4,jq)
            DO m=mm1a+1,mm1a+kfit(5,jq)
            jj=ieq(m,jobs)
            ROW(jj)=ROW(JJ)+EEIJ*dd(m,jobs)
            end do
         end if

c        ... genetic effects for the animal
         if(nanim>0)then
            mm1=nfix(jq)+kfit(5,jq)
            if(ieqmod.eq.0)mm1=mm1+kfit(4,jq)
            mm2=mm1+kfit(2,jq)
            if(ioprn2.eq.1)then
               do m=mm1+1,mm2
               jj= IEQ(M,NOBS)
               ROW(jj)=ROW(JJ)+EEIJ*dd(m,jobs)
               end do
            end if
            DO m = mm2+1,mm2+kfit(1,jq)
            jj=ieq(m,jobs)
            ROW(jj)=ROW(JJ)+EEIJ*dd(m,jobs)
            end do
         end if
      end if
      end do ! loop over jobs

C     CONTRIBUTION TO MMM FOR THIS RECORD ...

C     ... WEIGHTED SS
      dia(nsrow)=dia(nsrow)+ROW(1)*yvec(iobs)

C     ... COV * COV (LOWER TRIANGLE ONLY)
      DO K=1,nfr(iq)
      XX=xcov(K,IOBS)
      IROW=NFRST(iq)+K
      irnew=ieqnew(irow)
      ij=isploc(irnew,nsrow)
      xspars(ij)=xspars(ij)+xx*row(1)  ! rhs x cov
      DO JROW=lim0+1,IROW-1
      ij=isploc(irnew,ieqnew(jrow))
      xspars(ij)=xspars(ij)+xx*row(jrow) ! cov x cov
      end do
      DIA( irnew ) = DIA( irnew ) + XX*ROW(IROW) 
      END DO ! end loop K

C     ... COV * FE/RE
      DO K=1,NEFF(IQ)
      JROW=IEQ(K,IOBS)-LIM1
      QHQ(:lim1+nr,JROW)=QHQ(:lim1+nr,JROW)+ROW(:lim1+nr)*dd(k,iobs)
      end do

      end do ! loop over iobs

C     ALL RECORDS FOR THE ANIMAL PROCESSED : PUT CONTRIBUTIONS FOR FIXED
C                                    AND RANDOM EFFECTS INTO RIGHT PLACE
      DO I=1,NR
      IROW=IICOL(I)
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
         IF((iicol(j)>0.and.IICOL(J)<IROW) .AND. (QHQ(J1,I).NE.0) )THEN
            kk=isploc(irow,iicol(j))
            xspars(kk)=xspars(kk)+qhq(j1,i)
         else IF(IICOL(J).eq.IROW .AND. QHQ(J1,I).NE.0 ) then
            dia(irow)=dia(irow)+qhq(j1,i)
         END IF
         END DO

      END IF
      END DO
      GO TO 500

598   yry=dia(nsrow)

      deallocate(row,qhq,dd,stat=ii)
      if(ii>0)stop 'dfmmd3 : de-alloc '

      RETURN
      END subroutine dxmmd3













