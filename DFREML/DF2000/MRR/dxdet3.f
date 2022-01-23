c===========================================================================
      SUBROUTINE  DXDET3 (parvec,xlike,detzhz)
C===========================================================================

      use parameters, only : mxparm
      use units
      use parmap
      use numbers
      use like_components

!     arguments
      real(8), dimension(mxparm), intent(in)  :: parvec
      real(8), intent(in)                     :: xlike
      real(8), intent(out)                    :: detzhz

!     local variables
      real(8), dimension(mxparm)              :: varvec
      real(8)                                 :: detcc,detxvx,zlike,
     &                                           fvalue=0.d0
      integer                                 :: iout,kkopt
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!     transform back to var comp scale
      VARVEC(:kparm)=PARVEC(:kparm)*FFVEC(:kparm)
      if(kopt.eq.3.or.kopt.eq.1)then
         call dxsca33(varvec)
      else if(kopt>0)then
         call dxsca3(kopt,varvec)
      end if

      kkopt=2
      iout=0
      detzhz=0.d0
      detcc=det

C     SET UP RANDOM EFFECTS  COVARIANCE MATRICES
      CALL DXRAN3 (VARVEC,FVALUE,IOUT,0)
      IF(IOUT.EQ.1.OR.dabs(FVALUE-BIG)<zz0) RETURN

C     SET UP RESIDUAL COVARIANCE MATRICES
      CALL DxRES3 (VARVEC,FVALUE,IOUT,0)
      IF(IOUT.EQ.1.OR.dabs(FVALUE-BIG)<zz0) RETURN

C     SET UP DATA PART OF MMM - random effects only
      CALL Dxzhz3(VARVEC)

C     ADD INVERSE COVARIANCE MATRIX OF RANDOM EFFECTS
      CALL DFMMP3(DETL)

C     CARRY OUT CHOLESKY FACTORISATION
      CALL DFSPA3 (kkopt)

!     ML log likelihood 
      detzhz=det
      detxvx=detcc-detzhz
      zlike=xlike+0.5d0*detxvx

      WRITE(IUN66,906)'LOG DETERMINANT OF Z''R*(-1)Z+G**(-1) ',DETZHZ
      WRITE(IUN66,906)'LOG DETERMINANT OF X''V*(-1)X ',DETXVX
      WRITE(IUN66,906)'FULL (ML) LOG LIKELIHOOD ',ZLIKE
      WRITE(IUN66,*)' '

906   FORMAT(1X,A,T50,'=',G20.12)
      return
      END subroutine dxdet3

C===========================================================================
      SUBROUTINE  DxZHZ3 (xvec)
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
      use read_iun52
      use units

!     arguments
      real(8), dimension(mxparm), intent(in)  :: xvec

!     local variables
      real(8)                                 :: qq
      real(8), dimension(:),allocatable       :: row
      real(8), dimension (:,:),allocatable    :: qhq,dd
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      
!     set up random effects part of data part of MMM only

      nneff=maxval(neff)
      allocate (qhq(mqhq,mqhq),row(mqhq),dd(nneff,mobs),stat=ii)
      if(ii>0)stop 'dfzhz3 : alloc '

C     INITIALIZE
      REWIND(IUN52)
      dia=0.d0
      xspars=0.d0
      dd=0.d0

500   READ(IUN52,end=598)ICOMB,NOBS,NR,NNA(:NOBS,:),NNQ(:NOBS),
     *   IICOL(:NR),( (IEQ(K,L) ,K=1,NEFF(NNQ(L)) ),L=1,NOBS),
     *   ( (XCOV(K,L), K=kftfix(nnq(l))+1,NFR(NNQ(L)) ),L=1,NOBS),
     &    YVEC(:NOBS)

      qhq=0.d0
      if(ncomb.le.max_comb)then  
         sig(:nobs,:nobs)=eei(:nobs,:nobs,icomb)
      else 
         call ee_inverse(xvec,icomb,0,0)
      end if

      do iobs=1,nobs
      iq=nnq(iobs)
      kk1=nfix(iq)+kfit(4,iq)+kfit(5,iq)
      kk2=kk1+kfit(2,iq)

!     perm. env. effect due to animal
      if(ieqmod<1 .and. kfit(4,iq)>0)then
         iage=nna(iobs,irrmet(4,iq))
         dd(nfix(iq)+1:nfix(iq)+kfit(4,iq),iobs)=phi(iage,:kfit(4,iq),
     &                                                           4,iq) 
      end if
!     add. random effect
      if(kfit(5,iq)>0)then
         iage=nna(iobs,irrmet(5,iq))
         if(ieqmod.eq.0)then
            kk1a=nfix(iq)+kfit(4,iq)
            dd(kk1a+1:kk1a+kfit(5,iq),iobs)=phi(iage,:kfit(5,iq),5,iq) 
         else
            kk1a=nfix(iq)
            dd(kk1a+1:kk1a+kfit(5,iq),iobs)=phi(iage,:kfit(5,iq),5,iq) 
         end if
      end if
!     mat. genetic effect
      if(kfit(2,iq)>0)then
         iage=nna(iobs,irrmet(2,iq))
         dd(kk1+1:kk1+kfit(2,iq),iobs)=phi(iage,:kfit(2,iq),2,iq)
      end if
!     animal (additive genetic) effect
      iage=nna(iobs,irrmet(1,iq))
      dd(kk2+1:kk2+kfit(1,iq),iobs)=phi(iage,:kfit(1,iq),1,iq)
      end do ! loop for iobs

      DO  IOBS=1,NOBS
      ROW=0.d0

      DO JOBS=1,NOBS
      EEIJ=sig(JOBS,IOBS)
      if(eeij.ne.0)then
         jq=nnq(jobs)
c        ... right hand side
         ROW(1)=ROW(1)+EEIJ*yvec(jobs)
c        ... perm. environmental effect due to the animal
         if(ioprn1.eq.1 .and.ieqmod.eq.0 )then
            DO m=nfix(jq)+1,nfix(jq)+kfit(4,jq)
            jj=ieq(m,jobs)
            ROW(jj)=ROW(JJ)+EEIJ*dd(m,jobs)
            end do
         end if
c        ... additional random effect
         if(ioprn3.eq.1)then
            mm1a=nfix(jq)+kfit(4,jq)
            DO m=mm1a+1,mm1a+kfit(5,jq)
            jj=ieq(m,jobs)
            ROW(jj)=ROW(JJ)+EEIJ*dd(m,jobs)
            end do
         end if
c        ... genetic effects for the animal
         mm1=nfix(jq)+kfit(4,jq)+kfit(5,jq)
         mm2=mm1+kfit(2,jq)
         if(ioprn2.eq.1)then
            do m=mm1+1,mm2
            jj=IEQ(M,NOBS)
            ROW(jj)=ROW(JJ)+EEIJ*dd(m,jobs)
            end do
         end if
         DO m =mm2+1,mm2+kfit(1,jq)
         jj=ieq(m,jobs)
         ROW(jj)=ROW(JJ)+EEIJ*dd(m,jobs)
         end do
      end if
      end do ! jobs

      dia(nsrow)=dia(nsrow)+ROW(1)*yvec(iobs)
      DO  K=nfix(iq)+1,neff(iq)
      JROW=IEQ(K,IOBS)-LIM1
      QHQ(:lim1+nr,JROW)=QHQ(:lim1+nr,JROW)+ROW(:lim1+nr)*dd(k,iobs)
      end do
      end do ! loop over iobs

      DO I=1,NR
      IROW=IICOL(I)
      IF(IROW.GT.0)THEN
         kk=isploc(irow,nsrow)
         xspars(kk)=xspars(kk)+qhq(1,i)
         DO J=1,NR
         qq=qhq(lim1+j,i)
         if(abs(qq)<zero)cycle
         IF((iicol(j)>0.and.IICOL(J)<IROW) )THEN
            kk=isploc(irow,iicol(j))
            xspars(kk)=xspars(kk)+qq
         else IF(IICOL(J).eq.IROW) then
            dia(irow)=dia(irow)+qq
         END IF
         END DO
      END IF
      END DO
      GO TO 500

598   yry=dia(nsrow)

      deallocate(row,qhq,dd,stat=ii)
      if(ii>0)stop 'dfzhz3 : de-alloc '

      RETURN
      END subroutine dxzhz3












