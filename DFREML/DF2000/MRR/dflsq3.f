!===========================================================================
      SUBROUTINE DFLSQ3 (lim0)
!===========================================================================

      use units
      use means
      use levels
      use numbers
      use order
      use zero_rows
      use phimatrix
      use ages
      use read_iun52
      use legendre
      use like_components, only : nrnkx
      use params, only : mxxzhz, maxxhx

!     arguments
      integer, intent(inout)                 :: lim0

!     local variables
      integer, dimension(:),  allocatable    :: iflag,ivec,inext,ivcol,
     *                                          ifirst,nnvec,iqrow,krank
      integer, dimension(:,:),  allocatable  :: nnaa
      real(8), dimension(:),  allocatable    :: dia,xvec,rrow,zhz,row
      real(8), dimension(:,:),allocatable    :: xhx,qhq
      real(8), dimension(:),  allocatable    :: yres,yres0,ss,ssr,ssr0,
     &                                          zz,zzr,xmse
      real(8), dimension(:,:),  allocatable  :: y1,y2,yy1,yy2,y3,y4,
     &                                          y20,yy20
      real(8)                                :: dd
      integer                                :: maxrec,ii,nobs,
     &                                          nr,i,j,k,ndf
      integer                                :: iscale=0 ! set =1 if obs have
                                                         ! been scaled by SD
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      maxzhz=lim2*15*kfitmx                    ! suggested default value
      write(*,*)'give value for "MAXZHZ" '
      call optdef(mzhz,1,mxxzhz,maxzhz)

      LIMXHX=min0(maxxhx,lim2)
      LL=MIN0(LIMXHX,LIM2)
      nnfr=nfr(1)
      mqhq=lim1+mqhq

c     declare space ...
      allocate(iflag(lim2),dia(lim2),xhx(limxhx,limxhx),
     & ivec(lim2),nnvec(lim2),xvec(lim2),row(lim2+mqhq),rrow(lim2),
     & ifirst(lim2),inext(mzhz),ivcol(mzhz),zhz(mzhz),iqrow(lim2),
     & krank(nq),stat=ii)
      if(ii>0)stop 'dflsq3 : allocate'

      allocate (qhq(mqhq,mqhq),y1(mage,nq),y2(mage,nq),yy1(mage,nq),
     &          yy2(mage,nq),yres(mobs),yres0(mobs),xmse(nq),
     &          y3(mage,nq),y4(mage,nq),nnaa(mage,nq),
     &          y20(mage,nq),yy20(mage,nq),ss(nq),ssr(nq),ssr0(nq),
     &          zz(nq),zzr(nq),stat=ii)
      if(ii>0)stop 'dflsq3 : alloc 2'

!     set up phi-matrix
      mftfix=maxval(kftfix)
      if(mftfix>0)then
        allocate(astar(mage),phi(mage,mftfix,0:5,nq),stat=ii)
        if(ii>0)stop 'alloc : phimatrix'
        phi=0.d0
        call all_legendre (mftfix)
        do iq=1,nq
        call phimat(nage(irrmet(0,iq)),kftfix(iq),0,iq)
        end do
      end if
         
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
      NRNKX=0
500   READ(IUN52,END=598)icomb,NOBS,NR,NNA(:NOBS,:),NNQ(:Nobs),
     *   IICOL(:NR),( (IEQ(K,L) ,K=1,NEFF(NNQ(L)) ),L=1,NOBS),
     *   ( (XCOV(K,L), K=kftfix(nnq(l))+1,NFR(NNQ(L)) ),L=1,NOBS),
     &    YVEC(:NOBS)

      if(mftfix>0)then  ! fixed reg on orthog polynomial
         do iobs=1,nobs
         iq=nnq(iobs)
         xcov(:kftfix(iq),iobs)=phi(nna(iobs,irrmet(0,iq)),
     &                                         :kftfix(iq),0,iq)
         end do
      end if

C     ACCUMULATE CONTRIBUTIONS TO MMM FOR ONE OBSERVATION AT A TIME ...
C     ... SET UP ROW OF X'R(-1) FOR THIS OBSERVATION
      QHQ=0.D0
      DO IOBS=1,NOBS
      iq=nnq(iobs)
      row=0.d0
      ROW(1)=YVEC(IOBS)   ! right hand side
      ROW(nfrst(iq)+1:nfrst(iq)+nfr(iq))=XCOV(:nfr(iq),iOBS)
      DO  J=1,NFIX(iq)
      ROW(IEQ(J,iOBS))=1.D0
      end do

C     CONTRIBUTION TO MMM FOR THIS RECORD ...
C     ... COV * COV (LOWER TRIANGLE ONLY)
      DO  K=1,NFR(iq)
      IROW=nfrst(iq)+K
      XHX(:irow,IROW)=XHX(:irow,IROW)+XCOV(K,IOBS)*ROW(:irow)
      END DO

C     ... COV * FE/RE
      DO K=1,NFIX(IQ)
      JROW=IEQ(K,IOBS)-LIM1
      QHQ(:lim1+nr,JROW)=QHQ(:lim1+nr,JROW)+ROW(:lim1+nr)
      end do
      end do ! loop for iobs

      DO I=1,NR
      IROW=IICOL(I)
      IF(IROW.LE.LIMXHX)THEN
         XHX(:lim1,IROW)=XHX(:lim1,IROW)+QHQ(:lim1,I)
         DO J=1,NR
         IF(IICOL(J).LE.IROW)XHX(IICOL(J),IROW)=XHX(IICOL(J),IROW)
     *                                              +QHQ(LIM1+J,I)
         END DO
      ELSE IF(IROW>0 .and. irow.le.lim2)THEN
         NN=1               ! rhs
         IVEC(NN)=1
         XVEC(NN)=QHQ(1,I)
         DO J=LIM0+1,LIM1   ! regression coeffs
         IF(QHQ(J,I).NE.0)THEN
            NN=NN+1
            IVEC(NN)= J
            XVEC(NN)=QHQ(J,I)
         END IF
         END DO
         DO  J=1,NR         ! fe/re codes, lower triangle only
         IF((IICOL(J).LE.IROW) .AND. (QHQ(LIM1+J,I).NE.0) )THEN
            NN=NN+1
            IVEC(NN)=IICOL(J)
            XVEC(NN)=QHQ(LIM1+J,I)
         END IF
         end do
         CALL LNKROW(IROW,NN-1)
         DIA(IROW)=DIA(IROW)+XVEC(NN)
      END IF
      end do ! i=1,nr
      GO TO 500

C     STORE NO.S OF OBSERVATIONS & CELL MEANS
598   dia(lim0+1:limxhx) = (/ (xhx(i,i),i=lim0+1,limxhx) /)
      NNVEC(lim1+1:lim2)=DIA(lim1+1:lim2)
      ROW=0.D0
 
      ROW(LIM1+1:MIN0(LIMXHX,LIM2))=XHX(NTRAIT,LIM1+1:MIN0(LIMXHX,LIM2)) 
      DO IROW=LIMXHX+1,LIM2
      IJ=IFIRST(IROW)
      do while(ij>0 .and.ivcol(ij)<ntrait)
         ij=inext(ij)
      end do
      if(ij>0.and.ivcol(ij).eq.ntrait)row(irow)=zhz(ij)
      end do  ! irow

C     ACCOUNT FOR DEPENDENCIES AMONG FIXED EFFECTS
      IF(NRZERO.GT.0)CALL DFZER3

C     --------------------------------------------------
C     CHECK FOR RANK DEFICIENCIES & OBTAIN LSQ SOLUTIONS
C     --------------------------------------------------

!     mark trait no.s
      ii=lim0
      do iq=1,nq
      do i=1,nfr(iq)
      ii=ii+1
      iqrow(ii)=iq
      end do
      end do
      do iq=1,nq
      do i=1,nfl(iq)
      ii=ii+1
      iqrow(ii)=iq
      end do
      end do

C     CARRY OUT GAUSSIAN ELIMINATION
      CALL OPZERO(ZERO,0)
      iflag(:lim2)=0
      IEMPTY=-1
      KRANK=0
      kk=0

      do irow=lim2,lim0+1,-1
      DD=DIA(IROW)
      IF(DABS(DD).ge.ZERO)then
         kk=kk+1
         iq=iqrow(irow)
         KRANK(iq)=KRANK(iq)+1
         IFLAG(IROW)=KK
         CALL DFABRW(DD,IROW,IEMPTY)
      end if
      end do
      nrnkx=kk

      WRITE(*,*)'"DFLSQ3" : NO. OF FIXED EFFECTS EQUATIONS =',LIM2-LIM0
      WRITE(*,*)'           RANK OF COEFFICIENT MATRIX     =',KK
      WRITE(*,*)'           ... EXPECTED VALUE             =',LIM2-LIM0
     &                                                          -NRZERO

      IF(KK.NE.Lim2-lim0-nrzero)THEN
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
      IF(IFLAG(IROW)>0)THEN
         RHS=XHX(NTRAIT,IROW)-dot_product( xhx(lim0+1:irow-1,irow),
     &                                     xvec(lim0+1:irow-1) )
         XVEC(IROW)=RHS/DIA(IROW)
      END IF
      END DO
      DO IROW=LIMXHX+1,LIM2
      IJ=IFIRST(IROW)
      IF(IJ>0.and.IFLAG(IROW)>0)then
         RHS=0.D0
         do while (IJ>0)
            JCOL=IVCOL(IJ)
            IF(jcol.EQ.NTRAIT)then
               RHS=ZHZ(IJ)  ! pick out rhs
            else if(jcol>lim0)then
               RHS=RHS-ZHZ(IJ)*XVEC(JCOL)
            end if
            IJ=INEXT(IJ)
         end do
         XVEC(IROW)=RHS/DIA(IROW)
      end if
      end do

C     ... CALCULATE CLASS MEANS & SCALE SOLUTIONS
      sd=1.d0
      K=LIM0
      DO IQ=1,NQ
      k=k+kftfix(iq)
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
      DO  I=1,NFIX(IQ)
      DO  J=1,NLEV(I,IQ)
      K=K+1
      IF(NNVEC(K).GT.0)ROW(K)=YBAR(IQ)+ ((ROW(K)*SD)/FLOAT(NNVEC(K)))
      XVEC(K)=XVEC(K)*SD
      end do
      end do
      end do

C     WRITE OUT NO.S PER SUBCLASS & CLASS MEANS
      WRITE(IUN13)LIM2
      write(iun13)nnvec(1:lim2)
      write(iun13)row(1:lim2)
      write(iun13)xvec(1:lim2)

!     calculate residuals after fitting fixed effects
      call dxfix3

      deallocate(ivec,nnvec,iflag,ifirst,ivcol,inext,zhz,xvec,dia,xhx,
     *           row,rrow,y1,y2,y20,y3,y4,yy1,yy20,yy2,nnaa,ss,ssr,
     &           ssr0,zz,zzr,stat=ii)
      if(ii>0)print *,'dfslq3 : de-alloc'

      RETURN

      contains

c     ======================
      SUBROUTINE      DFZER3
c     ======================

      integer :: i,irow,ll,ij,ipre,jrow

      DO I=1,NRZERO
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
         do while (ij>0)
            ZHZ(IJ)=0.D0
            IJ=INEXT(IJ)
         end do
         IFIRST(IROW)=0
         LL=IROW
      END IF

C     ... PART OF COLUMN WHICH IS SPARSE-STORED
      DO JROW=LL+1,LIM2
      IJ=IFIRST(JROW)
      IPRE=0
402   IF(IJ>0)THEN
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
      END IF
      end do
      DIA(IROW)=0.D0
      end do
      RETURN

      END subroutine dfzer3

C     =================================
      SUBROUTINE DFABRW(DD,IROW,IEMPTY)
C     =================================

      integer,intent(in)    :: irow
      integer,intent(inout) :: iempty
      real(8), intent(in)   :: dd

      integer               :: k,i
      real(8)               :: tt,zz

      K=0
      TT=-1.D0/DD

C     ---------------
C     FULLSTORED PART
C     ---------------

      IF(IROW.LE.LIMXHX)THEN
         DO  I=1,IROW-1            ! PICK OUT NON-ZERO OFF-DIAGONAL ELEMENTS
         IF(ABS(XHX(I,IROW)).GT.ZERO)THEN
            K=K+1
            IVEC(K)=I
            RROW(K)=XHX(I,IROW)
         END IF
         end do

C        ABSORB
         DO  I=1,K
         JROW=IVEC(I)
         ZZ=TT*RROW(I)
         XVEC(:i)=ZZ*RROW(:i)
         DO  J=1,I-1
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
         do while (ij>0)
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
         DO I=1,K
         JROW=IVEC(I)
         ZZ=TT*RROW(I)
         XVEC(:i)=ZZ*RROW(:i)
         IF(JROW.LE.LIMXHX)THEN
            DO J=1,I-1
            XHX(IVEC(J),JROW)=XHX(IVEC(J),JROW)+XVEC(J)
            end do
         ELSE
            IJ=IFIRST(JROW)
            IPRE=0
            DO J=1,I-1
            JCOL=IVEC(J)
104         IF( (IJ.GT.0) .AND. (IVCOL(IJ).LT.JCOL) )THEN
               IPRE=IJ
               IJ=INEXT(IJ)
               GO TO 104
            ELSE IF( (IJ.GT.0) .AND. (JCOL.EQ.IVCOL(IJ)) )THEN
               ZHZ(IJ)=ZHZ(IJ)+XVEC(J)
            ELSE
               IF(IEMPTY.LE.0)THEN      ! NO EMPTY SPACES WHICH CAN BE FILLED
                  NRCZHZ=NRCZHZ+1
                  IF(NRCZHZ.GT.MZHZ)THEN
                     WRITE(*,*)'ROUTINE "ABSROW" : DIMENSION EXCEEDED'
                     WRITE(*,*)'ABSORPTION STEP  : ROW =',IROW
                     WRITE(*,*)'                   COL =',JCOL
                     write(*,*)mzhz
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
                 IFIRST(JROW)=NEWADD
                 INEXT(NEWADD)=0
              ELSE IF( (IJ.EQ.0) .AND. (IFIRST(JROW).GT.0) )THEN
                 INEXT(IPRE)=NEWADD
                 INEXT(NEWADD)=0
              ELSE IF( (IJ.GT.0) .AND. (IPRE.EQ.0) )THEN
                 INEXT(NEWADD)=IFIRST(JROW)
                 IFIRST(JROW)=NEWADD
              ELSE
                 INEXT(NEWADD)=INEXT(IPRE)
                 INEXT(IPRE)=NEWADD
              END IF
              IJ=NEWADD
            END IF
            end do
         END IF
         DIA(JROW)=DIA(JROW)+XVEC(I)
         end do  ! do i=1,k

      END IF
      RETURN

      END subroutine dfabrw

C     ==========================
      SUBROUTINE LNKROW(IROW,NN)
C     ==========================

      integer, intent(in) :: irow,nn
      
      integer             :: kk,ipre,icol,ipcol,iplace

      IF(NN.EQ.0)RETURN

      KK=0
      IPRE=0
      IPCOL=0
      IPLACE=IFIRST(IROW)
20    KK=KK+1
      ICOL=IVEC(KK)
      IF(ICOL.LE.IPCOL)THEN
        WRITE(*,*)'SUBROUTINE "LNKROW" : COL. NO.S OUT OF ORDER !'
        WRITE(*,*)' ROW NO. =',IROW
        DO I=1,NN
        WRITE(*,*)' COLUMN   ',I,IVEC(I),XVEC(I)
        end do
        STOP
      END IF

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
      IPCOL=ICOL
      IF(KK.LT.NN)GO TO 20
      RETURN

      END subroutine lnkrow
 
!     ==================
      SUBROUTINE Dxfix3 
!     ==================

      nnaa=0
      y1=0.d0
      y2=0.d0
      y20=0.d0
      y3=1.d12
      y4=-1.d12
      yy1=0.d0
      yy2=0.d0
      yy20=0.d0
      ss=0.d0
      ssr=0.d0
      ssr0=0.d0
      nrc=0
      REWIND(IUN52)

500   READ(IUN52,END=598)icomb,NOBS,NR,NNA(:NOBS,:),NNQ(:nobs),
     &    IICOL(:NR),
     *   ( (IEQ(K,L) ,K=1,NEFF(NNQ(L)) ),L=1,NOBS),
     *   ( (XCOV(K,L), K=kftfix(nnq(l))+1,NFR(NNQ(L)) ),L=1,NOBS),
     &    YVEC(:NOBS)

      nrc=nrc+1
      do iobs=1,nobs
      iage=nna(iobs,1)
      iq=nnq(iobs)
      y1(iage,iq)=y1(iage,iq)+yvec(iobs)
      yy1(iage,iq)=yy1(iage,iq)+yvec(iobs)*yvec(iobs)
      nnaa(iage,iq)=nnaa(iage,iq)+1
      end do

      do iobs=1,nobs
      iage=nna(iobs,irrmet(0,iq))
      iq=nnq(iobs)
      if(kftfix(iq)>0) ! fixed reg on orthog polynomial
     & xcov(:kftfix(iq),iobs)=phi(iage,:kftfix(iq),0,iq)
      end do

!     adjust for fixed effects
      yres(:nobs)=yvec(:nobs)
      do iobs=1,nobs
      iq=nnq(iobs)
      do i=1,nfix(iq)
      irow=ieq(i,iobs)-lim1
      if(irow.gt.0)yres(iobs)=yres(iobs)-xvec( iicol(irow) )
      end do
      end do

!     adjust for covariables 
      yres0(:nobs)=yres(:nobs)
      do iobs=1,nobs
      iq=nnq(iobs)
      do i=1,nfr(iq)
      yres(iobs)=yres(iobs)-xcov(i,iobs)*xvec(nfrst(iq)+i)
      end do
      end do
      write(24,'(i6,i4,(t11,7(i5,f9.3)))')nrc,nobs,(nna(l,1),yres(l),
     &                                                   l=1,nobs)
      do iobs=1,nobs
      iage=nna(iobs,1)
      iq=nnq(iobs)
      y20(iage,iq)=y20(iage,iq)+yres0(iobs) ! residual w/o adjust. for cov.s
      y2(iage,iq)=y2(iage,iq)+yres(iobs)    ! residuals
      yy20(iage,iq)=yy20(iage,iq)+yres0(iobs)*yres0(iobs)
      yy2(iage,iq)=yy2(iage,iq)+yres(iobs)*yres(iobs)
      if(yres(iobs).lt.y3(iage,iq))y3(iage,iq)=yres(iobs)
      if(yres(iobs).gt.y4(iage,iq))y4(iage,iq)=yres(iobs)
      write(25,'(i5,i4,i6,f10.1,f10.3)')nrc,iobs,iiage(iage,1),
     &                              ybar(iq)+yvec(iobs),yres(iobs)
      end do

      go to 500

 598  open(iun14,file='DF14#DAT',status='unknown')
      do iq=1,nq
      zz(iq)=sum(yy1(:,iq))
      zzr(iq)=sum(yy2(:,iq))
      end do
      do i=1,nage(1)
      do j=1,nq
      if(nnaa(i,j).eq.0)cycle
      yy20(i,j)= yy20(i,j)-(y20(i,j)*y20(i,j)) /nnaa(i,j)
      yy1(i,j)= yy1(i,j)-y1(i,j)*y1(i,j)/nnaa(i,j)
      yy2(i,j)= yy2(i,j)-y2(i,j)*y2(i,j)/nnaa(i,j)
      ss(j)=ss(j)+yy1(i,j)
      ssr(j)=ssr(j)+yy2(i,j)
      ssr0(j)=ssr0(j)+yy20(i,j)
      if(nnaa(i,j).gt.1)then
        if(yy1(i,j)>zero)yy1(i,j)=sqrt( yy1(i,j)/(nnaa(i,j)-1) )
        if(y2(i,j)>0)yy2(i,j)=sqrt( yy2(i,j)/(nnaa(i,j)-1) )
        if(yy20(i,j)>0)yy20(i,j)=sqrt( yy20(i,j)/(nnaa(i,j)-1) )
        y1(i,j)=y1(i,j)/nnaa(i,j)
        y2(i,j)=y2(i,j)/nnaa(i,j)
        y20(i,j)=y20(i,j)/nnaa(i,j)
      end if
      end do
      end do
      do iq=1,nq
      if(nq>1)write(iun14,'(1x,a,i4)')'Trait No. =',iq
      do i=1,nage(1)
      if(nnaa(i,iq)<1)cycle
      write(*,'(a,i3,a,i5,2(a,g11.4))')'age=',iiage(i,1),
     * ' n=',nnaa(i,iq), ' obs=', ybar(iq)+y1(i,iq),' res=',y2(i,iq)
      if(kftfix(iq)>0)then
        pp=dot_product(xvec(nfrst(iq)+1:nfrst(iq)+kftfix(iq)),
     &                               phi(i,:kftfix(iq),0,iq))+ybar(iq)
        pp=pp-xvec(nfrst(iq)+1)*phi(i,1,0,iq)
!       ybar(iq)+y1(i) : "raw" mean
!       ybar(iq)+y20(i): residual mean not accounting for fixed regression
!       ybar(iq)+y2(i): residual mean
!       yy2(i) : residual SD
!        yy1(i) : "raw" SD
        write(iun14,'(i6,7g12.5)')iiage(i,1),ybar(iq)+y1(i,iq),pp,
     &       ybar(iq)+y20(i,iq),ybar(iq)+y2(i,iq),yy20(i,iq),
     &       yy2(i,iq),yy1(i,iq)
      end if
      end do ! i
      n=sum(nnaa(:,iq))
      ndf=n-nage(1)
      ss(iq)=ss(iq)/ndf   ! within age classes
      ssr(iq)=ssr(iq)/ndf
      ssr0(iq)=ssr0(iq)/ndf
      write(*,'(1x,a,t12,3g18.6,i6)')'variance =',ss(iq),ssr0(iq),
     &                                                    ssr(iq),ndf
      write(*,'(1x,a,t12,3g18.6)')'St.Dev. =',sqrt(ss(iq)),
     *                                     sqrt(ssr0(iq)),sqrt(ssr(iq))
      write(*,'(1x,a,t12,3g18.6,f8.2)')'ss total',zz(iq),zzr(iq),
     &                     zz(iq)-zzr(iq),(zz(iq)-zzr(iq))*100.0/zz(iq)
      xmse(iq)=zzr(iq)/(n-krank(iq))
      write(*,'(1x,a,t12,g18.6,2i6)')'MSE',xmse(iq),krank(iq),n
     &                                                       -krank(iq)
      end do ! iq

      write(iun13)ss,ssr,zz,zzr,xmse,krank
      write(iun13)nnaa
      write(iun13)y1
      write(iun13)y2
      write(iun13)y3
      write(iun13)y4
      write(iun13)yy1
      write(iun13)yy2

      return
      END subroutine dxfix3

      END subroutine dflsq3

!============================================================================
      subroutine DFLSP3 (lim0)
!============================================================================  

      use names
      use units
      use levels
      use numbers
      use order
      use zero_rows
      use ages
      use means
      use names
      use phimatrix

!     arguments
      integer, intent(in)                    :: lim0

!     local variables
      character(len=20),dimension(0:3)       :: txt =(/
     &                                          ' Ordinary polynomial',
     &                                          ' Legendre polynomial',
     &                                          ' User defined       ',
     &                                          ' LRS Dairy model    '/)
      integer, dimension(:),  allocatable    :: nnvec,krank
      integer, dimension(:,:),  allocatable  :: nnaa
      real(8), dimension(:),  allocatable    :: xvec,row
      real(8), dimension(:,:),  allocatable  :: y1,y2,yy1,yy2,y3,y4
      real(8),dimension(:),allocatable       :: ss,ssr,zz,zzr,xmse
      integer                                :: i,j,k,ii
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      REWIND(IUN13)
      READ(IUN13)LIM2
     
      allocate(nnvec(lim2),row(lim2),xvec(lim2),y1(mage,nq),y2(mage,nq),
     &       yy1(mage,nq),yy2(mage,nq),y3(mage,nq),y4(mage,nq),nnaa
     &       (mage,nq),ss(nq),ssr(nq),zz(nq),zzr(nq),xmse(nq),
     &       krank(nq), stat=ii)
      if(ii>0)stop 'dflsp3 : alloc'

      read(iun13)nnvec
      read(iun13)row
      read(iun13)xvec
      read(iun13)ss,ssr,zz,zzr,xmse,krank
      read(iun13)nnaa
      read(iun13)y1
      read(iun13)y2
      read(iun13)y3
      read(iun13)y4
      read(iun13)yy1
      read(iun13)yy2

      WRITE(IUN66,*)'  '
      WRITE(IUN66,*)'----------------------------------------------'
      WRITE(IUN66,*)'Mean observations & residuals for ages        '
      WRITE(IUN66,*)'----------------------------------------------'

      do iq=1,nq
      if(nq>1)write(iun66,*)'Trait no. =',iq,' ',trait(iq)
      write(iun66,'(1x,T17,''mean'',4x,''-overall m.'',4x,''st.dev.'',
     &  2x,''residual'',5x,''st.dev.'',3x,''minimum'',5x,''maximum'')')
      do i=1,nage(1)
      if(nnaa(i,iq)>0)
     & write(iun66,'(i4,i6,i5,T13,3g12.5,2x,4g12.5)')i,iiage(i,1),
     &        nnaa(i,iq),ybar(iq)+y1(i,iq),y1(i,iq),yy1(i,iq),y2(i,iq),
     &        yy2(i,iq),y3(i,iq),y4(i,iq)
      end do
!     within age classes
      write(iun66,'(a,t20,''='',2g16.6)')'pooled variances ',ss(iq),
     &                                                       ssr(iq)
      write(iun66,'(a,t20,''='',2g16.6)')'...    standard dev.',
     &                                    sqrt(ss(iq)),sqrt(ssr(iq))
      write(iun66,*)' '
      n=sum( nnrec(:,iq) )
      write(iun66,910)'Total SS (after mean)',n-1,zz(iq)
      rsq=(zz(iq)-zzr(iq))*100.d0/zz(iq)
      write(iun66,910)'SS due to fixed effects',krank(iq),
     &                                              zz(iq)-zzr(iq),rsq
      write(iun66,910)'SS & MS residual',n-krank(iq),zzr(iq),xmse(iq)
 910  format(1x,a,t35,'=',i6,2g16.6)
      end do ! iq

      WRITE(IUN66,*)'  '
      WRITE(IUN66,*)'----------------------------------------------'
      WRITE(IUN66,*)'LSQ SOLUTION FOR FIXED EFFECTS AND COVARIABLES'
      WRITE(IUN66,*)'----------------------------------------------'

      WRITE(IUN66,913)
      DO IQ=1,NQ
      if(nq>1)write(iun66,'(a,i3,2x,a)')'Trait No.',iq,trait(iq)
      K=nfrst(iq)
      if(kftfix(iq)>0)then
         write(iun66,*)'Fixed regression (population trajectory) '
         write(iun66,*)'... Form of regression  ',txt(irropt(0,iq))
         if(nmeta>1)write(iun66,*)'... Regression on meta-meter no.',
     &                                              irrmet(0,iq)
         do j=1,kftfix(iq)
         k=k+1
         write(iun66,911)k,'Order',j,iq,xvec(k)
         end do
      end if
      DO I=1,NCOV(IQ)
      WRITE(IUN66,909)'COVARIABLE NO.',I,COVAR(I,IQ)
      DO  J=1,NPOW(I,IQ)
      K=K+1
      WRITE(IUN66,911)K,'ORDER',J,IQ,XVEC(K)
      end do
      end do
      end do ! iq

      WRITE(IUN66,913)
      DO  IQ=1,NQ
      if(nfix(iq)>0)then
         if(nq>1)write(iun66,'(a,i3,2x,a)')'Trait No.',iq,trait(iq)
         DO  I=1,NFIX(IQ)
         WRITE(IUN66,909)'FIXED EFFECT NO.',I,FIXED(I,IQ)
         DO  J=1,NLEV(I,IQ)
         K=K+1
         WRITE(IUN66,912)K,'LEVEL',J,IQ,NNVEC(K),ROW(K),XVEC(K)
         end do
         end do
      end if
      end do ! iq

      return

909   FORMAT(/1X,A,I4,4X,A12)
911   FORMAT(I5,1X,A,T20,2I6,T40,21X,G16.9)
912   FORMAT(I5,1X,A,T20,2I6,T40,I5,2F16.8)
913   FORMAT(/' EQ.NO.',T28,'TRAIT',T40,2X,'NREC',11X,'MEAN',8X,
     *                                                'SOLUTION')
      end subroutine dflsp3



















