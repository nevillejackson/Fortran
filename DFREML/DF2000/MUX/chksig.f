!============================================================================
      SUBROUTINE chksig (pp,zero,xvec,sig,ibegn,nq,ioprun)
!============================================================================

      CHARACTER(len=8), intent (in)                       :: PP
      real(8),intent(in)                                  :: zero
      real(8), dimension(nq*(nq+1)/2), intent(inout)      :: sig
      real(8), dimension(ibegn+nq*(nq+1)/2), intent(inout) :: xvec
      integer, intent(in)                                 :: ibegn,nq,
     &                                                       ioprun

      real(8)                               :: es
      real(8), dimension (:), allocatable   :: EIG
      real(8), dimension (:,:), allocatable :: VV

      allocate(eig(nq),vv(nq,nq),stat=ii)
      if(ii>0)stop 'chksig : alloc'

      call eigem(sig,eig,zero,es,nq,vv,nq,ineg)
      if(ineg.eq.0)go to 99 !  matrix o.k. 

      if(ioprun.ne.-2)then
          write(*,*)' matrix ',pp,' is not positive definite !'
          do i=1,nq
          write(*,'(i3,(6f12.5))')i,(sig(ihmssf(i,j,nq)),j=1,i)          
          end do
          write(*,*)'eigenvalues : ',eig
      end if
      nqq=nq*(nq+1)/2
!     set negative eigenvalues to small positive value
      do i=1,nq
      if(ioprun.ne.-2)write(*,'(i4,f12.5)')i,eig(i)
      if(eig(i).lt.zero)eig(i)=0.1d0
      end do
!     re-construct cov matrix
      sig(:nqq)=0.d0
      sig( (/ (ihmii(i,nq),i=1,nq) /) ) =eig(:nq)
      call invrt(vv,nq,nq)
      call matrba(vv,sig,nq,nq)
      if(ioprun.ne.-2)then
         write(*,*)'possible replacement (setting negative eigenvalues'
     *,             ' to',0.1,' )'
         do i=1,nq
         write(*,'(i4,(t6,6f10.4))')i,(sig(ihmssf(i,j,nq)),j=1,i)          
         end do
         if(pp.eq.'SIG AM  ')stop '"CHKSIG"'
         write(*,*)'accept ?'
         call yndef(ii,1)
         if(ii.eq.0)stop '"CHKSIG"'
      end if
      xvec(ibegn+1:ibegn+nqq)=sig(:nqq)

 99   deallocate(eig,vv)
      return
      end subroutine chksig

c=========================================================================
      subroutine eigem(sig,eig,zero,es,iv,vv,nq,ineg)
c=========================================================================

      real(8),intent(in)                              :: zero
      real(8), intent(out)                            :: es
      real(8) , dimension(nq*(nq+1)/2), intent(in)    :: sig
      real(8) , dimension(nq), intent(out)            :: eig
      real(8) , dimension(iv,iv), intent(out)         :: vv
      integer, intent(in)                             :: nq,iv
      integer, intent(out)                            :: ineg

      real(8), dimension(:), allocatable              :: work
      integer                                         :: nqq,ii

      nqq=nq*(nq+1)/2
      allocate(work(nqq),stat=ii)
      if(ii>0)stop 'eigen : alloc'

      work=sig(:nqq)

      call eignvl(1.d-6,nq,300)
      es=sum(eig)
      ineg=0
      do i=1,nq
      if(dabs(eig(i)).gt.zero.and.eig(i).lt.0)ineg=ineg+1
      end do

      deallocate(work)

      return
       
      contains

C     ============================
      SUBROUTINE EIGNVL(EPS,N,MAX)
C     ============================

      real(8),intent(in) :: eps
      integer, intent(in) :: n,max

      real(8) :: epsqu,ss,app,apq,theta,t,x,aqq,c,s,cs,csa,cc,aijp,aijq
      integer :: n1,nsweep,ij,ipp,ipq,ip,iq

      NSWEEP=0
      N1=N+1
      EPSQU=EPS*EPS

      vv=0.d0
      DO I=1,N
      vv(i,i)=1.d0
      end do

C     CALCULATE SUM OF SQUARES OF OFF-DIAGONAL ELEMENTS

5     SS=0.D0
      NSWEEP=NSWEEP+1
      IJ=0
      DO  I=1,N-1
      IJ=IJ+1
      DO  J=I+1,N
      IJ=IJ+1
      SS=SS+Work(IJ)*Work(IJ)
      end do
      end do
      SS=2.D0*SS

C     TEST WHETHER SS ARE SUFFICIENTLY SMALL
      IF(SS.LT.EPSQU)THEN
         DO I=1,N
         EIG(I)=Work(Ihmii(i,n))
         end do
         RETURN
      END IF

C     MATRIX NOT DIAGONAL YET, NEED MORE TRANSFORMATIONS

      IPP=-N
      IPQ=0
      DO 2 IP=1,N-1

      IPP=IPP+N1
      APP=Work(IPP)
      IPQ=IPQ+1
      DO 3 IQ=IP+1,N
      IPQ=IPQ+1
      APQ=Work(IPQ)

      IF(APQ.NE.0)THEN
C     ... APPLY P-Q ROTATION TO MATRIX A
         IQQ=IHMII(IQ,N)
         AQQ=Work(IQQ)
         THETA=0.5D0*(Work(IQQ)-APP)/APQ
         IF(THETA.EQ.0)THEN
            T=1.D0
         ELSE
           X=DSQRT(1.D0+THETA*THETA)
           IF(THETA.LT.0)X=-X
           T=1.D0/(THETA+X)
         END IF
         C=1.D0/DSQRT(1.D0+T*T)
         S=C*T
         CC=C*C
         CS=C*S
         SS=S*S
         CSA=2.D0*CS*APQ
         Work(IPP)=CC*APP-CSA+SS*AQQ
         Work(IPQ)=CS*(APP-AQQ)+(CC-SS)*APQ
         Work(IQQ)=SS*APP+CSA+CC*AQQ
         APP=Work(IPP)

         DO  J=1,IP-1
         IJP=IHMJI(IP,J,N)
         IJQ=IHMSSF(IQ,J,N)
         AIJP=Work(IJP)
         AIJQ=Work(IJQ)
         Work(IJP)=C*AIJP-S*AIJQ
         Work(IJQ)=S*AIJP+C*AIJQ
         end do

         IJP=IPP
         DO J=IP+1,IQ-1
         IJP=IJP+1
         IJQ=IHMJI(IQ,J,N)
         AIJP=Work(IJP)
         AIJQ=Work(IJQ)
         Work(IJP)=C*AIJP-S*AIJQ
         Work(IJQ)=S*AIJP+C*AIJQ
         end do

         IJQ=IQQ
         DO  J=IQ+1,N
         IJP=IHMIJ(IP,J,N)
         IJQ=IJQ+1
         AIJP=Work(IJP)
         AIJQ=Work(IJQ)
         Work(IJP)=C*AIJP-S*AIJQ
         Work(IJQ)=S*AIJP+C*AIJQ
         end do

C        ACCUMULATE TRANSFORMATIONS TO FORM MATRIX OF EIGENVECTORS
         DO I=1,N
         VP=Vv(I,IP)
         VQ=Vv(I,IQ)
         Vv(I,IP)=C*VP-S*VQ
         Vv(I,IQ)=S*VP+C*VQ
         end do
      END IF

3     CONTINUE
2     IPP=IPP-IP

      IF(NSWEEP.LT.MAX)GO TO 5

      PRINT *,'MAX. NO. OF SWEEPS REACHED    =',MAX
      PRINT *,'LAST SS OF OFF-DIAG. ELEMENTS =',SS
      PRINT *,'ACCURACY TOLERANCE GIVEN      =',EPSQU,EPS
      stop 'eignvl : cannot determine eigenvalues '

      END subroutine eignvl

      end subroutine eigem







