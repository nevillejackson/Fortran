C============================================================================
      SUBROUTINE chksig (pp,zero,xvec,sig,ibegn,nq,ioprun,jpt)
c============================================================================

      use eigen_wrk
  
      CHARACTER(len=8), intent (in)                        :: PP
      real(8),intent(in)                                   :: zero
      real(8), dimension(nq*(nq+1)/2), intent(inout)       :: sig
      real(8), dimension(ibegn+nq*(nq+1)/2), intent(inout) :: xvec
      integer, intent(in)                                  :: ibegn,nq,
     &                                                        ioprun,jpt
      real(8)                                              :: es
 
      call all_eigen(nq)

      call eigem(sig,zero,es,nq,ineg)

      if(ineg.eq.0)go to 99   ! matrix is o.k.

      if(ioprun.ne.-2)then
          write(*,*)' matrix ',pp,' is not positive definite !'
          do i=1,nq
          write(*,'(i4,(t6,6f10.4))')i,(sig(ihmssf(i,j,nq)),j=1,i)          
          end do
          write(*,*)'eigenvalues : ',eig
      end if
      nqq=nq*(nq+1)/2
      do i=1,nq
      if(eig(i).lt.zero)eig(i)=zero*5.d0
      end do

      call reconst_matrix(sig,zero,nq)

      if(ioprun.ne.-2)then
         write(*,*)'possible replacement (setting negative eigenvalues'
     *,             ' to',zero*10,' )'
         do i=1,nq
         write(*,'(i4,(t6,6f10.4))')i,(sig(ihmssf(i,j,nq)),j=1,i)          
         end do
         if(pp.eq.'SIG AM  ')stop '"CHKSIG"'
         write(*,*)'accept ?'
!         call yndef(ii,1)
         if(jpt.eq.0)stop '"CHKSIG"'
      end if
      xvec(ibegn+1:ibegn+nqq)=sig(:nqq)

 99   call deall_eigen
      return
      end subroutine chksig

c=========================================================================
      subroutine eigem(sig,zero,es,nq,ineg)
c=========================================================================

      use eigen_wrk

      real(8),intent(in)                              :: zero
      real(8), intent(out)                            :: es
      real(8), dimension(nq*(nq+1)/2), intent(in)     :: sig
      integer, intent(in)                             :: nq
      integer, intent(out)                            :: ineg
      integer                                         :: i
      external                           ihmssf,ihmii,ihmji

      ework=sig(:nq*(nq+1)/2)

      call eigenvl(1.d-6,nq,300)
      es=sum(eig)
      ineg=0
      do i=1,nq
      if(dabs(eig(i)).ge.zero.and.eig(i).lt.0)ineg=ineg+1
      end do

      return
       
      contains

C     ============================
      SUBROUTINE EIGENVL(EPS,N,MAX)
C     ============================

      real(8),intent(in)    :: eps
      integer, intent(in)  :: n,max

      real(8) :: epsqu,ss,app,apq,theta,t,x,aqq,c,s,cs,csa,cc,aijp,aijq
      integer :: nsweep,ij,ipp,ipq,ip,iq

      NSWEEP=0
      EPSQU=EPS*EPS

      vv=0.d0
      do i=1,n
      vv(i,i)=1.d0
      end do

C     CALCULATE SUM OF SQUARES OF OFF-DIAGONAL ELEMENTS

5     SS=0.D0
      NSWEEP=NSWEEP+1
      DO  I=1,N-1
      DO  J=I+1,N
      IJ=ihmssf(i,j,n)
      SS=SS+ework(IJ)*ework(IJ)
      end do
      end do
      SS=2.D0*SS

C     TEST WHETHER SS ARE SUFFICIENTLY SMALL
      IF(SS.LT.EPSQU)THEN
         EIG(:n)=ework( (/ (Ihmii(i,n),i=1,n) /) )
         RETURN
      END IF

C     MATRIX NOT DIAGONAL YET, NEED MORE TRANSFORMATIONS

      DO IP=1,N-1

      IPP=ihmii(ip,n)
      APP=Ework(IPP)
      DO IQ=IP+1,N
      IPQ=ihmssf(ip,iq,n)
      APQ=Ework(IPQ)

      IF(APQ.NE.0)THEN         !    APPLY P-Q ROTATION TO MATRIX A
         IQQ=IHMII(IQ,N)
         AQQ=Ework(IQQ)
         THETA=0.5D0*(Ework(IQQ)-APP)/APQ
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
         Ework(IPP)=CC*APP-CSA+SS*AQQ
         Ework(IPQ)=CS*(APP-AQQ)+(CC-SS)*APQ
         Ework(IQQ)=SS*APP+CSA+CC*AQQ
         APP=Ework(IPP)

         DO  J=1,IP-1
         IJP=IHMJI(IP,J,N)
         IJQ=IHMSSF(IQ,J,N)
         AIJP=Ework(IJP)
         AIJQ=Ework(IJQ)
         Ework(IJP)=C*AIJP-S*AIJQ
         Ework(IJQ)=S*AIJP+C*AIJQ
         end do

         IJP=IPP
         DO  J=IP+1,IQ-1
         IJP=IJP+1
         IJQ=IHMJI(IQ,J,N)
         AIJP=Ework(IJP)
         AIJQ=Ework(IJQ)
         Ework(IJP)=C*AIJP-S*AIJQ
         Ework(IJQ)=S*AIJP+C*AIJQ
         end do

         IJQ=IQQ
         DO  J=IQ+1,N
         IJP=IHMIJ(IP,J,N)
         IJQ=IJQ+1
         AIJP=Ework(IJP)
         AIJQ=Ework(IJQ)
         Ework(IJP)=C*AIJP-S*AIJQ
         Ework(IJQ)=S*AIJP+C*AIJQ
         end do

C        ACCUMULATE TRANSFORMATIONS TO FORM MATRIX OF EIGENVECTORS
         DO I=1,N
         VP=Vv(I,IP)
         VQ=Vv(I,IQ)
         Vv(I,IP)=C*VP-S*VQ
         Vv(I,IQ)=S*VP+C*VQ
         end do

      END IF
      end do
      end do

      IF(NSWEEP.LT.MAX)GO TO 5

      write(*,*)'Routine "EIGEM" : Failed to diogonalise matrix !'
      write(*,*)' ... numerical problem : probably at boundary ',
     &          'of parameter space'
      write(*,'(a,i10)')'MAX. NO. OF SWEEPS REACHED    =',MAX
      write(*,'(a,f16.6)')'LAST SS OF OFF-DIAG. ELEMENTS =',SS
      write(*,'(a,2f16.6)')'ACCURACY TOLERANCE GIVEN      =',EPSQU,EPS
      stop 'eignvl : cannot determine eigenvalues '

      END subroutine eigenvl

      end subroutine eigem

!==========================================================================
      subroutine reconst_matrix(sig,zero,nq)
!==========================================================================

      use eigen_wrk

      real(8), dimension(nq*(nq+1)/2), intent(out) :: sig
      real(8), intent(in)                          :: zero
      integer, intent(in)                          :: nq
      real(8), dimension (:,:), allocatable        :: h1,h2
      integer, dimension(:), allocatable           :: iflag
      integer                                      :: i,j,ii

      allocate(iflag(nq),h1(nq,nq),h2(nq,nq),stat=ii)
      if(ii>0)stop 'alloc reconst_matrix'

!     invert matrix of eigenvectors
      call invert_vv

!     pre-and postmultiply ...
      h1=0.d0
      do i=1,nq
      h1(i,i)=eig(i)
      end do

      h2=matmul(h1,vv)
      h1=matmul(transpose(vv),h2)
      sig(:nq*(nq+1)/2)=(/ (( h1(i,j),j=i,nq),i=1,nq) /)

      deallocate(iflag,h1,h2,stat=ii)
      if(ii>0)stop 'de-alloc reconst_matrix'
      return
      
      contains

!     ===========================
      subroutine invert_vv
!     ===========================

      integer                                 :: imax,i,j,k,isave
      real(8)                                 :: xx,zz,save

      do i=1,nq
      iflag(i)=i
      end do

      DO  I=1,NQ           ! FIND MAXIMUM IN COLUMN (START AT I-TH EL. ONLY)
      XX=DABS(vv(I,I))
      IMAX=I
      DO J=I+1,NQ
      ZZ=DABS(vv(J,I))
      IF(ZZ.GT.XX)THEN
         XX=ZZ
         IMAX=J
      END IF
      end do

      IF(XX.LT.ZERO)stop '"INVERT_VV" : MATRIX IS SINGULAR'

C     INTERCHANGE ROW I AND ROW WITH MAX. ELEMENT IN THE COLUMN
      IF(IMAX.GT.I)THEN
         DO  K=1,NQ
         SAVE=vv(I,K)
         vv(I,K)=vv(IMAX,K)
         vv(IMAX,K)=SAVE
         end do
         ISAVE=IFLAG(I)
         IFLAG(I)=IFLAG(IMAX)
         IFLAG(IMAX)=ISAVE
      END IF

C     TRANSFORM THE MATRIX
      SAVE=1.D0/vv(I,I)
      vv(:nq,I)=vv(:nq,I)*SAVE
      vv(I,I)=SAVE
      DO K=1,NQ
      IF(K.ne.I)then
         DO  J=1,NQ
         IF(J.NE.I)vv(J,K)=vv(J,K)-vv(J,I)*vv(I,K)
         end do
         vv(I,K)=-vv(I,K)*SAVE
      end if
      end do
      end do !  loop over i

C     INTERCHANGE COLUMNS (ANALOGOUS TO PREVIOUS ROW CHANGES )
      DO I=1,NQ
      ework( (/ (iflag(k),k=1,nq) /) ) =vv(i,:nq)  
      vv(I,:nq)=ework(:nq)
      end do

      RETURN
      END subroutine invert_vv

      end subroutine reconst_matrix




























