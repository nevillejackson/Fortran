!========================================================================
      SUBROUTINE DxCOVM (NPARM,IBEGIN,NQ,NQQ,IOUT,IOPT,isig,
     *                           SIG,PARVEC,WORK,FVALUE,DETX)
!========================================================================

      use params
      use names
      use units

      integer, intent(in)                           :: nparm,ibegin,nq,
     &                                                 nqq,iopt,isig
      integer, intent(out)                          :: iout
      real(8), dimension (isig,isig), intent(out)   :: sig
      real(8), dimension (mxparm), intent(in)       :: parvec
      real(8), dimension (nqq), intent(inout)       :: work
      real(8), intent(out)                          :: fvalue, detx

!     local variables
      integer                                       :: i,i1,j1,nneg
      real(8)                                       :: es
      real(8), dimension(:), allocatable            :: eig,w1,w2
      real(8), dimension(:,:), allocatable          :: vv
      integer, dimension(:), allocatable            :: iwork
! need to change these values for sigam ne 0 !!
      character(len=12)                             :: PARA
      character(len=8)                              :: PP,PP1
      integer, external                             :: ihmii,ihmssf
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      allocate(eig(nq),w1(nq),w2(nq),vv(nq,nq),iwork(nq), stat=ii)
      if(ii>0)stop 'alloc :dxcovm'

      IOUT=0

C     PICK OUT COVARIANCE MATRIX AND HALFSTORE ...
      IF(IOPT.GE.1)THEN
         work=0.d0
         PARA=PARVAR(IBEGIN+1)
         READ(PARA,201)PP
 201     FORMAT(A8,2I2)
         work=0.d0
         DO I=IBEGIN+1,MIN0(IBEGIN+NQQ,NPARM)
         PARA=PARVAR(I)
         READ(PARA,201)PP1,I1,J1
         IF(PP.NE.PP1)GO TO 41
         WORK(IHMSSF(I1,J1,NQ))=PARVEC(I)
         end do
      END IF

C     .... CHECK THAT IT IS WITHIN PARAMETER SPACE
 41   IF(IOPT.LE.2)THEN
         CALL EIGEm(WORK,EIG,EIGZER,ES,nq,VV,NQ,NNEG)
         if(nneg.gt.0)then
             iout=1
             fvalue=big
             go to 99
          end if
          work=0.d0
c         ... set eigenvalues to operational zero if necessary
          do i=1,nq
          if(eig(i).lt.eigzer .and. eig(i).ne.0.d0 )eig(i)=eigzer
          work(ihmii(i,nq))=eig(i)
          end do
          call invrt(vv,nq,nq)
          call matrba(vv,work,nq,nq)
       end if

C     ... INVERT
      IF(IOPT.GE.0 .AND. IOPT.LT.2)CALL DKMwhf(WORK,W1,W2,DETX,ZERO,
     *                                             IWORK,IRANK,NQ,0)

C     FULLSTORE INVERSE OR MATRIX
      do i=1,nq
      do j=1,nq
      sig(j,i)= work(ihmssf(i,j,nq) )
      end do
      end do

 99   deallocate(eig,w1,w2,vv,iwork,stat=ii)
      if(ii>0)stop 'de-alloc dxcovm'
      RETURN
      END subroutine dxcovm








