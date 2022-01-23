C========================================================================
      SUBROUTINE DxCOVM(NPARM,IBEGIN,LQ,IOUT,IOPT,izig,ZIG,PARVEC,WORK,
     *                                                     FVALUE,DETX)
C========================================================================

      use parameters, only : mxparm
      use names
      use units
      use numbers
      use eigen_wrk

      integer, intent(in)                           :: nparm,ibegin,lq,
     &                                                 iopt,izig
      integer, intent(out)                          :: iout
      real(8), dimension (izig,izig), intent(out)   :: zig
      real(8), dimension (mxparm), intent(in)       :: parvec
      real(8), dimension (lq*(lq+1)/2), intent(out) :: work
      real(8), intent(out)                          :: fvalue, detx

      integer                                       :: i,i1,j1,nneg
      real(8)                                       :: es
      real(8), dimension(:), allocatable            :: w1,w2
      integer, dimension(:), allocatable            :: iwork
      character(len=8)                              :: PP,PP1 !not for sigam
      integer, external                             :: ihmii,ihmssf
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C     PICK OUT COVARIANCE MATRIX AND HALFSTORE ...
      IF(IOPT.GE.1)THEN
         work=0.d0
         READ(PARVAR(IBEGIN+1),'(a8,2i3)')PP
         WORK=0.D0
         DO I=IBEGIN+1,MIN0(IBEGIN+LQ*(LQ+1)/2,NPARM)
         READ(PARVAR(i),'(a8,2i3)')PP1,I1,J1
         IF(PP.NE.PP1)exit
         WORK(IHMSSF(I1,J1,LQ))=PARVEC(I)
         end do
      END IF

C     CHECK THAT MATRIX IS WITHIN PARAMETER SPACE
      IOUT=0
      IF(IOPT.LE.2)THEN
         call all_eigen(lq)
         CALL EIGEm(WORK,EIGZER,ES,LQ,NNEG)
         if(nneg.gt.0)then
             if(kopt.eq.3)print *,'neg eigenval',nneg,eig
             iout=1
             fvalue=big
             call deall_eigen
             return
          end if
!         set eigenvalues to operational zero if necessary
          where (eig<eigzer )
!          where (eig<eigzer .and. eig.ne.0.d0)
             eig=eigzer
          end where
!         construct full rank matrix
          call reconst_matrix(work,zero,lq)
       end if

C     Calculate inverse
      IF(IOPT.GE.0 .AND. IOPT.LT.2)then
         allocate(w1(lq),w2(lq),iwork(lq),stat=ii)
         if(ii>0)stop 'alloc :dxcovm'
         CALL DKMWHF(WORK,W1,W2,DETX,ZERO,IWORK,IRANK,LQ,0)
         deallocate(w1,w2,iwork,stat=ii)
         if(ii>0)stop 'de-alloc :dxcovm'
      end if

C     FULLSTORE MATRIX (OR INVERSE)
      do i=1,lq
      do j=1,lq
      zig(j,i)=work(ihmssf(i,j,lq))
      end do
      end do
      
      call deall_eigen
      RETURN
      END SUBROUTINE dxcovm

