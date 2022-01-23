C=======================================================================
      SUBROUTINE INVRT( A, IA, N)
C=======================================================================

      real(8),dimension(ia,ia), intent(inout) :: a
      integer, intent(in)                     :: ia,n 

      real(8), dimension(:,:), allocatable    :: b
      real(8), dimension(:), allocatable      :: vec
      integer, dimension(:), allocatable      :: iflag
      integer                                 :: ii,imax,i,j,k
      real(8)                                 :: zero=1.d-12,diag,off
     &,                                          xx,zz
      allocate(b(n,n),vec(n),iflag(n),stat=ii)
      if(ii>0)stop 'alloc invrt'

      DIAG=0.D0
      OFF=0.D0
      iflag=(/ (i,i=1,n) /)
      B=A(:n,:n)

      DO 2 I=1,N

C     FIND MAXIMUM ELEMENT IN THE COLUMN (START AT I-TH EL. ONLY)
      XX=DABS(A(I,I))
      IMAX=I
      DO 3 J=I+1,N
      ZZ=DABS(A(J,I))
      IF(ZZ.GT.XX)THEN
         XX=ZZ
         IMAX=J
      END IF
 3    CONTINUE

C     CHECK FOR SINGULARITY
      IF(XX.LT.ZERO)THEN
          WRITE(*,*)'"INVRT" : MATRIX IS SINGULAR'
          STOP
      END IF

C     INTERCHANGE ROW I AND ROW WITH MAX. ELEMENT IN THE COLUMN
      IF(IMAX.GT.I)THEN
         DO  K=1,N
         SAVE=A(I,K)
         A(I,K)=A(IMAX,K)
         A(IMAX,K)=SAVE
         end do
         ISAVE=IFLAG(I)
         IFLAG(I)=IFLAG(IMAX)
         IFLAG(IMAX)=ISAVE
      END IF

C     TRANSFORM THE MATRIX
      SAVE=1.D0/A(I,I)
      A(:n,I)=A(:n,I)*SAVE
      A(I,I)=SAVE
      DO K=1,N
      IF(K.ne.I)then
         DO  J=1,N
         IF(J.NE.I)A(J,K)=A(J,K)-A(J,I)*A(I,K)
         end do
         A(I,K)=-A(I,K)*SAVE
      end if
      end do

 2    CONTINUE

C     INTERCHANGE COLUMNS (ANALOGOUS TO PREVIOUS ROW CHANGES )
      DO I=1,N
      vec( (/ (iflag(k),k=1,n) /) ) =a(i,:n)
      A(I,:n)=VEC
      end do

C     MULTIPLY MATRIX WITH ITS INVERSE, CHECK ELEMENTS
      DO I=1,N
      DO J=1,N
      XX=dot_product(a(:n,j),b(i,:))
      IF(I.EQ.J)THEN
C        IF(DABS(XX-1.D0).GT.ZERO)PRINT *,I,XX
         DIAG=DIAG+XX
      ELSE
C        IF(DABS(XX).GT.ZERO)PRINT *,I,J,XX
         OFF=OFF+XX
      END IF
      end do
      end do
C     XX=DIAG/N
C     PRINT *,'DIAGONAL : SUM =',DIAG,'     AVERAGE =',XX
C     XX=OFF/(N*(N-1))
C     PRINT *,'OFF-DIAG : SUM =',OFF ,'     AVERAGE =',XX

      deallocate( b,vec,iflag,stat=ii)
      if(ii>0)stop 'dealloc invrt'

      RETURN
      END subroutine invrt








