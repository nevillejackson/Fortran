C=======================================================================
        SUBROUTINE MULABA (A,B,N)                                       
C=======================================================================
                                                                        
C     PURPOSE :  ROUTINE TO SET UP THE MATIX PRODUCT ABA, 
C                WHERE A AND B ARE HALFSTORED SYMMETRIC MATRICES.
C                THE RESULT IS A SYMMETRIC MATRIX WHICH IS RETURNED IN B
          
      INTEGER, INTENT(IN)                           :: n
      real(8), dimension( n*(n+1)/2), intent(in)    :: a
      real(8), dimension( n*(n+1)/2), intent(inout) :: b
      real(8),dimension(:,:), allocatable           :: c
      EXTERNAL ihmssf
      INTEGER :: i,j,k,ii

      allocate(c(n,n),stat=ii)
      if(ii>0)stop 'alloc mulaba'
                                                                        
      DO  I=1,N
      DO  J=1,N
      XX=0.D0                                                         
      DO  K=1,N                                                      
      XX=XX+A(ihmssf(i,k,n))*B(ihmssf(k,j,n))                   
      end do
      C(I,J)=XX                                                       
      end do
      end do
      DO I=1,N                                                      
      DO J=I,N                                                      
      XX=0.D0                                                         
      DO  K=1,N                                                      
      XX=XX+C(I,K)*A(IHMSSF(J,K,N))
      end do
      B(IHMSSF(I,J,N))=XX
      end do
      end do
      deallocate(c,stat=ii)
      if(ii>0)stop 'dealloc mulaba'
      RETURN                                                          
      END subroutine mulaba                                                             
C=======================================================================
      SUBROUTINE MABATR (A,B,IA,N)
C=======================================================================

      real(8), dimension (ia,ia), intent(in)        :: a
      real(8), dimension (n*(n+1)/2), intent(inout) :: b
      integer, intent(in)                           :: ia,n

      real(8), dimension (:,:), allocatable         :: h1,h2
      integer                                       :: i,j,ij,ii

      allocate(h1(n,n),h2(n,n),stat=ii)
      if(ii>0)stop 'mabatr : alloc'

C     POSTMULTIPLY WITH A'
      do i=1,n
      do j=1,n
      h1(i,j)=b(ihmssf(i,j,n))
      end do
      end do

      h2=matmul(h1,transpose(a(:n,:n)))

C     PREMULTIPLY WITH A
      h1=matmul(a(:n,:n),h2)
      ij=0
      do i=1,n
      do j=i,n
      ij=ij+1
      b(ij)=h1(i,j)
      end do
      end do

      deallocate(h1,h2,stat=ii)
      if(ii>0)stop 'deall mabatr'

      RETURN
      END subroutine mabatr

C=======================================================================
      SUBROUTINE MATRBA(A,B,IA,N)
C=======================================================================

!     ROUTINE TO EVALUATE THE MATRIX PRODUCT  A'BA

      real(8), dimension (ia,ia), intent(in)        :: a
      real(8), dimension (n*(n+1)/2), intent(inout) :: b
      integer, intent(in)                           :: ia,n

      real(8), dimension (:,:), allocatable         :: h1,h2
      integer                                       :: i,j,ij,ii

      allocate(h1(n,n),h2(n,n),stat=ii)
      if(ii>0)stop 'matrba : alloc'

C     POSTMULTIPLY WITH A
      do i=1,n
      do j=1,n
      h1(j,i)=b(ihmssf(i,j,n))
      end do
      end do
      h2=matmul(h1,a(:n,:n))

C     PREMULTIPLY WITH A'
      h1=matmul(transpose(a(:n,:n)),h2)
      ij=0
      do i=1,n
      do j=i,n
      ij=ij+1
      b(ij)=h1(i,j)
      end do
      end do

      deallocate(h1,h2,stat=ii)
      if(ii>0)stop 'deall matrba'

      RETURN
      END subroutine matrba












