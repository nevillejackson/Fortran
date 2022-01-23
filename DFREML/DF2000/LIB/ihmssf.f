C======================================================================
      INTEGER FUNCTION IHMSSF(I,J,N)
C======================================================================

C     PURPOSE : FUNCTION TO EVALUATE THE ADDRESS OF ELEMENT (I,J) IN
C               A SYMMETRIC MATRIX OF SIZE N, WHOSE UPPER TRIANGLE
C               HAS BEEN HALFSTORED ROW-WISE IN A VECTOR

C               SPECIALIZED VERSIONS OF IHMSSF HAVE BEEN SET UP FOR
C                 I = J  : IHMII
C                 I < J  : IHMIJ
C                 I > J  : IHMJI

C        I  : INTEGER*4, ROW NO.
C        J  : INTEGER*4, COLUMN NO.
C        N  : INTEGER*4, ORDER OF THE SYMMETRIC MATRIX
C----------------------------------------------------------------------

      IF(I.LE.J)THEN
         I1=I-1
         IHMSSF=N*I1-I*I1/2+J
      ELSE
         J1=J-1
         IHMSSF=N*J1-J*J1/2+I
      END IF
      RETURN
      END

C======================================================================
      INTEGER FUNCTION IHMII(I,N)
C======================================================================

C     SPECIALISED FORM OF IHMSSF FOR I=J
      IHMII=N*(I-1)+I*(3-I)/2
      RETURN
      END

C======================================================================
      INTEGER FUNCTION IHMIJ(I,J,N)
C======================================================================

C     SPECIALISED FORM OF IHMSSF FOR I<J
      I1=I-1
      IHMIJ=N*I1-I*I1/2+J
      RETURN
      END

C======================================================================
      INTEGER FUNCTION IHMJI(I,J,N)
C======================================================================

C     SPECIALISED FORM OF IHMSSF FOR I>J
      J1=J-1
      IHMJI=N*J1-J*J1/2+I
      RETURN
      END

C======================================================================
      INTEGER FUNCTION IHMI(I,N)
C======================================================================

      I1=I-1
      IHMI=N*I1-I*I1/2
      RETURN
      END

