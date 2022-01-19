*TEXT                                                                                     
      SUBROUTINE ITOA (M,A,N)                                           DK040010          
C     -------------------------------------------                       DK040020          
C     SUBROUTINE TO CONVERT AN INTEGER VARIABLE TO AN ALPHANUMERIC      DK040030          
C      VARIABLE ON SYSTEM 360                                           DK040040          
C     M IS A FOUR BYTE DECIMAL INTEGER INPUT VARIABLE                   DK040050          
C     A IS AN EIGHT BYTE ALPHANUMERIC OUTPUT WORD                       DK040060          
C     L CONSISTS OR TWO FOUR BYTE WORDS WHICH ARE EQUIVALENT TO AN      DK040070          
C     EIGHT BYTE WORD                                                   DK040080          
C     HEX CONTAINS THE ALPHA NUMERIC NUMBERS                            DK040090          
C     K CONTAINS THE INDIVIDUAL DECIMAL NUMBERS                         DK040100          
C     N IS THE NUMBER OF CHARACTERS DESIRED IN THE OUTPUT VARIABLE      DK040110          
C     IC IS A COUNTER TO COUNT THE NUMBER OF OUTPUT CHARACTERS          DK040120          
C     -------------------------------------------                       DK040130          
      DIMENSION HEX(10),K(8),L(2)                                       DK040140          
      INTEGER*2 I,J,NBL,ND,II,IC,IJ                                     DK040150          
      INTEGER*4 HEX                                                     DK040160          
      REAL*8 A,B                                                        DK040170          
      EQUIVALENCE (B,L(1))                                              DK040180          
      DATA HEX/Z000000F1,Z000000F2,Z000000F3,Z000000F4,Z000000F5,Z000000DK040190          
     XF6,Z000000F7,Z000000F8,Z000000F9,Z000000F0/                       DK040200          
      DATA IBLK,MASK,MOUT,MINUS/Z00000040,Z00000080,Z0000007F,Z80000000/DK040210          
      M1=M                                                              DK040220          
      DO 5 J=1,8                                                        DK040230          
    5 K(J)=0                                                            DK040240          
      I=0                                                               DK040250          
C     -------------------------------------------                       DK040260          
C     SET EACH DECIMAL CHARACTER OF M UP IN K                           DK040270          
C     -------------------------------------------                       DK040280          
      J=9                                                               DK040290          
   10 J=J-1                                                             DK040300          
      IF (J.EQ.0) GO TO 20                                              DK040310          
      K(J)=M-(M/10)*10                                                  DK040320          
      IF (K(J).EQ.0) K(J)=10                                            DK040330          
      I=I+1                                                             DK040340          
      M=M/10                                                            DK040350          
      IF (M.EQ.0) GO TO 20                                              DK040360          
      GO TO 10                                                          DK040370          
   20 IF (I.GT.N) I=N                                                   DK040380          
      NBL=(N-I)/2                                                       DK040390          
      M=M1                                                              DK040400          
      IC=0                                                              DK040410          
C     -------------------------------------------                       DK040420          
C     ADD IN HIGH ORDER BLANKS                                          DK040430          
C     -------------------------------------------                       DK040440          
      IF (NBL.EQ.0) GO TO 22                                            DK040450          
      DO 21 IJ=1,NBL                                                    DK040460          
      IC=IC+1                                                           DK040470          
   21 K(IC)=IBLK                                                        DK040480          
C     -------------------------------------------                       DK040490          
C     STORE OUTPUT CHARACTERS IN THE CORRECT ORDER IN K                 DK040500          
C     -------------------------------------------                       DK040510          
   22 DO 23 IJ=J,8                                                      DK040520          
      IC=IC+1                                                           DK040530          
      II=K(IJ)                                                          DK040540          
   23 K(IC)=HEX(II)                                                     DK040550          
      IC=IC+1                                                           DK040560          
      IF (IC.GT.8) GO TO 25                                             DK040570          
      DO 24 IJ=IC,8                                                     DK040580          
   24 K(IJ)=IBLK                                                        DK040590          
C     -------------------------------------------                       DK040600          
C     DEVELOP FIRST HALF OF OUTPUT WORD A                               DK040610          
C     -------------------------------------------                       DK040620          
   25 L(1)=0                                                            DK040630          
      M1=0                                                              DK040640          
      IF (IAND(K(1),MASK).LE.0) GO TO 26                                DK040650          
      M1=1                                                              DK040660          
      K(1)=IAND(K(1),MOUT)                                              DK040670          
   26 DO 27 II=1,4                                                      DK040680          
   27 L(1)=L(1)*2**8+K(II)                                              DK040690          
      IF (M1.EQ.1) L(1)=IOR(L(1),MINUS)                                 DK040700          
C     -------------------------------------------                       DK040710          
C     DEVELOP SECOND HALF OF OUTPUT WORD                                DK040720          
C     -------------------------------------------                       DK040730          
      L(2)=0                                                            DK040740          
      M1=0                                                              DK040750          
      IF (IAND(K(5),MASK).LE.0) GO TO 28                                DK040760          
      M1=1                                                              DK040770          
      K(5)=IAND(K(5),MOUT)                                              DK040780          
   28 DO 29 II=5,8                                                      DK040790          
   29 L(2)=L(2)*2**8+K(II)                                              DK040800          
      IF (M1.EQ.1) L(2)=IOR(L(2),MINUS)                                 DK040810          
      A=B                                                               DK040820          
      RETURN                                                            DK040830          
      END                                                               DK040840          
*ENDTEXT                                                                                  
