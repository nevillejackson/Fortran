*TEXT                                                                                     
      SUBROUTINE ITOA (M1, M2, N)                                       DEC03000          
C      SUBROUTINE TO CONVERT INTERGER VARIABLE TO ALPHANUMERIC VARIABLE DEC03001          
C      M1 IS THE INPUT VARIABLE                                         DEC03002          
C      M2 IS THE OUTPUT VARIABLE                                        DEC03003          
C      N IS THE NUMBER OF DIGITS IN THE OUTPUT VARIABLE                 DEC03004          
C      IF THE NUMBER OF DIGITS IN M1 IS GREATER THAN THE NUMBER OF      DEC03005          
C        DIGITS IN M2 THE LOW ORDER DIGITS OF M1 ARE TRUNCATED SO THAT  DEC03006          
C        THE TWO NUMBER HAVE THE SAME NUMBER OF DIGITS                  DEC03007          
      DIMENSION IOCT (9)                                                DEC03008          
      DATA (IOCT=01B,02B,03B,04B,05B,06B,07B,10B,11B)                   M0103009          
      DATA (IBLANK=60B)                                                 M0103010          
      DATA (IZERO=00B)                                                  M0103010          
      DATA (MCK=0037777777777777B)                                      M0103010          
C      CHECK FOR NUMBER OF SIGNIFICANT DIGITS                           DEC03011          
      M3=M1                                                             DEC03012          
      MR = M1                                                           DEC03013          
      J=0                                                               DEC03014          
      DO 4 I=1,11                                                       DEC03015          
      J=J+1                                                             DEC03016          
      M3=M3/10                                                          DEC03017          
    4 IF (IABS(M3).EQ.0) GO TO 6                                        DEC03018          
      NN=11                                                             DEC03019          
      GO TO 7                                                           DEC03020          
    6 NN=J                                                              DEC03021          
C      DETERMINE IF THERE ARE MORE DIGITS IN THE INPUT VARIABLE THAN    DEC03022          
C        DESIRED IN THE OUTPUT VARIABLE                                 DEC03023          
    7 IF (NN.LE.N) GO TO 8                                              DEC03024          
      NN=NN-N                                                           DEC03025          
C      REMOVE LOW ORDER DIGITS OF THE INPUT VARIABLE IF NEEDED          DEC03026          
      MR=MR/10**NN                                                      DEC03027          
C      CONVERT INTERGER VARIABLE TO ALPHANUMERIC VARIABLE               DEC03028          
    8 M2=0                                                              DEC03029          
      NS = 0                                                            DEC03030          
      ICK = 0                                                           DEC03031          
      DO 10 I=1,N                                                       DEC03032          
      MM = MR/(10**(N - I))                                             DEC03033          
      MR=MR-(MM*10**(N-I))                                              DEC03034          
      IF (M2.GT.MCK) NS = 1                                             DEC03035          
      IF (ICK.EQ.0.AND.MM.NE.0.OR.I.EQ.N) ICK = 1                       DEC03036          
      IF (MM.EQ.0.AND.ICK.EQ.0) M2 =IABS(M2*2**6) +IBLANK               DEC03037          
      IF(MM.EQ.0.AND.ICK.EQ.1)  M2=IABS(M2*2**6) + IZERO                M0103038          
      IF (MM.NE.0) M2 =IABS(M2*2**6) + IOCT(MM)                         DEC03039          
   10 CONTINUE                                                          DEC03040          
      IF (N.EQ.6) GO TO 20                                              DEC03041          
C      ADD LOW ORDER BLANKS WHEN NEEDED                                 DEC03042          
      N1 = N + 1                                                        DEC03043          
      DO 15 I=N1,6                                                      DEC03044          
      IF (M2.GT.MCK) NS = 1                                             DEC03045          
   15 M2 =IABS(M2*2**6) +IBLANK                                         DEC03046          
C      SET SIGN BIT                                                     DEC03047          
   20 IF (NS.EQ.1) M2 = -M2                                             DEC03048          
      M3=SHIFT(M2,12)                                                   M0103048          
      M2=M3                                                             M0103048          
      RETURN                                                            DEC03049          
      END                                                               DEC03050          
*ENDTEXT                                                                                  
