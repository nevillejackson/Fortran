      SUBROUTINE ITOA (M1, M2, N)                                         050001          
C      SUBROUTINE TO CONVERT INTERGER VARIABLE TO ALPHANUMERIC VARIABLE   050002          
C      M1 IS THE INPUT VARIABLE                                           050003          
C      M2 IS THE OUTPUT VARIABLE                                          050004          
C      N IS THE NUMBER OF DIGITS IN THE OUTPUT VARIABLE                   050005          
C      IF THE NUMBER OF DIGITS IN M1 IS GREATER THAN THE NUMBER OF        050006          
C        DIGITS IN M2 THE LOW ORDER DIGITS OF M1 ARE TRUNCATED SO THAT    050007          
C        THE TWO NUMBER HAVE THE SAME NUMBER OF DIGITS                    050008          
      DIMENSION IOCT (9)                                                  050009          
      DATA (IOCT=34B,35B,36B,37B,40B,41B,42B,43B,44B)                     050010          
      DATA (IBLANK=55B)                                                   050011          
      DATA (MCK=00377777777777777777B)                                    050012          
C      CHECK FOR NUMBER OF SIGNIFICANT DIGITS                             050013          
      M3=M1                                                               050014          
      MR = M1                                                             050015          
      J=0                                                                 050016          
      DO 4 I=1,11                                                         050017          
      J=J+1                                                               050018          
      M3=M3/10                                                            050019          
      IF(IABS(M3).EQ.0) GO TO 6                                           050020          
    4 CONTINUE                                                            050021          
      NN=11                                                               050022          
      GO TO 7                                                             050023          
    6 NN=J                                                                050024          
C      DETERMINE IF THERE ARE MORE DIGITS IN THE INPUT VARIABLE THAN      050025          
C        DESIRED IN THE OUTPUT VARIABLE                                   050026          
    7 IF (NN.LE.N) GO TO 8                                                050027          
      NN=NN-N                                                             050028          
C      REMOVE LOW ORDER DIGITS OF THE INPUT VARIABLE IF NEEDED            050029          
      MR=MR/10**NN                                                        050030          
C      CONVERT INTERGER VARIABLE TO ALPHANUMERIC VARIABLE                 050031          
    8 M2=0                                                                050032          
      NS = 0                                                              050033          
      ICK = 0                                                             050034          
      DO 10 I=1,N                                                         050035          
      MM = MR/(10**(N - I))                                               050036          
      MR=MR-(MM*10**(N-I))                                                050037          
      IF (M2.GT.MCK) NS = 1                                               050038          
      IF (ICK.EQ.0.AND.MM.NE.0.OR.I.EQ.N) ICK = 1                         050039          
      IF (MM.EQ.0.AND.ICK.EQ.0) M2 =IABS(M2*2**6) +IBLANK                 050040          
      IF(MM.EQ.0.AND.ICK.EQ.1) M2=IABS(M2*2**6)+33B                       050041          
      IF (MM.NE.0) M2 =IABS(M2*2**6) + IOCT(MM)                           050042          
   10 CONTINUE                                                            050043          
      IF (N.EQ.6) GO TO 20                                                050044          
C      ADD LOW ORDER BLANKS WHEN NEEDED                                   050045          
      N1 = N + 1                                                          050046          
      DO 15 I=N1,6                                                        050047          
      IF (M2.GT.MCK) NS = 1                                               050048          
   15 M2 =IABS(M2*2**6) +IBLANK                                           050049          
C      SET SIGN BIT                                                       050050          
   20 IF (NS.EQ.1) M2 = -M2                                               050051          
C-----THE FOLLOWING STATEMENT APPLIES TO FORTRAN EXTENDED COMPILER ONLY   050052          
C-----FOR RUN FORTRAN COMPILER REPLACE BY   M3=LS(M2,24)                  050053          
      M3=SHIFT(M2,24)                                                     050054          
      M2=M3                                                               050055          
      RETURN                                                              050056          
      END                                                                 050057          
