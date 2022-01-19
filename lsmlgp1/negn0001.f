*TEXT                                                                                     
      SUBROUTINE NEGN(T,BEG,LFD,LIT,I)                                  DEC09000          
C     SUBROUTINE USED TO CHECK FOR FORTRAN TYPE NEGATIVE NUMBERS        DEC09000          
C     ----------------------------------------------------              DEC09001          
      INTEGER BEG                                                       DEC09003          
      DIMENSION LIT(480),BEG(90),LFD(90)                                DEC09004          
      DATA(IBLK=1H )                                                    M0109005          
      DATA(IMIN=1H-)                                                    M0109005          
      M=BEG(I)                                                          M0109006          
      M1=M+LFD(I)-1                                                     DEC09007          
    1 IF (LIT(M).NE.IBLK) GO TO 5                                       DEC09008          
      IF (M.EQ.M1) GO TO 10                                             DEC09009          
      LIT(M)=0                                                          DEC09010          
      M=M+1                                                             DEC09011          
      GO TO 1                                                           DEC09012          
    5 IF (LIT(M).NE.IMIN) GO TO 8                                       DEC09013          
      LIT(M)=0                                                          DEC09014          
      T=-1.                                                             DEC09015          
      GO TO 10                                                          DEC09016          
    8 T=1.                                                              DEC09017          
   10 CONTINUE                                                          DEC09018          
      RETURN                                                            DEC09019          
      END                                                               DEC09020          
*ENDTEXT                                                                                  
