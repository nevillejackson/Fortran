*TEXT                                                                                     
      SUBROUTINE NEGN(T,BEG,LFD,LIT,I)                                    120001          
C     SUBROUTINE USED TO CHECK FOR FORTRAN TYPE NEGATIVE NUMBERS          120002          
C     ----------------------------------------------------                120003          
      INTEGER BEG                                                         120004          
      DIMENSION LIT(1),BEG(1),LFD(1)                                      120005          
      DATA(IBLK=1H )                                                      120006          
      DATA(IMIN=1H-)                                                      120007          
      M=BEG(I)                                                            120008          
      M1=M+LFD(I)-1                                                       120009          
    1 IF (LIT(M).NE.IBLK) GO TO 5                                         120010          
      IF (M.EQ.M1) GO TO 10                                               120011          
      LIT(M)=0                                                            120012          
      M=M+1                                                               120013          
      GO TO 1                                                             120014          
    5 IF (LIT(M).NE.IMIN) GO TO 8                                         120015          
      LIT(M)=0                                                            120016          
      T=-1.                                                               120017          
      GO TO 10                                                            120018          
    8 T=1.                                                                120019          
   10 CONTINUE                                                            120020          
      RETURN                                                              120021          
      END                                                                 120022          
*ENDTEXT                                                                                  
