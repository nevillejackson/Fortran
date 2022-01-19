*TEXT                                                                                     
      SUBROUTINE NEGF (T,BEG,LFD,LIT,I)                                 DK120010          
C     -------------------------------------------                       DK120020          
C     SUBROUTINE USED TO CHECK FOR FORTRAN TYPE NEGATIVE NUMBERS        DK120030          
C     ----------------------------------------------------              DK120040          
      REAL*8 T                                                          DK120050          
      INTEGER BEG                                                       DK120060          
      DIMENSION LIT(480),BEG(90),LFD(90)                                DK120070          
      DATA IBLK,IMIN/Z00000040,Z00000060/                               DK120080          
      M=BEG(I)                                                          DK120090          
      M1=M+LFD(I)-1                                                     DK120100          
    1 IF (LIT(M).NE.IBLK) GO TO 5                                       DK120110          
      IF (M.EQ.M1) GO TO 10                                             DK120120          
      M=M+1                                                             DK120130          
      GO TO 1                                                           DK120140          
    5 IF (LIT(M).NE.IMIN) GO TO 8                                       DK120150          
      LIT(M)=IBLK                                                       DK120160          
      T=-1.                                                             DK120170          
      GO TO 10                                                          DK120180          
    8 T=1.                                                              DK120190          
   10 CONTINUE                                                          DK120200          
      RETURN                                                            DK120210          
      END                                                               DK120220          
*ENDTEXT                                                                                  
