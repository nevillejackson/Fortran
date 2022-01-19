      SUBROUTINE NEGF (T,BEG,LFD,LIT,I)                                 DK120010          
C     -------------------------------------------                       DK120020          
C     SUBROUTINE USED TO CHECK FOR FORTRAN TYPE NEGATIVE NUMBERS        DK120030          
C     ----------------------------------------------------              DK120040          
      INTEGER BEG                                                       DK120060          
      DIMENSION LIT(480),BEG(90),LFD(90)                                DK120070          
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      M=BEG(I)                                                          DK120090          
      M1=M+LFD(I)-1                                                     DK120100          
    1 IF(IJCHCM(IN(LIT,M),IN(KONST(46),MACHC))) 5,4,5                                     
    4 CONTINUE                                                                            
      IF (M.EQ.M1) GO TO 10                                             DK120120          
      M=M+1                                                             DK120130          
      GO TO 1                                                           DK120140          
    5 IF(IJCHCM(IN(LIT,M),IN(KONST(39),MACHC))) 8,7,8                                     
    7 CALL OUT(LIT,M,IN(KONST(46),MACHC))                                                 
      T=-1.                                                             DK120170          
      GO TO 10                                                          DK120180          
    8 T=1.                                                              DK120190          
   10 CONTINUE                                                          DK120200          
      RETURN                                                            DK120210          
      END                                                               DK120220          
