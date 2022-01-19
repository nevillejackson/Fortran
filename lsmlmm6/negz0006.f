*TEXT                                                                                     
      SUBROUTINE NEGZ  (T,BEG,LFD,LIT,I)                                DK110010          
C     -------------------------------------------                       DK110020          
C     SUBROUTINE USED TO CHECK FOR 1620 TYPE NEGATIVE NUMBERS           DK110030          
C     -------------------------------------------                       DK110040          
      REAL*8 T                                                          DK110050          
      INTEGER BEG                                                       DK110060          
      DIMENSION LIT(480),BEG(90),LFD(90)                                DK110070          
      M=BEG(I)+LFD(I)-1                                                 DK110080          
      T=1.0                                                             DK110090          
      KC=LIT(M)/16                                                      DK110100          
      LIT(M)=LIT(M)-KC*16                                               DK110110          
      IF (KC.GT.0) T=-1.0                                               DK110120          
      RETURN                                                            DK110130          
      END                                                               DK110140          
*ENDTEXT                                                                                  
