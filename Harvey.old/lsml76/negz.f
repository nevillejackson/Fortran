      SUBROUTINE NEGZ  (T,BEG,LFD,LIT,I)                                DK110010          
C     -------------------------------------------                       DK110020          
C     SUBROUTINE USED TO CHECK FOR 1620 TYPE NEGATIVE NUMBERS           DK110030          
C     -------------------------------------------                       DK110040          
      INTEGER BEG                                                       DK110060          
      DIMENSION LIT(480),BEG(90),LFD(90)                                DK110070          
      M=BEG(I)+LFD(I)-1                                                 DK110080          
      T=1.0                                                             DK110090          
      IF(KOVP29(LIT,M)) 1,2,2                                                             
    1 T=-1.0                                                                              
    2 RETURN                                                                              
      END                                                               DK110140          
