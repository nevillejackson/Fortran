      SUBROUTINE NEGZ(T,BEG,LFD,LIT,I)                                                    
C-----                                                                                    
C-----CHECKS FOR 1620 TYPE NEGATIVE NUMBERS                                               
C-----                                                                                    
      INTEGER BEG                                                                         
C-----RETURNS T=1. IF LAST COL OF FIELD IS NOT OVERPUNCHED 11                             
C-----RETURNS T=-1 IF LAST COL OF FIELD IS OVERPUNCHED 11                                 
      DIMENSION LIT(480),BEG(90),LFD(90)                                                  
C-----DEFINE OCTAL CONSTANTS                                                              
      DATA IZER,IZ,IONE,ININ/00000000000000000066B,00000000000000000033B                  
     1 ,00000000000000000012B,00000000000000000022B/                                      
C-----FIND LAST COL                                                                       
      M=BEG(I)+LFD(I)-1                                                                   
C-----                                                                                    
      T=1.                                                                                
      KC=IGETRZ(LIT,1,M,48)                                                               
      IF(KC.NE.IZER) GO TO 1                                                              
      KC=IPUTR(LIT,1,M,48,IZ)                                                             
      GO TO 2                                                                             
    1 IF(KC.LT.IONE.OR.KC.GT.ININ) GO TO 3                                                
      KC=KC+ININ                                                                          
      KC=IPUTR(LIT,1,M,48,KC)                                                             
    2 T=-1.                                                                               
    3 RETURN                                                                              
      END                                                                                 
