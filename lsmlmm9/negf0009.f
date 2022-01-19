      SUBROUTINE NEGF(T,BEG,LFD,LIT,I)                                                    
C-----                                                                                    
C-----  CHECKS FOR FORTRAN TYPE NEGATIVE NUMBERS                                          
C-----                                                                                    
      INTEGER BEG                                                                         
C-----RETURNS T=1 IF COLUMN M OF LIT( ) IS NOT A MINUS SIGN                               
C-----RETURNS T=-1 IF COLUMN M OF LIT( ) IS A MINUS SIGN                                  
C-----                                                                                    
      DIMENSION LIT(480),BEG(90),LFD(90)                                                  
C-----NOTE BINARY ZERO (00) AND BLANK (55) FILL BOTH PRINT AS BLANK UNDER A FMT           
C-----DEFINE OCTAL CONSTANTS WITH ZERO FILL                                               
      DATA IBLK,IMIN/00000000000000000055B,00000000000000000046B/                         
C-----RIGHT JUSTIFIED ZERO FILL WORD IS USED BECAUSE IGET WORKS FASTEST THIS WAY          
C-----GET FIELD OF LIT TO BE TESTED                                                       
      M=BEG(I)                                                                            
      M1=M+LFD(I)-1                                                                       
    1 ICOD=IGETRZ(LIT,1,M,48)                                                             
      IF(ICOD.NE.IBLK) GO TO 5                                                            
      IF(M.EQ.M1) GO TO 10                                                                
      M=M+1                                                                               
      GO TO 1                                                                             
    5 IF(ICOD.NE.IMIN) GO TO 8                                                            
      ICOD=IPUTR(LIT,1,M,48,ICOD)                                                         
      T=-1.                                                                               
      GO TO 10                                                                            
    8 T=1.                                                                                
   10 CONTINUE                                                                            
      RETURN                                                                              
      END                                                                                 
