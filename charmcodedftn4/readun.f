      SUBROUTINE READUN(LU,LIT,IFLAG,LENW)                                                
C-----WRITTEN N.J. 1978                                                                   
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
C-----                                                                                    
C-----READS A CHARM RECORD OF LERNGTH LENW WORDS FROM UNIT LU                             
C----- INTO ARRAY LIT( )                                                                  
C-----RETURNS IFLAG=-1 IF EOF                                                             
C-----        IFLAG=0  IF OK                                                              
C-----        IFLAG=+1 IF PARITY ERROR                                                    
C-----                                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      DIMENSION LIT(1)                                                                    
      K=0                                                                                 
   10 IF(LENW) 999,20,22                                                                  
   20 READ(LU,100)                                                                        
      GO TO 21                                                                            
   22 READ(LU,100)(LIT(I),I=1,LENW)                                                       
  100 FORMAT(100A10)                                                                      
   21 IFLAG=0                                                                             
      IF(EOF(LU)) 1,2,1                                                                   
    1 IFLAG=-1                                                                            
      GO TO 4                                                                             
    2 IF(IOCHEC(LU)) 3,4,3                                                                
    3 K=K+1                                                                               
      IF(K-10) 13,13,14                                                                   
   13 BACKSPACE LU                                                                        
      GO TO 10                                                                            
   14 IFLAG=1                                                                             
      WRITE(LP,15) LU                                                                     
   15 FORMAT(27H0READ PARITY ERROR ON UNIT ,I4)                                           
      CALL JOBEND                                                                         
    4 RETURN                                                                              
  999 CALL LOGIC(6HREADUN)                                                                
      RETURN                                                                              
      END                                                                                 
