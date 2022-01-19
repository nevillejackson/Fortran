      FUNCTION KEYMAT(KEYP,KEYS,LEN)                                                      
C-----WRITTEN N.J. 1977                                                                   
C-----RETURNS -1 FOR KEYP.LT.KEYS                                                         
C-----         0 FOR KEYP.EQ.KEYS                                                         
C-----        +1 FOR KEYP.GT.KEYS                                                         
C-----KEYP AND KEYS LONGER THAN ONE WORD                                                  
C-----LEN IS LENGTH IN CHARACTERS                                                         
      DIMENSION KEYP(1),KEYS(1)                                                           
C-----LENGTH IN WORDS                                                                     
      LENW=LWORDS(LEN)                                                                    
C-----LOOP OVER WORDS                                                                     
      DO 1 LW=1,LENW                                                                      
      IF(IJWDCM(KEYP(LW),KEYS(LW))) 2,1,3                                                 
    1 CONTINUE                                                                            
C-----EQUAL                                                                               
      KEYMAT=0                                                                            
      GO TO 4                                                                             
C-----LESS THAN                                                                           
    2 KEYMAT=-1                                                                           
      GO TO 4                                                                             
C-----GREATER THAN                                                                        
    3 KEYMAT=1                                                                            
C-----                                                                                    
    4 RETURN                                                                              
      END                                                                                 
