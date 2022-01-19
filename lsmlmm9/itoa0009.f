      SUBROUTINE ITOA(M,IA,N)                                                             
C-----CONVERT INTEGER VARIABLE M TO ALPHA VARIABLE IA CONTAINING N CHARACTERS             
C-----                                                                                    
      IA=IHOL(M)                                                                          
C-----IA WILL BE RIGHT JUSTIFIED ,BLANK FILL                                              
C-----LEFT SHIFT FOR LEFT JUSTIFIED OUTPUT OF SIZE N                                      
      IA=SHIFT(IA,(10-N)*6)                                                               
C-----                                                                                    
      RETURN                                                                              
      END                                                                                 
