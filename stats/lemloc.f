      FUNCTION LEMLOC(NFAC,NI,INT,INTY,LEVN,NK)                                           
C-----CALCULATE SUBSCRIPT NO OF ELEMENTS WITHIN TABLE INT OF TYPE INTY                    
C-----LEVN GIVES LEVEL NO OF EACH FACTOR IN TABLE                                         
      DIMENSION NI(1),LEVN(1),NK(1)                                                       
      IF(INTY) 1,1,2                                                                      
    1 LEMLOC=1                                                                            
      RETURN                                                                              
    2 L=1                                                                                 
      JNTYMX=INTY-1                                                                       
      IF(JNTYMX) 3,3,4                                                                    
C-----ADD OVER ALL TYPES LOWER THAN INTY                                                  
    4 DO 5 JNTY=1,JNTYMX                                                                  
      JNTMX=ICOMB(NFAC,JNTY)                                                              
      DO 5 JNT=1,JNTMX                                                                    
    5 L=L+IELNO(NFAC,JNT,JNTY,NI)                                                         
C-----ADD OVER TABLES WITHIN INTY UP TO TABLE LOWER THAN INT                              
    3 JNTMX=INT-1                                                                         
      IF(JNTMX) 6,6,7                                                                     
    7 DO 8 JNT=1,JNTMX                                                                    
    8 L=L+IELNO(NFAC,JNT,INTY,NI)                                                         
C-----ADD OVER ELEMENTS WITHIN TABLE INT                                                  
    6 L=L+MLOC(LEVN,NK,INTY)                                                              
C-----                                                                                    
      LEMLOC=L                                                                            
      RETURN                                                                              
      END                                                                                 
