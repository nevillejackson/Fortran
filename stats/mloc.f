      FUNCTION MLOC(I,NI,NFAC)                                                            
      DIMENSION I(1),NI(1)                                                                
      LOC=0                                                                               
      DO 1 IT=1,NFAC                                                                      
      JL=IT-1                                                                             
      IF(JL) 3,3,4                                                                        
    3 M=I(IT)                                                                             
      GO TO 1                                                                             
    4 M=I(IT)-1                                                                           
      DO 2 JT=1,JL                                                                        
    2 M=M*NI(JT)                                                                          
    1 LOC=LOC+M                                                                           
      MLOC=LOC                                                                            
      RETURN                                                                              
      END                                                                                 
