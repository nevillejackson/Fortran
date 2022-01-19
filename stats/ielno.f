      FUNCTION IELNO(NFAC,NT,NTY,NI)                                                      
C-----RETURNS NO OF ELEMENTS IN TABLE NT OF TYPE NTY                                      
      DIMENSION NI(1)                                                                     
      IF(NTY) 2,2,3                                                                       
    2 IELNO=1                                                                             
      RETURN                                                                              
    3 NN=1                                                                                
      DO 4 J=1,NTY                                                                        
      K=INTFAC(NFAC,NTY,NT,J)                                                             
    4 NN=NN*NI(K)                                                                         
      IELNO=NN                                                                            
      RETURN                                                                              
      END                                                                                 
