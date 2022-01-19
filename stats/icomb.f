      FUNCTION ICOMB(IN,IR)                                                               
      IF(IR)2,2,3                                                                         
    2 ICOMB=1                                                                             
      RETURN                                                                              
    3 FN=1                                                                                
      DO 1 I=1,IR                                                                         
    1 FN=FN*FLOAT(IN-I+1)/FLOAT(I)                                                        
      ICOMB=FN+.1                                                                         
      RETURN                                                                              
      END                                                                                 
