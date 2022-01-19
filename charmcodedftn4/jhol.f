      FUNCTION JHOL(INT)                                                                  
C-----WRITTEN N.J. 1978                                                                   
C-----MACHINE DEPENDENT ROUTINE - CYBER 76 VERSION                                        
C-----CONVERTS A SINGLE DECIMAL DIGIT INTO A SINGLE HOLLERITH                             
C-----CHAR RIGHT JUSTIFIED ZERO FILLED                                                    
      COMMON /CONST/ KONST(64)                                                            
      JHOL=INT+KONST(28)                                                                  
      RETURN                                                                              
      END                                                                                 
