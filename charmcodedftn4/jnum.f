      FUNCTION JNUM(ICH)                                                                  
C-----WRITTEN N.J. 1978                                                                   
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
C-----CONVERTS A SINGLE CHAR RIGHT JUSTIFIED IN A ZERO FILLED WORD                        
C----- TO AN INTEGER VARIABLE                                                             
      COMMON /CONST/ KONST(64)                                                            
      JNUM=ICH-KONST(28)                                                                  
      RETURN                                                                              
      END                                                                                 
