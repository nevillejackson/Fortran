      FUNCTION DSQRT(X)                                                                   
      IF(X) 1,1,2                                                                         
    1 DSQRT=0.0                                                                           
      GO TO 3                                                                             
    2 DSQRT=SQRT(X)                                                                       
    3 RETURN                                                                              
      END                                                                                 
