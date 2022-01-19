      FUNCTION IEQ(LIST,NL,IVAL)                                                          
      DIMENSION LIST(1)                                                                   
C-----ASKS WHETHER IVAL IS IN LIST( )                                                     
C-----RETURNS 0 = NO MATCH                                                                
C-----        I = MATCHES ITH ELEMENT                                                     
C-----LIST() AND IVAL ARE INTEGERS NOT STRINGS                                            
      IEQ=0                                                                               
      DO 1 I=1,NL                                                                         
      IF(IVAL-LIST(I)) 1,2,1                                                              
    1 CONTINUE                                                                            
      RETURN                                                                              
    2 IEQ=I                                                                               
      RETURN                                                                              
      END                                                                                 
