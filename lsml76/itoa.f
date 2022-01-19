      SUBROUTINE ITOA(INT,NUM)                                                            
C-----CONVERTS INTEGER INT TO CHAR STRING NUM LEFT JUST                                   
      INDEX=1                                                                             
      CALL CONVI(INT,NUM,INDEX)                                                           
C-----RIGHT JUSTIFY CHARS IN NUM FOR A6 PRINTOUT                                          
C-----BLANK FILL                                                                          
C----INDEX IS POS FOLLOWING LAST CHAR OF STRING AFTER CONVI                               
C-----IPOS IS STARTING POS FOR COPY , LENGTH IS INDEX-1                                   
      IPOS=8-INDEX                                                                        
C-----COPY CHARS                                                                          
      CALL COPYST(NUM,INDEX-1,NUM,IPOS)                                                   
C-----IPOS IS END POS FOR BLANKS                                                          
      IPOS=7-INDEX                                                                        
C-----BLANK CHARS                                                                         
      CALL SETBLK(NUM,0,IPOS)                                                             
      RETURN                                                                              
      END                                                                                 
