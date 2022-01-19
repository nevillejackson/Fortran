      FUNCTION INUM(I)                                                                    
C-----CONVERTS I FROM HOLLERITH TO INTEGER                                                
C-----I MUST BE RIGHT JUSTIFIED          BLANK FILL                                       
C-----MAY BE ANY NO OF DIGITS IN FIELD UP TO 10                                           
      DECODE(10,1000,I) INUM                                                              
 1000 FORMAT(I10)                                                                         
      RETURN                                                                              
C-----CONVERTS I FROM INTEGER TO HOLLERITH                                                
C-----IHOL WILL BE RIGHT JUSTIFIED ,BLANK FILL                                            
      ENTRY IHOL                                                                          
      ENCODE(10,1000,INUM) I                                                              
      RETURN                                                                              
      END                                                                                 
