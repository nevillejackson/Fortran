      FUNCTION INUM(I)                                                                    
C-----WRITTEN N.J. 1975                                                                   
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
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
C-----IHOLZ WILL BE RIGHT JUSTIFIED ZERO FILL                                             
      ENTRY IHOLZ                                                                         
      ENCODE(10,2000,INUM) I                                                              
 2000 FORMAT(I10.10)                                                                      
      RETURN                                                                              
      END                                                                                 
