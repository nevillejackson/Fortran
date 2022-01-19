      SUBROUTINE DATA(NFAC,NOT,LEVC,X,KEY)                                                
      INTEGER X                                                                           
      DIMENSION LEVC(1),X(1)                                                              
C-----BOTH CODES AND TRAITS IN A FORMAT                                                   
      READ(1,100)LEVC(1),X(1),X(2)                                                        
  100 FORMAT(10X,A1,13X,2A3)                                                              
      IF(EOF(1)) 10,2                                                                     
   10 KEY=1                                                                               
    2 RETURN                                                                              
      END                                                                                 
