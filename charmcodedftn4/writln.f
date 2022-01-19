      SUBROUTINE WRITLN(LU,LINE,IFLAG,LENW)                                               
C-----WRITTEN N.J. 1978                                                                   
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
C-----                                                                                    
C-----WRITES A FORMATTED LINE IMAGE OF LENGTH LENW WORDS                                  
C----- FROM ARRAY LINE( ) ONTO UNIT LU                                                    
C-----CARRIAGE CONTROL INSERTED IF IFLAG=0                                                
C-----                                                                                    
      DIMENSION LINE(1)                                                                   
      IF(IFLAG) 2,1,2                                                                     
    1 WRITE(LU,1001)(LINE(I),I=1,LENW)                                                    
 1001 FORMAT(1H ,14A10)                                                                   
      GO TO 3                                                                             
    2 WRITE(LU,1002)(LINE(I),I=1,LENW)                                                    
 1002 FORMAT(14A10)                                                                       
    3 RETURN                                                                              
      END                                                                                 
