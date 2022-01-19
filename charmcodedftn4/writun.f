      SUBROUTINE WRITUN(LU,LIT,LENW)                                                      
C-----WRITTEN N.J. 1978                                                                   
C-----PROBABLY NOT MACHINE DEPENDENT                                                      
C-----                                                                                    
C-----WERITES A CHARM RECORD OF LENGTH LENW WORDS                                         
C----- FROM ARRAY LIT( ) ONTO UNIT LU                                                     
      DIMENSION LIT(1)                                                                    
      IF(LENW) 999,1,2                                                                    
    1 WRITE(LU,100)                                                                       
      GO TO 3                                                                             
    2 WRITE(LU,100)(LIT(I),I=1,LENW)                                                      
  100 FORMAT(100A10)                                                                      
    3 RETURN                                                                              
  999 CALL LOGIC(6HWRITUN)                                                                
      RETURN                                                                              
      END                                                                                 
