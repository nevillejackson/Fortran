      SUBROUTINE PRNTSI(LU,ISEQ,LIT,LEN,JH)                                               
C-----WRITTEN N.J. 1978                                                                   
C-----MACHINE DEPENDENT ROUTINE - CYBER 76 VERSION                                        
C-----PRINTS SEQUENCED RECORDS , SEQ NO FIRST , FOR PGM COUNT                             
C-----WHOLE RECORD OR EXTRACTED FIELDS PRINTED DEPENDING ON HOW LIT                       
C-----IS SET UP                                                                           
C-----PRINTS HEADING FOR EXTRACTED FIELDS AND RIGHT JUSTIFIES FIELD                       
C-----HEADING WORDS BEFORE PRINTING ( TO LINE WITH RIGHT BOUNDRY OF                       
C-----EXTRACTED FIELD                                                                     
      DIMENSION ISEQ(1),LIT(1)                                                            
      IF(JH) 999,1,2                                                                      
    1 WRITE(LU,1000) ISEQ(1),(LIT(I),I=1,LEN)                                             
 1000 FORMAT(1H ,A10,5X,12A10,(/1H ,15X,12A10))                                           
      RETURN                                                                              
    2 WRITE(LU,1001)(ISEQ(I),I=1,JH),(JRIGHT(LIT(I)),I=1,LEN)                             
 1001 FORMAT(1H ,4X,A10,1X,12A10,(/1H ,15X,12A10))                                        
      RETURN                                                                              
  999 CALL LOGIC(6HPRNTSI)                                                                
      RETURN                                                                              
      END                                                                                 
