      SUBROUTINE PICKUP( ARRAY, I, STRING, N, LENG )                                      
C                                                                                         
      DIMENSION ARRAY( 1 ),STRING( 1 )                                                    
      INTEGER ARRAY,STRING                                                                
C                                                                                         
C     PICKUP                            WRITTEN 20/8/75/ AUTHOR J.N.                      
C     ------                                                                              
C                                                                                         
C     CALL PICKUP( ARRAY, I, STRING, N, LENG )                                            
C                                                                                         
C     SCANS ARRAY FROM CHARACTER POSITION ARRAY( I ) LOOKING FOR ANY                      
C     STRING.  RETRUNS IT IN STRING.  SKIPS LEADING BLANKS                                
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     ARRAY  STRING TO BE SEARCHED                                                        
C     I      SEARCH STARTS AT CHARACTER ARRAY( I )                                        
C     STRING IF FOUND STRING RETURNED HERE                                                
C     N      LENGTH OF 'STRING'  ZERO IF NONE                                             
C     LENG   LENGTH OF 'ARRAY'                                                            
C                                                                                         
C                                                                                         
      N = 0                                                                               
C                                       DON'T BOTHER IF ALREADY                           
C                                       AT END OF CARD                                    
      IF( I .GT. LENG ) GOTO 150                                                          
      J = I                                                                               
C                                       TEST FOR NON BLANK                                
100   IF( LETTER( IN( ARRAY,J ) ) .NE. -1 ) GOTO 300                                      
C                                       BLANK FOUND,  KEEP LOOKING                        
      J = J + 1                                                                           
      IF( J .LE. LENG ) GOTO 100                                                          
C                                       END OF CARD,  NO FIELD                            
150   J = LENG                                                                            
C                                       UPDATE POINTER 'I'                                
200   I = J                                                                               
      RETURN                                                                              
C                                       PICK UP REST OF STRING                            
300   N = N + 1                                                                           
C                                       COPY THE CHARACTERS                               
      CALL OUT( STRING, N, IN( ARRAY,J ) )                                                
      J = J + 1                                                                           
      IF( J .GT. LENG ) GOTO 200                                                          
C                                       LOOK FOR TERMINATING BLANK                        
      IF(LETTER(IN(ARRAY,J))+1) 300,200,300                                               
C                                                                                         
      END                                                                                 
