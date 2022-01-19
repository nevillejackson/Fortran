      SUBROUTINE NEXT(ARRAY,I,STRING,N,LREC)                                              
C                                                                        NEXT             
      DIMENSION ARRAY( 1 ),STRING( 1 )                                   NEXT             
      INTEGER ARRAY,STRING                                                                
C                                                                                         
C     NEXT                             WRITTEN 1973 AUTHOR J.N.                           
C     ----                                                                                
C                                                                                         
C                                                                                         
C-----ROUTINE TO SCAN ARRAY FROM ARRAY(I) TO ARRAY(LREC). IF STRING                       
C     FOUND RETURNS IT IN STRING AND LENGTH IN N.  STRING TERMINATED B                    
C     A BLANK.  N = 0 IF NO STRING FOUND.  I IS UPDATED ON EXIT TO BE                     
C     LREC OR TO POINT TO BLANK FOLLOWING STRING                                          
C     LEADING BLANKS SKIPPED                                                              
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     ARRAY    TREATED AS CHARACTER ARRAY                                                 
C     I        POINTS TO NEXT CHARACTER ON ARRAY. UPDATED ON EXIT                         
C     STRING   HOLD ANY STRING FOUND                                                      
C     N        NUMBER OF CHARACTERS IN THE STRING OR ZERO                                 
C                                                                                         
      N = 0                                                              NEXT             
C                                       DON'T BOTHER IF ALREADY          NEXT             
C                                       AT END OF CARD                   NEXT             
      IF(I.GT.LREC-1) GO TO 150                                                           
      J = I                                                              NEXT             
C                                       TEST FOR NON BLANK               NEXT             
100   IF( LETTER( IN( ARRAY,J ) ) .NE. -1 ) GOTO 300                     NEXT             
C                                       BLANK FOUND,  KEEP LOOKING       NEXT             
      J = J + 1                                                          NEXT             
      IF(J.LT.LREC+1) GO TO 100                                                           
C                                       END OF CARD,  NO FIELD           NEXT             
  150 J=LREC                                                                              
C                                       UPDATE POINTER 'I'               NEXT             
200   I = J                                                              NEXT             
      RETURN                                                             NEXT             
C                                       PICK UP REST OF STRING           NEXT             
300   N = N + 1                                                          NEXT             
C                                       COPY THE CHARACTERS              NEXT             
      CALL OUT( STRING, N, IN( ARRAY,J ) )                               NEXT             
      J = J + 1                                                          NEXT             
      IF(J.GT.LREC) GO TO 200                                                             
C                                       LOOK FOR TERMINATING BLANK       NEXT             
      IF(LETTER(IN(ARRAY,J))+1) 300,200,300                                               
C                                                                        NEXT             
      END                                                                NEXT             
