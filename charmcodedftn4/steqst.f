      SUBROUTINE STEQST(RECORD,INDEX,S1,L1,S2,L2,LREC)                                    
C                                                                                         
      DIMENSION RECORD( 1 ),S1( 1 ),S2( 1 )                                               
      INTEGER RECORD,S1,S2                                                                
C                                                                                         
C     STEQST                            WRITTEN 1974 AUTHOR J.N.                          
C     ------                                                                              
C                                                                                         
C                                                                                         
C     SEARCHES RECORD FROM RECORD(INDEX) TO RECORD(LREC) FOR A                            
C     PARAMETER OF THE FORM                                                               
C                                                                                         
C                       STRING = STRING                                                   
C                           S1 = S2                                                       
C                                                                                         
C     IF SUCCESSFUL RETURNS STRINGS IN 'S1' AND 'S2' AND CORRESPONDING                    
C     LENGTHS IN 'L1' AND 'L2'.  IF NO STRINGS L1=L2=0.  IF NO S2 THEN                    
C     L2=0 ( BUT = MUST BE PRESENT ELSE L1=0 )                                            
C     IF L1 NON ZERO INDEX IS UPDATED                                                     
C                                                                                         
C     EXAMPLES                                                                            
C                                                                                         
C     STRING = STRING          L1 = 6   L2 = 6                                            
C       STR= STRING            L1 = 3   L2 = 6                                            
C     STRI=STRINGING           L1 = 4   L2 = 9                                            
C     STRING =                 L1 = 6   L2 = 0                                            
C     STRING                   L1 = 0   L2 = 0                                            
C       =  STRING              L1 = 0   L2 = 6                                            
C           =                  L1 = 0   L2 = 0                                            
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     RECORD  TEXT TO BE SCANNED                                                          
C     INDEX   SCAN STARTS AT RECORD (INDEX)                                               
C     S1      STRING BEFORE 'EQUALS'                                                      
C     L1      LENGTH OF 'S1'                                                              
C     S2      STRING AFTER 'EQUALS'                                                       
C     L2      LENGTH OF 'S2'                                                              
C                                                                                         
C                                                                                         
      L1 = 0                                                                              
      L2 = 0                                                                              
      I = INDEX                                                                           
C                                       GET FIRST STRING                                  
      CALL NEXT(RECORD,I,S1,L,LREC)                                                       
C                                       RETURN IF EMPTY RECORD                            
      IF( L .EQ. 0 ) RETURN                                                               
C                                       LOOK FOR EQUALS SIGN IMBEDDED                     
      DO 200 J = 1,L                                                                      
C                                                                                         
      IF( ITYPE( IN( S1,J ) ) .EQ. 5 ) GOTO 500                                           
200   CONTINUE                                                                            
C                                       NO = FOUND SO S1 IS OK.  NOW LET                  
C                                       US LOOK FOR = IN RECORD                           
      CALL NEXT(RECORD,I,S2,LL,LREC)                                                      
C                                       FIRST CHARACTER MUST BE =                         
      IF( ITYPE( IN( S2,1 ) ) .NE. 5 ) RETURN                                             
C                                       STEP OVER = SIGN                                  
      I = I - LL + 1                                                                      
400   L1 = L                                                                              
C                                       GET SECOND STRING                                 
      CALL NEXT(RECORD,I,S2,L,LREC)                                                       
C                                                                                         
      IF( L .EQ. 0 ) RETURN                                                               
      L2 = L                                                                              
450   INDEX = I                                                                           
      RETURN                                                                              
C                                                                                         
C     HAVE EITHER EMBEDDED = OR LAST =                                                    
500   IF( J .NE. L ) GOTO 600                                                             
      L = L - 1                                                                           
      GOTO 400                                                                            
C                                       S2 IS IN S1                                       
600   L1 = J - 1                                                                          
      L2 = L - J                                                                          
C                                       COPY S2 FROM S1                                   
      CALL COPYC (S1,J+1,S2,1,L2)                                                         
      GOTO 450                                                                            
C                                                                                         
      END                                                                                 
