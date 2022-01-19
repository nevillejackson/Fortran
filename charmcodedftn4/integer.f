      SUBROUTINE INTEGR(CARD,I,INT,IFLAG,LREC)                                            
C                                                                        INTEGER          
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
      DIMENSION CARD(1),STRING(100)                                                       
      INTEGER CARD,STRING                                                                 
      DATA  MINUS / 1H- /                                                                 
C                                                                                         
C     INTEGR                            WRITTEN 1974 AUTHOR J.N.                          
C     -------                                                                             
C                                                                                         
C     CALL INTEGR( CARD, I, INT, IFLAG )                                                  
C                                                                                         
C     ROUTINE TO SCAN CARD FROM CARD(I) TO CARD(LREC)                                     
C     IF FIRST FIELD IS AN INTEGER IT IS CONVERTED TO BINARY                              
C     AND RETURNED THROUGH THE PARAMETER 'INT'                                            
C     INTEGER MUST BE TERMINATED BY A BLANK                                               
C     ON EXIT                                                                             
C     IFLAG = 0  NO INTEGER FOUND IN NEXT FIELD                                           
C     IFLAG > 0  HOLDS NUMBER OF CHARACTERS IN INTEGER                                    
C                FIELD AND NUMBER IS IN 'INT'                                             
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     CARD    LREC COL CARD IMAGE                                                         
C     I       POINTER TO CURRENT CHARACTER                                                
C     INT     BINARY FORM OF FIRST INTEGER FOUND                                          
C     IFLAG   ON EXIT = 0 NO INTEGER FOUND                                                
C              ELSE HOLDS NUMBER OF CHARACTER IN INTEGER                                  
C                                                                                         
C                                                                                         
      CALL NEXT(CARD,I,STRING,N,LREC)                                                     
      IF( N .EQ. 0 ) GOTO 300                                            INTEGER          
      NEG = 1                                                            INTEGER          
      NUM = 0                                                            INTEGER          
      J = 1                                                              INTEGER          
      IF( IN( STRING,1 ) .NE. IN( MINUS,1 ) ) GOTO 100                                    
      NEG = -1                                                           INTEGER          
      J = 2                                                              INTEGER          
C                                                                        INTEGER          
100   DO 200 L = J,N                                                     INTEGER          
      IF( NUMBER( IN( STRING,L ) ) .NE. 1 ) GOTO 400                     INTEGER          
C                                                                        INTEGER          
200   NUM = NUM*10 + IN( STRING,L ) - 27                                 INTEGER          
      INT = NUM*NEG                                                      INTEGER          
300   IFLAG = N                                                                           
      RETURN                                                             INTEGER          
C                                                                        INTEGER          
400   N = 0                                                              INTEGER          
      GOTO 300                                                           INTEGER          
C                                                                        INTEGER          
      END                                                                INTEGER          
