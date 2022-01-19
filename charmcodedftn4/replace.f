      SUBROUTINE REPLAC(THIS,WITH,L,CARD,IFLAG,LREC)                                      
      DIMENSION THIS( 1 ),WITH( 1 ),CARD( 1 )                            REPLACE          
      INTEGER THIS,WITH ,CARD                                                             
C                                                                                         
C     REPLACE                           WRITTEN 1974 AUTHOR J.N.                          
C     -------                                                                             
C                                                                                         
C                                                                                         
C     ROUTINE TO SEARCH 'CARD' FOR THE CHARACTER STRING 'THIS' OF                         
C     LENGTH 'L' CHARACTERS.  IF FOUND REPLACES STRING BY STRING IN                       
C     'WITH' AND RETURNS IFLAG = 1.  ELSE ON EXIT IFLAG = 0                               
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     THIS   TARGET TEXT STRING                                                           
C     WITH   REPLACEMENT STRING                                                           
C     L      LENGTH OF 'THIS' AND 'WITH'                                                  
C     CARD   SOURCE TEXT                                                                  
C     IFLAG  ON EXIT = 1 IF REPLACEMENT OCCURRED ELSE= 0                                  
C                                                                                         
      IFLAG = 0                                                          REPLACE          
      M=LREC+1-L                                                                          
C                                                                        REPLACE          
      DO 300 I = 1,M                                                     REPLACE          
      K = 1                                                              REPLACE          
      MI = I + L - 1                                                     REPLACE          
C                                                                        REPLACE          
      DO 200 J = I,MI                                                    REPLACE          
      IF( IN( THIS,K ) .NE. IN( CARD,J ) ) GOTO 300                      REPLACE          
200   K = K + 1                                                          REPLACE          
      GOTO 400                                                           REPLACE          
300   CONTINUE                                                           REPLACE          
      RETURN                                                             REPLACE          
400   IFLAG = 1                                                          REPLACE          
      CALL COPYC(WITH,1,CARD,I,L)                                                         
      RETURN                                                             REPLACE          
      END                                                                REPLACE          
