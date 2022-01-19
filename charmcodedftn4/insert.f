      SUBROUTINE INSERT(CARD,I,STRING,L,LREC)                                             
C                                                                        INSERT           
      DIMENSION CARD( 1 ),STRING( 1 )                                    INSERT           
      INTEGER CARD,STRING                                                                 
C                                                                                         
C     INSERT                            WRITTEN 1974 AUTHOR J.N.                          
C                                        MODIFIED 1977 N.J.                               
C     ------                                                                              
C                                                                                         
C     CALL INSERT (CARD,I,STRING,L,LREC)                                                  
C                                                                                         
C     ROUTINE TO INSERT THE 'STRING' OF LENGTH 'L' IN 'CARD' STARTING                     
C     AT CARD( I+1 ) AND ENDING AT CARD( I+L )                                            
C     CARD ASSUMED TO BE LREC CHARACTERS LONG . IF STRING OVERFLOWS                       
C     THEN IT IS TRUNCATED.  CARD IS SHIFTED RIGHT TO ACCOMMODATE                         
C     AND 'L' CHARACTERS LOST.                                                            
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     CARD    SOURCE TEXT                                                                 
C     I       INDEXES CARD                                                                
C     STRING  INSERTION TEXT                                                              
C     L       LENGTH OF STRING IN CHARS                                                   
C     LREC    LENGTH OF CARD IN CHARS                                                     
C                                                                                         
      II = I                                                             INSERT           
      LL = L                                                             INSERT           
C                                       IGNORE STUPID LENGTH             INSERT           
      IF( LL .LE. 0 ) RETURN                                             INSERT           
C                                       CANT INSERT AFTER COL LREC                        
      IF(II.GT.(LREC-1)) RETURN                                                           
C                                       TRIM STRING LENGTH IF IT         INSERT           
C                                       OVERFLOWS BEYOND COLUMN LREC                      
      IF(LL+II.GT.LREC) LL=LREC-II                                                        
      M=LREC-LL                                                                           
C                                        MOVE UP 'LL' CHARACTERS TO      INSERT           
C                                       MAKE GAP FOR STRING              INSERT           
      IF(M.LE.II) GO TO 101                                                               
100   CALL OUT( CARD, M+LL, IN( CARD,M ) )                               INSERT           
      M = M - 1                                                          INSERT           
      IF( M .GT. II ) GOTO 100                                           INSERT           
C                                       COPY THE STRING                  INSERT           
  101 CONTINUE                                                                            
      DO 200 K = 1,LL                                                    INSERT           
      M = M + 1                                                          INSERT           
200   CALL OUT( CARD, M, IN( STRING,K ) )                                INSERT           
C                                                                        INSERT           
      RETURN                                                             INSERT           
C                                                                        INSERT           
      END                                                                INSERT           
