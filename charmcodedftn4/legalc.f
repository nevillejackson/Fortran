      SUBROUTINE LEGALC(CARD,IFLAG,LREC)                                                  
C                                                                                         
      DIMENSION CARD( 1 )                                                                 
      INTEGER CARD                                                                        
C                                                                                         
C                                                                                         
C     LEGALC                            WRITTEN 1974 AUTHOR J.N.                          
C     ------                                                                              
C                                                                                         
C     CALL LEGALC(CARD,IFLAG,LREC)                                                        
C                                                                                         
C      CHECKS FOR ILLEGAL CHARACTERS ON A CARD                                            
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     CARD   ARRAY HOLDING AN LREC COL CARD PACKRD USING                                  
C            ROUTINES 'OUT' OR 'READCRD'                                                  
C     IFLAG  ON EXIT = 0 ALL OK                                                           
C                    > 0 POINTS TO CROOK CHARACTER                                        
C                                                                                         
C                                                                                         
      DO 100 J=1,LREC                                                                     
      IF( LEGAL( IN( CARD,J ) ) .EQ. 0 ) GOTO 200                                         
100   CONTINUE                                                                            
C                                                                                         
      IFLAG = 0                                                                           
      RETURN                                                                              
C                                                                                         
200   IFLAG = J                                                                           
      RETURN                                                                              
C                                                                                         
      END                                                                                 
