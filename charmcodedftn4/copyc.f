      SUBROUTINE COPYC(SOURCE,I,DEST,J,NCHARS)                                            
      DIMENSION SOURCE(1),DEST(1)                                                         
      INTEGER SOURCE,DEST                                                                 
C                                                                                         
C     COPYC                                                                               
C     ----                                                                                
C                                                                                         
C     CALL COPYC(SOURCE,I,DEST,J,NCHARS)                                                  
C                                                                                         
C     ROUTINE TO COPY "NCHARS" FROM SOURCE ARRAY STARTING AT                              
C     CHARACTER SOURCE( I ) TO DESTINATION ARRAY STARTING                                 
C     AT DEST( J )                                                                        
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     SOURCE  SOURCE CHARACTER STRING                                                     
C     I       INDEXES 'SOURCE'                                                            
C     DEST    DESTINATION ARRAY                                                           
C     J       INDEXES 'DEST'                                                              
C     NCHARS  NUMBER OF CHARACTERS TO BE COPIED                                           
C                                                                                         
      IF( NCHARS .LE. 0 ) GOTO 200                                                        
      L = I                                                                               
      M = J                                                                               
C                                                                                         
      DO 100 K = 1,NCHARS                                                                 
      CALL OUT( DEST, M, IN( SOURCE, L ) )                                                
      L = L + 1                                                                           
100   M = M + 1                                                                           
C                                                                                         
200   RETURN                                                                              
C                                                                                         
      END                                                                                 
