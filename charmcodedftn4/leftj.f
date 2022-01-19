      SUBROUTINE LEFTJ( S, L, M )                                                         
C                                                                                         
      INTEGER S( 1 )                                                                      
C                                                                                         
C     LEFTJ                             WRITTEN 22/3/76  AUTHOR JN                        
C     -----                                                                               
C                                                                                         
C     CALL LEFTJ( S, L, M )                                                               
C                                                                                         
C     LEFT JUSTIFY TEXT IN STRING 'S' OF LENGTH 'L'.  RETURNS NUMBER                      
C     OF CHARACTERS BEFORE TRAILING BLANKS IN 'M'                                         
C                                                                                         
C     PARAMETERS                                                                          
C     S      STRING ( SET UP USING 'OUT' OR OTHERWISE ) CHARACTER ARRAY                   
C     L      LENGTH OF ARRAY 'S' IN CHARACTERS                                            
C     M      TRAILING BLANKS START AT S( M+1 )                                            
C                                                                                         
C                                       LOOK FOR TRAILING BLANKS                          
      I = L                                                                               
C                                                                                         
      DO 100 J = 1,L                                                                      
      IF( LETTER( IN( S,I ) ) .NE. -1 ) GOTO 150                                          
100   I = I - 1                                                                           
      M = 0                                                                               
      GOTO 500                                                                            
C                                       LOOK FOR LEADING BLANKS                           
150   DO 200 J = 1,I                                                                      
      IF( LETTER( IN( S,J ) ) .NE. -1 ) GOTO 300                                          
200   CONTINUE                                                                            
C                                                                                         
      CALL LOGIC( 10HLEFTJ   01 )                                                         
300   M = I - J + 1                                                                       
C                                       SHIFT STRING LEFT                                 
      IF( J .EQ. 1 ) GOTO 500                                                             
      DO 400 K = 1,M                                                                      
      CALL OUT( S, K, IN( S,J ) )                                                         
400   J = J + 1                                                                           
C                                       CLEAR JUNK                                        
      CALL SETBLK( S, M, J )                                                              
500   RETURN                                                                              
C                                                                                         
      END                                                                                 
