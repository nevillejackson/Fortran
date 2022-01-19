      FUNCTION MATCHF(ARRAY,I,STRING,N)                                                   
C                                                                                         
      DIMENSION ARRAY( 1 ),STRING( 1 )                                                    
      INTEGER ARRAY,STRING                                                                
C                                                                                         
C     MATCH                             WRITTEN 1974 AUTHOR J.N.                          
C     -----                                                                               
C                                                                                         
C     K = MATCH( ARRAY, I, STRING, N )                                                    
C                                                                                         
C     CHARACTER HANDLING ROUTINE TO SEE IF THE 'N' CHARACTERS STARTING                    
C     AT CHARACTER POSITION ARRAY( I ) ARE THE SAME AS THOSE IN STRING                    
C                                                                                         
C     RETURNS                                                                             
C                                                                                         
C     0 NO MATCH                                                                          
C     1 MATCH FOUND                                                                       
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     ARRAY   USED AS CHARACTER ARRAY                                                     
C     I       POINTS TO CHARACTER IN ARRAY                                                
C     STRING  TO BE COMPARED                                                              
C     N       LENGTH OF STRING                                                            
C                                                                                         
      M = I                                                                               
      K = 0                                                                               
C                                                                                         
      DO 100 J =1, N                                                                      
      IF(IJCHCM(IN(ARRAY,M),IN(STRING,J))) 200,100,200                                    
100   M = M + 1                                                                           
C                                                                                         
      K = 1                                                                               
C                                                                                         
  200 MATCHF=K                                                                            
      RETURN                                                                              
C                                                                                         
      END                                                                                 
