      FUNCTION LENGS( RECORD, INDEX, LENGTH )                                             
C                                                                                         
      DIMENSION RECORD( 1 )                                                               
      INTEGER RECORD                                                                      
C                                                                                         
C     LENGS                             WRITTEN 1974 AUTHOR J.N.                          
C     -----                                                                               
C                                                                                         
C     K = LENGS( RECORD, INDEX, LENGTH )                                                  
C                                                                                         
C     EXAMINES STRING STARTING AT RECORD ( INDEX ).  ( TREATS RECORD AS                   
C     CHARACTER ARRAY. )  STRING OF 'LENGTH' CHARACTERS.  SCANS BACK-                     
C     WARDS FROM END OF STRING.  RETURNS LENGTH OF STRING WITHOUT                         
C     TRAILING BLANKS.  EXAMPLE OF USE :-                                                 
C             K = LENGS( CARD, 1, 80 )                                                    
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     RECORD  USED AS CHARACTER ARRAY - MAY BE ANY LENGTH                                 
C             LOOKS FOR TRAILING BLANKS ON A CARD.                                        
C     INDEX   INFORMATION STATE AT RECORD ( INDEX )                                       
C     LENGTH  LENGTH OF INFORMATION STATE AT RECORD ( INDEX )                             
C                                                                                         
      M = LENGTH - 1                                                                      
C                                                                                         
      DO 100 J = 1,LENGTH                                                                 
      IF( LETTER( IN( RECORD, INDEX+M ) ) ) 100,200,200                                   
C                                        BLANK, NON BLANK                                 
100   M = M - 1                                                                           
C                                                                                         
200   LENGS = M + 1                                                                       
      RETURN                                                                              
C                                                                                         
      END                                                                                 
