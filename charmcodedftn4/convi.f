      SUBROUTINE CONVI(INTEGR,CARD,INDEX)                                                 
C                                                                                         
      DIMENSION CARD( 1 ),STRING( 3 )                                                     
      INTEGER CARD,STRING                                                                 
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C                                                                                         
C     CONVI                             WRITTEN 1973 AUTHOR J.N.                          
C     -----                                                                               
C                                                                                         
C     CALL CONVI( N, CARD, INDEX )                                                        
C                                                                                         
C     CONVERTS A BINARY NUMBER 'N' TO DISPLAY CODE                                        
C     UPDATES 'INDEX' ON EXIT                                                             
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     N      NUMBER TO BE CONVERTED                                                       
C     CARD   DESTINATION FOR DISPLAY CODE                                                 
C     INDEX  START OF FIELD                                                               
C                                                                                         
      CALL MCHL(46,IMIN,1)                                                                
      CALL MCH(39,IMIN,1)                                                                 
      NUMBER=INTEGR                                                                       
      L = 0                                                                               
      NEG = 0                                                                             
      IF( NUMBER .GE. 0 ) GOTO 100                                                        
      NUMBER = -NUMBER                                                                    
      NEG = 1                                                                             
C                                                                                         
  100 K=MOD(NUMBER,10)                                                                    
      NUMBER=NUMBER/10                                                                    
      L = L + 1                                                                           
      CALL OUT(STRING,L,JHOL(K))                                                          
      IF( NUMBER .NE. 0 ) GOTO 100                                                        
      IF(NEG.EQ.1) CALL COPYST(IMIN,1,CARD,INDEX)                                         
      K = L                                                                               
C                                                                                         
      DO 200 I = 1,L                                                                      
      CALL OUT( CARD, INDEX, IN( STRING,K ) )                                             
      K = K - 1                                                                           
200   INDEX = INDEX + 1                                                                   
C                                                                                         
      RETURN                                                                              
C                                                                                         
      END                                                                                 
