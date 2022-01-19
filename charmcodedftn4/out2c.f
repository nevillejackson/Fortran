      SUBROUTINE OUT2C( ARRAY, I, VALUE )                                                 
      DIMENSION ARRAY( 1 )                                                                
      INTEGER VALUE                                                                       
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C                                                                                         
C     OUT2C                             WRITTEN 1974 AUTHOR J.N.                          
C     -----                                                                               
C                                                                                         
C     CALL OUT2C( ARRAY, I, VALUE )                                                       
C                                                                                         
C     TAKES THE RIGHT MOST TWO CHARACTERS FROM 'VALUE' AND INSERTS THE                    
C     CHARACTER POSITIONS ARRAY( I ) , ARRAY( I+1 )                                       
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     ARRAY  USED AS 'CHARACTER' ADDRESSED ARRAY                                          
C     I      INDEXES 'ARRAY'                                                              
C     VALUE  (INTEGER) TWO CHARACTERS RIGHT JUSTIFIED                                     
C                                                                                         
      CALL OUT( ARRAY, I, IN( VALUE, MACHC-1 ) )                                          
      CALL OUT( ARRAY, I+1, IN( VALUE, MACHC ) )                                          
C                                                                                         
      RETURN                                                                              
C                                                                                         
      END                                                                                 
