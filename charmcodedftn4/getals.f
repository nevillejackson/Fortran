      LOGICAL FUNCTION GETALS( ARRAY, START, LENGTH, PARAMS )                             
C                                                                                         
      INTEGER ARRAY( 1 ),START,PARAMS( 3 ),BLANKS,A( 8 )                                  
      LOGICAL VAL                                                                         
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      CALL MCHL(46,BLANKS,MACHC)                                                          
C                                                                                         
C     GETALS                            WRITTEM 19/11/75  AUTHOR J.N.                     
C     ------                                                                              
C                                                                                         
C     LOGIC = GETALS( ARRAY, START, LENGTH, PARAMS )                                      
C                                                                                         
C     STRIPS SHORT ALPHA-NUMERIC PARAMETERS FROM 'ARRAY'.  SCAN STARTS                    
C     AT CHARACTER POSITION ARRAY( START ).  IF ERROR FOUND RETURNS A                     
C     VALUE OF .FALSE.  RETURNS PARAMETERS LEFT JUSTIFIED IN PARAMS.                      
C     ON ENTRY PARAMS( 1 ) = LENGTH OF PARAMS AS DIMENSIONED                              
C     ON EXIT  PARAMS( 2 ) = NUMBER OF PARAMETERS RETURNED.  UNUSED                       
C     LOCATIONS ARE BLANKED OUT.  IF MORE THAN PARAMS(1)-2 PARAMETERS                     
C     FOUND THEY SKIPPED AND ERROR STATUS ( .FALSE. ) RETURNED.                           
C     RETURNS .FALSE. IF NON ALPHA NUM FOUND                                              
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     ARRAY  HOLDS STRINGS TO BE SCANNED                                                  
C     START  SCAN STARTS AT CHARACTER POSITION ARRAY ( START )                            
C     LENGTH SIZE OF ARRAY IN CHARACTERS                                                  
C     PARAMS HOLDS STRINGS FOUND                                                          
C                                                                                         
      L = PARAMS( 1 ) - 2                                                                 
      VAL = L .GT. 0                                                                      
      IF( .NOT. VAL ) GOTO 500                                                            
C                                       CLEAR PARAMS TO BLANKS                            
      DO 100 I = 1,L                                                                      
100   PARAMS( I+2 ) = BLANKS                                                              
      I = 0                                                                               
      J = START                                                                           
200   CALL PICKUP( ARRAY, J, A, N, 80 )                                                   
      IF( N.EQ. 0 ) GOTO 400                                                              
      IF( N .GT. MACHC ) N = MACHC                                                        
      CALL COPYC(A,1,PARAMS(I+3),1,N)                                                     
      I = I + 1                                                                           
      IF( I .LE. L ) GOTO 200                                                             
      I=L                                                                                 
C                                                                                         
      DO 300 K = J,LENGTH                                                                 
      VAL = ITYPE( IN( ARRAY, K ) ) .EQ. 6                                                
      IF( .NOT. VAL ) GOTO 400                                                            
300   CONTINUE                                                                            
C                                                                                         
400   PARAMS( 2 ) = I                                                                     
500   GETALS = VAL                                                                        
C                                                                                         
      RETURN                                                                              
C                                                                                         
      END                                                                                 
