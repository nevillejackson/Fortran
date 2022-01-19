      FUNCTION NUMBER( CHAR )                                                             
C                                                                                         
      INTEGER CHAR,TABLE                                                                  
C                                                                                         
      DIMENSION TABLE( 64 )                                                               
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C                                                                                         
      DATA TABLE /                                                                        
     -        0, 0, 0, 0, 0, 0, 0, 0,                                                     
C             :  A  B  C  D  E  F  G                                                      
     -        0, 0, 0, 0, 0, 0, 0, 0,                                                     
C             H  I  J  K  L  M  N  O                                                      
     -        0, 0, 0, 0, 0, 0, 0, 0,                                                     
C             P  Q  R  S  T  U  V  W                                                      
     -        0, 0, 0, 1, 1, 1, 1, 1,                                                     
C             X  Y  Z  0  1  2  3  4                                                      
     -        1, 1, 1, 1, 1,-1,-1, 0,                                                     
C             5  6  7  8  9  +  -  *                                                      
     -        0, 0, 0, 0, 0, 0, 0, 0,                                                     
C             /  (  )  $  =     ,   .                                                     
     -        0, 0, 0, 0, 0, 0, 0, 0,                                                     
C             #  [  ]  :        !  &                                                      
     -        0, 0, 0, 0, 0, 0, 0, 0  /                                                   
C             '  ?  <  >  @  \  ^  ;                                                      
C                                                                                         
C     NUMBER                            WRITTEN 1974 AUTHOR J.N.                          
C     ------                                                                              
C                                                                                         
C     K = NUMBER( CHAR )                                                                  
C                                                                                         
C     ROUTINE TO EXAMINE CHARACTER 'CHAR' AND RETURN                                      
C                                                                                         
C      -1 IT IS + OR -                                                                    
C      0 IF IT IS NON NUMERIC AND NOT + AND NOT -                                         
C      1 IF IT IS A DIGIT                                                                 
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     CHAR   CHARACTER RIGHT JUSTIFIED, ZERO FILLED AS SET UP                             
C            BY ROUTINE 'IN'.                                                             
C                                                                                         
C                                                                                         
      L=AND(CHAR,COMPL(MASK(MACHB*(MACHC-1))))                                            
      NUMBER = TABLE( L+1 )                                                               
      RETURN                                                                              
C                                                                                         
      END                                                                                 
