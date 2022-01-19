      FUNCTION ITYPE( CHAR )                                                              
C                                                                                         
      INTEGER CHAR,TABLE                                                                  
C                                                                                         
      DIMENSION TABLE( 64 )                                                               
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C                                                                                         
      DATA TABLE /                                                                        
     -        7, 1, 1, 1, 1, 1, 1, 1,                                                     
C             :  A  B  C  D  E  F  G                                                      
     -        1, 1, 1, 1, 1, 1, 1, 1,                                                     
C             H  I  J  K  L  M  N  O                                                      
     -        1, 1, 1, 1, 1, 1, 1, 1,                                                     
C             P  Q  R  S  T  U  V  W                                                      
     -        1, 1, 1, 2, 2, 2, 2, 2,                                                     
C             X  Y  Z  0  1  2  3  4                                                      
     -        2, 2, 2, 2, 2, 3, 4, 7,                                                     
C             5  6  7  8  9  +  -  *                                                      
     -        7, 7, 7, 7, 5, 6, 7, 7,                                                     
C             /  (  )  $  =     ,   .                                                     
     -        7, 7, 7, 7, 7, 7, 7, 7,                                                     
C             #  [  ]  :        !  &                                                      
     -        7, 7, 7, 7, 7, 7, 7, 7  /                                                   
C             '  ?  <  >  @  \  ^  ;                                                      
C                                                                                         
C     ITYPE                             WRITTEN 1974 AUTHOR G.C.                          
C     -----                                                                               
C                                                                                         
C     K = ITYPE(CHAR)                                                                     
C                                                                                         
C     ROUTINE TO EXAMINE CHARACTER 'CHAR' AND RETURN                                      
C                                                                                         
C      1 IF ALPHABETIC CHAR ( A - Z )                                                     
C      2 IF DIGIT ( 0 - 9 )                                                               
C      3 IF + SIGN                                                                        
C      4 IF - SIGN                                                                        
C      5 IF = SIGN                                                                        
C      6 IF BLANK                                                                         
C      7 NONE OF ABOVE TYPE OF CHARS                                                      
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     CHAR   CHARACTER RIGHT JUSTIFIED, ZERO FILLED AS SET UP                             
C            BY ROUTINE 'IN'.                                                             
C                                                                                         
C                                                                                         
C                                                                                         
      L=AND(CHAR,COMPL(MASK(MACHB*(MACHC-1))))                                            
      ITYPE = TABLE( L+1 )                                                                
      RETURN                                                                              
C                                                                                         
      END                                                                                 
