      FUNCTION LEGAL( CHAR )                                                              
C                                                                                         
      INTEGER CHAR,TABLE                                                                  
C                                                                                         
      DIMENSION TABLE( 64 )                                                               
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C                                                                                         
      DATA TABLE /                                                                        
     -        1, 1, 1, 1, 1, 1, 1, 1,                                                     
C             :  A  B  C  D  E  F  G                                                      
     -        1, 1, 1, 1, 1, 1, 1, 1,                                                     
C             H  I  J  K  L  M  N  O                                                      
     -        1, 1, 1, 1, 1, 1, 1, 1,                                                     
C             P  Q  R  S  T  U  V  W                                                      
     -        1, 1, 1, 1, 1, 1, 1, 1,                                                     
C             X  Y  Z  0  1  2  3  4                                                      
     -        1, 1, 1, 1, 1, 1, 1, 0,                                                     
C             5  6  7  8  9  +  -  *                                                      
     -        1, 1, 1, 1, 1,-1, 1, 1,                                                     
C             /  (  )  $  =     ,   .                                                     
     -        1, 1, 1, 1, 1, 1, 1, 1,                                                     
C             #  [  ]  :        !  &                                                      
     -        1, 1, 0, 0, 1, 1, 0, 1  /                                                   
C             '  ?  <  >  @  \  ^  ;                                                      
C                                                                                         
C     LEGAL                             WRITTEN 1974 AUHTOR J.N.                          
C     -----                                                                               
C                                                                                         
C     K = LEGAL( CHAR )                                                                   
C                                                                                         
C     ROUTINE TO EXAMINE CHARACTER 'CHAR' AND RETURN                                      
C                                                                                         
C     -1 IF BLANK                                                                         
C      0 IF ILLEGAL                                                                       
C     +1 IF LEGAL                                                                         
C                                                                                         
C     PARAMETER                                                                           
C                                                                                         
C     CHAR   CHARACTER RIGHT JUSTIFIED ZERO FILLED                                        
C            AS RETURNED BY FUNCTION 'IN'                                                 
C                                                                                         
C                                                                                         
      L=AND(CHAR,COMPL(MASK(MACHB*(MACHC-1))))                                            
      LEGAL = TABLE( L+1 )                                                                
      RETURN                                                                              
C                                                                                         
      END                                                                                 
