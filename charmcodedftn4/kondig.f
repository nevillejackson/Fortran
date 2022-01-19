      FUNCTION KONDIG( CHAR )                                                             
C                                                                                         
      INTEGER CHAR,TABLE                                                                  
C                                                                                         
      DIMENSION TABLE( 64 )                                                               
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C                                                                                         
      DATA TABLE /                                                                        
     -       -1,-1,-1,-1,-1,-1,-1,-1,                                                     
C             :  A  B  C  D  E  F  G                                                      
     -       -1,-1,-1,-1,-1,-1,-1,-1,                                                     
C             H  I  J  K  L  M  N  O                                                      
     -       -1,-1,-1,-1,-1,-1,-1,-1,                                                     
C             P  Q  R  S  T  U  V  W                                                      
     -       -1,-1,-1, 0, 1, 2, 3, 4,                                                     
C             X  Y  Z  O  1  2  3  4                                                      
     -        5, 6, 7, 8, 9,-1,-1,-1,                                                     
C             5  6  7  8  9  +  -  *                                                      
     -       -1,-1,-1,-1,-1, 0,-1,-1,                                                     
C             /  (  )  $  =     ,   .                                                     
     -       -1,-1,-1,-1,-1,-1,-1,-1,                                                     
C             #  [  ]  :        !  &                                                      
     -       -1,-1,-1,-1,-1,-1,-1,-1  /                                                   
C             '  ?  <  >  @  \  ^  ;                                                      
C                                                                                         
C     KONDIG                            WRITTEN 1974 AUTHOR J.N.                          
C     ------                                                                              
C                                                                                         
C     K = KONDIG( CHAR )                                                                  
C                                                                                         
C     ROUTINE TO EXAMINE CHARACTER 'CHAR' AND RETURN                                      
C                                                                                         
C      -1 IF IT IS NOT A DIGIT OR BLANK                                                   
C     0 - 9 IF IT IS 0 - 9                                                                
C     0 IF IT IS BLANK                                                                    
C                                                                                         
C     PARAMETER                                                                           
C                                                                                         
C     CHAR   CHARACTER RIGHT JUSTIFIED, ZERO FILLED AS SET UP                             
C            BY ROUTINE 'IN'                                                              
C                                                                                         
      L=AND(CHAR,COMPL(MASK(MACHB*(MACHC-1))))                                            
      KONDIG = TABLE( L+1 )                                                               
      RETURN                                                                              
C                                                                                         
      END                                                                                 
