      FUNCTION LETTER( CHAR )                                            LETTER           
C                                                                        LETTER           
      INTEGER CHAR,TABLE                                                 LETTER           
C                                                                        LETTER           
      DIMENSION TABLE( 64 )                                              LETTER           
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C                                                                        LETTER           
      DATA TABLE /                                                                        
     -        0, 1, 1, 1, 1, 1, 1, 1,                                    LETTER           
C             :  A  B  C  D  E  F  G                                     LETTER           
     -        1, 1, 1, 1, 1, 1, 1, 1,                                    LETTER           
C             H  I  J  K  L  M  N  O                                     LETTER           
     -        1, 1, 1, 1, 1, 1, 1, 1,                                    LETTER           
C             P  Q  R  S  T  U  V  WS                                    LETTER           
     -        1, 1, 1, 0, 0, 0, 0, 0,                                    LETTER           
C             X  Y  Z  0  1  2  3  4                                     LETTER           
     -        0, 0, 0, 0, 0, 0, 0, 0,                                    LETTER           
C             5  6  7  8  9  +  -  *                                     LETTER           
     -        0, 0, 0, 0, 0,-1, 0, 0,                                    LETTER           
C             /  (  )  $  =     ,   .                                    LETTER           
     -        0, 0, 0, 0, 0, 0, 0, 0,                                    LETTER           
C             #  [  ]  :        !  &                                     LETTER           
     -        0, 0, 0, 0, 0, 0, 0, 0  /                                  LETTER           
C             '  ?  <  >  @  \  ^  ;                                     LETTER           
C                                                                                         
C     LETTER                            WRITTEN 1974 AUTHOR J.N.                          
C     ------                                                                              
C                                                                                         
C     K = LETTER( CHAR )                                                                  
C                                                                                         
C     ROUTINE TO EXAMINE CHARACTER 'CHAR' AND RETURN                                      
C                                                                                         
C     -1 IF BLANK                                                                         
C      0 IF NEITHER BLANK NOR LETTER                                                      
C     +1 IF ALPHABETIC CHARACTER ( A - Z )                                                
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     CHAR   CHARACTER RIGHT JUSTIFIED, ZERO FILLED AS SET UP                             
C            BY ROUTINE 'IN'.                                                             
C                                                                                         
C                                                                        LETTER           
      L=AND(CHAR,COMPL(MASK(MACHB*(MACHC-1))))                                            
      LETTER = TABLE( L+1 )                                              LETTER           
      RETURN                                                             LETTER           
C                                                                        LETTER           
      END                                                                LETTER           
