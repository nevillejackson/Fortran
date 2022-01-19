      SUBROUTINE CHECKH( M, H )                                                           
C                                                                                         
      DIMENSION M( 2 )                                                                    
C                                                                                         
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      WRITE(LP,1001) H,M,M                                                                
C                                                                                         
      RETURN                                                                              
C                                                                                         
 1001 FORMAT(16H + + + CHECK OF ,A10,2X,2A10,2(2X,O20))                                   
C                                                                                         
      END                                                                                 
