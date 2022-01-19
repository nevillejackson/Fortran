      LOGICAL FUNCTION POINT( ICH )                                                       
C                                                                                         
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C                                                                                         
C     POINT                             WRITTEN 1975 AUTHOR J.N.                          
C     -----                                                                               
C                                                                                         
C     LOGIC = POINT( ICH )                                                                
C                                                                                         
C     INSPECTS CHARACTER 'ICH' AND RETURNS .TRUE. IF IT IS A FULL                         
C     STOP ( PERIOD OR POINT ) OTHERWISE RETURNS .FALSE.                                  
C                                                                                         
C     PARAMETER                                                                           
C                                                                                         
C     ICH    CHARACTER RIGHT JUSTIFIED AND ZERO FILLED                                    
C                                                                                         
      POINT=IN(ICH,MACHC).EQ.IN(KONST(48),MACHC)                                          
      RETURN                                                                              
C                                                                                         
      END                                                                                 
