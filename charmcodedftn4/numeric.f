      FUNCTION NMERIC(CHAR)                                                               
C                                                                                         
      INTEGER CHAR,CHRTAB                                                                 
C                                                                                         
      DIMENSION CHRTAB(10)                                                                
C                                                                                         
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C                                                                                         
      EQUIVALENCE(CHRTAB(1),KONST(28))                                                    
C                                                                                         
C     NUMERIC                           WRITTEN 1974 AUHTOR J.N.                          
C     -------                                                                             
C                                                                                         
C     K=NMERIC(CHAR)                                                                      
C                                                                                         
C     FUNCTION TO EXAMINE THE CHARACTER "CHAR" AND RETURN ONE OF                          
C     THE FOLLOWING VALUES                                                                
C                                                                                         
C     -1 IF NUMERIC                                                                       
C      0 IF BLANK                                                                         
C     +1 IF ANYTHING ELSE                                                                 
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     CHAR   CHARACTER RIGHT JUSTIFIED, ZERO FILLED AS SET UP                             
C            BY ROUTINE 'IN'.                                                             
C                                                                                         
      K = -1                                                                              
C                                                                                         
      DO 100 I = 1,10                                                                     
      IF(IJCHCM(CHAR,CHRTAB(I))) 100,200,100                                              
100   CONTINUE                                                                            
C                                                                                         
      K = 1                                                                               
      IF(IJCHCM(CHAR,IN(KONST(46),MACHC))) 200,199,200                                    
  199 K=0                                                                                 
C                                                                                         
  200 NMERIC=K                                                                            
C                                                                                         
      RETURN                                                                              
C                                                                                         
      END                                                                                 
