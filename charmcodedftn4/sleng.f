      SUBROUTINE SLENG(STRING,FROM,LENGTH,LREC)                                           
C                                                                                         
      DIMENSION STRING( 1 )                                                               
      INTEGER FROM                                                                        
      INTEGER STRING                                                                      
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C                                                                                         
C     SLENG                             WRITTEN 1974 AUTHOR J.N.                          
C     -----                                                                               
C                                                                                         
C                                                                                         
C     ROUTINE TO RETURN 'LENGTH' OF 'STRING'.  STRING STARTS AT                           
C     CHARACTER POSITION STRING( FROM ) AND IS TERMINATED BY FIRST                        
C     BLANK OR COLUMN LREC                                                                
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     STRING UP TO LREC COLS                                                              
C     FROM    STARTING POINT OF STRING                                                    
C     LENGTH  LENGTH OF STRING                                                            
C                                                                                         
      LENGTH = 0                                                                          
C                                                                                         
      IF(FROM.GT.LREC-1) RETURN                                                           
C                                                                                         
      DO 100 I=FROM,LREC                                                                  
      IF(IJCHCM(IN(STRING,I),IN(KONST(46),MACHC))) 100,200,100                            
100   LENGTH = LENGTH + 1                                                                 
C                                                                                         
200   RETURN                                                                              
C                                                                                         
      END                                                                                 
