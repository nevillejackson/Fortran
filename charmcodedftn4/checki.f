      SUBROUTINE CHECKI( INT, HOLL )                                                      
C                                                                                         
C     CHECK OUT ROUTINE USED WHILE DEBUGGING TO CHECK PRINT THE                           
C     VALUE OF AN INTEGER TYPE VARIABLE AS NUMBER AND AS CHARACTER                        
C     STRING.                                                                             
C                                                                                         
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      WRITE(LP,1001) HOLL,INT,INT,INT                                                     
      RETURN                                                                              
C                                                                                         
 1001 FORMAT(19H + + + CHECK PRINT ,A10,I12,2X,O20,2X,A10)                                
C                                                                                         
      END                                                                                 
