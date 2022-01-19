      SUBROUTINE SETBLK( A, I, J )                                                        
C                                                                                         
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C                                                                                         
C     SETBLK                            WRITTEN 25/10/75  AUTHOR J.N.                     
C     ------                                                                              
C                                                                                         
C     CALL SETBLK( A, I, J )                                                              
C                                                                                         
C     ROUTINE TO INSERT TRAILING BLANKS FROM CHARACTER                                    
C     POSITION A( I+1 ) TO A( J )                                                         
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     A   ARRAY TO BE BLANKED                                                             
C     I   START FROM A( I+1 )                                                             
C     J   END OF A IS A( J )                                                              
C                                                                                         
      L = I + 1                                                                           
      IF( L .GT. J ) GOTO 200                                                             
C                                                                                         
      DO 100 K=L,J                                                                        
  100 CALL OUT(A,K,IN(KONST(46),MACHC))                                                   
C                                                                                         
200   RETURN                                                                              
C                                                                                         
      END                                                                                 
