      SUBROUTINE OCTAL(CARD,I,IVAL,IFLAG,LREC)                                            
C                                                                                         
      INTEGER CARD( 1 )                                                                   
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C                                                                                         
C     OCTAL                             WRITTEN 31/3/76  AUTHOR JN                        
C                                                                                         
C                                                                                         
C     CRACKS THE NEXT OCTAL NUMBER FROM 'CARD'                                            
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     CARD   LREC CHARACTER STRING                                                        
C     I      SCAN STARTS AT CHARACTER POSITION CARD( I )                                  
C     IVAL   OCTAL NUMBER RETURNED HERE                                                   
C     IFLAG  ON EXIT IS = 0 IF ERROR ELSE = 1                                             
C                                                                                         
      IFLAG = 0                                                                           
      IVAL = 0                                                                            
100   K = IN( CARD,I )                                                                    
      IF( ITYPE( K ) .NE. 6 ) GOTO 200                                                    
      I = I + 1                                                                           
      IF(I.LT.LREC+1) GO TO 100                                                           
      GOTO 400                                                                            
C                                                                                         
200   IF( ITYPE( K ) .NE. 2 ) GOTO 400                                                    
      K = KONDIG( K )                                                                     
      IF( K .LT. 8 ) GOTO 300                                                             
      IFLAG = 0                                                                           
      GOTO 400                                                                            
  300 IVAL=SHIFT(IVAL,MACHB/2)                                                            
      IVAL = OR( IVAL,K )                                                                 
      I = I + 1                                                                           
      IFLAG = 1                                                                           
      IF(I.GT.LREC) GO TO 400                                                             
      K = IN( CARD,I )                                                                    
      GOTO 200                                                                            
400   RETURN                                                                              
C                                                                                         
      END                                                                                 
