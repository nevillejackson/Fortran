      SUBROUTINE SHIFTR(R,N,LREC)                                                         
      INTEGER R(1)                                                                        
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C     ROUTINE TO SHIFT CHARACTERS IN "R" LEFT UNTIL COLUMN ONE IS NON                     
C     BLANK AND RETURN LENGTH ON CHARACTER STRING IN "N"                                  
C                                                                                         
      IC=0                                                                                
      IA=0                                                                                
      IB=0                                                                                
C                                                                                         
      DO 200 I=1,LREC                                                                     
C                                                                                         
      IF(IJCHCM(IN(R,I),IN(KONST(46),MACHC))) 100,101,100                                 
  101 CONTINUE                                                                            
C                                        JUMP IF WE HAVE ALREADY HIT A                    
C                                        CHAR                                             
      IF( IC .EQ. 1 ) GOTO 200                                                            
C                                        IA POINTS TO LAST LEADING                        
C                                        BLANK                                            
      IA = I                                                                              
      GOTO 200                                                                            
C                                                                                         
100   IC = 1                                                                              
C                                        IB WILL POINT TO LAST NON BLANK                  
C                                        CHARACTER                                        
      IB = I                                                                              
200   CONTINUE                                                                            
C                                                                                         
      N = 0                                                                               
C                                        IC = 0 MEANS BLANK RECORD                        
      IF( IC .EQ. 0 ) RETURN                                                              
      N = IB - IA                                                                         
C                                        IA = 0 MEANS NO LEADING BLANKS                   
      IF( IA .EQ. 0 ) RETURN                                                              
      IC = 1                                                                              
      IA = IA + 1                                                                         
C                                        NOW SHUFFLE LEFT                                 
      DO 300 I=IA,LREC                                                                    
      CALL OUT( R,IC,IN( R,I ) )                                                          
300   IC = IC+1                                                                           
C                                                                                         
      IA = N + 1                                                                          
C                                        BLANK OUT CHARS ON RIGHT                         
      DO 400 I = IA,IB                                                                    
  400 CALL OUT(R,I,IN(KONST(46),MACHC))                                                   
C                                                                                         
      RETURN                                                                              
C                                                                                         
      END                                                                                 
