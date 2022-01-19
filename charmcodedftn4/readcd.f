      SUBROUTINE READCD(LU,KARD,IFLAG,LENW)                                               
C-----WRITTEN N.J. 1978                                                                   
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
C-----                                                                                    
C-----READS A FORMATTED CARD IMAGE OF LENGTH LENW WORDS FROM UNIT LU                      
C----- INTO ARRAY KARD( )                                                                 
C-----RETURNS IFLAG=-1 IF EOF                                                             
C-----        IFLAG= 0 IF OK                                                              
C-----        IFLAG=+1 IF PARITY ERROR                                                    
C-----                                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      DIMENSION KARD(1)                                                                   
      K=0                                                                                 
   10 READ(LU,1001)(KARD(I),I=1,LENW)                                                     
 1001 FORMAT(8A10)                                                                        
      IFLAG=0                                                                             
      IF(EOF(LU)) 1,2,1                                                                   
    1 IFLAG=-1                                                                            
      GO TO 4                                                                             
    2 IF(IOCHEC(LU)) 3,4,3                                                                
    3 K=K+1                                                                               
      IF(K-10) 13,13,14                                                                   
   13 BACKSPACE LU                                                                        
      GO TO 10                                                                            
   14 IFLAG=1                                                                             
      WRITE(LP,15) LU                                                                     
   15 FORMAT(27H0READ PARITY ERROR ON UNIT ,I4)                                           
      CALL JOBEND                                                                         
    4 RETURN                                                                              
      END                                                                                 
