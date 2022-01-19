      SUBROUTINE READVF(LU,KARD,IFLAG,LENW,IVF)                                           
C---- WRITTEN R.E. 1978                                                                   
C---- MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
C---- READS A FORMATTED RECORD OF LENGTH LENW WORDS FROM UNIT LU INTO                     
C---- ARRAY KARD( ) VIA A VARIABLE FORMAT IVF( )                                          
C---- RETURNS IFLAG=-1 IF EOF                                                             
C---- RETURNS IFLAG=0  IF O.K.                                                            
C---- RETURNS IFLAG=+1 IF PARITY ERROR                                                    
      COMMON/MACH/MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                     
      DIMENSION KARD(1),IVF(1)                                                            
      K=0                                                                                 
   10 READ(LU,IVF) (KARD(I),I=1,LENW)                                                     
      IFLAG=0                                                                             
      IF(EOF(LU))1,2,1                                                                    
    1 IFLAG=-1                                                                            
      GO TO 4                                                                             
    2 IF(IOCHEC(LU))3,4,3                                                                 
    3 K=K+1                                                                               
      IF(K-10)13,13,14                                                                    
   13 BACKSPACE LU                                                                        
      GO TO 10                                                                            
   14 IFLAG=1                                                                             
      WRITE(LP,15) LU                                                                     
   15 FORMAT(27H0READ PARITY ERROR ON UNIT ,I4)                                           
      CALL JOBEND                                                                         
    4 RETURN                                                                              
      END                                                                                 
