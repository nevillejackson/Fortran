      SUBROUTINE STATIN(M,N,XBAR,STD,RX,R,B)                            
      DIMENSION XBAR(1),STD(1),RX(1),R(1),B(1)                          
      ncr=5
C-----READ MEANS                                                        
      READ (ncr,*)(XBAR(I),I=1,M)                                            
C-----READ STAND DEVNS                                                  
      READ (ncr,*)(STD(I),I=1,M)                                             
C-----READ CORRELATIONS                                                 
      DO 3 I=1,M                                                        
      CALL LOC(I,I,IR,M,M,1)                                            
      R(IR)=1.0                                                         
C-----PUNCH CORRELATIONS UPPER TRIANGLE COLUMNWISE = LOWER TR ROWWISE   
c-----include the 1.0's
      JL=I*(I+1)/2                                                      
      JF=JL-I+1                                                         
      JL=JL-1                                                           
      IF(JF-JL) 4,4,3                                                   
    4 READ(ncr,*)(R(J),J=JF,JL)                                         
    3 CONTINUE                                                          
C-----GENERATE SP MATRIX FROM CORRELATIONS                              
C-----SET DF AND STD() = 1 IF WANT RX()=R()                             
      DF=N-1                                                            
      DO 6 I=1,M                                                        
      DO 6 J=1,M                                                        
      CALL LOC(I,J,IR,M,M,1)                                            
      CALL LOC(I,J,IRX,M,M,0)                                           
    6 RX(IRX)=R(IR)*STD(I)*STD(J)*DF                                    
C-----PUT DIAG RX() INTO B()                                            
      DO 7 I=1,M                                                        
      CALL LOC(I,I,IRX,M,M,0)                                           
    7 B(I)=RX(IRX)                                                      
      RETURN                                                            
      END                                                               
