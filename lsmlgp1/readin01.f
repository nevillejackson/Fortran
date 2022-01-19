*TEXT                                                                                     
      SUBROUTINE READIN(IC,IN,NTRA,IBLK,K,L,KD)                                           
C-----                                                                                    
C-----READS 80 COLUMNS OF A CARD INTO ARRAY IC LOCATIONS K TO L                           
C-----NTRA=0 IF CARD BLANK   =1 IF CARD NOT BLANK                                         
C-----   KD IS CARD NUMBER                                                                
C-----  L=KD*80  AND  K=L-79                                                              
C-----                                                                                    
      DIMENSION IRWC(10)                                                                  
      DIMENSION IC(1)                                                                     
      NTRA=0                                                                              
      READ(IN,2000)(IRWC(I),I=1,10)                                                       
 2000 FORMAT(10A8)                                                                        
      DO 1 I=1,10                                                                         
C-----IF ANY COLUMN IS NOT BLANK GO TO 2                                                  
      IF(IRWC(I).NE.IBLK) GO TO 2                                                         
    1 CONTINUE                                                                            
      IF(NTRA.EQ.0) RETURN                                                                
    2 NTRA=1                                                                              
      DECODE(80,2001,IRWC(1))(IC(I),I=K,L)                                                
 2001 FORMAT(80I1)                                                                        
      RETURN                                                                              
      ENTRY PRERED                                                                        
      RETURN                                                                              
      END                                                                                 
*ENDTEXT                                                                                  
