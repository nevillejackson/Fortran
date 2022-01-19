      SUBROUTINE RCARD(IN,K,L,IC,KD,KEY)                                                  
C-----READS DATA CARDS                                                                    
C-----CHECKS FOR BLANK TRAILER OR EOF                                                     
C-----RETURNS KEY=0 FOR EOF OR BLANK                                                      
C-----RETURNS KEY=1 FOR NORMAL DATA CARD                                                  
C-----                                                                                    
      DIMENSION IC(1)                                                                     
      DATA IBLK/10H          /                                                            
C-----READ ONE CARD                                                                       
    1 READ(IN,2)(IC(I),I=K,L)                                                             
    2 FORMAT(8A10)                                                                        
C-----CHECK FOR EOF                                                                       
      IF(EOF(IN)) 4,3                                                                     
C-----CHECK FOR BLANK CARD                                                                
    3 DO 5 I=1,8                                                                          
      IF(IC(I)-IBLK) 6,5,6                                                                
    5 CONTINUE                                                                            
C-----EXIT FOR EOF OR BLANK                                                               
    4 KEY=0                                                                               
      RETURN                                                                              
C-----BRANCH FOR CARD ACCEPTED                                                            
    6 CONTINUE                                                                            
C-----INSERT TRANSFORMATIONS HERE                                                         
C-----                                                                                    
C-----                                                                                    
C-----EXIT FOR CARD ACCEPTED                                                              
      RETURN                                                                              
C-----ENTRY POINT TO INITIALIZE                                                           
      ENTRY ROPEN                                                                         
      KEY=1                                                                               
      RETURN                                                                              
      END                                                                                 
