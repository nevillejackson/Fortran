      SUBROUTINE NEWSEQ(KEYO,KEYN,LEN)                                                    
      DIMENSION KEYO(1),KEYN(1),KTAB(64)                                                  
C-----WRITTEN N.J. 1977                                                                   
C-----CONVERTS STRING OF LENGTH LEN CHARS IN KEYO NNTO NEW STRING KEYN                    
C-----CONVERSION MODIFIES COLLATING SEQUENCE FROM DISPLAY TO ASCII6                       
      DATA KTAB / 32B,41B,42B,43B,44B,45B,46B,47B,                                        
     1                      50B,51B,52B,53B,54B,55B,56B,57B,                              
     2                      60B,61B,62B,63B,64B,65B,66B,67B,                              
     3                      70B,71B,72B,                                                  
     4                      20B,21B,22B,23B,24B,25B,26B,27B,                              
     5                      30B,31B,                                                      
     6                      13B,15B,12B,17B,10B,11B,04B,35B,00B,                          
     7                      14B,16B,03B,07B,01B,05B,02B,77B,75B,                          
     8                      06B,40B,37B,36B,73B,34B,74B,76B,33B/                          
C-----                                                                                    
      DO 1 I=1,LEN                                                                        
      K=IN(KEYO,I)                                                                        
    1 CALL OUT(KEYN,I,KTAB(K+1))                                                          
      RETURN                                                                              
      END                                                                                 
