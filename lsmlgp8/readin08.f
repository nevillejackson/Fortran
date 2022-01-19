      SUBROUTINE READIN(IC,IN,NTRA,IBLK,K,L,KD)                                           
C-----                                                                                    
C-----READS 80 COLS OF A CARD INTO IC( ) LOCATIONS K TO L                                 
C-----NTRA = 0 IF CARD BLANK  = 1 IF NOT BLANK                                            
C-----KD IS CARD NO                                                                       
C-----L = KD*80   K = L-79                                                                
C-----IN IS INPUT UNIT FOR DETAIL CARDS                                                   
C-----IBLK CONTAINS HOLLERITH BLANK                                                       
C-----                                                                                    
      DIMENSION IRWC(8),IC(1)                                                             
      NTRA=0                                                                              
C-----READ A CARD                                                                         
      READ(IN,2000)(IRWC(I),I=1,8)                                                        
 2000 FORMAT(8A10)                                                                        
C-----CHECK CARD IS NOT BLANK                                                             
      DO 1 I=1,8                                                                          
      IF(IRWC(I).NE.IBLK) GO TO 2                                                         
    1 CONTINUE                                                                            
      IF(NTRA.EQ.0) RETURN                                                                
    2 NTRA=1                                                                              
C-----INSERT TRANSFORMATIONS HERE                                                         
C-----DECODE IRWC INTO IC                                                                 
      DECODE(80,2007,IRWC(1))(IC(I),I=K,L)                                                
 2007 FORMAT(80A1)                                                                        
C-----CONVERT INTEGER ELEMENTS OF IC TO INTEGERS AND LEAVE OTHER                          
C-----ELEMENTS IN HOLLERITH                                                               
      CALL ATOIA(IC,K,L)                                                                  
      RETURN                                                                              
      ENTRY PRERED                                                                        
      RETURN                                                                              
      ENTRY WRITIC                                                                        
C-----PRINT IC ELEMENTS K TO L                                                            
      WRITE(6,1000)                                                                       
 1000 FORMAT(37H1IC ARRAY LISTED ONE ELEMENT PER LINE/5H0   I,2X,                         
     1 20H             INTEGER,2X,10H HOLLERITH,2X,                                       
     1 20H               OCTAL)                                                           
      WRITE(6,1001)(I,IC(I),IC(I),IC(I),I=K,L)                                            
 1001 FORMAT(1H ,I4,2X,I20,2X,A10,2X,O20)                                                 
      WRITE(6,1002) K,L,KD                                                                
 1002 FORMAT(5H0K = ,I5,7H   L = ,I5,8H   KD = ,I5)                                       
      RETURN                                                                              
      END                                                                                 
