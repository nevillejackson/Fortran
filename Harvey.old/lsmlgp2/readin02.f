      SUBROUTINE READIN(IC,IN,NTRA,IBLK,K,L,KD)                           220001          
C-----                                                                    220002          
C-----READS 80 COLUMNS OF A CARD INTO ARRAY IC LOCATIONS K TO L           220003          
C-----NTRA=0 IF CARD BLANK   =1 IF CARD NOT BLANK                         220004          
C-----   KD IS CARD NUMBER                                                220005          
C-----  L=KD*80  AND  K=L-79                                              220006          
C-----                                                                    220007          
      DIMENSION IRWC(8)                                                   220008          
      DIMENSION IC(1)                                                     220009          
      DIMENSION IB(80)                                                                    
      DATA (IMIN=1H-)                                                                     
      NTRA=0                                                              220010          
C-----READ A CARD                                                                         
      READ(IN,2000)(IRWC(I),I=1,8)                                        220011          
 2000 FORMAT(8A10)                                                        220012          
C-----CHECK CARD IS NOT BLANK                                                             
      DO 1 I=1,8                                                          220013          
      IF(IRWC(I).NE.IBLK) GO TO 2                                         220015          
    1 CONTINUE                                                            220016          
      IF(NTRA.EQ.0) RETURN                                                220017          
    2 NTRA=1                                                              220018          
C-----DECODE IRWC INTO IB                                                                 
      DECODE(80,2007,IRWC(1))(IB(I),I=1,80)                                               
 2007 FORMAT(80A1)                                                                        
C-----DECODE IRWC INTO IC                                                                 
      DECODE(80,2001,IRWC(1))(IC(I),I=K,L)                                220019          
 2001 FORMAT(80I1)                                                        220020          
C-----CONVERT BLANK AND MINUS IN IC TO HOLLERITH FORM                                     
      DO 10 I=1,80                                                                        
      LL=I+(KD-1)*80                                                                      
      IF(IB(I).NE.IBLK) GO TO 11                                                          
      IC(LL)=IBLK                                                                         
   11 IF(IB(I).NE.IMIN) GO TO 10                                                          
      IC(LL)=IMIN                                                                         
   10 CONTINUE                                                                            
      RETURN                                                              220021          
      ENTRY PRERED                                                        220022          
      RETURN                                                              220023          
      END                                                                 220024          
