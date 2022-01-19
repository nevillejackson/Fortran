      SUBROUTINE JOKA(LIST,LOT,LAIR,LRECW,LREC80)                                         
C-----WRITTEN N.J. 1977                                                                   
C-----JOINS MULTIPLE CARD IMAGES FROM LIST INTO SINGLE RECORD LOT                         
      DIMENSION LIST(200,9)                                                               
      DIMENSION LOT(1)                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      LREC80=LCARDS(LRECW)                                                                
      LREM80=LREC80*MACHCD-LRECW                                                          
      IZ=MACHCD                                                                           
      DO 50 IKD=1,LREC80                                                                  
      LSUB=LAIR+IKD-1                                                                     
      IF(IKD-LREC80) 52,53,54                                                             
   54 CALL JOBEND                                                                         
   52 IZ=LREM80                                                                           
   53 DO 51 I=1,IZ                                                                        
      LOTSUB=(IKD-1)*MACHCD+I                                                             
   51 LOT(LOTSUB)=LIST(LSUB,I)                                                            
   50 CONTINUE                                                                            
C-----RETURN LAIR OF LAST DATA CARD IN LIST                                               
      LAIR=LSUB                                                                           
      RETURN                                                                              
      END                                                                                 
