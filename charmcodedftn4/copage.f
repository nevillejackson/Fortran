      SUBROUTINE COPAGE(I,J,K,KARD,IPOS,LEN)                                              
      DIMENSION KARD(1)                                                                   
      DIMENSION IP(14)                                                                    
      LEVEL 2, IPAGE                                                                      
      COMMON/ONE/IPAGE(14,70,80)                                                          
C---- WRITTEN R.E. 1977                                                                   
C---- ARRANGES MULTIPLE PAGES FOR OUTPUT                                                  
C---- COPIES LEN CHARACTERS BEGINNING AT KARD(IPOS) INTO ARRAY IPAGE                      
C---- BEGINNING AT ITH. CHARACTER IN THE JTH. ROW ON THE KTH. PAGE                        
      IF(LEN)200,200,50                                                                   
   50 IFR=IPOS                                                                            
      ITO=I                                                                               
      DO 60 NW=1,14                                                                       
      IP(NW)=IPAGE(NW,J,K)                                                                
   60 CONTINUE                                                                            
      DO 100 L=1,LEN                                                                      
      CALL OUT(IP(1),ITO,IN(KARD,IFR))                                                    
      IFR=IFR+1                                                                           
  100 ITO=ITO+1                                                                           
      DO 150 NW=1,14                                                                      
      IPAGE(NW,J,K)=IP(NW)                                                                
  150 CONTINUE                                                                            
  200 CONTINUE                                                                            
      RETURN                                                                              
      END                                                                                 
