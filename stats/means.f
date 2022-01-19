      SUBROUTINE MEANS(SUMX1,SUMX2,SUMX3,SUMX4,SUMX5,EM,EMI,EMJ,EMK,SUMY                  
     2,EMAIN,TSUM,EIJ,EIK,EJK,EMIJ,EMIK,EMJK,NI,NJ,NK,NE,LV,SEM,LC,NW,C)                  
      DIMENSION SUMY(NI,NJ,NK),SUMX1(NI,NJ,NK),                                           
     1 SUMX2(NI,NJ,NK),SUMX3(NI,NJ,NK),SUMX4(NI,NJ,NK),SUMX5(NI,NJ,NK),                   
     1EMAIN(3,NI,LV),EM(NI,NJ,NK),EMI(NI),EMJ(NJ),EMK(NK),                                
     3 TSUM(LV),EIJ(NI,NJ,LV),EIK(NI,NK,LV),EJK(NJ,NK,LV),                                
     4 EMIJ(NI,NJ),EMIK(NI,NK),EMJK(NJ,NK),C(LV,8)                                        
 1033 FORMAT(27H1CELL TOTALS, COUNTS, MEANS)                                              
 1034 FORMAT(1H ,4X,10F10.4)                                                              
 1035 FORMAT(28H0I X J TOTALS, COUNTS, MEANS)                                             
 1036 FORMAT(28H0I X K TOTALS, COUNTS, MEANS)                                             
 1037 FORMAT(28H0J X K TOTALS, COUNTS, MEANS)                                             
 1038 FORMAT(24H1I TOTALS, COUNTS, MEANS)                                                 
 1039 FORMAT(24H0J TOTALS, COUNTS, MEANS)                                                 
 1040 FORMAT(24H0K TOTALS, COUNTS, MEANS)                                                 
 1041 FORMAT(1H ,2X,10F10.4)                                                              
 1133 FORMAT(1H ,4X,10F10.0)                                                              
 1134 FORMAT(1H0,2I2,10F10.4)                                                             
 1141 FORMAT(1H0, I2,10F10.4)                                                             
 1143 FORMAT(1H ,2X,10F10.0)                                                              
 1042 FORMAT(12H0GRAND MEANS/6F10.2)                                                      
 1044 FORMAT(15H0ADJ. J EFFECTS/1H ,10F10.2)                                              
 1043 FORMAT(15H0ADJ. I EFFECTS/1H ,10F10.2)                                              
 1045 FORMAT(15H0ADJ. K EFFECTS/1H ,10F10.2)                                              
 1046 FORMAT(19H0ADJ. I X J EFFECTS)                                                      
 1047 FORMAT(19H0ADJ. I X K EFFECTS)                                                      
 1048 FORMAT(19H0ADJ. J X K EFFECTS)                                                      
 1049 FORMAT(16H0ADJ. CELL MEANS)                                                         
 1070 FORMAT(24H1CELL MEANS FOR CONTROLS)                                                 
 1080 FORMAT(10H0ADJMT SUM/1H ,5F10.2)                                                    
  207 PRINT 1033                                                                          
      DO 208 I=1,NI                                                                       
      DO 208 J=1,NJ                                                                       
      PRINT 1134,I,J,(SUMY(I,J,K),K=1,NK)                                                 
      PRINT 1133,(EM(I,J,K),K=1,NK)                                                       
      DO 209 K=1,NK                                                                       
  209 SUMY(I,J,K)=SUMY(I,J,K)/EM(I,J,K)                                                   
  208 PRINT 1034,(SUMY(I,J,K),K=1,NK)                                                     
      IF(NK-1)9999,212,210                                                                
  210 PRINT 1035                                                                          
  212 DO 214 I=1,NI                                                                       
      DO 214 J=1,NJ                                                                       
      IF(NK-1)9999,211,213                                                                
  213 PRINT 1134,I,J,(EIJ(I,J,L),L=1,LV)                                                  
      PRINT 1133,(EMIJ(I,J),L=1,LV)                                                       
  211 DO 200 L=1,LV                                                                       
  200 EIJ(I,J,L)=EIJ(I,J,L)/EMIJ(I,J)                                                     
      IF(NK-1)9999,214,201                                                                
  201 PRINT 1034,(EIJ( I,J,L),L=1,LV)                                                     
  214 CONTINUE                                                                            
      IF(NK-1)9999,219,215                                                                
  215 PRINT 1036                                                                          
      DO 216 I=1,NI                                                                       
      DO 216 K=1,NK                                                                       
      PRINT 1134,I,K,(EIK(I,K,L),L=1,LV)                                                  
      PRINT 1133,(EMIK(I,K),L=1,LV)                                                       
      DO 217 L=1,LV                                                                       
  217 EIK(I,K,L)=EIK(I,K,L)/EMIK(I,K)                                                     
  216 PRINT 1034,(EIK(I,K,L),L=1,LV)                                                      
      PRINT 1037                                                                          
      DO 218 J=1,NJ                                                                       
      DO 218 K=1,NK                                                                       
      PRINT 1134,J,K,(EJK(J,K,L),L=1,LV)                                                  
      PRINT 1133,(EMJK(J,K),L=1,LV)                                                       
      DO 202 L=1,LV                                                                       
  202 EJK(J,K,L)=EJK(J,K,L)/EMJK(J,K)                                                     
  218 PRINT 1034,(EJK(J,K,L),L=1,LV)                                                      
  219 PRINT 1038                                                                          
      DO 220 I=1,NI                                                                       
      PRINT 1141,I,(EMAIN(1,I,L),L=1,LV)                                                  
      PRINT 1143,(EMI(I),L=1,LV)                                                          
      DO 221 L=1,LV                                                                       
  221 EMAIN(1,I,L)=EMAIN(1,I,L)/EMI(I)                                                    
  220 PRINT 1041,(EMAIN(1,I,L),L=1,LV)                                                    
      PRINT 1039                                                                          
      DO 222 J=1,NJ                                                                       
      PRINT 1141,J,(EMAIN(2,J,L),L=1,LV)                                                  
      PRINT 1143,(EMJ(J),L=1,LV)                                                          
      DO 203 L=1,LV                                                                       
  203 EMAIN(2,J,L)=EMAIN(2,J,L)/EMJ(J)                                                    
  222 PRINT 1041,(EMAIN(2,J,L),L=1,LV)                                                    
      IF(NK-1)9999,235,223                                                                
  223 PRINT 1040                                                                          
      DO 224 K=1,NK                                                                       
      PRINT 1141,K,(EMAIN(3,K,L),L=1,LV)                                                  
      PRINT 1143,(EMK(K),L=1,LV)                                                          
      DO 204 L=1,LV                                                                       
  204 EMAIN(3,K,L)=EMAIN(3,K,L)/EMK(K)                                                    
  224 PRINT1041,(EMAIN(3,K,L),L=1,LV)                                                     
  235 DO 236 L=1,LV                                                                       
  236 TSUM(L)=TSUM(L)/SEM                                                                 
      PRINT 1042,(TSUM(L),L=1,LV)                                                         
      IF(LC) 9999,9999,237                                                                
  237 DO 240 I=1,NI                                                                       
      SUM=0.0                                                                             
      DO 238 L=1,LC                                                                       
  238 SUM=SUM+C(L,NE)*(EMAIN(1,I,L+1)-TSUM(L+1))                                          
  240 EMAIN(1,I,1)=EMAIN(1,I,1)-SUM                                                       
      PRINT 1043,(EMAIN(1,I,1),I=1,NI)                                                    
      DO 244 J=1,NJ                                                                       
      SUM=0.0                                                                             
      DO 242 L=1,LC                                                                       
  242 SUM=SUM+C(L,NE)*(EMAIN(2,J,L+1)-TSUM(L+1))                                          
  244 EMAIN(2,J,1)=EMAIN(2,J,1)-SUM                                                       
      PRINT 1044,(EMAIN(2,J,1),J=1,NJ)                                                    
      IF(NK-1) 9999,250,245                                                               
  245 DO 248 K=1,NK                                                                       
      SUM=0.0                                                                             
      DO 246 L=1,LC                                                                       
  246 SUM=SUM+C(L,NE)*(EMAIN(3,K,L+1)-TSUM(L+1))                                          
  248 EMAIN(3,K,1)=EMAIN(3,K,1)-SUM                                                       
      PRINT 1045,(EMAIN(3,K,1),K=1,NK)                                                    
  250 PRINT 1046                                                                          
      DO 260 J=1,NJ                                                                       
      DO 260 I=1,NI                                                                       
      SUM=0.0                                                                             
      DO 255 L=1,LC                                                                       
  255 SUM=SUM+C(L,NE)*(EIJ(I,J,L+1)-TSUM(L+1))                                            
  260 EIJ(I,J,1)=EIJ(I,J,1)-SUM                                                           
      DO 265 I=1,NI                                                                       
  265 PRINT 1141,I,  (EIJ(I,J,1),J=1,NJ)                                                  
      IF(NK-1) 9999,9999,270                                                              
  270 PRINT 1047                                                                          
      DO 280 I=1,NI                                                                       
      DO 280 K=1,NK                                                                       
      SUM=0.0                                                                             
      DO 275 L=1,LC                                                                       
  275 SUM=SUM+C(L,NE)*(EIK(I,K,L+1)-TSUM(L+1))                                            
  280 EIK(I,K,1)=EIK(I,K,1)-SUM                                                           
      DO 285 I=1,NI                                                                       
  285 PRINT 1141,I,(EIK(I,K,1),K=1,NK)                                                    
  290 PRINT 1048                                                                          
      DO 300 J=1,NJ                                                                       
      DO 300 K=1,NK                                                                       
      SUM=0.0                                                                             
      DO 295 L=1,LC                                                                       
  295 SUM=SUM+C(L,NE)*(EJK(J,K,L+1)-TSUM(L+1))                                            
  300 EJK(J,K,1)=EJK(J,K,1)-SUM                                                           
      DO 305 J=1,NJ                                                                       
  305 PRINT 1141,J,(EJK(J,K,1),K=1,NK)                                                    
      PRINT 1070                                                                          
      DO 360 L=1,LC                                                                       
      DO 360 I=1,NI                                                                       
      DO 360 J=1,NJ                                                                       
      DO 360 K=1,NK                                                                       
      GO TO (351,352,353,354,355),L                                                       
  351 SUMX1(I,J,K)=SUMX1(I,J,K)/EM(I,J,K)                                                 
  410 PRINT 1134,I,J,SUMX1(I,J,K)                                                         
      GO TO 360                                                                           
  352 SUMX2(I,J,K)=SUMX2(I,J,K)/EM(I,J,K)                                                 
  420 PRINT 1134,I,J,SUMX2(I,J,K)                                                         
      GO TO 360                                                                           
  353 SUMX3(I,J,K)=SUMX3(I,J,K)/EM(I,J,K)                                                 
  430 PRINT 1134,I,J,SUMX3(I,J,K)                                                         
      GO TO 360                                                                           
  354 SUMX4(I,J,K)=SUMX4(I,J,K)/EM(I,J,K)                                                 
  440 PRINT 1134,I,J,SUMX4(I,J,K)                                                         
      GO TO 360                                                                           
  355 SUMX5(I,J,K)=SUMX5(I,J,K)/EM(I,J,K)                                                 
  450 PRINT 1134,I,J,SUMX5(I,J,K)                                                         
  360 CONTINUE                                                                            
      DO 401 I=1,NI                                                                       
      DO 401 J=1,NJ                                                                       
      DO 401 K=1,NK                                                                       
      SUM=0.0                                                                             
      SUM=SUM+C(1,NE)*(SUMX1(I,J,K)-TSUM(2))                                              
      IF(LC-1) 9999,400,380                                                               
  380 SUM=SUM+C(2,NE)*(SUMX2(I,J,K)-TSUM(3))                                              
      IF(LC-2) 9999,400,385                                                               
  385 SUM=SUM+C(3,NE)*(SUMX3(I,J,K)-TSUM(4))                                              
      IF(LC-3) 9999,400,390                                                               
  390 SUM=SUM+C(4,NE)*(SUMX4(I,J,K)-TSUM(5))                                              
      IF(LC-4) 9999,400,395                                                               
  395 SUM=SUM+C(5,NE)*(SUMX5(I,J,K)-TSUM(6))                                              
  400 PRINT 1080,SUM,C(1,NE),SUMX1(I,J,K),TSUM(2)                                         
  401 SUMY(I,J,K)=SUMY(I,J,K)-SUM                                                         
      PRINT 1049                                                                          
      DO 405 I=1,NI                                                                       
      DO 405 J=1,NJ                                                                       
  405 PRINT 1134,I,J,(SUMY(I,J,K),K=1,NK)                                                 
 9999 RETURN                                                                              
      END                                                                                 
