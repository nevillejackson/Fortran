      SUBROUTINE ANOVA(SSCP,NI,NJ,NK,NE,LV,SEM,LC,NW,C,A,B)                               
      DIMENSION SSCP(LV,LV, 9),A(LV,LV),B(LV),C(LV,8),D(8),DF(8),SQ(8)                    
 1018 FORMAT(1H ,6F14.6)                                                                  
 1019 FORMAT(22H0WITHIN-PLUS RESIDUALS / 8F14.6)                                          
 1020 FORMAT(24H0REGRESSION COEFFICIENTS)                                                 
 1021 FORMAT(27H1ANALYSIS OF VARIANCE TABLE)                                              
 1022 FORMAT(7H0SOURCE,12X,3HNDF,14X,2HSS,12X,7HMEAN SQ)                                  
 1023 FORMAT(10H0I EFFECT ,3F14.4)                                                        
 1024 FORMAT(10H0J EFFECT ,3F14.4)                                                        
 1025 FORMAT(10H0K EFFECT ,3F14.4)                                                        
 1026 FORMAT(10H0I X J    ,3F14.4)                                                        
 1027 FORMAT(10H0I X K    ,3F14.4)                                                        
 1028 FORMAT(10H0J X K    ,3F14.4)                                                        
 1029 FORMAT(10H0I X J X K,3F14.4)                                                        
 1030 FORMAT(10H0WITHIN      , 3F14.4)                                                    
 1031 FORMAT(19H1CORRELATION MATRIX)                                                      
 1032 FORMAT(1H ,6F14.4)                                                                  
 1050 FORMAT(10H0TOTAL    ,2F14.4)                                                        
 1051 FORMAT(20H0STANDARD DEVIATIONS/6F14.4)                                              
      IF(LC) 9999,188,175                                                                 
  175 DO 176 M=2,NE                                                                       
      DO 176 L=1,LV                                                                       
      DO 176 LL=1,LV                                                                      
  176 SSCP(L,LL,M)=SSCP(L,LL,M)+SSCP(L,LL,NW)                                             
      DO 184 M=2,NW                                                                       
      DO 178 L=2,LV                                                                       
      DO 178 LL=2,LV                                                                      
  178 A(L-1,LL-1)=SSCP(L,LL,M)                                                            
      DO 180 L=2,LV                                                                       
  180 B(L-1)=SSCP(1,L,M)                                                                  
      CALL MATINV(A,LC,B,1,DETERM)                                                        
      DO 182 L=1,LC                                                                       
  182 C(L,M-1)=B(L)                                                                       
      D(M-1)=0.0                                                                          
      DO 184 L=1,LC                                                                       
  184 D(M-1)=D(M-1)+B(L)*SSCP(1,L+1,M)                                                    
      DO 185 M=2,NW                                                                       
  185 D(M-1)=SSCP(1,1,M)-D(M-1)                                                           
      PRINT 1019,(D(M),M=1,NE)                                                            
      PRINT 1020                                                                          
      DO 186 M=1,NE                                                                       
  186 PRINT 1018,(C(L,M),L=1,LC)                                                          
      N=NE-1                                                                              
      DO 187 M=1,N                                                                        
  187 D(M)=D(M)-D(NE)                                                                     
      GO TO 190                                                                           
  188 DO 189 M=2,NW                                                                       
  189 D(M-1)=SSCP(1,1,M)                                                                  
  190 D(8)=D(NE)                                                                          
      PRINT 1021                                                                          
      PRINT 1022                                                                          
      N=SEM                                                                               
      DF(1)=NI-1                                                                          
      DF(2)=NJ-1                                                                          
      DF(3)=DF(1)*DF(2)                                                                   
      DF(4)=NK-1                                                                          
      DF(5)=DF(1)*DF(4)                                                                   
      DF(6)=DF(2)*DF(4)                                                                   
      DF(7)=DF(1)*DF(6)                                                                   
      DF(8)=N-(NI*NJ*NK+LC)                                                               
      DO 191 I=1,8                                                                        
      IF(DF(I)) 300,301,300                                                               
  301 SQ(I)=0.0                                                                           
      GO TO 191                                                                           
  300 SQ(I)=D(I)/DF(I)                                                                    
  191 CONTINUE                                                                            
      PRINT 1023,DF(1),D(1),SQ(1)                                                         
      PRINT 1024,DF(2),D(2),SQ(2)                                                         
      IF(NK-1) 9999,194,192                                                               
  192 PRINT 1025,DF(4),D(4),SQ(4)                                                         
  194 PRINT 1026,DF(3),D(3),SQ(3)                                                         
      IF(NK-1) 9999,198,196                                                               
  196 PRINT 1027,DF(5),D(5),SQ(5)                                                         
      PRINT 1028,DF(6),D(6),SQ(6)                                                         
      PRINT 1029,DF(7),D(7),SQ(7)                                                         
  198 PRINT 1030,DF(8),D(8),SQ(8)                                                         
      DFT=N-1-LC                                                                          
      DT=0.0                                                                              
      DO 199 M=1,NE                                                                       
  199 DT=DT+D(M)                                                                          
      PRINT 1050,DFT,DT                                                                   
      IF(LC)9999,9999,200                                                                 
  200 PRINT 1031                                                                          
      DO 202 L=1,LV                                                                       
  202 B(L)=SQRTF(SSCP(L,L,1))                                                             
      DO 204 L=1,LV                                                                       
      DO 204 LL=L,LV                                                                      
      SSCP(L,LL,1)=SSCP(L,LL,1)/(B(L)*B(LL))                                              
  204 SSCP(LL,L,1)=SSCP(L,LL,1)                                                           
      DO 206 L=1,LV                                                                       
  206 PRINT 1032,(SSCP(L,LL,1),LL=1,LV)                                                   
      DO 205 L=1,LV                                                                       
  205 B(L)=B(L)/SQRTF(SEM)                                                                
      PRINT 1051,(B(L),L=1,LV)                                                            
 9999 RETURN                                                                              
      END                                                                                 
