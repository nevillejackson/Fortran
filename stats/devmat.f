      SUBROUTINE DEVMAT(RSS,CT,SUMY,SUMX1,SUMX2,SUMX3,SUMX4,SUMX5,                        
     2   EM,EIJ,EIK,EJK,EMIJ,EMIK,EMJK,                                                   
     3   EMAIN,SSCP,EMI,EMJ,EMK,NI,NJ,NK,NE,LV,SEM,LC,NW)                                 
      DIMENSION RSS(LV,LV),CT(LV,LV),SUMY(NI,NJ,NK),SUMX1(NI,NJ,NK),                      
     1 SUMX2(NI,NJ,NK),SUMX3(NI,NJ,NK),SUMX4(NI,NJ,NK),SUMX5(NI,NJ,NK),                   
     2EMAIN(3,NI,LV),SSCP(LV,LV, 9),EMI(NI),EMJ(NJ),EMK(NK),                              
     3 EJK(NJ,NK,LV),EMIJ(NI,NJ),EMIK(NI,NK),EMJK(NJ,NK),                                 
     4 EM(NI,NJ,NK),EIJ(NI,NJ,LV),EIK(NI,NK,LV)                                           
 1008 FORMAT(26H1SS AND CP FOR ALL SOURCES)                                               
 1009 FORMAT(6H0TOTAL)                                                                    
 1010 FORMAT(9H0I EFFECT)                                                                 
 1011 FORMAT(9H0J EFFECT)                                                                 
 1012 FORMAT(9H0K EFFECT)                                                                 
 1013 FORMAT(18H0I X J INTERACTION)                                                       
 1014 FORMAT(18H0I X K INTERACTION)                                                       
 1015 FORMAT(18H0J X K INTERACTION)                                                       
 1016 FORMAT(22H0I X J X K INTERACTION)                                                   
 1017 FORMAT(7H0WITHIN)                                                                   
 1018 FORMAT(1H ,6F14.6)                                                                  
      DO 100 L=1,LV                                                                       
      DO 100 LL=L,LV                                                                      
      SSCP(L,LL,1)=RSS(L,LL)-CT(L,LL)                                                     
  100 SSCP(LL,L,1)=SSCP(L,LL,1)                                                           
      DO 104 L=1,LV                                                                       
      DO 104 LL=L,LV                                                                      
      SUM=0.0                                                                             
      DO 102 I=1,NI                                                                       
  102 SUM=SUM+(EMAIN(1,I,L)*EMAIN(1,I,LL)/EMI(I))                                         
      SSCP(L,LL,2)=SUM-CT(L,LL)                                                           
  104 SSCP(LL,L,2)=SSCP(L,LL,2)                                                           
      DO 108 L=1,LV                                                                       
      DO 108 LL=L,LV                                                                      
      SUM=0.0                                                                             
      DO 106 J=1,NJ                                                                       
  106 SUM=SUM+(EMAIN(2,J,L)*EMAIN(2,J,LL)/EMJ(J))                                         
      SSCP(L,LL,3)=SUM-CT(L,LL)                                                           
  108 SSCP(LL,L,3)=SSCP(L,LL,3)                                                           
      IF(NK-1) 9999,116,110                                                               
  110 DO 114 L=1,LV                                                                       
      DO 114 LL=L,LV                                                                      
      SUM=0.0                                                                             
      DO 112 K=1,NK                                                                       
  112 SUM=SUM+(EMAIN(3,K,L)*EMAIN(3,K,LL)/EMK(K))                                         
      SSCP(L,LL,5)=SUM-CT(L,LL)                                                           
  114 SSCP(LL,L,5)=SSCP(L,LL,5)                                                           
  116 DO 120 L=1,LV                                                                       
      DO 120 LL=L,LV                                                                      
      SUM=0.0                                                                             
      DO 118 I=1,NI                                                                       
      DO 118 J=1,NJ                                                                       
  118 SUM=SUM+(EIJ(I,J,L)*EIJ(I,J,LL)/EMIJ(I,J))                                          
      SSCP(L,LL,4)=SUM-CT(L,LL)-SSCP(L,LL,2)-SSCP(L,LL,3)                                 
  120 SSCP(LL,L,4)=SSCP(L,LL,4)                                                           
      IF(NK-1) 9999,131,122                                                               
  122 DO 126 L=1,LV                                                                       
      DO 126 LL=L,LV                                                                      
      SUM=0.0                                                                             
      DO 124 I=1,NI                                                                       
      DO 124 K=1,NK                                                                       
  124 SUM=SUM+(EIK(I,K,L)*EIK(I,K,LL)/EMIK(I,K))                                          
      SSCP(L,LL,6)=SUM-CT(L,LL)-SSCP(L,LL,2)-SSCP(L,LL,5)                                 
  126 SSCP(LL,L,6)=SSCP(L,LL,6)                                                           
      DO 130 L=1,LV                                                                       
      DO 130 LL=L,LV                                                                      
      SUM=0.0                                                                             
      DO 128 J=1,NJ                                                                       
      DO 128 K=1,NK                                                                       
  128 SUM=SUM+(EJK(J,K,L)*EJK(J,K,LL)/EMJK(J,K))                                          
      SSCP(L,LL,7)=SUM-CT(L,LL)-SSCP(L,LL,3)-SSCP(L,LL,5)                                 
  130 SSCP(LL,L,7)=SSCP(L,LL,7)                                                           
  131 DO 132 L=1,LV                                                                       
      DO 132 LL=L,LV                                                                      
  132 SSCP(L,LL,8)=0.0                                                                    
      DO 134 K=1,NK                                                                       
      DO 134 J=1,NJ                                                                       
      DO 134 I=1,NI                                                                       
      SSCP(1,1,8)=SSCP(1,1,8)+SUMY(I,J,K)**2/EM(I,J,K)                                    
      IF(LC) 9999,134,1                                                                   
    1 SSCP(1,2,8)=SSCP(1,2,8)+SUMY(I,J,K)*SUMX1(I,J,K)/EM(I,J,K)                          
      SSCP(1,3,8)=SSCP(1,3,8)+SUMY(I,J,K)*SUMX2(I,J,K)/EM(I,J,K)                          
      SSCP(1,4,8)=SSCP(1,4,8)+SUMY(I,J,K)*SUMX3(I,J,K)/EM(I,J,K)                          
      SSCP(1,5,8)=SSCP(1,5,8)+SUMY(I,J,K)*SUMX4(I,J,K)/EM(I,J,K)                          
      SSCP(1,6,8)=SSCP(1,6,8)+SUMY(I,J,K)*SUMX5(I,J,K)/EM(I,J,K)                          
      SSCP(2,2,8)=SSCP(2,2,8)+SUMX1(I,J,K)*SUMX1(I,J,K)/EM(I,J,K)                         
      SSCP(2,3,8)=SSCP(2,3,8)+SUMX1(I,J,K)*SUMX2(I,J,K)/EM(I,J,K)                         
      SSCP(2,4,8)=SSCP(2,4,8)+SUMX1(I,J,K)*SUMX3(I,J,K)/EM(I,J,K)                         
      SSCP(2,5,8)=SSCP(2,5,8)+SUMX1(I,J,K)*SUMX4(I,J,K)/EM(I,J,K)                         
      SSCP(2,6,8)=SSCP(2,6,8)+SUMX1(I,J,K)*SUMX5(I,J,K)/EM(I,J,K)                         
      IF(LC-1) 9999,134,2                                                                 
    2 SSCP(3,3,8)=SSCP(3,3,8)+SUMX2(I,J,K)*SUMX2(I,J,K)/EM(I,J,K)                         
      SSCP(3,4,8)=SSCP(3,4,8)+SUMX2(I,J,K)*SUMX3(I,J,K)/EM(I,J,K)                         
      SSCP(3,5,8)=SSCP(3,5,8)+SUMX2(I,J,K)*SUMX4(I,J,K)/EM(I,J,K)                         
      SSCP(3,6,8)=SSCP(3,6,8)+SUMX2(I,J,K)*SUMX5(I,J,K)/EM(I,J,K)                         
      IF(LC-2) 9999,134,3                                                                 
    3 SSCP(4,4,8)=SSCP(4,4,8)+SUMX3(I,J,K)*SUMX3(I,J,K)/EM(I,J,K)                         
      SSCP(4,5,8)=SSCP(4,5,8)+SUMX3(I,J,K)*SUMX4(I,J,K)/EM(I,J,K)                         
      SSCP(4,6,8)=SSCP(4,6,8)+SUMX3(I,J,K)*SUMX5(I,J,K)/EM(I,J,K)                         
      SSCP(5,5,8)=SSCP(5,5,8)+SUMX4(I,J,K)*SUMX4(I,J,K)/EM(I,J,K)                         
      SSCP(5,6,8)=SSCP(5,6,8)+SUMX4(I,J,K)*SUMX5(I,J,K)/EM(I,J,K)                         
      SSCP(6,6,8)=SSCP(6,6,8)+SUMX5(I,J,K)*SUMX5(I,J,K)/EM(I,J,K)                         
  134 CONTINUE                                                                            
      DO 136 L=1,LV                                                                       
      DO 136 LL=L,LV                                                                      
  136 SSCP(LL,L,8)=SSCP(L,LL,8)                                                           
      DO 138 L=1,LV                                                                       
      DO 138 LL=1,LV                                                                      
  138 SSCP(L,LL,9)=SSCP(L,LL,8)                                                           
      NW=NE+1                                                                             
      IF(NK-1) 9999,146,140                                                               
  140 DO 144 L=1,LV                                                                       
      DO 144 LL=L,LV                                                                      
      DO 142 M=2,7                                                                        
  142 SSCP(L,LL,8)=SSCP(L,LL,8)-SSCP(L,LL,M)                                              
      SSCP(L,LL,8)=SSCP(L,LL,8)-CT(L,LL)                                                  
  144 SSCP(LL,L,8)=SSCP(L,LL,8)                                                           
  146 DO 148 L=1,LV                                                                       
      DO 148 LL=1,LV                                                                      
  148 SSCP(L,LL,NW)=RSS(L,LL)-SSCP(L,LL,9)                                                
      PRINT 1008                                                                          
      PRINT 1009                                                                          
      DO 150 L=1,LV                                                                       
  150 PRINT 1018,(SSCP(L,LL,1),LL=1,LV)                                                   
      PRINT 1010                                                                          
      DO 152 L=1,LV                                                                       
  152 PRINT 1018,(SSCP(L,LL,2),LL=1,LV)                                                   
      PRINT 1011                                                                          
      DO 154 L=1,LV                                                                       
  154 PRINT 1018,(SSCP(L,LL,3),LL=1,LV)                                                   
      IF(NK-1) 9999,160,156                                                               
  156 PRINT 1012                                                                          
      DO 158 L=1,LV                                                                       
  158 PRINT 1018,(SSCP(L,LL,5),LL=1,LV)                                                   
  160 PRINT 1013                                                                          
      DO 162 L=1,LV                                                                       
  162 PRINT 1018,(SSCP(L,LL,4),LL=1,LV)                                                   
      IF(NK-1) 9999,172,164                                                               
  164 PRINT 1014                                                                          
      DO 166 L=1,LV                                                                       
  166 PRINT 1018,(SSCP(L,LL,6),LL=1,LV)                                                   
      PRINT 1015                                                                          
      DO 168 L=1,LV                                                                       
  168 PRINT 1018,(SSCP(L,LL,7),LL=1,LV)                                                   
      PRINT 1016                                                                          
      DO 170 L=1,LV                                                                       
  170 PRINT 1018,(SSCP(L,LL,8),LL=1,LV)                                                   
  172 PRINT 1017                                                                          
      DO 174 L=1,LV                                                                       
  174 PRINT 1018,(SSCP(L,LL,NW),LL=1,LV)                                                  
 9999 RETURN                                                                              
      END                                                                                 
