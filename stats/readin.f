      SUBROUTINE READIN(RSS,CT,SUMY,SUMX1,SUMX2,SUMX3,SUMX4,SUMX5,                        
     2  EMAIN,EM,EMI,EMJ,EMK,TSUM,FMT,EIJ,EIK,EJK,EMIJ,EMIK,EMJK                          
     3  ,NI,NJ,NK,NE,LV,SEM,LC,NW,A,B,NEQ)                                                
      DIMENSION RSS(LV,LV),CT(LV,LV),SUMY(NI,NJ,NK),SUMX1(NI,NJ,NK),                      
     1 SUMX2(NI,NJ,NK),SUMX3(NI,NJ,NK),SUMX4(NI,NJ,NK),SUMX5(NI,NJ,NK),                   
     2      EMAIN(3,NI,LV),EM(NI,NJ,NK),EMI(NI),EMJ(NJ),EMK(NK),X(8 ),                    
     3 TSUM(LV),FMT(12),EIJ(NI,NJ,LV),EIK(NI,NK,LV),EJK(NJ,NK,LV),                        
     4 EMIJ(NI,NJ),EMIK(NI,NK),EMJK(NJ,NK),A(LV,LV),B(LV)                                 
      DIMENSION CELDF(1000),CELVAR(1000)                                                  
      CON=ALOG(10.0)                                                                      
 1002 FORMAT(10X,20F3.0)                                                                  
 1003 FORMAT(12A6)                                                                        
 1005 FORMAT(33H0NUMBER OF I EFFECT CATEGORIES = ,I4)                                     
 1006 FORMAT(33H0NUMBER OF J EFFECT CATEGORIES = ,I4)                                     
 1007 FORMAT(33H0NUMBER OF K EFFECT CATEGORIES = ,I4)                                     
      PRINT 1005,NI                                                                       
      PRINT 1006,NJ                                                                       
      PRINT 1007,NK                                                                       
      READ 1003,(FMT(I),I=1,12)                                                           
      REPS=SEM/(NI*NJ*NK)                                                                 
      PRINT 1020,REPS                                                                     
 1020 FORMAT(8H0REPS = ,F10.2)                                                            
      IF(NK-1) 9999,13,11                                                                 
   11 DO 12 I=1,NI                                                                        
      DO 12 J=1,NJ                                                                        
      IF(NEQ) 111,112,111                                                                 
  111 READ 1002,(EM(I,J,K),K=1,NK)                                                        
      GO TO 12                                                                            
  112 DO 115 K=1,NK                                                                       
  115 EM(I,J,K)=REPS                                                                      
   12 CONTINUE                                                                            
      GO TO 15                                                                            
   13 DO 14 I=1,NI                                                                        
      IF(NEQ) 113,114,113                                                                 
  113 READ 1002,(EM(I,J,1),J=1,NJ)                                                        
      GO TO 14                                                                            
  114 DO 116 J=1,NJ                                                                       
  116 EM(I,J,1)=REPS                                                                      
   14 CONTINUE                                                                            
   15 DO 16 I=1,LV                                                                        
      DO 16 J=1,LV                                                                        
   16 RSS(I,J)=0.0                                                                        
      LC=LV-1                                                                             
      DO 20 I=1,NI                                                                        
      DO 20 J=1,NJ                                                                        
      DO 20 K=1,NK                                                                        
      SUMY(I,J,K)=0.0                                                                     
      SUMX1(I,J,K)=0.0                                                                    
      SUMX2(I,J,K)=0.0                                                                    
      SUMX3(I,J,K)=0.0                                                                    
      SUMX4(I,J,K)=0.0                                                                    
      SUMX5(I,J,K)=0.0                                                                    
   20 CONTINUE                                                                            
      IZER=0                                                                              
      ICEL=0                                                                              
      I=1                                                                                 
      J=1                                                                                 
      K=1                                                                                 
   21 N=EM(I,J,K)+0.1                                                                     
      DO 100 L1=1,LV                                                                      
      B(L1)=0.0                                                                           
      DO 100 L2=L1,LV                                                                     
  100 CT(L1,L2)=0.0                                                                       
   22 CALL DATAVC(FMT,X,LV)                                                               
      DO 101 L1=1,LV                                                                      
      B(L1)=B(L1)+X(L1)                                                                   
      DO 101 L2=L1,LV                                                                     
      CT(L1,L2)=CT(L1,L2)+X(L1)*X(L2)                                                     
  101 CT(L2,L1)=CT(L1,L2)                                                                 
      DO 23 L1=1,LV                                                                       
      DO 23 L2=L1,LV                                                                      
      RSS(L1,L2)=RSS(L1,L2)+X(L1)*X(L2)                                                   
   23 RSS(L2,L1)=RSS(L1,L2)                                                               
      SUMY(I,J,K)=SUMY(I,J,K)+X(1)                                                        
      SUMX1(I,J,K)=SUMX1(I,J,K)+X(2)                                                      
      SUMX2(I,J,K)=SUMX2(I,J,K)+X(3)                                                      
      SUMX3(I,J,K)=SUMX3(I,J,K)+X(4)                                                      
      SUMX4(I,J,K)=SUMX4(I,J,K)+X(5)                                                      
      SUMX5(I,J,K)=SUMX5(I,J,K)+X(6)                                                      
      N=N-1                                                                               
      IF(N) 9999,24,22                                                                    
   24 IF(ABS(EM(I,J,K)-1.0).LT.0.1)106,224                                                
  224 DO 102 L1=1,LV                                                                      
      DO 102 L2=L1,LV                                                                     
      CT(L1,L2)=CT(L1,L2)-B(L1)*B(L2)/EM(I,J,K)                                           
  102 CT(L2,L1)=CT(L1,L2)                                                                 
      ICEL=ICEL+1                                                                         
      CELDF(ICEL)=EM(I,J,K)-1.0                                                           
      CELVAR(ICEL)=CT(1,1)/CELDF(ICEL)                                                    
      IF(CELVAR(ICEL).LT.0.000001)IZER=1                                                  
      PRINT 1008,I,J,K                                                                    
 1008 FORMAT(23H0WITHIN CELL SSCP  I = ,I2,6H  J = ,I2,6H  K = ,I2)                       
      DO 103 L1=1,LV                                                                      
  103 PRINT 1009,(CT(L1,L2),L2=1,LV)                                                      
 1009 FORMAT(1H ,6F14.6)                                                                  
      IF(LC) 106,106,107                                                                  
  107 PRINT 1010                                                                          
 1010 FORMAT(24H0WITHIN CELL REGRESSIONS)                                                 
      DO 104 L1=2,LV                                                                      
  104 B(L1-1)=CT(1,L1)/CT(L1,L1)                                                          
      PRINT 1009,(B(L1),L1=1,LC)                                                          
      DO 105 L1=2,LV                                                                      
      B(L1-1)=CT(1,L1)                                                                    
      DO 105 L2=2,LV                                                                      
  105 A(L1-1,L2-1)=CT(L1,L2)                                                              
      CALL MATINV(A,LC,B,1,DETERM)                                                        
      PRINT 1011                                                                          
 1011 FORMAT(32H0WITHIN CELL PARTIAL REGRESSIONS)                                         
      PRINT 1009,(B(L1),L1=1,LC)                                                          
      D=0.0                                                                               
      DO 108 L=1,LC                                                                       
  108 D=D+B(L)*CT(1,L+1)                                                                  
      PRINT 1012                                                                          
 1012 FORMAT(21H0SS DUE TO REGRESSION)                                                    
      PRINT 1009,D                                                                        
      D=CT(1,1)-D                                                                         
      PRINT 1013                                                                          
 1013 FORMAT(33H0SS OF DEVIATIONS FROM REGRESSION)                                        
      PRINT 1009,D                                                                        
  106 CONTINUE                                                                            
      IF(NK-K) 9999,26,25                                                                 
   25 K=K+1                                                                               
      GO TO 21                                                                            
   26 IF(NJ-J) 9999,28,27                                                                 
   27 J=J+1                                                                               
      K=1                                                                                 
      GO TO 21                                                                            
   28 IF(NI-I) 9999,30,29                                                                 
   29 I=I+1                                                                               
      K=1                                                                                 
      J=1                                                                                 
      GO TO 21                                                                            
   30 IF(ICEL.LE.1)GO TO 31                                                               
      IF(IZER.EQ.1)76,77                                                                  
   76 STAT=COCH(CELDF,CELVAR,ICEL,MNDF,KPUT)                                              
      IF(KPUT.EQ.1)78,79                                                                  
   78 PRINT 1015 $ GO TO 31                                                               
   79 PRINT 1016,STAT,ICEL,MNDF                                                           
      GO TO 31                                                                            
   77 CHISQ=BART(CELDF,CELVAR,ICEL,CON)                                                   
      IDFCH=ICEL-1                                                                        
      PRINT 1014,CHISQ,IDFCH                                                              
 1014 FORMAT(*0  BARTLETT'S CHI-SQUARE STATISTIC FOR HET. OF CELL VARIAN                  
     .CES  *,F10.4,*    DF =*,I4)                                                         
 1015 FORMAT(*0  NO TEST FOR HET. OF VAR. - ALL VARIANCES ZERO*)                          
 1016 FORMAT(*0  COCHRAN'S STATISTIC FOR HET. OF VAR. *,F10.4,*   K = *,                  
     .I3,*   MEAN DF =*,I5)                                                               
   31 DO 44 L=1,LV                                                                        
      DO 44 I=1,NI                                                                        
      DO 44 J=1,NJ                                                                        
      EIJ(I,J,L)=0.0                                                                      
      DO 44 K=1,NK                                                                        
      GO TO (32,34,36,38,40,42),L                                                         
   32 EIJ(I,J,L)=EIJ(I,J,L)+SUMY(I,J,K)                                                   
      GO TO 44                                                                            
   34 EIJ(I,J,L)=EIJ(I,J,L)+SUMX1(I,J,K)                                                  
      GO TO 44                                                                            
   36 EIJ(I,J,L)=EIJ(I,J,L)+SUMX2(I,J,K)                                                  
      GO TO 44                                                                            
   38 EIJ(I,J,L)=EIJ(I,J,L)+SUMX3(I,J,K)                                                  
      GO TO 44                                                                            
   40 EIJ(I,J,L)=EIJ(I,J,L)+SUMX4(I,J,K)                                                  
      GO TO 44                                                                            
   42 EIJ(I,J,L)=EIJ(I,J,L)+SUMX5(I,J,K)                                                  
   44 CONTINUE                                                                            
      IF(NK-1) 9999,75,45                                                                 
   45 DO 60 L=1,LV                                                                        
      DO 60 I=1,NI                                                                        
      DO 60 K=1,NK                                                                        
      EIK(I,K,L)=0.0                                                                      
      DO 60 J=1,NJ                                                                        
      GO TO (46,48,50,52,54,56),L                                                         
   46 EIK(I,K,L)=EIK(I,K,L)+SUMY(I,J,K)                                                   
      GO TO 60                                                                            
   48 EIK(I,K,L)=EIK(I,K,L)+SUMX1(I,J,K)                                                  
      GO TO 60                                                                            
   50 EIK(I,K,L)=EIK(I,K,L)+SUMX2(I,J,K)                                                  
      GO TO 60                                                                            
   52 EIK(I,K,L)=EIK(I,K,L)+SUMX3(I,J,K)                                                  
      GO TO 60                                                                            
   54 EIK(I,K,L)=EIK(I,K,L)+SUMX4(I,J,K)                                                  
      GO TO 60                                                                            
   56 EIK(I,K,L)=EIK(I,K,L)+SUMX5(I,J,K)                                                  
   60 CONTINUE                                                                            
      DO 74 L=1,LV                                                                        
      DO 74 J=1,NJ                                                                        
      DO 74 K=1,NK                                                                        
      EJK(J,K,L)=0.0                                                                      
      DO 74 I=1,NI                                                                        
      GO TO (62,64,66,68,70,72),L                                                         
   62 EJK(J,K,L)=EJK(J,K,L)+SUMY(I,J,K)                                                   
      GO TO 74                                                                            
   64 EJK(J,K,L)=EJK(J,K,L)+SUMX1(I,J,K)                                                  
      GO TO 74                                                                            
   66 EJK(J,K,L)=EJK(J,K,L)+SUMX2(I,J,K)                                                  
      GO TO 74                                                                            
   68 EJK(J,K,L)=EJK(J,K,L)+SUMX3(I,J,K)                                                  
      GO TO 74                                                                            
   70 EJK(J,K,L)=EJK(J,K,L)+SUMX4(I,J,K)                                                  
      GO TO 74                                                                            
   72 EJK(J,K,L)=EJK(J,K,L)+SUMX5(I,J,K)                                                  
   74 CONTINUE                                                                            
   75 DO 80 L=1,LV                                                                        
      DO 80 I=1,NI                                                                        
      EMAIN(1,I,L)=0.0                                                                    
      DO 80 J=1,NJ                                                                        
   80 EMAIN(1,I,L)=EMAIN(1,I,L)+EIJ(I,J,L)                                                
      DO 82 L=1,LV                                                                        
      DO 82 J=1,NJ                                                                        
      EMAIN(2,J,L)=0.0                                                                    
      DO 82 I=1,NI                                                                        
   82 EMAIN(2,J,L)=EMAIN(2,J,L)+EIJ(I,J,L)                                                
      IF(NK-1) 9999,85,83                                                                 
   83 DO 84 L=1,LV                                                                        
      DO 84 K=1,NK                                                                        
      EMAIN(3,K,L)=0.0                                                                    
      DO 84 J=1,NJ                                                                        
   84 EMAIN(3,K,L)=EMAIN(3,K,L)+EJK(J,K,L)                                                
   85 DO 86 L=1,LV                                                                        
      TSUM(L)=0.0                                                                         
      DO 86 I=1,NI                                                                        
   86 TSUM(L)=TSUM(L)+EMAIN(1,I,L)                                                        
      DO 88 L1=1,LV                                                                       
      DO 88 L2=L1,LV                                                                      
      CT(L1,L2)=TSUM(L1)*TSUM(L2)                                                         
   88 CT(L2,L1)=CT(L1,L2)                                                                 
      DO 90 L1=1,LV                                                                       
      DO 90 L2=L1,LV                                                                      
      CT(L1,L2)=CT(L1,L2)/SEM                                                             
   90 CT(L2,L1)=CT(L1,L2)                                                                 
      DO 91 I=1,NI                                                                        
      DO 91 J=1,NJ                                                                        
      EMIJ(I,J)=0.0                                                                       
      DO 91 K=1,NK                                                                        
   91 EMIJ(I,J)=EMIJ(I,J)+EM(I,J,K)                                                       
      IF(NK-1) 9999,95,89                                                                 
   89 DO 92 I=1,NI                                                                        
      DO 92 K=1,NK                                                                        
      EMIK(I,K)=0.0                                                                       
      DO 92 J=1,NJ                                                                        
   92 EMIK(I,K)=EMIK(I,K)+EM(I,J,K)                                                       
      DO 93 J=1,NJ                                                                        
      DO 93 K=1,NK                                                                        
      EMJK(J,K)=0.0                                                                       
      DO 93 I=1,NI                                                                        
   93 EMJK(J,K)=EMJK(J,K)+EM(I,J,K)                                                       
   95 DO 94 I=1,NI                                                                        
      EMI(I)=0.0                                                                          
      DO 94 J=1,NJ                                                                        
   94 EMI(I)=EMI(I)+EMIJ(I,J)                                                             
      DO 96 J=1,NJ                                                                        
      EMJ(J)=0.0                                                                          
      DO 96 I=1,NI                                                                        
   96 EMJ(J)=EMJ(J)+EMIJ(I,J)                                                             
      DO 98 K=1,NK                                                                        
      EMK(K)=0.0                                                                          
      DO 98 J=1,NJ                                                                        
   98 EMK(K)=EMK(K)+EMJK(J,K)                                                             
 9999 RETURN                                                                              
      END                                                                                 
