      PROGRAM SELI(INPUT,OUTPUT)                                                          
C     SELECTION INDEX CALCULATIONS                                                        
C                                                                                         
C  PURPOSE OF PROGRAM                                                                     
C  ------- -- -------                                                                     
C                                                                                         
C      ACCEPTS AND LISTS "NJOBS" SETS OF HERITABILITIES,PHENOTYPIC VARIANCES,             
C      PHENOTYPIC AND GENETIC CORRILATIONS FOR "NTR" TRAITS. ACCEPTS AND LISTS            
C      "NEW" SETS OF ECONOMIC WEIGHTS FOR EACH SET OF HERITABILITIES ETC.                 
C      CALCULATES AND LISTS SELECTION INDEX COEFFICIENTS AND GENETIC GAIN IN              
C      UNITS OF RELATIVE ECONOMIC VALUE                                                   
C                                                                                         
C  STRUCTURE OF DATA DECK                                                                 
C  --------- -- ---- ----                                                                 
C                                                                                         
C      DATA CARD TYPE 1                                                                   
C      DATA CARDS TYPE 2 FOR JOB 1                                                        
C      DATA CARDS TYPE 3 FOR JOB 1                                                        
C      DATA CARDS TYPE 2 FOR JOB 2                                                        
C      DATA CARDS TYPE 3 FOR JOB 2                                                        
C       ---                                                                               
C       ---                                                                               
C      DATA CARDS TYPE 2 FOR JOB NJOBS                                                    
C      DATA CARDS TYPE 3 FOR JOB NJOBS                                                    
C                                                                                         
C  PUNCHING SCHEDULE FOR DATA CARD TYPE 1                                                 
C  -------- -------- --- ---- ---- ---- -                                                 
C                                                                                         
C      COLUMNS          SYMBOL          INFORMATION                                       
C      -------          ------          -----------                                       
C                                                                                         
C          1-3           NJOBS    NUMBER OF SETS OF GEN AND PHEN PARAMETERS               
C                                                                                         
C  PUNCHING SCHEDULE FOR DATA CARDS TYPE 2                                                
C  -------- -------- --- ---- ----- ---- -                                                
C                                                                                         
C      COLUMNS          SYMBOL          INFORMATION                                       
C      -------          ------          -----------                                       
C                                                                                         
C          1-3             NTR    NUMBER OF TRAITS (MAX=10)                               
C                                                                                         
C          1-4            H(1)    HERITIBILITY OF TRAIT 1 (F4.2)                          
C          5-8            H(2)    HERITABILITY OF TRAIT 2                                 
C           -               -      -                                                      
C           -           H(NTR)    HERITABILITY OF TRAIT NTR                               
C                                                                                         
C          1-4          SIG(1)    PHEN VARIANCE OF TRAIT 1 (F4.2)                         
C          5-8          SIG(2)    PHEN VARIANCE OF TRAIT 2                                
C           -               -      -                                                      
C           -         SIG(NTR)    PHEN VARIANCE OF TRAIT NTR                              
C                                                                                         
C          1-4          P(1,1)    PHEN CORRELATION BETWEEN TRAITS 1 AND 1 (F4.2)          
C          1-4          P(1,2)    PHEN CORRELATION BETWEEN TRAITS 1 AND 2                 
C          5-8          P(2,2)    PHEN CORRELATION BETWEEN TRAITS 2 AND 2                 
C           -               -      -                                                      
C          1-4        P(1,NTR)    PHEN CORRELATION BETWEEN TRAITS 1 AND NTR               
C           -               -      -                                                      
C           -       P(NTR,NTR)    PHEN CORRELATION BETWEEN TRAITS NTR AND NTR             
C                                                                                         
C          1-4          G(1,1)    GEN CORRELATION BETWEEN TRAITS 1 AND 1 (F4.2)           
C          1-4          G(1,2)    GEN CORRELATION BETWEEN TRAITS 1 AND 2                  
C          5-8          G(2,2)    GEN CORRELATION BETWEEN TRAITS 2 AND 2                  
C           -               -      -                                                      
C          1-4        G(1,NTR)    GEN CORRELATION BETWEEN TRAITS 1 AND NTR                
C           -               -      -                                                      
C           -       G(NTR,NTR)    GEN CORRELATION BETWEEN TRAITS NTR AND NTR              
C                                                                                         
C  PUNCHING SCHEDULE FOR DATA CARDS TYPE 3                                                
C  -------- -------- --- ---- ----- ---- -                                                
C                                                                                         
C      COLUMNS          SYMBOL          INFORMATION                                       
C      -------          ------          -----------                                       
C          1-3             NEW    NUMBER OF SETS OF ECONOMIC WEIGHTS                      
C                                   =NUMBER OF INDICES                                    
C                                                                                         
C          1-4            A(1)    ECONOMIC WEIGHT OF TRAIT 1 (F4.0)                       
C          5-8            A(2)    ECONOMIC WEIGHT OF TRAIT 2                              
C           -               -      -                                                      
C           -           A(NTR)    ECONOMIC WEIGHT OF TRAIT NTR                            
C                                                                                         
C                                                                                         
      DIMENSION H(50),SIG(50),P(50,50),G(50,50),A(50),X(50,50),B(50)                      
      READ1,NJOBS                                                                         
      DO99IJ=1,NJOBS                                                                      
      READ1,NTR                                                                           
      READ2,(H(I),I=1,NTR)                                                                
      READ2,(SIG(I),I=1,NTR)                                                              
      DO3I=1,NTR                                                                          
    3 READ2,(P(I,J),J=1,I)                                                                
      DO4I=1,NTR                                                                          
    4 READ2,(G(I,J),J=1,I)                                                                
      PRINT30,IJ                                                                          
      PRINT31,(H(I),I=1,NTR)                                                              
      PRINT32,(SIG(I),I=1,NTR)                                                            
      PRINT33                                                                             
      DO34I=1,NTR                                                                         
   34 PRINT35,(P(I,J),J=1,I)                                                              
      PRINT36                                                                             
      DO37I=1,NTR                                                                         
   37 PRINT35,(G(I,J),J=1,I)                                                              
      PRINT38                                                                             
   30 FORMAT(11H1   JOB NO.,I3)                                                           
   31 FORMAT(16H0 HERITABILITIES,/1H ,5F20.4/(1H ,5F20.4))                                
   32 FORMAT(14H0 PH VARIANCES,/1H ,5F20.4/(1H ,5F20.4))                                  
   33 FORMAT(17H0 PH CORRELATIONS)                                                        
   35 FORMAT(1H0,5F20.4/(1H ,5F20.4))                                                     
   36 FORMAT(18H0 GEN CORRELATIONS)                                                       
   38 FORMAT(31H0------------------------------)                                          
C     A=EC WTS  H=HERITYS  SIG=PH VARS  P=PH CORR  G=GEN CORR                             
      DO5I=1,NTR                                                                          
      SIG(I)=SQRT(SIG(I))                                                                 
    5 H(I)=SQRT(H(I))*SIG(I)                                                              
      PRINT 50                                                                            
   50 FORMAT(15H0PH STAND DEVNS)                                                          
      PRINT 35,(SIG(I),I=1,NTR)                                                           
      PRINT 51                                                                            
   51 FORMAT(16H0GEN STAND DEVNS)                                                         
      PRINT 35,(H(I),I=1,NTR)                                                             
    1 FORMAT(20I3)                                                                        
    2 FORMAT(8F10.2)                                                                      
C-----CONVERT CORRELN MATRICES TO COVARIANCES                                             
      DO6I=1,NTR                                                                          
      DO6J=1,I                                                                            
      P(I,J)=P(I,J)*SIG(I)*SIG(J)                                                         
    6 G(I,J)=G(I,J)*H(I)*H(J)                                                             
      CALL SQUP(P,NTR)                                                                    
      CALLSQUP(G,NTR)                                                                     
      PRINT 40                                                                            
   40 FORMAT(17H0PH (CO)VARIANCES)                                                        
      DO 41 I=1,NTR                                                                       
   41 PRINT 35,(P(I,J),J=1,I)                                                             
      PRINT 42                                                                            
   42 FORMAT(18H0GEN (CO)VARIANCES)                                                       
      DO 43 I=1,NTR                                                                       
   43 PRINT 35,(G(I,J),J=1,I)                                                             
      DO7I=1,NTR                                                                          
    7 CALLPIVOT(P,NTR,I)                                                                  
      PRINT 44                                                                            
   44 FORMAT(29H0INVERSE PH COVARIANCE MATRIX)                                            
      DO 45 I=1,NTR                                                                       
   45 PRINT 35,(P(I,J),J=1,I)                                                             
      DO8I=1,NTR                                                                          
      DO8J=1,NTR                                                                          
      SUM=0.0                                                                             
      DO9K=1,NTR                                                                          
    9 SUM=SUM+P(I,K)*G(K,J)                                                               
    8 X(I,J)=SUM                                                                          
      PRINT 46                                                                            
   46 FORMAT(18H0P-INVERSE * G = X)                                                       
      DO 47 I=1,NTR                                                                       
   47 PRINT 35,(X(I,J),J=1,NTR)                                                           
      READ1,NEW                                                                           
      DO98INEW=1,NEW                                                                      
      READ10,(A(I),I=1,NTR)                                                               
      DO13I=1,NTR                                                                         
      SUM=0.0                                                                             
      DO14K=1,NTR                                                                         
   14 SUM=SUM+X(I,K)*A(K)                                                                 
   13 B(I)=SUM                                                                            
      GAIN=0.0                                                                            
      DO15I=1,NTR                                                                         
      DO15J=1,NTR                                                                         
   15 GAIN=GAIN+A(I)*G(I,J)*B(J)                                                          
   10 FORMAT(10F4.0)                                                                      
      PRINT 38                                                                            
      PRINT20,INEW                                                                        
      PRINT21,(A(I),I=1,NTR)                                                              
      PRINT23,(B(I),I=1,NTR)                                                              
      PRINT22,GAIN                                                                        
   23 FORMAT(24H0 COEFFICIENTS FOR INDEX,/1H0,5F20.4/(1H ,5F20.4))                        
   20 FORMAT(19H0  ECONOMIC WTS SET,I3)                                                   
   21 FORMAT(1H0,5F20.4/(1H ,5F20.4))                                                     
   22 FORMAT(24H0 RELATIVE ECONOMIC GAIN,F20.4)                                           
   98 CONTINUE                                                                            
   99 CONTINUE                                                                            
       END                                                                                
