      PROGRAM RESI(TAPE10,TAPE11,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                   
C     RESTRICTED SELECTION INDEX CALCULATIONS                                             
C                                                                                         
C  PURPOSE OF PROGRAM                                                                     
C  ------- -- -------                                                                     
C                                                                                         
C      ACCEPTS AND LISTS "NJOBS" SETS OF HERITABILITIES,PHENOTYPIC VARIANCES,             
C      PHENOTYPIC AND GENETIC CORRILATIONS FOR "NTR" TRAITS. ACCEPTS AND LISTS            
C      "NEW" SETS OF ECONOMIC WEIGHTS FOR EACH SET OF HERITABILITIES ETC.                 
C     CALCULATES AND LISTS , FOR EACH SET OF ECONOMIC WEIGHTS , THE                       
C     UNRESTRICTED SELECTION INDEX COEFFICIENTS AND GENETIC GAINS IN                      
C      UNITS OF RELATIVE ECONOMIC VALUE (IE AS THE COV OF I AND H).                       
C     ACCEPTS AND LISTS "NSR" SETS OF LINEAR RESTRICTIONS TO THE INDEX                    
C     COEFFICIENTS FOR EACH SET OF HERITABILITIES ETC. CALCULATES AND                     
C     LISTS , FOR EACH SET OF ECONOMIC WEIGHTS , THE RESTRICTED                           
C     SELECTION INDEX COEFFICIENTS AND GENETIC GAINS , IN UNITS OF                        
C     RELATIVE ECONOMIC VALUE. ALSO LISTS THE GENETIC CORRELATION                         
C     BETWEEN THE INDEX AND EACH TRAIT.                                                   
C                                                                                         
C  STRUCTURE OF DATA DECK                                                                 
C  --------- -- ---- ----                                                                 
C                                                                                         
C      DATA CARDS TYPE 1                                                                  
C      DATA CARDS TYPE 2 FOR JOB 1                                                        
C      DATA CARDS TYPE 3 FOR JOB 1                                                        
C     DATA CARDS TYPE 4 FOR JOB 1                                                         
C      DATA CARDS TYPE 2 FOR JOB 2                                                        
C      DATA CARDS TYPE 3 FOR JOB 2                                                        
C     DATA CARDS TYPE 4 FOR JOB 2                                                         
C       ---                                                                               
C       ---                                                                               
C      DATA CARDS TYPE 2 FOR JOB NJOBS                                                    
C      DATA CARDS TYPE 3 FOR JOB NJOBS                                                    
C     DATA CARDS TYPE 4 FOR JOB NJOBS                                                     
C                                                                                         
C  PUNCHING SCHEDULE FOR DATA CARDS TYPE 1                                                
C  -------- -------- --- ---- ----- ---- -                                                
C                                                                                         
C      COLUMNS          SYMBOL          INFORMATION                                       
C      -------          ------          -----------                                       
C                                                                                         
C          1-3           NJOBS    NUMBER OF SETS OF GEN AND PHEN PARAMETERS               
C          4-6            LIOP    LIST OPTION  0=PART,1=FULL,-1=MINIMUM                   
C          7-9            MIOP    0=CARD INPUT   1=I,J INDEXED FILE INPUT                 
C                                                      OF H,G,P                           
C        10-12            MOOP    OUTPUT OPTION                                           
C                                                                                         
C         1-80           IFORM    VARIABLE FORMAT FOR READING CARDS                       
C                                                                                         
C  PUNCHING SCHEDULE FOR DATA CARDS TYPE 2                                                
C  -------- -------- --- ---- ----- ---- -                                                
C                                                                                         
C      COLUMNS          SYMBOL          INFORMATION                                       
C      -------          ------          -----------                                       
C                                                                                         
C     NOTE ALTERNATIVE INPUT VIA TAPE10 OF J,I,H OR G,P                                   
C     ONE ELEMENT PER RECORD , VARIABLE FORMAT                                            
C                                                                                         
C          1-3             NTR    NUMBER OF TRAITS (MAX=20)                               
C                                                                                         
C         VARY          SIG(1)    PHEN VARIANCE OF TRAIT 1                                
C           -           SIG(2)    PHEN VARIANCE OF TRAIT 2                                
C           -               -      -                                                      
C           -         SIG(NTR)    PHEN VARIANCE OF TRAIT NTR                              
C                                                                                         
C         VARY            H(1)    HERITABILITY OF TRAIT 1                                 
C           -             H(2)    HERITABILITY OF TRAIT 2                                 
C           -               -      -                                                      
C           -           H(NTR)    HERITABILITY OF TRAIT NTR                               
C                                                                                         
C         VARY          P(1,1)    PHEN CORRELATION BETWEEN TRAITS 1 AND 1                 
C                                                                                         
C         VARY          P(1,2)    PHEN CORRELATION BETWEEN TRAITS 1 AND 2                 
C           -           P(2,2)    PHEN CORRELATION BETWEEN TRAITS 2 AND 2                 
C           -               -      -                                                      
C                                                                                         
C         VARY        P(1,NTR)    PHEN CORRELATION BETWEEN TRAITS 1 AND NTR               
C           -               -      -                                                      
C           -       P(NTR,NTR)    PHEN CORRELATION BETWEEN TRAITS NTR AND NTR             
C                                                                                         
C         VARY          G(1,1)    GEN CORRELATION BETWEEN TRAITS 1 AND 1                  
C                                                                                         
C         VARY          G(1,2)    GEN CORRELATION BETWEEN TRAITS 1 AND 2                  
C           -           G(2,2)    GEN CORRELATIO  BETWEEN TRAITS 2 AND 2                  
C           -               -      -                                                      
C                                                                                         
C         VARY        G(1,NTR)    GEN CORRELATION BETWEEN TRAITS 1 AND NTR                
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
C         1-80           IFORM    VARIABLE FORMAT FOR READING CARDS                       
C                                                                                         
C         VARY          A(1,1)    ECONOMIC WEIGHT OF TRAIT 1                              
C           -           A(1,2)    ECONOMIC WEIGHT OF TRAIT 2                              
C           -               -      -                                                      
C           -         A(1,NTR)    ECONOMIC WEIGHT OF TRAIT NTR                            
C                                                                                         
C         REPEAT A FOR EACH SET OF ECONOMIC WEIGHTS                                       
C                                                                                         
C  PUNCHING SCHEDULE FOR DATA CARDS TYPE 4                                                
C  -------- -------- --- ---- ----- ---- -                                                
C                                                                                         
C      COLUMNS          SYMBOL          INFORMATION                                       
C      -------          ------          -----------                                       
C          1-3             NSR    NUMBER OF SETS OF RESTRICTIONS                          
C                                                                                         
C         1-80           IFORM    VARIABLE FORMAT FOR READING CARDS                       
C                                                                                         
C          1-3              NR    NUMBER OF RESTRICTIONS OF BINET TYPE                    
C          4-6              NS    NO OF RESTRICTIONS OF KEMPTHORNE OR                     
C                                    TALLIS TYPE                                          
C                                                                                         
C         VARY         EK(1,1)    LHM OF BINET TYPE RESTRICTIONS NO 1                     
C           -          EK(1,2)     -                                                      
C           -               -      -                                                      
C           -        EK(1,NTR)     -                                                      
C                                                                                         
C         VARY         EK(2,1)    LHM OF BINET TYPE RESTRICTION NO 2                      
C           -          EK(2,2)     -                                                      
C           -               -      -                                                      
C           -        EK(2,NTR)     -                                                      
C                                                                                         
C           -                      -                                                      
C           -                      -                                                      
C           -       EK(NR,NTR)     -                                                      
C                                                                                         
C         VARY            D(1)    RHMS OF BINET TYPE RESTRICTIONS                         
C           -             D(2)     -                                                      
C           -               -      -                                                      
C           -            D(NR)     -                                                      
C                                                                                         
C         VARY          C(1,1)    LHM OF TALLIS TYPE RESTRICTION NO 1                     
C           -           C(1,2)     -                                                      
C           -               -      -                                                      
C           -         C(1,NTR)     -                                                      
C                                                                                         
C         VARY          C(2,1)    LHM OF TALLIS TYPE RESTRICTION NO 2                     
C           -           C(2,2)     -                                                      
C           -               -      -                                                      
C           -         C(2,NTR)     -                                                      
C                                                                                         
C           -               -      -                                                      
C           -               -      -                                                      
C           -        C(NS,NTR)     -                                                      
C                                                                                         
C         VARY           EL(1)    RHMS OF TALLIS TYPE RESTRICTIONS                        
C           -            EL(2)     -                                                      
C           -               -      -                                                      
C           -            EL(NS)    -                                                      
C                                                                                         
C         REPEAT NR,NS,EK,D,C,EL, FOR EACH SET OF RESTRICTIONS                            
C                                                                                         
C                                                                                         
      DIMENSION P(50,50),G(50,50),X(50,50),V(50,50),PV(50,50),PP(50,50)                   
      DIMENSION EK(50,50),C(50,50),Q(50,50),W(50,50)                                      
      DIMENSION A(20,50),B(20,50,50)                                                      
      DIMENSION H(50),SIG(50),REG(50),T(50),S(50),PS(50),CYI(50)                          
      DIMENSION D(50),U(50),EL(50),DGY(50)                                                
      DIMENSION IFORM(8)                                                                  
      COMMON /LEV2/ A,B                                                                   
      LEVEL 2,A,B                                                                         
      LC=5                                                                                
      LP=6                                                                                
      MI=10                                                                               
      MO=11                                                                               
      READ (LC,1) NJOBS,LIOP,MIOP,MOOP                                                    
      WRITE(LP,200)NJOBS,LIOP,MIOP,MOOP                                                   
  200 FORMAT(9H0RESI RUN/9H0NJOBS = ,I3,10H   LIOP = ,I3,                                 
     1 10H   MIOP = ,I3,10H   MOOP = ,I3)                                                 
C-----LOOP OVER JOBS                                                                      
      DO 999 IJ=1,NJOBS                                                                   
C-----READ JOB PARAMETERS                                                                 
C-----P AND G MATRICES READ UPPER TRIANGULAR COLUMNWISE SYMMETRIC + DIAG                  
      READ(LC,250) IFORM                                                                  
  250 FORMAT(8A10)                                                                        
      WRITE(LP,251) IFORM                                                                 
  251 FORMAT(22H0VARIABLE INPUT FORMAT/1H ,8A10)                                          
      READ1,NTR                                                                           
      WRITE(LP,201) IJ,NTR                                                                
  201 FORMAT(8H0JOB NO ,I3/7H0NTR = ,I3)                                                  
      IF(NTR-50) 980,980,981                                                              
  981 WRITE(LP,982)                                                                       
  982 FORMAT(17H0NTR MAX EXCEEDED)                                                        
      STOP                                                                                
  980 CONTINUE                                                                            
      READ(LC,IFORM)(SIG(I),I=1,NTR)                                                      
      IF(MIOP) 381,380,381                                                                
C-----FILE INPUT                                                                          
C-----MATRICES MUST BE LOWER TRIANGLE                                                     
  381 READ(LC,250) IFORM                                                                  
      WRITE(LP,251) IFORM                                                                 
      LAST=NTR*(NTR+1)/2                                                                  
      DO 383 K=1,LAST                                                                     
  383 READ(MI,IFORM) I,J,G(I,J),P(I,J)                                                    
      DO 384 I=1,NTR                                                                      
      H(I)=G(I,I)                                                                         
  384 G(I,I)=1.0                                                                          
      GO TO 382                                                                           
C-----DATA CARD TYPE 2 INPUT                                                              
C-----MATRICES MUST BE LOWER TRIANGLE                                                     
  380 READ(LC,IFORM) (H(I),I=1,NTR)                                                       
      DO3I=1,NTR                                                                          
    3 READ(LC,IFORM)(P(I,J),J=1,I)                                                        
      DO4I=1,NTR                                                                          
    4 READ(LC,IFORM)(G(I,J),J=1,I)                                                        
  382 PRINT 30,IJ                                                                         
C-----                                                                                    
      IF(LIOP) 391,390,390                                                                
  390 CONTINUE                                                                            
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
   35 FORMAT(1H ,5F20.4)                                                                  
   36 FORMAT(18H0 GEN CORRELATIONS)                                                       
   38 FORMAT(1H0,10X,10(10H----------))                                                   
  391 CONTINUE                                                                            
C     A=EC WTS  H=HERITYS  SIG=PH VARS  P=PH CORR  G=GEN CORR                             
C-----CONVERT PHEN VARS TO SDS , HSQ TO GEN VARS TO GEN SDS                               
      DO5I=1,NTR                                                                          
      SIG(I)=SQRTF(SIG(I))                                                                
    5 H(I)=SQRTF(H(I))*SIG(I)                                                             
      PRINT 50                                                                            
   50 FORMAT(15H0PH STAND DEVNS)                                                          
      PRINT 35,(SIG(I),I=1,NTR)                                                           
      PRINT 51                                                                            
   51 FORMAT(16H0GEN STAND DEVNS)                                                         
      PRINT 35,(H(I),I=1,NTR)                                                             
    1 FORMAT(20I3)                                                                        
C-----CONVERT P AND G CORRELN MATRICES TO COVARIANCES                                     
      DO6I=1,NTR                                                                          
      DO6J=1,I                                                                            
      P(I,J)=P(I,J)*SIG(I)*SIG(J)                                                         
    6 G(I,J)=G(I,J)*H(I)*H(J)                                                             
C-----SQUARE UP P AND G COVARIANCES                                                       
      CALL SQUP(P,NTR)                                                                    
      CALLSQUP(G,NTR)                                                                     
      IF(LIOP) 392,393,393                                                                
  393 CONTINUE                                                                            
      PRINT 40                                                                            
   40 FORMAT(17H0PH (CO)VARIANCES)                                                        
      DO 41 I=1,NTR                                                                       
   41 PRINT 35,(P(I,J),J=1,I)                                                             
      PRINT 42                                                                            
   42 FORMAT(18H0GEN (CO)VARIANCES)                                                       
      DO 43 I=1,NTR                                                                       
   43 PRINT 35,(G(I,J),J=1,I)                                                             
  392 CONTINUE                                                                            
C-----STORE P IN PP BEFORE INVERSION                                                      
      DO 121 I=1,NTR                                                                      
      DO 121 J=1,NTR                                                                      
  121 PP(I,J)=P(I,J)                                                                      
C-----CALC P INVERSE                                                                      
      DO7I=1,NTR                                                                          
    7 CALLPIVOT(P,NTR,I)                                                                  
      IF(LIOP) 301,301,300                                                                
  300 CONTINUE                                                                            
      PRINT 44                                                                            
   44 FORMAT(29H0INVERSE PH COVARIANCE MATRIX)                                            
      DO 45 I=1,NTR                                                                       
   45 PRINT 35,(P(I,J),J=1,I)                                                             
  301 CONTINUE                                                                            
C-----MULT P-INVERSE BY G                                                                 
      DO8I=1,NTR                                                                          
      DO8J=1,NTR                                                                          
      SUM=0.0                                                                             
      DO9K=1,NTR                                                                          
    9 SUM=SUM+P(I,K)*G(K,J)                                                               
    8 X(I,J)=SUM                                                                          
      IF(LIOP) 303,303,302                                                                
  302 CONTINUE                                                                            
      PRINT 46                                                                            
   46 FORMAT(18H0P-INVERSE * G = X)                                                       
      DO 47 I=1,NTR                                                                       
   47 PRINT 35,(X(I,J),J=1,NTR)                                                           
  303 CONTINUE                                                                            
C-----ECONOMIC WEIGHTS                                                                    
      READ1,NEW                                                                           
      WRITE(LP,205) NEW                                                                   
  205 FORMAT(34H0NO OF SETS OF ECONOMIC WEIGHTS = ,I3)                                    
      IF(NEW-20) 984,984,985                                                              
  985 WRITE(LP,986)                                                                       
  986 FORMAT(17H0NEW MAX EXCEEDED)                                                        
      STOP                                                                                
  984 CONTINUE                                                                            
C-----LOOP OVER ECONOMIC WTS FOR UNRESTRICTED INDEX                                       
      READ(LC,250) IFORM                                                                  
      WRITE(LP,251) IFORM                                                                 
      DO 998 INEW=1,NEW                                                                   
      READ(LC,IFORM)(A(INEW,I),I=1,NTR)                                                   
      DO 13 I=1,NTR                                                                       
C-----CALC INDEX COEFFICIENTS                                                             
      SUM=0.0                                                                             
      DO 14 K=1,NTR                                                                       
   14 SUM=SUM+X(I,K)*A(INEW,K)                                                            
   13 B(INEW,I,1)=SUM                                                                     
C-----CALC COV(HI) AND GAIN                                                               
      CHI=0.0                                                                             
      DO 15 I=1,NTR                                                                       
      DO 15 J=1,NTR                                                                       
   15 CHI=CHI+A(INEW,I)*G(I,J)*B(INEW,J,1)                                                
   10 FORMAT(10F4.0)                                                                      
      PRINT 38                                                                            
      PRINT 20,INEW                                                                       
      WRITE(LP,35)(A(INEW,I),I=1,NTR)                                                     
      PRINT 23,(B(INEW,I,1),I=1,NTR)                                                      
      GAIN=SQRTF(CHI)                                                                     
      WRITE(LP,22) GAIN,CHI                                                               
      REG(INEW)=GAIN                                                                      
   23 FORMAT(37H0 COEFFICIENTS FOR UNRESTRICTED INDEX,/1H0,5F20.4/(                       
     1 1H ,5F20.4))                                                                       
   20 FORMAT(19H0  ECONOMIC WTS SET,I3)                                                   
   22 FORMAT(111H0GENETIC GAIN FOR UNRESTRICTED INDEX IN ECONOMIC WEIGHT                  
     1 UNITS PER UNIT SELECTION DIFFERENTIAL PER GENERATION = ,F20.4/                     
     6 11H0COV(HI) = ,F20.4)                                                              
C-----CALCULATE V(I) , V(H) , R(IH)                                                       
      VI=0.0                                                                              
      VH=0.0                                                                              
      DO 120 I=1,NTR                                                                      
      DO 120 J=1,NTR                                                                      
      VI=VI+B(INEW,I,1)*PP(I,J)*B(INEW,J,1)                                               
  120 VH=VH+A(INEW,I)*G(I,J)*A(INEW,J)                                                    
      SUM=VI*VH                                                                           
      IF(SUM) 122,123,122                                                                 
  123 RIH=0.0                                                                             
      GO TO 124                                                                           
  122 RIH=CHI/SQRT(SUM)                                                                   
  124 PRINT 125,VI,VH,RIH                                                                 
  125 FORMAT(8H0V(I) = ,F20.4,10H   V(H) = ,F20.4,11H   R(IH) = ,F20.4)                   
C-----GENETIC COVARIANCE BETWEEN Y(J) AND I                                               
      DO 130 J=1,NTR                                                                      
      CYI(J)=0.0                                                                          
      DO 130 I=1,NTR                                                                      
  130 CYI(J)=CYI(J)+B(INEW,I,1)*G(I,J)                                                    
      PRINT 116                                                                           
      PRINT 35,(CYI(J),J=1,NTR)                                                           
C-----CORRELATED CHANGES IN ACTUAL UNITS                                                  
      DO 210 J=1,NTR                                                                      
  210 DGY(J)=CYI(J)/SQRTF(VI)                                                             
      WRITE(LP,211)                                                                       
  211 FORMAT(89H0CORRELATED CHANGE IN Y(J) IN ACTUAL UNITS PER UNIT SELE                  
     1CTION DIFFERENTIAL PER GENERATION)                                                  
      WRITE(LP,35)(DGY(J),J=1,NTR)                                                        
C-----GENETIC CORRELATION B/N Y(J) AND I                                                  
      DO 131 J=1,NTR                                                                      
      SUM=H(J)*H(J)*VH                                                                    
      IF(SUM) 132,133,132                                                                 
  133 CYI(J)=0.0                                                                          
      GO TO 131                                                                           
  132 CYI(J)=CYI(J)/SQRT(SUM)                                                             
  131 CONTINUE                                                                            
      PRINT 127                                                                           
      PRINT 35,(CYI(J),J=1,NTR)                                                           
  998 CONTINUE                                                                            
C-----TEST WHETHER THIS JOB HAS SETS OF RESTRICTIONS                                      
      WRITE(LP,38)                                                                        
      WRITE(LP,220)                                                                       
  220 FORMAT(19H0RESTRICTED INDICES)                                                      
      READ 1,NSR                                                                          
      PRINT 52,NSR                                                                        
   52 FORMAT(30H0NO OF SETS OF RESTRICTIONS = ,I4)                                        
      IF(NSR-49) 987,987,988                                                              
  988 WRITE(LP,989)                                                                       
  989 FORMAT(17H0NSR MAX EXCEEDED)                                                        
      STOP                                                                                
  987 CONTINUE                                                                            
      IF(NSR) 999,999,996                                                                 
C-----LOOP OVER SETS OF RESTRICTIONS                                                      
  996 READ(LC,250) IFORM                                                                  
      WRITE(LP,251) IFORM                                                                 
      DO 60 ISR=1,NSR                                                                     
      WRITE(LP,38)                                                                        
      PRINT 58,ISR                                                                        
   58 FORMAT(20H0RESTRICTION SET NO ,I4)                                                  
      READ 1,NR,NS                                                                        
      PRINT 53,NR,NS                                                                      
   53 FORMAT(25H0NO OF RESI BINET TYPE = ,I4/                                             
     1 40H0NO OF RESI KEMPTHORNE OR TALLIS TYPE = ,I4)                                    
C-----BINET TYPE                                                                          
      IF(NR+NS-NTR) 57,60,60                                                              
   57 IF(NR) 54,54,55                                                                     
   55 IF(LIOP) 394,395,395                                                                
  395 PRINT 62                                                                            
  394 CONTINUE                                                                            
   62 FORMAT(9H0K MATRIX)                                                                 
      DO 61 I=1,NR                                                                        
      READ(LC,IFORM)(EK(I,J),J=1,NTR)                                                     
      DO 67 J=1,NTR                                                                       
   67 Q(I,J)=EK(I,J)                                                                      
      IF(LIOP) 61,397,397                                                                 
  397 PRINT 35,(EK(I,J),J=1,NTR)                                                          
   61 CONTINUE                                                                            
      READ(LC,IFORM)(D(I),I=1,NR)                                                         
      DO 68 I=1,NR                                                                        
   68 U(I)=D(I)                                                                           
      PRINT 63                                                                            
      IF(LIOP) 396,398,398                                                                
  398 CONTINUE                                                                            
   63 FORMAT(9H0D VECTOR)                                                                 
      PRINT 35,(D(I),I=1,NR)                                                              
  396 CONTINUE                                                                            
C-----KEMPTHORNE OR TALLIS TYPE                                                           
   54 IF(NS) 155,155,56                                                                   
   56 IF(LIOP) 399,400,400                                                                
  400 PRINT 64                                                                            
  399 CONTINUE                                                                            
   64 FORMAT(9H0C MATRIX)                                                                 
      DO 65 I=1,NS                                                                        
      READ(LC,IFORM)(C(I,J),J=1,NTR)                                                      
      DO 69 J=1,NTR                                                                       
      SUM=0.0                                                                             
      DO 70 K=1,NTR                                                                       
   70 SUM=SUM+C(I,K)*G(K,J)                                                               
   69 Q(I+NR,J)=SUM                                                                       
      IF(LIOP) 65,401,401                                                                 
  401 PRINT 35,(C(I,J),J=1,NTR)                                                           
   65 CONTINUE                                                                            
      READ(LC,IFORM)(EL(I),I=1,NS)                                                        
      DO 71 I=1,NS                                                                        
   71 U(I+NR)=EL(NS)                                                                      
      PRINT 66                                                                            
      IF(LIOP) 402,403,403                                                                
  403 CONTINUE                                                                            
   66 FORMAT(9H0L VECTOR)                                                                 
      PRINT 35,(EL(I),I=1,NS)                                                             
  402 CONTINUE                                                                            
C-----COMBINE BOTH TYPES IN PRINTOUT                                                      
  155 NRS=NR+NS                                                                           
      IF(NRS) 60,60,156                                                                   
  156 IF(LIOP) 404,405,405                                                                
  405 PRINT 73                                                                            
   73 FORMAT(9H0Q MATRIX)                                                                 
      DO 74 I=1,NRS                                                                       
   74 PRINT 35,(Q(I,J),J=1,NTR)                                                           
      PRINT 75                                                                            
   75 FORMAT(9H0U VECTOR)                                                                 
      PRINT 35,(U(I),I=1,NRS)                                                             
C-----CALC REG FOR EACH SET OF EC WTS                                                     
C-----CALC B FOR EACH SET OF CE WTS FOR THIS SET OF RESI                                  
C-----STEPS 1,2,3,4,5,7,8 ARE INDEPENDANT OF EC WTS AND SO OUTSIDE WTS LOOP               
C-----STEP 1 CALC Q * P-INVERSE * Q-TRANSPOSE = W                                         
  404 IF(LIOP) 306,306,305                                                                
  305 CONTINUE                                                                            
      PRINT 80                                                                            
   80 FORMAT(22H0Q * P-INVERSE * Q = W)                                                   
  306 CONTINUE                                                                            
      DO 81 I=1,NRS                                                                       
      DO 82 J=1,NRS                                                                       
      SUM=0.0                                                                             
      DO 83 K=1,NTR                                                                       
      DO 83 L=1,NTR                                                                       
      SUM=SUM+Q(I,K)*P(K,L)*Q(J,L)                                                        
   83 CONTINUE                                                                            
      W(I,J)=SUM                                                                          
   82 CONTINUE                                                                            
      IF(LIOP) 81,81,304                                                                  
  304 WRITE(LP,35)(W(I,J),J=1,NRS)                                                        
   81 CONTINUE                                                                            
C-----STEP 2 INVERT W                                                                     
      IF(LIOP) 308,307,307                                                                
  307 CONTINUE                                                                            
      PRINT 84                                                                            
   84 FORMAT(10H0W-INVERSE)                                                               
  308 CONTINUE                                                                            
      DO 85 I=1,NRS                                                                       
   85 CALL PIVOT(W,NRS,I)                                                                 
      IF(LIOP) 310,310,309                                                                
  309 CONTINUE                                                                            
      DO 86 I=1,NRS                                                                       
   86 PRINT 35,(W(I,J),J=1,NRS)                                                           
C-----STEP 3 CALC Q-TRANSPOSE * W-INVERSE * Q = V                                         
      PRINT 87                                                                            
   87 FORMAT(22H0Q * W-INVERSE * Q = V)                                                   
  310 CONTINUE                                                                            
      DO 88 I=1,NTR                                                                       
      DO 89 J=1,NTR                                                                       
      SUM=0.0                                                                             
      DO 90 K=1,NRS                                                                       
      DO 90 L=1,NRS                                                                       
      SUM=SUM+Q(K,I)*W(K,L)*Q(L,J)                                                        
   90 CONTINUE                                                                            
      V(I,J)=SUM                                                                          
   89 CONTINUE                                                                            
      IF(LIOP) 88,88,311                                                                  
  311 WRITE(LP,35)(V(I,J),J=1,NTR)                                                        
   88 CONTINUE                                                                            
C-----STEP 4 CALC P-INVERSE * V = PV                                                      
      IF(LIOP) 313,313,312                                                                
  312 CONTINUE                                                                            
      PRINT 91                                                                            
   91 FORMAT(19H0P-INVERSE * V = PV)                                                      
  313 CONTINUE                                                                            
      DO 92 I=1,NTR                                                                       
      DO 93 J=1,NTR                                                                       
      SUM=0.0                                                                             
      DO 94 K=1,NTR                                                                       
   94 SUM=SUM+P(I,K)*V(K,J)                                                               
      PV(I,J)=SUM                                                                         
   93 CONTINUE                                                                            
      IF(LIOP) 92,92,314                                                                  
  314 WRITE(LP,35)(PV(I,J),J=1,NTR)                                                       
   92 CONTINUE                                                                            
C-----STEP 5 CALC I - PV = PV                                                             
      IF(LIOP) 316,316,315                                                                
  315 CONTINUE                                                                            
      PRINT 95                                                                            
   95 FORMAT(12H0I - PV = PV)                                                             
  316 CONTINUE                                                                            
      DO 117 I=1,NTR                                                                      
      DO 96 J=1,NTR                                                                       
      IF(I-J) 97,98,97                                                                    
   97 PV(I,J)=-PV(I,J)                                                                    
      GO TO 96                                                                            
   98 PV(I,J)=1.-PV(I,J)                                                                  
   96 CONTINUE                                                                            
      IF(LIOP) 117,117,317                                                                
  317 WRITE(LP,35)(PV(I,J),J=1,NTR)                                                       
  117 CONTINUE                                                                            
C-----STEP 7 CALC Q-TRANSPOSE * W-INVERSE * U = S                                         
      IF(LIOP) 319,319,318                                                                
  318 CONTINUE                                                                            
      PRINT 102                                                                           
  102 FORMAT(22H0Q * W-INVERSE * U = S)                                                   
  319 CONTINUE                                                                            
      DO 103 I=1,NTR                                                                      
      SUM=0.0                                                                             
      DO 104 K=1,NRS                                                                      
      DO 104 L=1,NRS                                                                      
  104 SUM=SUM+Q(K,I)*W(K,L)*U(L)                                                          
  103 S(I)=SUM                                                                            
      IF(LIOP) 321,321,320                                                                
  320 CONTINUE                                                                            
      PRINT 35,(S(I),I=1,NTR)                                                             
C-----STEP 8 CALC P-INVERSE * S = PS                                                      
      PRINT 105                                                                           
  105 FORMAT(19H0P-INVERSE * S = PS)                                                      
  321 CONTINUE                                                                            
      DO 106 I=1,NTR                                                                      
      SUM=0.0                                                                             
      DO 107 J=1,NTR                                                                      
  107 SUM=SUM+P(I,J)*S(J)                                                                 
  106 PS(I)=SUM                                                                           
      IF(LIOP) 323,323,322                                                                
  322 CONTINUE                                                                            
      PRINT 35,(PS(I),I=1,NTR)                                                            
C-----LOOP OVER ECONOMIC WTS                                                              
  323 CONTINUE                                                                            
      DO 997 INEW=1,NEW                                                                   
C-----STEP 6 CALC PV * B-UNRESTRICTED = T                                                 
      WRITE(LP,38)                                                                        
      IF(LIOP) 325,325,324                                                                
  324 CONTINUE                                                                            
      PRINT 99                                                                            
   99 FORMAT(24H0PV * UNRESTRICTED-B = T)                                                 
  325 CONTINUE                                                                            
      DO 100 I=1,NTR                                                                      
      SUM=0.0                                                                             
      DO 101 J=1,NTR                                                                      
  101 SUM=SUM+PV(I,J)*B(INEW,J,1)                                                         
  100 T(I)=SUM                                                                            
      IF(LIOP) 327,327,326                                                                
  326 CONTINUE                                                                            
      PRINT 35,(T(I),I=1,NTR)                                                             
C-----STEP 9 RESTRICTED INDEX COEFFICIENTS                                                
  327 CONTINUE                                                                            
      PRINT 108                                                                           
  108 FORMAT(30H0RESTRICTED INDEX COEFFICIENTS)                                           
      DO 109 I=1,NTR                                                                      
  109 B(INEW,I,ISR+1)=T(I)+PS(I)                                                          
      PRINT 35,(B(INEW,I,ISR+1),I=1,NTR)                                                  
C-----PHEN VARIANCE OF INDEX VI                                                           
C-----GEN VARIANCE OF INDEX VH                                                            
C-----COVARIANCE OF H,I CHI                                                               
      VI=0.0                                                                              
      VH=0.0                                                                              
      CHI=0.0                                                                             
      DO 110 I=1,NTR                                                                      
      DO 110 J=1,NTR                                                                      
      VI=VI+B(INEW,I,ISR+1)*PP(I,J)*B(INEW,J,ISR+1)                                       
      VH=VH+A(INEW,I)*G(I,J)*A(INEW,J)                                                    
  110 CHI=CHI+A(INEW,I)*G(I,J)*B(INEW,J,ISR+1)                                            
      GAIN=SQRTF(CHI)                                                                     
      WRITE(LP,111) GAIN,CHI                                                              
  111 FORMAT(109H0GENETIC GAIN FOR RESTRICTED INDEX IN ECONOMIC WEIGHT U                  
     1NITS PER UNIT SELECTION DIFFERENTIAL PER GENERATION = ,F20.4/                       
     2 11H0COV(HI) = ,F20.4)                                                              
C-----CORRELATION BETWEEN I AND H  RIH                                                    
      SUM=VI*VH                                                                           
      IF(SUM) 112,113,112                                                                 
  113 RIH=0.0                                                                             
      GO TO 114                                                                           
  112 RIH=CHI/SQRT(SUM)                                                                   
  114 PRINT 125,VI,VH,RIH                                                                 
C-----GENETIC COVARIANCE BETWEEN Y(J) AND I = BT*G(J) = CYI(J)                            
      DO 115 J=1,NTR                                                                      
      CYI(J)=0.0                                                                          
      DO 115 I=1,NTR                                                                      
  115 CYI(J)=CYI(J)+B(INEW,I,ISR+1)*G(I,J)                                                
      PRINT 116                                                                           
  116 FORMAT(38H0GENETIC COVARIANCE BETWEEN Y(J) AND I)                                   
      PRINT 35,(CYI(J),J=1,NTR)                                                           
C-----CORRELATED CHANGE IN ACTUAL UNITS                                                   
      DO 230 J=1,NTR                                                                      
  230 DGY(J)=CYI(J)/SQRTF(VI)                                                             
      WRITE(LP,211)                                                                       
      WRITE(LP,35)(DGY(J),J=1,NTR)                                                        
C-----GENETIC CORRELATION BETWEEN Y(J) AND I                                              
      DO 118 J=1,NTR                                                                      
      SUM=H(J)*H(J)*VH                                                                    
      IF(SUM) 119,126,119                                                                 
  126 CYI(J)=0.0                                                                          
      GO TO 118                                                                           
  119 CYI(J)=CYI(J)/SQRT(SUM)                                                             
  118 CONTINUE                                                                            
      PRINT 127                                                                           
  127 FORMAT(39H0GENETIC CORRELATION BETWEEN Y(J) AND I)                                  
      PRINT 35,(CYI(J),J=1,NTR)                                                           
  997 CONTINUE                                                                            
   60 CONTINUE                                                                            
  999 CONTINUE                                                                            
      STOP                                                                                
      END                                                                                 
