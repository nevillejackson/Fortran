      PROGRAM SELIND(INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                               
C      PROGRAM SELIND BY E.P.CUNNINGHAM                                                   
C      PROGRAM SELIND COMPUTES SELECTION INDEXES B"X FOR AGGREGATE GENO                   
C      TYPE V"Y. REQUIRES N(NO. OF X"S),M(NO. OF Y"S) P(N*N COV MATRIX O                  
C     X"S), G(N*M COV MATRIX OF X"S WITH Y"S), C(M*M COV MATRIX OF Y"S),                  
C     V(M*1 VECTOR OF ECON VALUES OF Y"S), XNAMES, YNAMES. REDUCED INDEXES                
C     AND/OR RESTRICTED INDEXES CAN BE CALLED BY TRAILER CARDS                            
C                                                                                         
C      MULTI-STAGE VERSION BY E.P.CUNNINGHAM AND G,A.T.MAHON                              
C      TWO-STAGE INDEXES CAN BE CALLED BY TRAILER CARDS,WHICH SHOULD                      
C      SPECIFY VARIATES *NOT* INCLUDED IN FIRST ROUND OF SELECTION                        
C      AND PERCENTAGE OF CANDIDATES SELECTED IN FIRST ROUND                               
C                                                                                         
       COMMON P(325),G(25,25),BSAVE(25),C(325),                                           
     1PSAVE(325),GSAVE(25,25),CSAVE(325),NPC,VARINS                                       
      DIMENSION V(25),WORK(25),W(325),KM(25),                                             
     1XNAME(25,5),YVAR(25),YNAME(25,5),RHM(25),RVAL(25),                                  
     2CHANGE(25),XNAMES(25,5),LIMIT(5),CASH(25),B(25)                                     
      EQUIVALENCE(W(1),C(1))                                                              
      REAL NUM(25)                                                                        
      DATA NUM/"  1","  2","  3","  4","  5","  6","  7","  8","  9"," 1                  
     1 "," 11"," 12"," 13"," 14"," 15"," 16"," 17"," 18"," 19"," 20"," 2                  
     21"," 22"," 23"," 24"," 25"/                                                         
      DATA XN1,XN2,XN3,XN4/"DUMM","Y  T","RAIT","    "/                                   
C     INPUT N,M,XNAMES,P,YNAMES+V,G,C                                                     
       MULT=0                                                                             
   55 READ(5,1)N,M,NR                                                                     
  1    FORMAT(3I5)                                                                        
      NSAVE=N                                                                             
       MSAVE=M                                                                            
      WRITE(6,60)N,M                                                                      
  60  FORMAT("1","SELECTION INDEX - INPUT DATA.",I5," VARIATES",I5                        
     >" TRAITS"///)                                                                       
      DO 2 I=1,N                                                                          
      READ(5,3)(XNAME(I,J),J=1,5)                                                         
    3 FORMAT(5A4)                                                                         
      WRITE(6,61)I,(XNAME(I,J),J=1,5)                                                     
  61  FORMAT(I3,2X,5A4)                                                                   
      DO 302 J=1,5                                                                        
  302 XNAMES(I,J)=XNAME(I,J)                                                              
    2 CONTINUE                                                                            
      N1  =(N*(N+1))/2                                                                    
      WRITE(6,62)                                                                         
  62  FORMAT(///1X,"P-MATRIX"/)                                                           
      WRITE(6,69)                                                                         
  69  FORMAT(10X,"J         K            COVARIANCE"/)                                    
      DO 6 I=1,N1                                                                         
      READ(5,7)J,K,COV                                                                    
    7 FORMAT(2I5,F10.0)                                                                   
      L=(J-1)*N-((J-1)*J)/2+K                                                             
      P(L)=COV                                                                            
      PSAVE(L)=COV                                                                        
      WRITE(6,63)J,K,COV                                                                  
  63  FORMAT(1X,2I10,F20.5)                                                               
    6 CONTINUE                                                                            
      WRITE(6,64)                                                                         
  64  FORMAT(///)                                                                         
      DO 9 I=1,M                                                                          
      READ(5,10)(YNAME(I,J),J=1,5),V(I)                                                   
   10 FORMAT(5A4,F10.0)                                                                   
      WRITE(6,66)I,(YNAME(I,J),J=1,5),V(I)                                                
  66  FORMAT(I3,2X,5A4,F20.5)                                                             
    9 CONTINUE                                                                            
      N1=N*M                                                                              
      WRITE(6,67)                                                                         
  67  FORMAT(///1X,"G-MATRIX"/)                                                           
      WRITE(6,69)                                                                         
      DO 11 I=1,N1                                                                        
      READ(5,7)J,K,COV                                                                    
      G(J,K)=COV                                                                          
      GSAVE(J,K)=COV                                                                      
      WRITE(6,63)J,K,COV                                                                  
   11 CONTINUE                                                                            
      WRITE(6,68)                                                                         
  68  FORMAT(///1X"C-MATRIX"/)                                                            
      WRITE(6,69)                                                                         
      N1=(M*(M+1)/2)                                                                      
      DO 13 I=1,N1                                                                        
      READ(5,7)J,K,COV                                                                    
      L=(J-1)*M-((J-1)*J)/2+K                                                             
      C(L)=COV                                                                            
      CSAVE(L) =COV                                                                       
      WRITE(6,63)J,K,COV                                                                  
      IF(J-K)13,40,13                                                                     
   40 YVAR(J)=SQRT(COV)                                                                   
   13 CONTINUE                                                                            
       WRITE(6,1999)                                                                      
 1999  FORMAT("1",22(1H*)/" SINGLE STAGE SELECTION"/" ",22(1H*))                          
       GO TO 2000                                                                         
 2001  CONTINUE                                                                           
       K=1                                                                                
        DO 2002 J=1,M                                                                     
       YVAR(J)=SQRT(C(K))                                                                 
       K=K+M-J+1                                                                          
 2002  CONTINUE                                                                           
 2000  CONTINUE                                                                           
C     GET VAR OF AGGREGATE GENOTYPE = VCV                                                 
      N1=N                                                                                
      N=M                                                                                 
      VARTBV=0                                                                            
      DO 202 I=1,N                                                                        
      SUM=0                                                                               
      DO 203 J=1,N                                                                        
      IF(I-J)204,204,205                                                                  
  204 L=(I-1)*N-((I-1)*I)/2+J                                                             
      GO TO 203                                                                           
  205 L=(J-1)*N-((J-1)*J)/2+I                                                             
  203 SUM=SUM+C(L)*V(J)                                                                   
  202 WORK(I)=SUM                                                                         
      DO 17 I=1,M                                                                         
      VARTBV=VARTBV+WORK(I)*V(I)                                                          
   17 CONTINUE                                                                            
      N=N1                                                                                
C     MULTIPLY G BY V                                                                     
       DO 14 I=1,N                                                                        
      RHM(I)=0                                                                            
      DO 15 J=1,M                                                                         
      RHM(I)=RHM(I)+G(I,J)*V(J)                                                           
   15 CONTINUE                                                                            
   14 CONTINUE                                                                            
C     INVERT P INTO W                                                                     
  992 DO 995 I=2,N                                                                        
      IM1=I-1                                                                             
      DO 993 K=1,IM1                                                                      
 770  KI=N*(K-1)-(K*(K-1))/2+I                                                            
      KK=N*(K-1)-(K*(K-3))/2                                                              
  993 WORK(K)=P(KI)/P(KK)                                                                 
      DO 995 J=I,N                                                                        
      IJ=N*(I-1)-(I*(I-1))/2+J                                                            
      SUM=P(IJ)                                                                           
      DO 994 K=1,IM1                                                                      
      KJ=N*(K-1)-(K*(K-1))/2+J                                                            
  994 SUM=SUM-P(KJ)*WORK(K)                                                               
  995 P(IJ)=SUM                                                                           
      DO 911 I=1,N                                                                        
      IP=N+1-I                                                                            
      DO 911 J=I,N                                                                        
      JP=N+1-J                                                                            
      IF(IP-JP)997,996,997                                                                
  996 SUM=1.                                                                              
      IF(IP-N)998,953,998                                                                 
  997 SUM=0.                                                                              
  998 JP1=JP+1                                                                            
      DO 999 K=JP1,N                                                                      
      JPK=N*(JP-1)-(JP*(JP-1))/2+K                                                        
      IF(IP-K)950,950,951                                                                 
  950 IPK=N*(IP-1)-(IP*(IP-1))/2+K                                                        
      GO TO 999                                                                           
  951 IPK=N*(K-1)-(K*(K-1))/2+IP                                                          
  999 SUM=SUM-P(JPK)*W(IPK)                                                               
      IF(IP-JP)952,952,953                                                                
  952 IPJ=N*(IP-1)-(IP*(IP-1))/2+JP                                                       
      GO TO 954                                                                           
  953 IPJ=N*(JP-1)-(JP*(JP-1))/2+IP                                                       
  954 JPJ=N*(JP-1)-(JP*(JP-3))/2                                                          
  911 W(IPJ)=SUM/P(JPJ)                                                                   
C      OPTIONAL PRINTING OF P-INVERSE                                                     
       NLIM=(N*(N+1))/2                                                                   
       WRITE(6,1001)(W(I),I=1,NLIM)                                                       
 1001  FORMAT(1H-,7(2X,7F11.6//))                                                         
C     GET B-VALUES=WGV                                                                    
      DO 232 I=1,N                                                                        
      SUM=0                                                                               
      DO 213 J=1,N                                                                        
      IF(I-J)214,214,215                                                                  
  214 L=(I-1)*N-((I-1)*I)/2+J                                                             
      GO TO 213                                                                           
  215 L=(J-1)*N-((J-1)*J)/2+I                                                             
  213 SUM=SUM+W(L)*RHM(J)                                                                 
  232 B(I)=SUM                                                                            
C     GET VAR OF INDEX =BPB = BGV                                                         
      VARIND=0                                                                            
      DO 16 I=1,N                                                                         
      VARIND = VARIND+RHM(I)*B(I)                                                         
   16 CONTINUE                                                                            
       VARINS=VARIND                                                                      
      IF(VARIND)556,557,557                                                               
  557 SDIND=SQRT(VARIND)                                                                  
      SDTBV=SQRT(VARTBV)                                                                  
      CORR=SDIND/SDTBV                                                                    
C     GET REL VALUES OF VARIATES                                                          
      DO 19 I=1,N                                                                         
      L=(I-1)*N-((I-1)*I)/2+I                                                             
      IF(VARIND-(B(I)*B(I))/W(L)) 250,250,251                                             
  251 RVAL(I)=100-SQRT((VARIND-(B(I)*B(I))/W(L))/VARIND)*100                              
      GO TO 19                                                                            
  250 RVAL(I)=100                                                                         
   19 CONTINUE                                                                            
C     GET REGR AND CORR OF TRAITS WITH INDEX                                              
      DO 20 J=1,M                                                                         
      CHANGE(J)=0                                                                         
      WORK(J)=0                                                                           
      DO 21 I=1,N                                                                         
      CHANGE(J)=CHANGE(J)+G(I,J)*B(I)                                                     
   21 CONTINUE                                                                            
      WORK(J)=CHANGE(J)/(SDIND*YVAR(J))                                                   
      CHANGE(J)=CHANGE(J)/VARIND                                                          
      CASH(J)=CHANGE(J)*V(J)*100                                                          
   20 CONTINUE                                                                            
C     BEGIN OUTPUT                                                                        
      WRITE(6,23)                                                                         
      WRITE(6,405)                                                                        
   23 FORMAT("1","SELECTION INDEX  - MAIN INDEX")                                         
      DO 30 I=1,N                                                                         
      WRITE(6,407)I,(XNAME(I,J),J=1,5),B(I),RVAL(I)                                       
   30 CONTINUE                                                                            
      WRITE(6,24)VARIND                                                                   
  24   FORMAT(///" VARIANCE OF INDEX    ",F10.4)                                          
      WRITE(6,25)SDIND                                                                    
   25 FORMAT("  STD DEV OF INDEX    ",F10.4,"  (2)")                                      
      WRITE(6,26)VARTBV                                                                   
   26 FORMAT(//22H VARIANCE OF AGG GENOT,F10.4)                                           
      WRITE(6,27) SDTBV                                                                   
   27 FORMAT("  STD DEV OF AGG GENOT",F10.4)                                              
      WRITE(6,28)CORR                                                                     
   28 FORMAT(///35H CORRELATION OF INDEX AND AGG GENOT,F10.4)                             
      WRITE(6,38)                                                                         
   38 FORMAT(///" REGRESSION OF EACH TRAIT ON INDEX,CORRELATION OF EACH                   
     1TRAIT WITH INDEX,PERCENT OF ECONOMIC GAINS (3)")                                    
      WRITE(6,37)                                                                         
   37 FORMAT(" ",12X,"TRAIT",12X,"REGRESSION   CORRELATION  PCT OF GAIN"                  
     1)                                                                                   
      DO 39 I=1,M                                                                         
      WRITE(6,42) I,(YNAME(I,J),J=1,5),CHANGE(I),WORK(I),CASH(I)                          
   42 FORMAT(I3,2X,5A4,4X,F10.4,4X,2F10.4)                                                
   39 CONTINUE                                                                            
      WRITE(6,29)                                                                         
   29 FORMAT(///" (1) VALUE OF EACH VARIATE IN THE INDEX = PERCENT REDUC                  
     1TION"/" IN RATE OF OVERALL GENETIC GAIN IF THAT VARIATE IS OMITTED                  
     2"/"0(2) THIS IS THE VALUE,IN ECONOMIC UNITS,OF THE GENETIC GAIN IN                  
     3"/" AGGREGATE GENOTYPE ACHIEVED BY ONE STANDARD DEVIATION OF SELEC                  
     4TION ON THE INDEX"/"0(3) THESE FIGURES ARE THE PERCENTAGES OF TOTA                  
     5L GAIN (2)"/" ACCOUNTED FOR BY GAIN IN EACH TRAIT")                                 
C     GET SINGLE TRAIT SUBINDEXES                                                         
  400 DO 401 K=1,M                                                                        
      DO 402 I=1,N                                                                        
      SUM=0                                                                               
      DO 403 J=1,N                                                                        
      IF(I-J)414,414,415                                                                  
  414 L=(I-1)*N-((I-1)*I)/2+J                                                             
      GO TO 403                                                                           
  415 L=(J-1)*N-((J-1)*J)/2+I                                                             
  403 SUM=SUM+W(L)*G(J,K)                                                                 
  402 B(I)=SUM                                                                            
C     GET VARIANCE OF SUBINDEX                                                            
      VARIND=0                                                                            
      DO 416 I=1,N                                                                        
  416 VARIND=VARIND+B(I)*G(I,K)                                                           
      IF(VARIND)566,558,558                                                               
  558 CORR=(SQRT(VARIND))/YVAR(K)                                                         
C     GET REL VALUES OF VARIATES IN SUBINDEX                                              
      DO 419 I=1,N                                                                        
      L=(I-1)*N-((I-1)*I)/2+I                                                             
      IF(VARIND-(B(I)*B(I))/W(L))350,350,351                                              
  351 RVAL(I)=100-SQRT((VARIND-(B(I)*B(I))/W(L))/VARIND)*100                              
      GO TO 419                                                                           
  350 RVAL(I)=100                                                                         
  419 CONTINUE                                                                            
C     OUTPUT SUBINDEX                                                                     
      WRITE(6,404)(YNAME(K,J),J=1,5),VARIND,CORR                                          
  404 FORMAT("1","SUBINDEX FOR TRAIT",4X,5A4,/"0","VARIANCE OF SUBINDEX"                  
     1,4X,F10.4,6X,"CORRELATION OF SUBINDEX AND TRAIT",4X,F10.4)                          
      WRITE(6,405)                                                                        
  405 FORMAT(///10X,"VARIATE",15X,"B-VALUE",13X,"VALUE OF VARIATE (1)")                   
      DO 406 I=1,N                                                                        
      WRITE(6,407)I,(XNAME(I,J),J=1,5),B(I),RVAL(I)                                       
  407 FORMAT(I3,2X,5A4,4X,F10.4,14X,F10.4)                                                
  406 CONTINUE                                                                            
      GO TO 401                                                                           
  566 WRITE(6,567)(YNAME(K,I),I=1,5),VARIND                                               
  567 FORMAT("1","VARIANCE OF SUBINDEX FOR TRAIT",5X,5A4,5X,"IS NEGATIVE                  
     2  ",F10.4)                                                                          
  401 CONTINUE                                                                            
C     BEGIN REDUCED AND/OR RESTRICTED INDEXES                                             
  559 N=NSAVE                                                                             
       IF(MULT.EQ.0)GO TO 561                                                             
       CALL MULTI(NSAVE,MSAVE,NMVAR)                                                      
       WRITE(6,563)                                                                       
 563   FORMAT(//"1************************************"                                   
     1/       "MULTI-STAGE SELECTION  SECOND ROUND "                                      
     2/       "*****************************************")                                
       N=NSAVE                                                                            
       M=MSAVE                                                                            
       MJLT=0                                                                             
       GO TO 2001                                                                         
 561   CONTINUE                                                                           
      READ(5,110)(KM(I),I=1,25),(LIMIT(I),I=1,5),MULT,NPC                                 
  110 FORMAT(32I2)                                                                        
      IF(KM(1)+LIMIT(1))55,5,555                                                          
  555 NN=N                                                                                
       IF(MULT.EQ.1)WRITE(6,562)NPC                                                       
 562   FORMAT(//"1*************************************"                                  
     1/       " MULTI-STAGE SELECTION   FIRST ROUND "                                     
     2/       "****************************************"                                  
     3/"      PROPORTION SELECTED = ",I2,"(")                                             
      DO 188 I=1,25                                                                       
      IF(KM(I))189,188,189                                                                
  189 NN=NN-1                                                                             
  188 CONTINUE                                                                            
      DO 190 I=1,5                                                                        
      IF(LIMIT(I))191,190,191                                                             
  191 NN=NN+1                                                                             
  190 CONTINUE                                                                            
C     RESTORE P G AND XNAME                                                               
  155 L=N*(N+1)/2                                                                         
      DO 235 I=1,L                                                                        
      P(I)=PSAVE(I)                                                                       
      W(I)=0                                                                              
  235 CONTINUE                                                                            
      DO 206 I=1,N                                                                        
      DO 207 J=1,M                                                                        
      G(I,J)=GSAVE(I,J)                                                                   
  207 CONTINUE                                                                            
  206 CONTINUE                                                                            
      DO 208 I=1,N                                                                        
      DO 209 J=1,5                                                                        
      XNAME(I,J)=XNAMES(I,J)                                                              
  209 CONTINUE                                                                            
  208 CONTINUE                                                                            
       L=M*(M+1)/2                                                                        
       DO 236 J=1,L                                                                       
       C(J)=CSAVE(J)                                                                      
 236   CONTINUE                                                                           
C      ANY RESTRICTIONS +                                                                 
      IF(LIMIT(1))187,186,187                                                             
C     IMPOSE RESTRICTIONS                                                                 
  187 DO 184 K=1,5                                                                        
      IF(LIMIT(K))184,184,185                                                             
  185 NR=N+1                                                                              
      KL=LIMIT(K)                                                                         
      XNAME(NR,1)=XN1                                                                     
      XNAME(NR,2)=XN2                                                                     
      XNAME(NR,3)=XN3                                                                     
      XNAME(NR,4)=NUM(KL)                                                                 
      XNAME(NR,5)=XN4                                                                     
      DO 183 J=1,M                                                                        
  183 G(NR,J)=0                                                                           
C     ADD A COLUMN OF G TO P                                                              
      DO 180 III=1,N                                                                      
      I=N-III+1                                                                           
      DO 180 JJJ=1,N                                                                      
      J=N-JJJ+1                                                                           
      L=(I-1)*N-((I-1)*I)/2+J                                                             
      LN=L+I-1                                                                            
  180 P(LN)=P(L)                                                                          
      L=0                                                                                 
      DO 181 I=1,NR                                                                       
      L=L+NR-I+1                                                                          
  181 P(L)=G(I,KL)                                                                        
  184 N=NR                                                                                
C     ANY DELETIONS+                                                                      
  186 IF(KM(1))198,112,198                                                                
C     MARK ELEMENTS OF G AND P FOR DELETION                                               
  198 DO 100 I=1,N                                                                        
      IF(KM(I))101,101,102                                                                
  102 II=KM(I)                                                                            
      G(II,1)=99999999                                                                    
      DO 103 J=1,N                                                                        
      IF(II-J)130,130,131                                                                 
  131 L=(J-1)*N-((J-1)*J)/2+II                                                            
      GO TO 132                                                                           
  130 L=(II-1)*N-((II-1)*II)/2+J                                                          
  132 P(L)=99999999                                                                       
  103 CONTINUE                                                                            
  100 CONTINUE                                                                            
C     REMOVE ROWS AND COLS FROM P MATRIX                                                  
  101 N1=N*(N+1)/2                                                                        
      DO 106 I=1,N1                                                                       
      IF(P(I)-99999999)106,107,106                                                        
  107 IK=I+1                                                                              
  122 IF(P(IK)-99999999)120,121,120                                                       
  121 IK=IK+1                                                                             
      IF(IK-N1)122,122,161                                                                
  120 P(I)=P(IK)                                                                          
      P(IK)=99999999                                                                      
  106 CONTINUE                                                                            
C     REMOVE ROWS FROM G MATRIX AND XNAME MATRIX                                          
  161 DO 111 I=1,N                                                                        
      IF(G(I,1)-99999999)111,115,111                                                      
  115 IK=I+1                                                                              
  116 IF(G(IK,1)-99999999)113,119,113                                                     
  119 IK=IK+1                                                                             
      IF(IK-N)116,116,112                                                                 
  113 DO 118 J=1,M                                                                        
      G(I,J)=G(IK,J)                                                                      
  118 CONTINUE                                                                            
      G(IK,1)=99999999                                                                    
      DO 114 J=1,5                                                                        
      XNAME(I,J)=XNAME (IK,J)                                                             
  114 CONTINUE                                                                            
  111 CONTINUE                                                                            
C     START CALCULATIONS FOR NEW INDEX                                                    
  112 N=NN                                                                                
      GO TO 2000                                                                          
  556 WRITE(6,560)VARIND                                                                  
  560 FORMAT("1","VARIANCE OF INDEX IS NEGATIVE  ",F10.4)                                 
      GO TO 559                                                                           
    5 STOP                                                                                
      END                                                                                 
       SUBROUTINE MULTI(N,M,R)                                                            
       COMMON P(325),G(25,25),B(25),C(325),                                               
     1PSAVE(325),GSAVE(25,25),CSAVE(325),NPC,VARINS                                       
       DIMENSION PE(15,15),GT(15,15),CE(15,15),GM(15,15)                                  
     1,S(30,30),SS(30,30),SSP(30,30)                                                      
     2,BT(30),BTT(30),T(15,30),SK(99)                                                     
       DATA SK/0.9042,0.8897,0.8783,0.8695,0.8614,0.8547,0.8484,0.8421,                   
     10.8361,0.8305,0.8256,0.8201,0.8150,0.8101,0.8053,0.8001,0.7953,                     
     10.7906,0.7860,0.7814,0.7768,0.7721,0.7677,0.7629,0.7583,0.7539,                     
     10.7493,0.7447,0.7400,0.7355,0.7307,0.7262,0.7216,0.7168,0.7122,                     
     10.7074,0.7026,0.6979,0.6931,0.6882,0.6833,0.6783,0.6733,0.6682,                     
     10.6631,0.6580,0.6527,0.6474,0.6420,0.6366,0.6311,0.6256,0.6199,                     
     10.6141,0.6083,0.6024,0.5964,0.5903,0.5840,0.5777,0.5713,0.5648,                     
     10.5581,0.5513,0.5443,0.5372,0.5299,0.5225,0.5150,0.5072,0.4992,                     
     10.4911,0.4827,0.4741,0.4653,0.4562,0.4469,0.4372,0.4273,0.4170,                     
     10.4063,0.3953,0.3839,0.3719,0.3595,0.3466,0.3330,0.3188,0.3038,                     
     10.2879,0.2711,0.2532,0.2339,0.2131,0.1903,0.1652,0.1368,0.1039,                     
     10.0634/                                                                             
       W=SK(NPC)/VARINS                                                                   
       J=N*(N+1)/2                                                                        
        K=M*(M+1)/2                                                                       
       NT=N+M                                                                             
       NP=N+1                                                                             
       CALL STORE(PSAVE,J,PE,N,1)                                                         
       CALL STORE(PSAVE,K,CE,M,1)                                                         
       CALL SMAT(GSAVE,25,25,1,1,GM,N,M,1,1,N,M)                                          
       CALL MTRANS(GM,GT,N,M)                                                             
       CALL SMAT(PE,N,N,1,1,S,NT,NT,1,1,N,N)                                              
       CALL SMAT(GM,N,M,1,1,S,NT,NT,1,NP,N,M)                                             
       CALL SMAT(GT,M,N,1,1,S,NT,NT,NP,1,M,N)                                             
       CALL SMAT(CE,M,M,1,1,S,NT,NT,NP,NP,M,M)                                            
       CALL SMAT(S,NT,NT,1,1,T,NR,NT,1,1,NR,NT)                                           
       CALL MMMULT(B,T,BT,1,NR,NT)                                                        
       CALL MTRANS(BT,BTT,1,NT)                                                           
       CALL MMMULT(BTT,BT,SS,NT,1,NT)                                                     
       CALL MSMULT(SS,W,SSP,NT,NT)                                                        
       CALL MSUB(S,SSP,SS,NT,NT)                                                          
       CALL SMAT(SS,NT,NT,1,1,PE,N,N,1,1,N,N)                                             
       CALL SMAT(SS,NT,NT,1,NP,GM,N,M,1,1,N,M)                                            
       CALL SMAT(SS,NT,NT,NP,NP,CE,M,M,1,1,M,M)                                           
       CALL SMAT(GM,N,M,1,1,G,25,25,1,1,N,M)                                              
       CALL STORE(P,J,PE,N,0)                                                             
       CALL STORE(C,K,CE,M,0)                                                             
       RETURN                                                                             
       END                                                                                
       SUBROUTINE MSUB(RAY1,RAY2,RAYS,N,M)                                                
       DIMENSION RAY1(N,M),RAY2(N,M),RAYS(N,M)                                            
       DO 1 J=1,N                                                                         
       DO 1 K=1,M                                                                         
 1     RAYS(J,K)=RAY1(J,K)-RAY2(J,K)                                                      
       RETURN                                                                             
       END                                                                                
       SUBROUTINE STORE(RAYS,K,RAYG,N,ICODE)                                              
       DIMENSION RAYS(K),RAYG(N,N)                                                        
       M=0                                                                                
       L=0                                                                                
       IF(ICODE.EQ.0)GO TO 2                                                              
       DO 1 I=1,N                                                                         
       L=L+1                                                                              
       DO 1 J=L,N                                                                         
       M=M+1                                                                              
       RAYG(I,J)=RAYS(M)                                                                  
 1     RAYG(J,I)=RAYS(M)                                                                  
       GO TO 4                                                                            
 2     DO 3 I=1,N                                                                         
       L=L+1                                                                              
       DO 3 J=L,N                                                                         
       M=M+1                                                                              
 3     RAYS(M)=RAYG(I,J)                                                                  
 4     RETURN                                                                             
       END                                                                                
       SUBROUTINE MSMULT(RAY1,SCA,RAYP,N,M)                                               
       DIMENSION RAY1(N,M),RAYP(N,M)                                                      
       DO 1 J=1,N                                                                         
       DO 1 K=1,M                                                                         
 1     RAYP(J,K)=RAY1(J,K)*SCA                                                            
       RETURN                                                                             
       END                                                                                
       SUBROUTINE MMMULT(RAY1,RAY2,RAYP,L,M,N)                                            
        DIMENSION RAY1(L,M),RAY2(M,N),RAYP(L,N)                                           
       DO 3 I=1,L                                                                         
       DO 2 J=1,N                                                                         
       RAYP(I,J)=0                                                                        
       DO 1 K=1,M                                                                         
       RAYP(I,J)=RAYP(I,J)+RAY1(I,K)*RAY2(K,J)                                            
 1     CONTINUE                                                                           
 2     CONTINUE                                                                           
 3     CONTINUE                                                                           
       RETURN                                                                             
       END                                                                                
       SUBROUTINE MTRANS(RAY1,RAY2,I,J)                                                   
       DIMENSION RAY1(I,J),RAY2(J,I)                                                      
       DO 1 K=I,J                                                                         
       DO 1 L=1,I                                                                         
 1     RAY2(K,L)=RAY1(L,K)                                                                
       RETURN                                                                             
       END                                                                                
       SUBROUTINE SMAT(RAY1,I,J,K,L,RAY2,II,JJ,KK,LL,M,N)                                 
       DIMENSION RAY1(I,J),RAY2(II,JJ)                                                    
       DO 1 IR=1,M                                                                        
       IR1=IR+K-1                                                                         
       IR2=IR+KK-1                                                                        
       DO 1 IC=1,N                                                                        
       IC1=IC+L-1                                                                         
       IC2=IC+LL-1                                                                        
       RAY2(IR2,IC2)=RAY1(IR1,IC1)                                                        
 1     CONTINUE                                                                           
       RETURN                                                                             
       END                                                                                
