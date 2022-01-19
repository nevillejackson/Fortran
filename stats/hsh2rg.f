C-----UNALTERED 3066 DECKS                                                                
      PROGRAM HSH2RG                                                                      
      DIMENSION NSITG(3,3,3)                                                              
      DIMENSION SUM(25),GSUM(25),X(25),RI(25),                                            
     1          W(650),B(650),SP(650),PW(650),PB(650)                                     
      COMMON W,B,SP,PW,PB                                                                 
      LUNI=20                                                                             
      REWIND LUNI                                                                         
      NCR=60                                                                              
      NLP=61                                                                              
      LUN1=1                                                                              
      LUN2=2                                                                              
      READ(NCR,1) M                                                                       
    1 FORMAT(20I4)                                                                        
      MX=M*(M+1)/2                                                                        
      READ(NCR,1) NMG,NSX,NDR                                                             
      WRITE(NLP,40)NMG,NSX,NDR                                                            
   40 FORMAT(7H0NMG = ,I2,9H   NSX = ,I2,9H   NDR = ,I2)                                  
      READ(NCR,1)(((NSITG(I,J,K),K=1,NDR),J=1,NSX),I=1,NMG)                               
      WRITE(NLP,41)(((NSITG(I,J,K ),K=1,NDR),J=1,NSX),I=1,NMG)                            
   41 FORMAT(1H0,30I4)                                                                    
      READ(NCR,1)IAV                                                                      
      DO 2 I=1,NMG                                                                        
      DO 3 J=1,NSX                                                                        
      DO 4 K=1,NDR                                                                        
      WRITE(NLP,42) I,J,K                                                                 
   42 FORMAT(17H1NEW  GROUP  I = ,I3,7H   J = ,I3,7H   K = ,I3)                           
      ENISQ=0.0                                                                           
      ENDOT=0.0                                                                           
      DO 14 IM=1,M                                                                        
   14 GSUM(IM)=0.0                                                                        
      DO 15 IM=1,MX                                                                       
      W(IM)=0.0                                                                           
   15 B(IM)=0.0                                                                           
      LL=NSITG(I,J,K)                                                                     
      IF(LL) 443,443,444                                                                  
  444 DO 5 L=1,LL                                                                         
      N=0                                                                                 
      DO 6 IM=1,M                                                                         
    6 SUM(IM)=0.0                                                                         
      DO 7 IM=1,MX                                                                        
    7 SP(IM)=0.0                                                                          
   11 CALL DATA(X,M,KEY,LUNI)                                                             
      IF(KEY) 8,9,10                                                                      
   10 CALL Q8QERROR(0,10H ERROR 10.)                                                      
    9 N=N+1                                                                               
      DO 12 IM=1,M                                                                        
      SUM(IM)=SUM(IM)+X(IM)                                                               
      DO 12 JM=IM,M                                                                       
      CALL LOC(IM,JM,IRX,M,M,1)                                                           
   12 SP(IRX)=SP(IRX)+X(IM)*X(JM)                                                         
      GO TO 11                                                                            
    8 CONTINUE                                                                            
      CALL SGOUT(N,SUM,SP,M,L)                                                            
      ENISQ=ENISQ+N*N                                                                     
      ENDOT=ENDOT+N                                                                       
      DO 13 IM=1,M                                                                        
      GSUM(IM)=GSUM(IM)+SUM(IM)                                                           
      DO 13 JM=IM,M                                                                       
      CALL LOC(IM,JM,IRX,M,M,1)                                                           
      W(IRX)=W(IRX)+SP(IRX)                                                               
      IF(N) 13,13,113                                                                     
  113 B(IRX)=B(IRX)+SUM(IM)*SUM(JM)/N                                                     
   13 CONTINUE                                                                            
    5 CONTINUE                                                                            
  443 DO 16 IM=1,M                                                                        
      DO 16 JM=IM,M                                                                       
      CALL LOC(IM,JM,IRX,M,M,1)                                                           
      W(IRX)=W(IRX)-B(IRX)                                                                
      IF(ENDOT) 16,16,116                                                                 
  116 B(IRX)=B(IRX)-GSUM(IM)*GSUM(JM)/ENDOT                                               
   16 CONTINUE                                                                            
      DFB=NSITG(I,J,K)-1                                                                  
      DFW=ENDOT-NSITG(I,J,K)                                                              
      IF(ENDOT) 180,180,181                                                               
  180 ENISQ=0.                                                                            
      GO TO 82                                                                            
  181 ENISQ=ENISQ/ENDOT                                                                   
   82 IF(DFB) 83,83,84                                                                    
   83 EK=0.0                                                                              
      DFB=0.0                                                                             
      GO TO 85                                                                            
   84 EK=(ENDOT-ENISQ)/DFB                                                                
   85 CALL AVSTR(M,B,W,DFB,DFW,ENDOT,ENISQ,I,J,K,LUN1)                                    
      CALL AVH2RG(M,B,W,DFB,DFW,EK,I,J,K,RI,IAV)                                          
    4 CONTINUE                                                                            
    3 CONTINUE                                                                            
    2 CONTINUE                                                                            
      REWIND LUN1                                                                         
      REWIND LUN2                                                                         
      READ(NCR,1)IAV                                                                      
C     POOL OVER DROPS   STAGE 2                                                           
      IF(NDR-1) 55,55,56                                                                  
   56 DO 50 I=1,NMG                                                                       
      DO 51 J=1,NSX                                                                       
      PNDOT=0.                                                                            
      PNISQ=0.                                                                            
      PDFB=0.                                                                             
      PDFW=0.                                                                             
      DO 53 IM=1,MX                                                                       
      PB(IM)=0.                                                                           
   53 PW(IM)=0.                                                                           
      DO 52 K=1,NDR                                                                       
      CALL AVUNST(M,B,W,DFB,DFW,ENDOT,ENISQ,NI,NJ,NK,LUN1,KEY)                            
      IF(KEY) 300,57,300                                                                  
   57 PDFB=PDFB+DFB                                                                       
      PDFW=PDFW+DFW                                                                       
      PNDOT=PNDOT+ENDOT                                                                   
      PNISQ=PNISQ+ENISQ                                                                   
      DO 54 IM=1,MX                                                                       
      PB(IM)=PB(IM)+B(IM)                                                                 
   54 PW(IM)=PW(IM)+W(IM)                                                                 
   52 CONTINUE                                                                            
      CALL AVSTR(M,PB,PW,PDFB,PDFW,PNDOT,PNISQ,I,J,0,LUN2)                                
      PK=(PNDOT-PNISQ)/PDFB                                                               
      CALL AVH2RG(M,PB,PW,PDFB,PDFW,PK,I,J,0,RI,IAV)                                      
   51 CONTINUE                                                                            
   50 CONTINUE                                                                            
      GO TO 80                                                                            
   55 CONTINUE                                                                            
      KSW=LUN1                                                                            
      LUN1=LUN2                                                                           
      LUN2=KSW                                                                            
   80 REWIND LUN1                                                                         
      REWIND LUN2                                                                         
      READ(NCR,1)IAV                                                                      
C     POOL OVER SEXES   STAGE 3                                                           
      IF(NSX-1) 65,65,66                                                                  
   66 DO 60 I=1,NMG                                                                       
      PNDOT=0.                                                                            
      PNISQ=0.                                                                            
      PDFB=0.                                                                             
      PDFW=0.                                                                             
      DO 63 IM=1,MX                                                                       
      PB(IM)=0.                                                                           
   63 PW(IM)=0.                                                                           
      DO 61 J=1,NSX                                                                       
      CALL AVUNST(M,B,W,DFB,DFW,ENDOT,ENISQ,NI,NJ,NK,LUN2,KEY)                            
      IF(KEY) 300,67,300                                                                  
   67 PDFB=PDFB+DFB                                                                       
      PDFW=PDFW+DFW                                                                       
      PNDOT=PNDOT+ENDOT                                                                   
      PNISQ=PNISQ+ENISQ                                                                   
      DO 64 IM=1,MX                                                                       
      PB(IM)=PB(IM)+B(IM)                                                                 
   64 PW(IM)=PW(IM)+W(IM)                                                                 
   61 CONTINUE                                                                            
      PK=(PNDOT-PNISQ)/PDFB                                                               
      CALL AVSTR(M,PB,PW,PDFB,PDFW,PNDOT,PNISQ,I,0,0,LUN1)                                
      CALL AVH2RG(M,PB,PW,PDFB,PDFW,PK,I,0,0,RI,IAV)                                      
   60 CONTINUE                                                                            
      GO TO 81                                                                            
   65 CONTINUE                                                                            
      KSW=LUN1                                                                            
      LUN1=LUN2                                                                           
      LUN2=KSW                                                                            
   81 REWIND LUN1                                                                         
      REWIND LUN2                                                                         
      READ(NCR,1)IAV                                                                      
C     POLL OVER MATING GROUPS   STAGE 4                                                   
      IF(NMG-1) 75,75,76                                                                  
   76 CONTINUE                                                                            
      PDFB=0.                                                                             
      PDFW=0.                                                                             
      PNDOT=0.                                                                            
      PNISQ=0.                                                                            
      DO 73 IM=1,MX                                                                       
      PB(IM)=0.                                                                           
   73 PW(IM)=0.                                                                           
      DO 70 I=1,NMG                                                                       
      CALL AVUNST(M,B,W,DFB,DFW,ENDOT,ENISQ,NI,NJ,NK,LUN1,KEY)                            
      IF(KEY) 300,77,300                                                                  
   77 PDFB=PDFB+DFB                                                                       
      PDFW=PDFW+DFW                                                                       
      PNDOT=PNDOT+ENDOT                                                                   
      PNISQ=PNISQ+ENISQ                                                                   
      DO 74 IM=1,MX                                                                       
      PB(IM)=PB(IM)+B(IM)                                                                 
   74 PW(IM)=PW(IM)+W(IM)                                                                 
   70 CONTINUE                                                                            
      PK=(PNDOT-PNISQ)/PDFB                                                               
      CALL AVH2RG(M,PB,PW,PDFB,PDFW,PK,0,0,0,RI,IAV)                                      
   75 CONTINUE                                                                            
      REWIND LUNI                                                                         
  300 RETURN                                                                              
      END                                                                                 
      SUBROUTINE AVSTR(M,B,W,DFB,DFW,ENDOT,ENISQ,NI,NJ,NK,LUN)                            
      DIMENSION B(1),W(1)                                                                 
      MX=M*(M+1)/2                                                                        
      WRITE(LUN) NI,NJ,NK,DFB,DFW,ENDOT,ENISQ,(B(I),I=1,MX),                              
     1     (W(I),I=1,MX)                                                                  
      RETURN                                                                              
      END                                                                                 
      SUBROUTINE AVUNST(M,B,W,DFB,DFW,ENDOT,ENISQ,NI,NJ,NK,LUN,KEY)                       
      DIMENSION B(1),W(1)                                                                 
      NLP=61                                                                              
      MX=M*(M+1)/2                                                                        
      IBACK=0                                                                             
    5 READ(LUN) NI,NJ,NK,DFB,DFW,ENDOT,ENISQ,(B(I),I=1,MX),(W(I),I=1,MX)                  
      IF(EOF,LUN) 1,2                                                                     
    2 IF(IOCHECK,LUN) 3,4                                                                 
    4 KEY=0                                                                               
      RETURN                                                                              
    1 WRITE(NLP,104) LUN                                                                  
  104 FORMAT(12H EOF ON LUN ,I2)                                                          
      KEY=-1                                                                              
      RETURN                                                                              
    3 WRITE(NLP,105) LUN                                                                  
  105 FORMAT(17H PARITY ERROR ON ,I2)                                                     
      BACKSPACE LUN                                                                       
      IBACK=IBACK+1                                                                       
      IF(IBACK-10) 5,5,6                                                                  
    6 KEY=1                                                                               
      RETURN                                                                              
      END                                                                                 
      SUBROUTINE SGOUT(N,SUM,SP,M,L)                                                      
      DIMENSION SUM(1),SP(1),BUF(20)                                                      
      NLP=61                                                                              
      WRITE(NLP,1) L,M ,N                                                                 
    1 FORMAT(16H0SIRE GROUP NO. ,I3,10X,I3,7H TRAITS,10X,I4,11H REPLICATES)               
     1ES)                                                                                 
      IF(N) 5,6,5                                                                         
    6 RETURN                                                                              
    5 WRITE(NLP,2)                                                                        
    2 FORMAT(6H0TRAIT,15X,5HTOTAL,16X,4HMEAN,13X,7HCORR.SS,16X,4H VAR,                    
     1 10X,10HSTAND.DEV.,16X,4HC.V.)                                                      
    3 FORMAT(1H ,I6,6F20.4)                                                               
      DO 4 I=1,M                                                                          
      AV=SUM(I)/N                                                                         
      CALL LOC(I,I,IR,M,M,1)                                                              
      CSS=SP(IR)-SUM(I)*SUM(I)/N                                                          
      IF(N-1) 7,7,8                                                                       
    7 VAR=0.0                                                                             
      SD=0.0                                                                              
      CV=0.0                                                                              
      GO TO 9                                                                             
    8 VAR=CSS/(N-1)                                                                       
      IF(VAR) 10,10,11                                                                    
   10 SD=0.0                                                                              
      CV=0.0                                                                              
      GO TO 9                                                                             
   11 SD=SQRTF(VAR)                                                                       
      IF(AV) 12,12,13                                                                     
   12 CV=0.0                                                                              
      GO TO 9                                                                             
   13 CV=SD*100./AV                                                                       
    9 WRITE(NLP,3)I,SUM(I),AV,CSS,VAR,SD,CV                                               
    4 CONTINUE                                                                            
      RETURN                                                                              
      END                                                                                 
      SUBROUTINE MXROWS(A,N,M,MS,NLP)                                                     
      DIMENSION A(1),BUF(25)                                                              
      DO 1 I=1,N                                                                          
      WRITE(NLP,4) I                                                                      
    4 FORMAT(5H I = ,I3)                                                                  
      DO 2 J=1,M                                                                          
      CALL LOC(I,J,IR,N,M,MS)                                                             
    2 BUF(J)=A(IR)                                                                        
    1 WRITE(NLP,3)(BUF(J),J=1,M)                                                          
    3 FORMAT(1H ,10E13.5)                                                                 
      RETURN                                                                              
      END                                                                                 
      SUBROUTINE AVH2RG(M,SPB,SPW,DFB,DFW,EK,NI,NJ,NK,RI,IAV)                             
      DIMENSION SPB(1),SPW(1),RI(1)                                                       
      NLP=61                                                                              
      MX=M*(M+1)/2                                                                        
      WRITE(NLP,1) NI,NJ,NK                                                               
    1 FORMAT(21H1ANALYSIS OF VARIANCE,10X,15HMATING GROUP = ,I2,10X,6HSEX = ,I2           
     1X = ,I2,10X,7HDROP = ,I2)                                                           
      WRITE(NLP,2)DFB                                                                     
    2 FORMAT(18H0D.F. FOR SIRES = ,F10.1)                                                 
      WRITE(NLP,3)DFW                                                                     
    3 FORMAT(15H0D.F. WITHIN = ,F10.1)                                                    
      WRITE(NLP,4) EK                                                                     
    4 FORMAT(6H0EK = ,F20.6)                                                              
C                                                                                         
      IF(DFB) 207,207,208                                                                 
  208 DO 7 I=1,MX                                                                         
    7 SPB(I)=SPB(I)/DFB                                                                   
  207 IF(DFW) 209,209,210                                                                 
  210 DO 77 I=1,MX                                                                        
   77 SPW(I)=SPW(I)/DFW                                                                   
  209 CONTINUE                                                                            
      IF(IAV) 400,400,401                                                                 
  401 WRITE(NLP,8)                                                                        
    8 FORMAT(17H0MP BETWEEN SIRES)                                                        
      CALL MXROWS(SPB,M,M,1,NLP)                                                          
      WRITE(NLP,9)                                                                        
    9 FORMAT(16H0MP WITHIN SIRES)                                                         
      CALL MXROWS(SPW,M,M,1,NLP)                                                          
  400 CONTINUE                                                                            
C                                                                                         
C     HERITABILITIES                                                                      
      WRITE(NLP,11)                                                                       
   11 FORMAT(34H1HERITABILITY ANALYSIS OF VARIANCE)                                       
      WRITE(NLP,12)                                                                       
   12 FORMAT(4H0NO.,8X,6HF-TEST,7X,7HG COMP.,7X,7HE COMP.,7X,7HP COMP.,                   
     1 7X,7HINTRA-R,2X,12HHERITABILITY,11X,3HVRI,10X,4HVHSQ,9X,5HSEHSQ)                   
      DO 10 I=1,M                                                                         
      CALL LOC(I,I,IR,M,M,1)                                                              
      IF(SPW(IR)) 211,211,212                                                             
  211 F=0.0                                                                               
      GO TO 213                                                                           
  212 F=SPB(IR)/SPW(IR)                                                                   
  213 IF(EK) 214,214,215                                                                  
  214 SPB(IR)=0.0                                                                         
      GO TO 216                                                                           
  215 SPB(IR)=(SPB(IR)-SPW(IR))/EK                                                        
  216 PTVAR=SPB(IR)+SPW(IR)                                                               
      IF(PTVAR) 217,217,218                                                               
  217 RI(I)=0.0                                                                           
      GO TO 219                                                                           
  218 RI(I)=SPB(IR)/PTVAR                                                                 
  219 HSQ=4.*RI(I)                                                                        
      ABLE=EK*(EK-1.)*DFB                                                                 
      IF(ABLE) 220,221,220                                                                
  221 VRI=0.0                                                                             
      GO TO 222                                                                           
  220 VRI=(2.*(1.-RI(I))**2)*((1.+(EK-1.)*RI(I))**2)/ABLE                                 
  222 VHSQ=16.*VRI                                                                        
      SEHSQ=SQRTF(VHSQ)                                                                   
      WRITE(NLP,13)I,F,SPB(IR),SPW(IR),PTVAR,RI(I),HSQ,VRI,VHSQ,SEHSQ                     
   10 CONTINUE                                                                            
   13 FORMAT(1H ,I3,9F14.4)                                                               
C                                                                                         
      WRITE(NLP,20)                                                                       
   20 FORMAT(22H1CORRELATIONS ANALYSIS)                                                   
      WRITE(NLP,21)                                                                       
   21 FORMAT(7H TRAITS,8X,7HG COMP.,8X,7HE COMP.,8X,7HP COMP.,13X,                        
     1  2HRG,13X,2HRE13X,2HRP,12X,3HVRG,11X,4HSERG)                                       
   22 FORMAT(1H 2I3,8F15.4)                                                               
      DO 15 I=1,M                                                                         
      DO 15 J=I,M                                                                         
      IF(I-J) 16,15,15                                                                    
   16 CALL LOC(I,J,IJ,M,M,1)                                                              
      CALL LOC(I,I,II,M,M,1)                                                              
      CALL LOC(J,J,JJ,M,M,1)                                                              
      IF(EK) 223,223,224                                                                  
  223 SPB(IJ)=0.0                                                                         
      GO TO 225                                                                           
  224 SPB(IJ)=(SPB(IJ)-SPW(IJ))/EK                                                        
  225 ABLE=SPB(II)*SPB(JJ)                                                                
      IF(ABLE) 120,120,121                                                                
  120 RG=0.0                                                                              
      GO TO 122                                                                           
  121 RG=SPB(IJ)/SQRTF(ABLE)                                                              
  122 ABLE=(SPW(II)-2.*SPB(II))*(SPW(JJ)-2.*SPB(JJ))                                      
      IF(ABLE) 123,123,124                                                                
  123 RE=0.0                                                                              
      GO TO 125                                                                           
  124 RE=(SPW(IJ)-2.*SPB(IJ))/SQRTF(ABLE)                                                 
  125 PCOM=SPB(IJ)+SPW(IJ)                                                                
      ABLE=(SPB(II)+SPW(II))*(SPB(JJ)+SPW(JJ))                                            
      IF(ABLE) 126,126,127                                                                
  126 RP=0.0                                                                              
      GO TO 128                                                                           
  127 RP=PCOM/SQRTF(ABLE)                                                                 
  128 CONTINUE                                                                            
      A=1.+RG**2                                                                          
      ABLE=RI(I)*RI(J)                                                                    
      IF(ABLE) 129,129,130                                                                
  129 B=0.0                                                                               
      GO TO 131                                                                           
  130 B=RG*SQRTF(ABLE)                                                                    
  131 CONTINUE                                                                            
      IF(EK.EQ.0.0.OR.DFB.EQ.0.0.OR.DFW.EQ.0.0.OR.RI(I).EQ.0.0.OR.RI(J)                   
     1  .EQ.0.0) 226,227                                                                  
  226 VRG=0.0                                                                             
      SERG=0.0                                                                            
      GO TO 228                                                                           
  227 C=1./RI(I)+1./RI(J)                                                                 
      D=(RG**2)*((RI(I)-RI(J))**2)/(2.*RI(I)*RI(J))                                       
      E=EK-1.                                                                             
      VRG=1./(EK*EK*DFB*RI(I)*RI(J))*(A*((1.+E*RI(I))*(1.+E*RI(J))+                       
     1  (RP+E*B)**2)-2.*B*(RP+E*B)*(C+2.*E)+D)                                            
     2  +1./(EK*EK*DFW*RI(I)*RI(J))*(A*((1.-RI(I))*(1.-RI(J))+(RP-B)**2)                  
     3   -2.*B*(RP-B)*(C-2.)+D)                                                           
      SERG=SQRTF(ABSF(VRG))                                                               
  228 WRITE(NLP,22) I,J,SPB(IJ),SPW(IJ),PCOM,RG,RE,RP,VRG,SERG                            
   15 CONTINUE                                                                            
      RETURN                                                                              
      END                                                                                 
       SUBROUTINE LOC(I,J,IR,N,M,MS)                                                      
C                                                                                         
      IX=I                                                                                
      JX=J                                                                                
      IF(MS-1) 10,20,30                                                                   
   10 IRX=N*(JX-1)+IX                                                                     
      GO TO 36                                                                            
   20 IF(IX-JX) 22,24,24                                                                  
   22 IRX=IX+(JX*JX-JX)/2                                                                 
      GO TO 36                                                                            
   24 IRX=JX+(IX*IX-IX)/2                                                                 
      GO TO 36                                                                            
   30 IRX=0                                                                               
      IF(IX-JX) 36,32,36                                                                  
   32 IRX=IX                                                                              
   36 IR=IRX                                                                              
      RETURN                                                                              
      END                                                                                 
      SUBROUTINE DATA(D,M,KEY,LUN)                                                        
      DIMENSION D(1)                                                                      
      DIMENSION K(49),X(45),KARD(30)                                                      
      DATA(AVLDAY=274.72496)                                                              
      DATA(ADJSPP=-0.28626504)                                                            
      DATA(ADJG=-0.02874094)                                                              
      DATA(ADJW=-0.01989180)                                                              
      DATA(ADJB=-0.26290938)                                                              
      DATA(ADJL=-0.02564295)                                                              
      DATA(ADJD=-0.03262075)                                                              
      N=0                                                                                 
   40 READ(LUN,10)(KARD(I),I=1,30)                                                        
   10 FORMAT(10A8)                                                                        
      IF(ISTAT(LUN)) 2,1,3                                                                
    2 KEY=-1                                                                              
      RETURN                                                                              
    3 KEY=1                                                                               
      RETURN                                                                              
    1 CONTINUE                                                                            
      DECODE(240,9,KARD)(K(I),I=1,10),(X(I),I=1,10),K(11),K(12),                          
     1    (X(I),I=11,14),K(13),X(15),K(14),K(15),(K(I),I=16,25),                          
     2    (X(I),I=16,25),K(26),K(27),(X(I),I=26,29),K(28),X(30),K(29),                    
     3    K(30),(K(I),I=31,44),(X(I),I=31,40),K(45),K(46),(X(I),I=41,44),                 
     4    ,K(47),X(45),K(48),K(49)                                                        
    9 FORMAT(2(I6,2I2,I1,I2,I1,I6,I2,I1,I3,2F3.1,F4.1,3F3.1,F4.2,F4.1,                    
     1 F2.0,F1.0,2I1,F5.2,F3.1,F3.2,F2.0,I1,F4.3,I1,2X,I1),I6,2I2,I1,I2,                  
     2I1,I2,4I1,I2,2I1,2X,2F3.1,F4.1,3F3.1,F4.2,F4.1,F2.0,F1.0,2I1,F5.2,                  
     3 F3.1,F3.2,F2.0,I1,F4.3,I1,2X,I1)                                                   
      IF(K(10)) 40,40,28                                                                  
   28 IF(K(11)) 40,21,40                                                                  
   21 IF(K(13)) 40,22,40                                                                  
   22 IF(K(26)) 40,23,40                                                                  
   23 IF(K(28)) 40,24,40                                                                  
   24 IF(K(45)) 40,25,40                                                                  
   25 IF(K(47)) 40,26,40                                                                  
   26 CONTINUE                                                                            
      DEV=K(10)-AVLDAY                                                                    
      D(1)=X(1)-ADJL*DEV                                                                  
      D(2)=X(2)                                                                           
      D(3)=X(4)-ADJD*DEV                                                                  
      D(4)=X(5)-ADJG*DEV                                                                  
      D(5)=X(6)                                                                           
      D(6)=X(7)-ADJW*DEV                                                                  
      D(7)=X(8)-ADJB*DEV                                                                  
      D(8)=X(11)-ADJSPP*DEV                                                               
      D(9)=X(12)                                                                          
      D(10)=X(13)                                                                         
      D(11)=X(14)                                                                         
      DO 20 I=12,25                                                                       
   20 D(I)=X(I+4)                                                                         
      KEY=0                                                                               
      RETURN                                                                              
      END                                                                                 
      FUNCTION ISTAT(LUN)                                                                 
      DATA(IER=0)                                                                         
C-----RETURNS -1 IF EOF                                                                   
C-----RETURNS 0 IF OK                                                                     
C-----RETURNS +1 IF PARITY ERROR                                                          
      IF(EOF,LUN) 1,2                                                                     
    2 IF(IOCHECK,LUN) 3,4                                                                 
C-----OK                                                                                  
    4 ISTAT=0                                                                             
      RETURN                                                                              
C-----EOF                                                                                 
    1 ISTAT=-1                                                                            
      PRINT 5,LUN                                                                         
    5 FORMAT(13H0EOF ON UNIT ,I3)                                                         
    7 RETURN                                                                              
C-----PARITY                                                                              
    3 PRINT 6,LUN                                                                         
    6 FORMAT(21H0PARITY ERROR ON UNIT ,I3)                                                
      BACKSPACE LUN                                                                       
      ISTAT=+1                                                                            
      IER=IER+1                                                                           
      IF(IER-50) 7,7,8                                                                    
    8 PRINT 9,LUN                                                                         
    9 FORMAT(36H0MORE THAN 50 PARITY ERRORS ON UNIT ,I3)                                  
      STOP                                                                                
      END                                                                                 
