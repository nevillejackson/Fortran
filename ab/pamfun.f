      PROGRAM PAMFUN(INPUT,OUTPUT)                                                        
      DIMENSION AVE(20),WS(20),WAVE(20),S(20),HSQ(20),H(20),RP(20,20),                    
     1 RG(20,20),WVP(20,20),WVG(20,20),CV(20),                                            
     1 CC(20),WCOVP(20),WCOVG(20),RPF(20),RGF(20),                                        
     1 C(20,10)                                                                           
      DIMENSION COVP(20),COVG(20)                                                         
      DIMENSION DD(20),WSPFA(20),WSGFA(20),AVEFA(20)                                      
      DIMENSION WCOVPF(10,10),WCOVGF(10,10),RPFF(10,10),RGFF(10,10)                       
      DIMENSION COVPF(10,10),COVGF(10,10)                                                 
      DIMENSION LABEL(8)                                                                  
      READ 110,LABEL                                                                      
  110 FORMAT(8A10)                                                                        
      PRINT 111,LABEL                                                                     
  111 FORMAT(1H1,10X,8A10)                                                                
      PRINT 100                                                                           
  100 FORMAT(11H0DATA INPUT)                                                              
C-----                                                                                    
      READ 1,NOT,NOF,LSW                                                                  
    1 FORMAT(16I5)                                                                        
      PRINT 2,NOT,NOF,LSW                                                                 
    2 FORMAT(7H0NOT = ,I3,8H  NOF = ,I3,8H  LSW = ,I3)                                    
C-----MEANS                                                                               
      READ 3,(AVE(I),I=1,NOT)                                                             
      PRINT 20                                                                            
   20 FORMAT(5H0MEAN)                                                                     
      PRINT 5,(AVE(I),I=1,NOT)                                                            
C-----PHEN S.D.                                                                           
      READ 3,(S(I),I=1,NOT)                                                               
    3 FORMAT(16F5.0)                                                                      
      PRINT 4                                                                             
    4 FORMAT(25H0PHEN. STANDARD DEVIATION)                                                
      PRINT 5,(S(I),I=1,NOT)                                                              
    5 FORMAT(1H0,10F13.5)                                                                 
C-----HSQ                                                                                 
      READ 3,(HSQ(I),I=1,NOT)                                                             
      PRINT 6                                                                             
    6 FORMAT(13H0HERITABILITY)                                                            
      PRINT 5,(HSQ(I),I=1,NOT)                                                            
C-----RP UPPER TRIANG ROWWISE                                                             
      NUT=NOT-1                                                                           
      DO 7 I=1,NUT                                                                        
      JF=I+1                                                                              
      READ 3,(RP(I,J),J=JF,NOT)                                                           
      DO 8 J=JF,NOT                                                                       
    8 RP(J,I)=RP(I,J)                                                                     
    7 RP(I,I)=1.0                                                                         
      RP(NOT,NOT)=1.0                                                                     
      PRINT 9                                                                             
    9 FORMAT(23H0PHENOTYPIC CORRELATION)                                                  
      DO 10 I=1,NOT                                                                       
   10 PRINT 5,(RP(I,J),J=1,NOT)                                                           
C-----RG UPPER TRIANG ROWWISE                                                             
      DO 11 I=1,NUT                                                                       
      JF=I+1                                                                              
      READ 3,(RG(I,J),J=JF,NOT)                                                           
      DO 12 J=JF,NOT                                                                      
   12 RG(J,I)=RG(I,J)                                                                     
   11 RG(I,I)=1.0                                                                         
      RG(NOT,NOT)=1.0                                                                     
      PRINT 13                                                                            
   13 FORMAT(20H0GENETIC CORRELATION)                                                     
      DO 14 I=1,NOT                                                                       
   14 PRINT 5,(RG(I,J),J=1,NOT)                                                           
C-----COEFFICIENTS OR POWERS                                                              
      PRINT 15                                                                            
   15 FORMAT(23H0COEFFICIENTS OR POWERS)                                                  
      DO 71 J=1,NOF                                                                       
      READ 3,(C(I,J),I=1,NOT)                                                             
      PRINT 5,(C(I,J),I=1,NOT)                                                            
   71 CONTINUE                                                                            
      PRINT 101                                                                           
  101 FORMAT(11H0WORK AREAS)                                                              
C-----CV AND H                                                                            
      DO 16 I=1,NOT                                                                       
      H(I)=SQRT(HSQ(I))                                                                   
   16 CV(I)=S(I)/AVE(I)                                                                   
      PRINT 17                                                                            
   17 FORMAT(11H0COEFF.VAR.)                                                              
      PRINT 5,(CV(I),I=1,NOT)                                                             
C-----WORKING SD"S AND WORKING AVERAGES                                                   
      DO 18 I=1,NOT                                                                       
      IF(LSW) 21,19,22                                                                    
   21 STOP                                                                                
   19 WS(I)=S(I)                                                                          
      WAVE(I)=AVE(I)                                                                      
      GO TO 18                                                                            
   22 WS(I)=SQRT(ALOG(1.+CV(I)*CV(I)))                                                    
      WAVE(I)=ALOG(AVE(I))-0.5*ALOG(1.+CV(I)*CV(I))                                       
   18 CONTINUE                                                                            
      PRINT 50                                                                            
   50 FORMAT(13H0WORKING MEAN)                                                            
      PRINT 5,(WAVE(I),I=1,NOT)                                                           
      PRINT 51                                                                            
   51 FORMAT(13H0WORKING S.D.)                                                            
      PRINT 5,(WS(I),I=1,NOT)                                                             
C-----CONSTRUCT G AND P COV MATRICES                                                      
      CALL DIAGMY(WS,RP,NOT,WVP)                                                          
      CALL DIAGMY(H,RG,NOT,WVG)                                                           
      CALL DIAGMY(WS,WVG,NOT,WVG)                                                         
      PRINT 23                                                                            
   23 FORMAT(30H0WORKING PHENOTYPIC COV MATRIX)                                           
      DO 24 I=1,NOT                                                                       
   24 PRINT 5,(WVP(I,J),J=1,NOT)                                                          
C-----                                                                                    
      PRINT 25                                                                            
   25 FORMAT(27H0WORKING GENETIC COV MATRIX)                                              
      DO 26 I=1,NOT                                                                       
   26 PRINT 5,(WVG(I,J),J=1,NOT)                                                          
C-----LOOP OVER FUNCTIONS                                                                 
      DO 200 IOF=1,NOF                                                                    
      PRINT 102,IOF                                                                       
  102 FORMAT(27H1FUNCTION PARAMETERS NOF = ,I3)                                           
C-----EXTRACT COEFFS                                                                      
      DO 201 I=1,NOT                                                                      
  201 CC(I)=C(I,IOF)                                                                      
C-----WORKING MEAN OF FUNCTION                                                            
      WAVEF=0.0                                                                           
      DO 40 I=1,NOT                                                                       
   40 WAVEF=WAVEF+CC(I)*WAVE(I)                                                           
      PRINT 72,WAVEF                                                                      
   72 FORMAT(16H0WORKING MEAN = ,F15.6)                                                   
C-----WORKING VARIANCE OF FUNCTION                                                        
      WVPF=QUADF(CC,WVP,NOT)                                                              
      WVGF=QUADF(CC,WVG,NOT)                                                              
      WSPF=SQRT(WVPF)                                                                     
      WSGF=SQRT(WVGF)                                                                     
      WSPFA(IOF)=WSPF                                                                     
      WSGFA(IOF)=WSGF                                                                     
      PRINT 27,WVPF,WSPF                                                                  
   27 FORMAT(22H0WORKING PHEN. VAR. = ,F15.6,9H  S.D. = ,F15.6)                           
      PRINT 28,WVGF,WSGF                                                                  
   28 FORMAT(22H0WORKING GENE. VAR. = ,F15.6,9H  S.D. = ,F15.6)                           
C-----TRUE MEAN OF FUNCTION                                                               
      IF(LSW) 41,42,43                                                                    
   41 STOP                                                                                
   43 AVEF=EXP(WAVEF+0.5*ALOG(1.+WVPF))                                                   
      GO TO 44                                                                            
   42 AVEF=WAVEF                                                                          
   44 PRINT 70,AVEF                                                                       
      AVEFA(IOF)=AVEF                                                                     
   70 FORMAT(8H0MEAN = ,F15.6)                                                            
C-----TRUE VARIENCE OF FUNCTION                                                           
      IF(LSW) 29,30,31                                                                    
   29 STOP                                                                                
   30 VPF=WVPF                                                                            
      VGF=WVGF                                                                            
      GO TO 32                                                                            
   31 VPF=WVPF*AVEF*AVEF                                                                  
      VGF=WVGF*AVEF*AVEF                                                                  
   32 CONTINUE                                                                            
      SPF=SQRT(VPF)                                                                       
      SGF=SQRT(VGF)                                                                       
      PRINT 33,VPF,SPF                                                                    
   33 FORMAT(14H0PHEN. VAR. = ,F15.6,9H  S.D. = ,F15.6)                                   
      PRINT 34,VGF,SGF                                                                    
   34 FORMAT(14H0GENE. VAR. = ,F15.6,9H  S.D. = ,F15.6)                                   
C-----HSQ OF FUNCTION                                                                     
      HSQF=VGF/VPF                                                                        
      PRINT 6                                                                             
      PRINT 5,HSQF                                                                        
C-----WORKING COV OF FUNCTION WITH ALL TRAITS                                             
      DO 60 I=1,NOT                                                                       
      WCOVP(I)=0.0                                                                        
      WCOVG(I)=0.0                                                                        
      DO 60 J=1,NOT                                                                       
      WCOVP(I)=WCOVP(I)+CC(J)*WVP(I,J)                                                    
   60 WCOVG(I)=WCOVG(I)+CC(J)*WVG(I,J)                                                    
      PRINT 23                                                                            
      PRINT 5,(WCOVP(I),I=1,NOT)                                                          
      PRINT 25                                                                            
      PRINT 5,(WCOVG(I),I=1,NOT)                                                          
C-----CORRELATOONS WITH FUNCTION                                                          
      DO 61 I=1,NOT                                                                       
      RPF(I)=WCOVP(I)/(WSPF*WS(I))                                                        
   61 RGF(I)=WCOVG(I)/(WSGF*WS(I)*H(I))                                                   
      PRINT 9                                                                             
      PRINT 5,(RPF(I),I=1,NOT)                                                            
      PRINT 13                                                                            
      PRINT 5,(RGF(I),I=1,NOT)                                                            
C-----TRUE COV OF FUNCTION WITH ALL TRAITS                                                
      DO 62 I=1,NOT                                                                       
      IF(LSW) 63,64,65                                                                    
   63 STOP                                                                                
   64 COVP(I)=WCOVP(I)                                                                    
      COVG(I)=WCOVG(I)                                                                    
      GO TO 62                                                                            
   65 COVP(I)=WCOVP(I)*AVEF*AVE(I)                                                        
      COVG(I)=WCOVG(I)*AVEF*AVE(I)                                                        
   62 CONTINUE                                                                            
      PRINT 66                                                                            
   66 FORMAT(10H0PHEN. COV)                                                               
      PRINT 5,(COVP(I),I=1,NOT)                                                           
      PRINT 67                                                                            
   67 FORMAT(10H0GENE. COV)                                                               
      PRINT 5,(COVG(I),I=1,NOT)                                                           
  200 CONTINUE                                                                            
C-----WORKING COVS AMONG FUNCTIONS                                                        
      IF(NOF-1) 300,300,301                                                               
  301 PRINT 220                                                                           
  220 FORMAT(49H1COVARIANCES AND CORRELATIONS AMONG THE FUNCTIONS)                        
      DO 210 IOF=1,NOF                                                                    
      DO 211 I=1,NOT                                                                      
  211 CC(I)=C(I,IOF)                                                                      
      DO 210 JOF=1,NOF                                                                    
      DO 212 J=1,NOT                                                                      
  212 DD(J)=C(J,JOF)                                                                      
      WCOVPF(IOF,JOF)=HERMF(CC,DD,WVP,NOT)                                                
  210 WCOVGF(IOF,JOF)=HERMF(CC,DD,WVG,NOT)                                                
      PRINT 23                                                                            
      DO 221 IOF=1,NOF                                                                    
  221 PRINT 5,(WCOVPF(IOF,JOF),JOF=1,NOF)                                                 
      PRINT 25                                                                            
      DO 222 IOF=1,NOF                                                                    
  222 PRINT 5,(WCOVGF(IOF,JOF),JOF=1,NOF)                                                 
C-----CORRELATIONS AMONG FUNCTIONS                                                        
      DO 230 IOF=1,NOF                                                                    
      DO 230 JOF=1,NOF                                                                    
      RPFF(IOF,JOF)=WCOVPF(IOF,JOF)/(WSPFA(IOF)*WSPFA(JOF))                               
  230 RGFF(IOF,JOF)=WCOVGF(IOF,JOF)/(WSGFA(IOF)*WSGFA(JOF))                               
      PRINT 9                                                                             
      DO 231 IOF=1,NOF                                                                    
  231 PRINT 5,(RPFF(IOF,JOF),JOF=1,NOF)                                                   
      PRINT 13                                                                            
      DO 232 IOF=1,NOF                                                                    
  232 PRINT 5,(RGFF(IOF,JOF),JOF=1,NOF)                                                   
C-----TRUE COVS AMONG FUNCTIONS                                                           
      DO 240 IOF=1,NOF                                                                    
      DO 240 JOF=1,NOF                                                                    
      IF(LSW) 241,242,243                                                                 
  241 STOP                                                                                
  242 COVPF(IOF,JOF)=WCOVPF(IOF,JOF)                                                      
      COVGF(IOF,JOF)=WCOVGF(IOF,JOF)                                                      
      GO TO 240                                                                           
  243 COVPF(IOF,JOF)=WCOVPF(IOF,JOF)*AVEFA(IOF)*AVEFA(JOF)                                
      COVGF(IOF,JOF)=WCOVGF(IOF,JOF)*AVEFA(IOF)*AVEFA(JOF)                                
  240 CONTINUE                                                                            
      PRINT 66                                                                            
      DO 250 IOF=1,NOF                                                                    
  250 PRINT 5,(COVPF(IOF,JOF),JOF=1,NOF)                                                  
      PRINT 67                                                                            
      DO 251 IOF=1,NOF                                                                    
  251 PRINT 5,(COVGF(IOF,JOF),JOF=1,NOF)                                                  
  300 STOP                                                                                
      END                                                                                 
