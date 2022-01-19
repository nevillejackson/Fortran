      PROGRAM NEWDOC(INPUT,OUTPUT,TAPE60=INPUT,TAPE61=OUTPUT                              
     1 ,TAPE1,TAPE2,TAPE3,TAPE20)                                                         
C-----UNALTERED 3600 DECKS                                                                
      DIMENSION X(1),XBAR(50),STD(50),RX(2500),PR(1275),B(50),D(50),                      
     1 T(50),PZ(1225),RG(2500),SERG(1275),SP(2500),Z(1225),BART(50),                      
     2 CHISQ(1225)                                                                        
      DIMENSION IOUT(17),ISAME(50,2),NW(5)                                                
      COMMON RG,SP,CHISQ,Z,SERG,PZ,RX                                                     
      NCR=60 $ NLP=61                                                                     
      READ(NCR,1) LUN1,LUN2,LUN3                                                          
    1 FORMAT(20I4)                                                                        
      WRITE(NLP,1000) LUN1,LUN2,LUN3                                                      
 1000 FORMAT(1H0,30I4)                                                                    
      READ(NCR,1) NT,NOT,NDT,NBSC,NLSC,NSAME                                              
      WRITE(NLP,1000) NT,NOT,NDT,NBSC,NLSC,NSAME                                          
      READ (NCR,1)(NW(I),I=1,NLSC)                                                        
      WRITE(NLP,1000)(NW(I),I=1,NLSC)                                                     
      IF(NSAME) 4000,4000,4001                                                            
 4001 READ(NCR,1)((ISAME(I,J),J=1,2),I=1,NSAME)                                           
      WRITE(NLP,1000)((ISAME(I,J),J=1,2),I=1,NSAME)                                       
 4000 READ(NCR,1)(IOUT(I),I=1,17)                                                         
      WRITE(NLP,1000)(IOUT(I),I=1,17)                                                     
      REWIND 20                                                                           
      EK=1.0                                                                              
      DO 2 I=1,NBSC                                                                       
      READ 1,N,M                                                                          
      PN=N                                                                                
      IF(N-2) 3000,3001,3002                                                              
 3001 CALL DATA(M,D)                                                                      
 3000 IF(N-1) 3031,3032,3032                                                              
 3032 CALL DATA(M,XBAR)                                                                   
 3031 DO 3003 II=1,NT                                                                     
      IF(N-1) 3033,3034,3035                                                              
 3035 B(II)=0.5*(XBAR(II)-D(II))**2                                                       
      ABLE=B(II)                                                                          
      STD(II)=ABLSQT(ABLE)                                                                
      XBAR(II)=(XBAR(II)+D(II))/2.                                                        
      GO TO 3036                                                                          
 3033 XBAR(II)=0.                                                                         
 3034 STD(II)=0.                                                                          
      B(II)=0.                                                                            
 3036 DO 3003 JJ=1,NT                                                                     
      CALL LOC(II,JJ,IR,NT,NT,0)                                                          
      CALL LOC(II,JJ,IS,NT,NT,1)                                                          
      CALL LOC(II,JJ,IT,NT,NT,3)                                                          
      IF(N-1) 3037,3037,3038                                                              
 3038 IF(II-JJ) 3037,3039,3037                                                            
 3039 RX(IR)=B(II)                                                                        
      GO TO 3040                                                                          
 3037 RX(IR)=0.                                                                           
 3040 PR(IS)=0.                                                                           
      IF(IT) 3003,3003,3005                                                               
 3005 PZ(IT)=0.                                                                           
 3003 CONTINUE                                                                            
      GO TO 3004                                                                          
 3002 CALL CORRE(N,NT,0,X,XBAR,STD,RX,PR,B,D,T)                                           
      PRINT 1002,I,N,M                                                                    
 1002 FORMAT(16H1NEW GROUP  I = ,I2,7H   N = ,I5,7H   M = ,I2)                            
      CALL MOUT(N,M,XBAR,STD,B)                                                           
      CALL ZOUT1(M,PR,PZ,NLP)                                                             
 3004 CALL NSTR(PN,RX,PZ,NT,LUN2)                                                         
      CALL NEWDRV(PN,RX,PZ,PR,NOT,NDT,NT,NSAME,ISAME,IOUT,NLSC,I,EK,                      
     1  RG,SERG,D,T)                                                                      
    2 CONTINUE                                                                            
      REWIND LUN1                                                                         
      REWIND LUN2                                                                         
      REWIND 20                                                                           
C     SELECT POOLING LEVEL                                                                
    3 NLSC=NLSC-1                                                                         
      IF(NLSC) 20,7,7                                                                     
    7 CONTINUE                                                                            
      READ(NCR,1)(IOUT(I),I=1,17)                                                         
      WRITE(NLP,1000)(IOUT(I),I=1,17)                                                     
      KSW=LUN2                                                                            
      LUN2=LUN3                                                                           
      LUN3=KSW                                                                            
      NPOOLD=NW(NLSC+1)                                                                   
      NLEFT=1                                                                             
      DO 4 I=1,NLSC                                                                       
    4 NLEFT=NLEFT*NW(I)                                                                   
      PRINT 1000,NPOOLD                                                                   
      XX=NPOOLD                                                                           
      EK=EK*XX                                                                            
      PRINT 1001,EK                                                                       
 1001 FORMAT(1H0,F14.4)                                                                   
      DO 5 I=1,NLEFT                                                                      
      CALL POOLI(EN,SP,Z,PN,RX,PZ,NT,LUN3,EK,CHISQ,BART,NPOOLD,IOUT)                      
      N=0                                                                                 
      DO 6 J=1,NPOOLD                                                                     
      CALL NUNSTR(EN,SP,Z,NT,LUN3,KEY,N)                                                  
      IF(KEY) 32,30,32                                                                    
   32 STOP 32                                                                             
   30 CALL POOLN(EN,SP,Z,PN,RX,PZ,NT,LUN3,EK,CHISQ,BART,NPOOLD,IOUT)                      
    6 CONTINUE                                                                            
      CALL RCALC(RX,PR,NT)                                                                
      CALL ZCALC(PZ,PN,EK,NT)                                                             
      CALL NSTR(PN,RX,PZ,NT,LUN2)                                                         
      CALL NEWDRV(PN,RX,PZ,PR,NOT,NDT,NT,NSAME,ISAME,IOUT,NLSC,NLEFT,EK,                  
     1  RG,SERG,D,T)                                                                      
      CALL POOLND(EN,SP,Z,PN,RX,PZ,NT,LUN3,EK,CHISQ,BART,NPOOLD,IOUT)                     
    5 CONTINUE                                                                            
      REWIND LUN2                                                                         
      REWIND LUN3                                                                         
      GO TO 3                                                                             
   20 RETURN                                                                              
      END                                                                                 
      SUBROUTINE NUNSTR(EN,SP,Z,NT,LUN3,KEY,N)                                            
      DIMENSION SP(1),Z(1)                                                                
      NLP=61                                                                              
      IBACK=0                                                                             
      LLT=NT*NT                                                                           
      MT=NT*(NT-1)/2                                                                      
    5 READ(LUN3) EN,(SP(I),I=1,LLT),(Z(I),I=1,MT)                                         
      IF(EOF(LUN3)) 1,2                                                                   
    2 IF(IOCHEC(LUN3)) 3,4                                                                
    4 CONTINUE                                                                            
      N=N+1                                                                               
      KEY=0                                                                               
      RETURN                                                                              
C                                                                                         
    1 WRITE(NLP,104) LUN3,N                                                               
  104 FORMAT(8H EOF ON ,I2,6H  N = ,I5/)                                                  
      KEY=-1                                                                              
      RETURN                                                                              
C                                                                                         
    3 WRITE(NLP,105) LUN3,N                                                               
  105 FORMAT(17H PARITY ERROR ON ,I2,6H  N = ,I5/)                                        
      BACKSPACE LUN3                                                                      
      IBACK=IBACK+1                                                                       
      IF(IBACK-10) 5,5,6                                                                  
    6 KEY=1                                                                               
      RETURN                                                                              
      END                                                                                 
      SUBROUTINE NEWDRV(EN,RX,Z,R,NOT,NDT,NT,NSAME,ISAME,IOUT,NLSC,                       
     1  ILEFT,EK,RG,SERG,D,T)                                                             
      DIMENSION ISAME(50,2),IOUT(17)                                                      
      DIMENSION RX(1),Z(1),R(1),RG(1),SERG(1),D(1),T(1)                                   
      NLP=61                                                                              
      WRITE(NLP,2) NLSC,ILEFT                                                             
    2 FORMAT(10H0SUBCLASS ,2I8/)                                                          
      WRITE(NLP,3)EN                                                                      
    3 FORMAT(14H0REPLICATES = ,F14.1)                                                     
      WRITE(NLP,4)EK                                                                      
    4 FORMAT(6H0EK = ,F14.1)                                                              
      ABLE=EN-3.*EK                                                                       
      F=1./ABLSQT(ABLE)                                                                   
      WRITE(NLP,1)F                                                                       
    1 FORMAT(19H0SE FOR POOLED Z = ,F14.6)                                                
      IF(IOUT(1)) 200,201,200                                                             
  200 CALL MXOUT(1,RX,NT,NT,0,60,132,1,NLP)                                               
  201 IF(IOUT(2)) 202,203,202                                                             
  202 CALL MXOUT(2,R,NT,NT,1,60,132,1,NLP)                                                
  203 IF(IOUT(3)) 204,205,204                                                             
  204 CALL MXOUT(3,Z,NT,NT,3,60,132,1,NLP)                                                
  205 LT=NT*(NT+1)/2                                                                      
      MT=NT*(NT-1)/2                                                                      
      IF(NSAME) 213,213,4001                                                              
 4001 DO 10 I=1,NSAME                                                                     
      I1=ISAME(I,1)                                                                       
      I2=ISAME(I,2)                                                                       
      CALL LOC(I1,I1,IIOO,NT,NT,0)                                                        
      CALL LOC(I1,I2,IIOD,NT,NT,0)                                                        
      CALL LOC(I2,I2,IIDD,NT,NT,0)                                                        
      IF(RX(IIDD)) 12,12,11                                                               
   12 D(I)=0.0                                                                            
      D(I+NSAME)=0.0                                                                      
      GO TO 10                                                                            
   11 D(I)=RX(IIOD)/RX(IIDD)                                                              
      ABLE=(RX(IIOO)-D(I)*RX(IIOD))/((EN-2.*EK)*RX(IIDD))                                 
      D(I+NSAME)=ABLSQT(ABLE)                                                             
   10 CONTINUE                                                                            
      IF(IOUT(5)) 206,207,206                                                             
  206 CALL MXOUT(5,D,NSAME,2,0,60,132,1,NLP)                                              
  207 DO 20 I=1,NSAME                                                                     
      DO 20 J=1,2                                                                         
      CALL LOC(I,J,IR,NSAME,2,0)                                                          
   20 D(IR)=D(IR)*2.                                                                      
      IF(IOUT(6)) 208,209,208                                                             
  208 CALL MXOUT(6,D,NSAME,2,0,60,132,1,NLP)                                              
  209 DO 30 I=1,NSAME                                                                     
      I1=ISAME(I,1)                                                                       
      I2=ISAME(I,2)                                                                       
      CALL LOC(I1,I2,IIOD,NT,NT,1)                                                        
   30 T(I)=R(IIOD)                                                                        
      IF(IOUT(7)) 210,211,210                                                             
  210 CALL MXOUT(7,T,NSAME,1,0,60,132,1,NLP)                                              
  211 DO 31 I=1,NSAME                                                                     
   31 T(I)=T(I)*2.                                                                        
      IF(IOUT(8)) 212,213,212                                                             
  212 CALL MXOUT(8,T,NSAME,1,0,60,132,1,NLP)                                              
  213 DO 44 I=1,MT                                                                        
   44 R(I)=(EXP(2.*Z(I))-1.)/(EXP(2.*Z(I))+1)                                             
      IF(IOUT(9)) 214,215,214                                                             
  214 CALL MXOUT(9,R,NT,NT,3,60,132,1,NLP)                                                
  215 IF(NSAME) 219,219,4003                                                              
 4003 DO 50 I=1,NSAME                                                                     
      I1=ISAME(I,1)                                                                       
      I2=ISAME(I,2)                                                                       
      CALL LOC(I1,I2,IIOD,NT,NT,3)                                                        
   50 T(I)=R(IIOD)                                                                        
      IF(IOUT(10)) 216,217,216                                                            
  216 CALL MXOUT(10,T,NSAME,1,0,60,132,1,NLP)                                             
  217 DO 51 I=1,NSAME                                                                     
   51 T(I)=T(I)*2.                                                                        
      IF(IOUT(11)) 218,219,218                                                            
  218 CALL MXOUT(11,T,NSAME,1,0,60,132,1,NLP)                                             
  219 DO 56 I=1,NT                                                                        
      DO 56 J=I,NT                                                                        
      CALL LOC(I,J,IRX,NT,NT,0)                                                           
      CALL LOC(I,J,IR,NT,NT,1)                                                            
      IF(EN-EK) 58,58,57                                                                  
   58 R(IR)=0.0                                                                           
      GO TO 56                                                                            
   57 R(IR)=RX(IRX)/(EN-EK)                                                               
   56 CONTINUE                                                                            
      IF(IOUT(12)) 220,221,220                                                            
  220 CALL MXOUT(12,R,NT,NT,1,60,132,1,NLP)                                               
  221 IF(NSAME) 120,120,4002                                                              
 4002 DO 149 I=1,NSAME                                                                    
      I1=ISAME(I,1)                                                                       
      I2=ISAME(I,2)                                                                       
      CALL LOC(I1,I1,IIO,NT,NT,1)                                                         
      CALL LOC(I2,I2,IID,NT,NT,1)                                                         
      CALL LOC(I1,I2,IIDO,NT,NT,1)                                                        
      DO 149 J=1,NSAME                                                                    
      J1=ISAME(J,1)                                                                       
      J2=ISAME(J,2)                                                                       
      CALL LOC(J1,J2,JJDO,NT,NT,1)                                                        
      CALL LOC(I1,J2,IJOD,NT,NT,1)                                                        
      CALL LOC(I,J,IJODX,NSAME,NSAME,0)                                                   
      IF(I-J) 153,154,153                                                                 
  154 IF(R(IIO)) 150,151,152                                                              
  150 STOP 150                                                                            
  151 RG(IJODX)=0.0                                                                       
      GO TO 149                                                                           
  152 IF(R(IID)) 155,156,157                                                              
  155 STOP 155                                                                            
  156 RG(IJODX)=0.0                                                                       
      GO TO 149                                                                           
  157 ABLE=R(IID)*R(IIO)                                                                  
      RG(IJODX)=2.*R(IIDO)/ABLSQT(ABLE)                                                   
      GO TO 149                                                                           
  153 IF(R(IIDO)) 159,159,160                                                             
  159 R(IJODX)=0.0                                                                        
      GO TO 149                                                                           
  160 IF(R(JJDO)) 162,162,163                                                             
  162 RG(IJODX)=0.0                                                                       
      GO TO 149                                                                           
  163 ABLE=R(IIDO)*R(JJDO)                                                                
      RG(IJODX)=R(IJOD)/ABLSQT(ABLE)                                                      
  149 CONTINUE                                                                            
      IF(IOUT(13)) 222,223,222                                                            
  222 CALL MXOUT(13,RG,NSAME,NSAME,0,60,132,1,NLP)                                        
  223 DO 84 I=1,NSAME                                                                     
      DO 84 J=I,NSAME                                                                     
      IF(I-J) 85,84,84                                                                    
   85 CALL LOC(I,J,IJOD,NSAME,NSAME,0)                                                    
      CALL LOC(J,I,IJDO,NSAME,NSAME,0)                                                    
      RG(IJOD)=0.5*(RG(IJOD)+RG(IJDO))                                                    
   84 CONTINUE                                                                            
      IF(IOUT(14)) 224,225,224                                                            
  224 CALL MXOUT(14,RG,NSAME,NSAME,0,60,132,1,NLP)                                        
  225 DO 70 I=1,NSAME                                                                     
      I1=ISAME(I,1)                                                                       
      CALL LOC(I1,I1,IIO,NT,NT,1)                                                         
      CALL LOC(I,I,IIOX,NSAME,NSAME,0)                                                    
      DO 70 J=I,NSAME                                                                     
      IF(I-J) 69,68,69                                                                    
   68 SERG(IIOX)=0.0                                                                      
      GO TO 70                                                                            
   69 J1=ISAME(J,1)                                                                       
      J2=ISAME(J,2)                                                                       
      CALL LOC(J1,J1,JJO,NT,NT,1)                                                         
      CALL LOC(I1,J1,IJOO,NT,NT,1)                                                        
      CALL LOC(J,J,JJOX,NSAME,NSAME,0)                                                    
      CALL LOC(I,J,IJX,NSAME,NSAME,0)                                                     
      CALL LOC(I,J,IJX1,NSAME,NSAME,1)                                                    
      IF(R(IIO)) 71,72,73                                                                 
   71 STOP 71                                                                             
   72 RP=0.0                                                                              
      GO TO 74                                                                            
   73 IF(R(JJO)) 75,76,77                                                                 
   75 STOP 75                                                                             
   76 RP=0.0                                                                              
      GO TO 74                                                                            
   77 ABLE=R(IIO)*R(JJO)                                                                  
      RP=R(IJOO)/ABLSQT(ABLE)                                                             
   74 ABLE=RG(IIOX)*RG(JJOX)                                                              
      C=ABLSQT(ABLE)                                                                      
      IF(C) 80,81,80                                                                      
   81 SERG(IJX1)=0.                                                                       
      GO TO 70                                                                            
   80 DD=0.5*(1./RG(IIOX)+1./RG(JJOX))                                                    
      GO TO 82                                                                            
   82 SERG(IJX1)=(1./(EN-EK))*(0.5*(1.-RG(IJX)**2)**2 +                                   
     1  2.*(1.-RG(IJX)**2)*(1.-RP**2)/(C**2)+4.*(RG(IJX)*DD-RP/C)**2)                     
      SERG(IJX1)=ABLSQT(SERG(IJX1))                                                       
   70 CONTINUE                                                                            
      IF(IOUT(15)) 226,120,226                                                            
  226 CALL MXOUT(15,SERG,NSAME,NSAME,1,60,132,1,NLP)                                      
  120 RETURN                                                                              
      END                                                                                 
      SUBROUTINE POOLN(EN,SP,Z,PN,RX,PZ,NT,LUN3,EK,CHISQ,BART,NPOOLD,                     
     1   IOUT)                                                                            
      DIMENSION IOUT(17)                                                                  
      DIMENSION SP(1),Z(1),RX(1),PZ(1),CHISQ(1),BART(1)                                   
      REAL LOGF                                                                           
      NLP=61                                                                              
      LLT=NT*NT                                                                           
      MT=NT*(NT-1)/2                                                                      
      PN=PN+EN                                                                            
      DO 1 I=1,LLT                                                                        
    1 RX(I)=RX(I)+SP(I)                                                                   
      DO 2 I=1,MT                                                                         
    2 PZ(I)=PZ(I)+Z(I)*(EN-3.*EK/NPOOLD)                                                  
      RETURN                                                                              
C                                                                                         
      ENTRY POOLI                                                                         
      LLT=NT*NT                                                                           
      MT=NT*(NT-1)/2                                                                      
      DO 5 I=1,LLT                                                                        
    5 RX(I)=0.0                                                                           
      PN=0.0                                                                              
      DO 6 I=1,MT                                                                         
    6 PZ(I)=0.0                                                                           
      RETURN                                                                              
C                                                                                         
      ENTRY POOLND                                                                        
      LLT=NT*NT                                                                           
      MT=NT*(NT-1)/2                                                                      
      XEK=EK/NPOOLD                                                                       
      DO 17 I=1,NT                                                                        
      CALL LOC(I,I,IR,NT,NT,0)                                                            
      IF(PN-EK) 19,19,18                                                                  
   19 BART(I)=0.0                                                                         
      GO TO 17                                                                            
   18 ABLE=RX(IR)/(PN-EK)                                                                 
      IF(ABLE) 118,118,119                                                                
  118 BART(I)=0.0                                                                         
      GO TO 17                                                                            
  119 BART(I)=(PN-EK)*ALOG(ABLE)                                                          
   17 CONTINUE                                                                            
      DO 10 I=1,MT                                                                        
   10 CHISQ(I)=0.0                                                                        
      DO 8 KK=1,NPOOLD                                                                    
    8 BACKSPACE LUN3                                                                      
      N=0                                                                                 
      DO 11 KK=1,NPOOLD                                                                   
      CALL NUNSTR(EN,SP,Z,NT,LUN3,KEY,N)                                                  
      IF(KEY) 31,30,31                                                                    
   31 STOP 31                                                                             
   30 DO 12 I=1,NT                                                                        
      CALL LOC(I,I,IR,NT,NT,0)                                                            
      IF(EN-XEK) 12,12,13                                                                 
   13 ABLE=SP(IR)/(EN-XEK)                                                                
      IF(ABLE) 113,113,114                                                                
  113 BART(I)=0.0                                                                         
      GO TO 12                                                                            
  114 BART(I)=BART(I)-(EN-XEK)*ALOG(ABLE)                                                 
   12 CONTINUE                                                                            
      DO 14 I=1,NT                                                                        
      DO 14 J=I,NT                                                                        
      IF(I-J) 15,14,14                                                                    
   15 IF(EN-3.*XEK) 14,14,16                                                              
   16 CALL LOC(I,J,I3,NT,NT,3)                                                            
      CHISQ(I3)=CHISQ(I3)+((Z(I3)-PZ(I3))**2)*(EN-3.*XEK)                                 
   14 CONTINUE                                                                            
   11 CONTINUE                                                                            
      IF(IOUT(16)) 402,402,400                                                            
  400 CALL MXOUT(16,BART,NT,1,0,60,132,1,NLP)                                             
  402 IF(IOUT(17)) 403,403,401                                                            
  401 CALL MXOUT(17,CHISQ,NT,NT,3,60,132,1,NLP)                                           
  403 RETURN                                                                              
      END                                                                                 
      SUBROUTINE MOUT(N,M,XBAR,STD,B)                                                     
      DIMENSION XBAR(1),STD(1),B(1)                                                       
      PRINT 1,M,N                                                                         
    1 FORMAT(1H0,18HNO OF VARIABLES = ,I3,10X,21HNO OF OBSERVATIONS = ,                   
     1 I3)                                                                                
      PRINT 2                                                                             
    2 FORMAT(1H0,5HTRAIT,11X,4HMEAN,5X,10HSTAND.DEV.,10X,8HCORR.SS.,                      
     17X,8HVARIANCE,5X,10HCOEFF.VAR.,5X,10HSTAND.ERR.)                                    
    3 FORMAT(1H ,I5,2F15.5,F17.5,3F15.5)                                                  
      DIV=N-1                                                                             
      DUV=N                                                                               
      DO 4 I=1,M                                                                          
      V=B(I)/DIV                                                                          
      C=STD(I)*100./XBAR(I)                                                               
      E=SQRT(V/DUV)                                                                       
    4 PRINT 3,I,XBAR(I),STD(I),B(I),V,C,E                                                 
      RETURN                                                                              
      END                                                                                 
      SUBROUTINE ZCALC(PZ,PN,EK,NT)                                                       
      DIMENSION PZ(1)                                                                     
      MT=NT*(NT-1)/2                                                                      
      DEN=PN-3.*EK                                                                        
      IF(DEN) 2,2,3                                                                       
    3 DO 1 I=1,MT                                                                         
    1 PZ(I)=PZ(I)/DEN                                                                     
      RETURN                                                                              
    2 DO 4 I=1,MT                                                                         
    4 PZ(I)=0.0                                                                           
      RETURN                                                                              
      END                                                                                 
      SUBROUTINE NSTR(PN,PSP,PZ,NT,LUN2)                                                  
      DIMENSION PSP(1),PZ(1)                                                              
      LLT=NT*NT                                                                           
      MT=NT*(NT-1)/2                                                                      
      WRITE(LUN2) PN,(PSP(I),I=1,LLT),(PZ(I),I=1,MT)                                      
      RETURN                                                                              
      END                                                                                 
      SUBROUTINE RCALC(RX,PR,NT)                                                          
      DIMENSION RX(1),PR(1)                                                               
      DO 400 I=1,NT                                                                       
      CALL LOC(I,I,II,NT,NT,0)                                                            
      DO 400 J=I,NT                                                                       
      CALL LOC(J,J,JJ,NT,NT,0)                                                            
      CALL LOC(I,J,IJ,NT,NT,0)                                                            
      CALL LOC(I,J,IJR,NT,NT,1)                                                           
      ABLE=RX(II)*RX(JJ)                                                                  
      IF(ABLE) 401,401,402                                                                
  401 PR(IJR)=0.                                                                          
      GO TO 400                                                                           
  402 PR(IJR)=RX(IJ)/SQRT(ABLE)                                                           
  400 CONTINUE                                                                            
      RETURN                                                                              
      END                                                                                 
      SUBROUTINE ZOUT1(M,R,Z,NLP)                                                         
      REAL LOGF                                                                           
      DIMENSION Z(1),R(1)                                                                 
      N=0                                                                                 
      DO 3 I=2,M                                                                          
      JL=I*(I+1)/2-1                                                                      
      JF=JL-I+2                                                                           
      KL=JF-1                                                                             
      KF=JF-I+1                                                                           
      DO 4 J=JF,JL                                                                        
      N=N+1                                                                               
      ABLE=(1.+R(J))/(1.-R(J))                                                            
      IF(ABLE) 5,5,6                                                                      
    5 Z(N)=0.99999999E+20                                                                 
      GO TO 4                                                                             
    6 Z(N)=0.5*ALOG(ABLE)                                                                 
    4 CONTINUE                                                                            
    3 CONTINUE                                                                            
      RETURN                                                                              
      END                                                                                 
      SUBROUTINE CORRE (N,M,IO,X,XBAR,STD,RX,R,B,D,T)                   CORRE 59          
      DIMENSION X(1),XBAR(1),STD(1),RX(1),R(1),B(1),D(1),T(1)           CORRE 60          
C                                                                       CORRE 79          
C     INITIALIZATION                                                    CORRE 80          
C                                                                       CORRE 81          
      DO 100 J=1,M                                                      CORRE 82          
      B(J)=0.0                                                          CORRE 83          
  100 T(J)=0.0                                                          CORRE 84          
      K=(M*M+M)/2                                                       CORRE 85          
      DO 102 I=1,K                                                      CORRE 86          
  102 R(I)=0.0                                                          CORRE 87          
      FN=N                                                              CORRE 88          
      L=0                                                               CORRE 89          
C                                                                       CORRE 90          
      IF(IO) 105, 127, 105                                              CORRE 91          
C                                                                       CORRE 92          
C     DATA ARE ALREADY IN CORE                                          CORRE 93          
C                                                                       CORRE 94          
  105 DO 108 J=1,M                                                      CORRE 95          
      DO 107 I=1,N                                                      CORRE 96          
      L=L+1                                                             CORRE 97          
  107 T(J)=T(J)+X(L)                                                    CORRE 98          
      XBAR(J)=T(J)                                                      CORRE 99          
  108 T(J)=T(J)/FN                                                      CORRE 00          
C                                                                       CORRE 01          
      DO 115 I=1,N                                                      CORRE 02          
      JK=0                                                              CORRE 03          
      L=I-N                                                             CORRE 04          
      DO 110 J=1,M                                                      CORRE 05          
      L=L+N                                                             CORRE 06          
      D(J)=X(L)-T(J)                                                    CORRE 07          
  110 B(J)=B(J)+D(J)                                                    CORRE 08          
      DO 115 J=1,M                                                      CORRE 09          
      DO 115 K=1,J                                                      CORRE 10          
      JK=JK+1                                                           CORRE 11          
  115 R(JK)=R(JK)+D(J)*D(K)                                             CORRE 12          
      GO TO 205                                                         CORRE 13          
C                                                                       CORRE 14          
C     READ OBSERVATIONS AND CALCULATE TEMPORARY                         CORRE 15          
C     MEANS FROM THESE DATA IN T(J)                                     CORRE 16          
C                                                                       CORRE 17          
  127 IF(N-M) 130, 130, 135                                             CORRE 18          
  130 KK=N                                                              CORRE 19          
      GO TO 137                                                         CORRE 20          
  135 KK=M                                                              CORRE 21          
  137 DO 140 I=1,KK                                                     CORRE 22          
      CALL DATA (M,D)                                                   CORRE 23          
      DO 140 J=1,M                                                      CORRE 24          
      T(J)=T(J)+D(J)                                                    CORRE 25          
      L=L+1                                                             CORRE 26          
  140 RX(L)=D(J)                                                        CORRE 27          
      FKK=KK                                                            CORRE 28          
      DO 150 J=1,M                                                      CORRE 29          
      XBAR(J)=T(J)                                                      CORRE 30          
  150 T(J)=T(J)/FKK                                                     CORRE 31          
C                                                                       CORRE 32          
C     CALCULATE SUMS OF CROSS-PRODUCTS OF DEVIATIONS                    CORRE 33          
C     FROM TEMPORARY MEANS FOR M OBSERVATIONS                           CORRE 34          
C                                                                       CORRE 35          
      L=0                                                               CORRE 36          
      DO 180 I=1,KK                                                     CORRE 06          
      JK=0                                                              CORRE 38          
      DO 170 J=1,M                                                      CORRE 39          
      L=L+1                                                             CORRE 40          
  170 D(J)=RX(L)-T(J)                                                   CORRE 41          
      DO 180 J=1,M                                                      CORRE 42          
      B(J)=B(J)+D(J)                                                    CORRE 43          
      DO 180 K=1,J                                                      CORRE 44          
      JK=JK+1                                                           CORRE 45          
  180 R(JK)=R(JK)+D(J)*D(K)                                             CORRE 46          
C                                                                       CORRE 47          
      IF(N-KK) 205, 205, 185                                            CORRE 48          
C                                                                       CORRE 49          
C     READ THE REST OF OBSERVATIONS ONE AT A TIME, SUM                  CORRE 50          
C     THE OBSERVATION, AND CALCULATE SUMS OF CROSS-                     CORRE 51          
C     PRODUCTS OF DEVIATIONS FROM TEMPORARY MEANS                       CORRE 52          
C                                                                       CORRE 53          
  185 KK=N-KK                                                           CORRE 54          
      DO 200 I=1,KK                                                     CORRE 55          
      JK=0                                                              CORRE 56          
      CALL DATA (M,D)                                                   CORRE 57          
      DO 190 J=1,M                                                      CORRE 58          
      XBAR(J)=XBAR(J)+D(J)                                              CORRE 59          
      D(J)=D(J)-T(J)                                                    CORRE 60          
  190 B(J)=B(J)+D(J)                                                    CORRE 61          
      DO 200 J=1,M                                                      CORRE 62          
      DO 200 K=1,J                                                      CORRE 63          
      JK=JK+1                                                           CORRE 64          
  200 R(JK)=R(JK)+D(J)*D(K)                                             CORRE 65          
C                                                                       CORRE 66          
C     CALCULATE MEANS                                                   CORRE 67          
C                                                                       CORRE 68          
  205 JK=0                                                              CORRE 69          
      DO 210 J=1,M                                                      CORRE 70          
      XBAR(J)=XBAR(J)/FN                                                CORRE 71          
C                                                                       CORRE 72          
C     ADJUST SUMS OF CROSS-PRODUCTS OF DEVIATIONS                       CORRE 73          
C     FROM TEMPORARY MEANS                                              CORRE 74          
C                                                                       CORRE 75          
      DO 210 K=1,J                                                      CORRE 76          
      JK=JK+1                                                           CORRE 77          
  210 R(JK)=R(JK)-B(J)*B(K)/FN                                          CORRE 78          
C                                                                       CORRE 79          
C     CALCULATE CORRELATION COEFFICIENTS                                CORRE 80          
C                                                                       CORRE 81          
      JK=0                                                              CORRE 82          
      DO 220 J=1,M                                                      CORRE 83          
      JK=JK+J                                                           CORRE 84          
  220 STD(J)= SQRT( ABS(R(JK)))                                         CORRE 85          
      DO 230 J=1,M                                                      CORRE 86          
      DO 230 K=J,M                                                      CORRE 87          
      JK=J+(K*K-K)/2                                                    CORRE 88          
      L=M*(J-1)+K                                                       CORRE 89          
      RX(L)=R(JK)                                                       CORRE 90          
      L=M*(K-1)+J                                                       CORRE 91          
      RX(L)=R(JK)                                                       CORRE 92          
      IF(STD(J)*STD(K)) 225, 222, 225                                   CORRE 01          
  222 R(JK)=0.0                                                         CORRE 02          
      GO TO 230                                                         CORRE 03          
  225 R(JK)=R(JK)/(STD(J)*STD(K))                                       CORRE 04          
  230 CONTINUE                                                          CORRE 05          
C                                                                       CORRE 94          
C     CALCULATE STANDARD DEVIATIONS                                     CORRE 95          
C                                                                       CORRE 96          
      FN=SQRT(FN-1.0)                                                   CORRE 97          
      DO 240 J=1,M                                                      CORRE 98          
  240 STD(J)=STD(J)/FN                                                  CORRE 99          
C                                                                       CORRE 00          
C     COPY THE DIAGONAL OF THE MATRIX OF SUMS OF CROSS-PRODUCTS OF      CORRE 01          
C     DEVIATIONS FROM MEANS.                                            CORRE 02          
C                                                                       CORRE 03          
      L=-M                                                              CORRE 04          
      DO 250 I=1,M                                                      CORRE 05          
      L=L+M+1                                                           CORRE 06          
  250 B(I)=RX(L)                                                        CORRE 07          
      RETURN                                                            CORRE 08          
      END                                                               CORRE 09          
      SUBROUTINE MXOUT(ICODE,A,N,M,MS,LINS,IPOS,ISP,NLP)                                  
      DIMENSION A(1),B(8)                                                                 
    1 FORMAT(1H1,5X,7HMATRIX ,I5,6X,I3,5H ROWS,6X,I3,8H COLUMNS,                          
     1 8X,13HSTORAGE MODE ,I1,8X,5HPAGE ,I2,/)                                            
    2 FORMAT(12X,8HCOLUMN  ,7(3X,I3,10X))                                                 
    3 FORMAT(1H )                                                                         
    4 FORMAT(1H ,7X,4HROW ,I3,7(E16.6))                                                   
    5 FORMAT(1H0,7X,4HROW ,I3,7(E16.6))                                                   
C                                                                                         
      J=1                                                                                 
C                                                                                         
C     WRITE HEADING                                                                       
C                                                                                         
      NEND=IPOS/16-1                                                                      
      LEND=(LINS/ISP)-2                                                                   
      IPAGE=1                                                                             
   10 LSTRT=1                                                                             
   20 WRITE(NLP,1) ICODE,N,M,MS,IPAGE                                                     
      JNT=J+NEND-1                                                                        
      IPAGE=IPAGE+1                                                                       
   31 IF(JNT-M) 33,33,32                                                                  
   32 JNT=M                                                                               
   33 CONTINUE                                                                            
      WRITE(NLP,2)(JCUR,JCUR=J,JNT)                                                       
      IF(ISP-1) 35,35,40                                                                  
   35 WRITE(NLP,3)                                                                        
   40 LTEND=LSTRT+LEND-1                                                                  
      DO 80 L=LSTRT,LTEND                                                                 
C                                                                                         
C     FORM OUTPUT ROW LINE                                                                
C                                                                                         
      DO 55 K=1,NEND                                                                      
      KK=K                                                                                
      JT=J+K-1                                                                            
      CALL LOC(L,JT,IJNT,N,M,MS)                                                          
      B(K)=0.0                                                                            
      IF(IJNT) 50,50,45                                                                   
   45 B(K)=A(IJNT)                                                                        
   50 CONTINUE                                                                            
C                                                                                         
C     CHECK IF LAST COL.IF YES GO TO 60                                                   
C                                                                                         
      IF(JT-M) 55,60,60                                                                   
   55 CONTINUE                                                                            
C                                                                                         
C     END OF LINE,NOW WRITE                                                               
C                                                                                         
   60 IF(ISP-1) 65,65,70                                                                  
   65 WRITE(NLP,4)L,(B(JW),JW=1,KK)                                                       
      GO TO 75                                                                            
   70 WRITE(NLP,5) L,(B(JW),JW=1,KK)                                                      
C                                                                                         
C     IF END OF ROWS,GO CHECK COLS                                                        
C                                                                                         
   75 IF(N-L) 85,85,80                                                                    
   80 CONTINUE                                                                            
C                                                                                         
C     END OF PAGE,NOW CHECK FOR MORE OUTPUT                                               
C                                                                                         
      LSTRT=LSTRT+LEND                                                                    
      GO TO 20                                                                            
C                                                                                         
C     END OF COLS,THEN RETURN                                                             
C                                                                                         
   85 IF(JT-M) 90,95,95                                                                   
   90 J=JT+1                                                                              
      GO TO 10                                                                            
   95 RETURN                                                                              
      END                                                                                 
      SUBROUTINE LOC(I,J,IR,N,M,MS)                                                       
      IX=I                                                                                
      JX=J                                                                                
      IRX=MS+1                                                                            
      GO TO (10,20,30,40),IRX                                                             
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
      GO TO 36                                                                            
   40 IF(IX-JX) 42,46,44                                                                  
   42 IRX=1+IX+JX*(JX-3)/2                                                                
      GO TO 36                                                                            
   44 IRX=1+JX+IX*(IX-3)/2                                                                
      GO TO 36                                                                            
   46 IRX=0                                                                               
   36 IR=IRX                                                                              
      RETURN                                                                              
      END                                                                                 
      FUNCTION ABLSQT(ARG)                                                                
      IF(ARG) 1,1,2                                                                       
    1 ABLSQT=0.                                                                           
      RETURN                                                                              
    2 ABLSQT=SQRT(ARG)                                                                    
      RETURN                                                                              
      END                                                                                 
      FUNCTION ISTAT(LUN)                                                                 
      DATA(IER=0)                                                                         
C-----RETURNS -1 IF EOF                                                                   
C-----RETURNS 0 IF OK                                                                     
C-----RETURNS +1 IF PARITY ERROR                                                          
      IF(EOF(LUN)) 1,2                                                                    
    2 IF(IOCHEC(LUN)) 3,4                                                                 
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
