      PROGRAM NEWDOC                                                                      
C-----NEWDOC UNGROUPED DATA                                                               
C-----UNALTERED 3600 DECK                                                                 
      DIMENSION X(1),XBAR(39),STD(39),RX(1521),PR(780),B(39),D(39),T(39)                  
     1 ,PZ(741),RG(1521),SERG(780),SP(1521),Z(741),BART(39),CHISQ(741)                    
      DIMENSION IOUT(17),ISAME(25,2),NW(3)                                                
      COMMON RG,SP,CHISQ,Z,SERG,PZ,RX                                                     
      COMMON/LABELS/ LABL,NLSC,NW,NLSCC,NWC                                               
      DIMENSION LABL(3,15),NWC(3)                                                         
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
      DO 8005 I=1,NLSC                                                                    
      JJ=NW(I)                                                                            
      READ(NCR,1)(LABL(I,J),J=1,JJ)                                                       
 8005 WRITE(NLP,1000)(LABL(I,J),J=1,JJ)                                                   
      READ(NCR,1)IBYPAS                                                                   
      WRITE(NLP,1000) IBYPAS                                                              
      REWIND 20                                                                           
      EK=1.0                                                                              
      DO 8003 I=1,NLSC                                                                    
 8003 NWC(I)=1                                                                            
      NWC(NLSC)=0                                                                         
      M=NT                                                                                
      DO 2 I=1,NBSC                                                                       
      NLSCC=NLSC                                                                          
 8011 NWC(NLSCC)=NWC(NLSCC)+1                                                             
      IF(NWC(NLSCC)-NW(NLSCC)) 8009,8009,8010                                             
 8010 NWC(NLSCC)=1                                                                        
      NLSCC=NLSCC-1                                                                       
      GO TO 8011                                                                          
 8009 CONTINUE                                                                            
C-----READ 20 AND COUNT RECORDS                                                           
      REWIND 20                                                                           
      N=0                                                                                 
      DO 8000 IC=1,20000                                                                  
      CALL NDATA(M,D,N,KEY)                                                               
      IF(KEY) 8001,8000,20                                                                
 8000 CONTINUE                                                                            
 8001 PRINT 8002,M,N                                                                      
 8002 FORMAT(5H0M = ,I3,6H  N = ,I5)                                                      
      REWIND 20                                                                           
C-----END RECORD COUNT                                                                    
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
      IF(IBYPAS) 7002,7002,7003                                                           
 7002 CONTINUE                                                                            
      CALL MOUT(N,M,XBAR,STD,B)                                                           
 7003 CONTINUE                                                                            
      CALL ZOUT1(M,PR,PZ,NLP)                                                             
 3004 CALL NSTR(PN,RX,PZ,NT,LUN2)                                                         
      IF(IBYPAS) 7000,7000,7001                                                           
 7000 CONTINUE                                                                            
      CALL NEWDRV(PN,RX,PZ,PR,NOT,NDT,NT,NSAME,ISAME,IOUT,NLSC,I,EK,                      
     1  RG,SERG,D,T)                                                                      
 7001 CONTINUE                                                                            
      REWIND 20                                                                           
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
   32 CALL Q8QERROR(0,10H ERROR 32.)                                                      
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
      SUBROUTINE DATA(M,D)                                                                
      COMMON/LABELS/ LABL,NLSC,NW,NLSCC,NWC                                               
      DIMENSION LABL(3,15),D(1),KREC(64),NW(3),NWC(3)                                     
      IAGE=3                                                                              
      N=0                                                                                 
  100 CALL GETREC(KREC,64,20,KEY,N)                                                       
      IF(KEY) 7,8,7                                                                       
    7 RETURN                                                                              
    8 CONTINUE                                                                            
C-----TEST REC IN REQD GRP                                                                
      L1=NWC(1)                                                                           
      L2=NWC(2)                                                                           
      L3=NWC(3)                                                                           
      IF(LABL(1,L1)-KREC(11)) 4,15,4                                                      
   15 IF(LABL(2,L2)-KREC(1)) 4,25,4                                                       
   25 IF(LABL(3,L3)-KREC(33)) 4,35,4                                                      
C-----COPY DATA TO ARRAY D                                                                
   35 DO 40 I=1,10                                                                        
      D(I)=KREC(I+11)                                                                     
   40 D(I+11)=KREC(I+43)                                                                  
C-----CONVERT TO FLOAT                                                                    
      D(1)=D(1)/10                                                                        
      D(2)=D(2)/10                                                                        
      D(4)=D(4)/10                                                                        
      D(5)=D(5)/10                                                                        
      D(6)=D(6)/10                                                                        
      D(7)=D(7)/100                                                                       
      D(8)=D(8)/10                                                                        
      D(12)=D(12)/10                                                                      
      D(13)=D(13)/10                                                                      
      D(15)=D(15)/10                                                                      
      D(16)=D(16)/10                                                                      
      D(17)=D(17)/10                                                                      
      D(18)=D(18)/100                                                                     
      D(19)=D(19)/10                                                                      
C-----IS DAMS YEAR IN RANGE                                                               
      IYR=KREC(1)+IAGE                                                                    
      IF(IYR-59) 4,45,45                                                                  
   45 IF(IYR-68) 55,55,4                                                                  
   55 D(11)=KREC(IYR+22-58)                                                               
C-----IS PROGENY YEAR IN RANGE                                                            
      IYR=KREC(33)+IAGE                                                                   
      IF(IYR-59) 4,65,65                                                                  
   65 IF(IYR-68) 75,75,4                                                                  
   75 D(22)=KREC(IYR+54-58)                                                               
C-----WERE DOGS SCORES MADE                                                               
      IF(D(11)) 4,4,85                                                                    
   85 IF(D(22)) 4,4,95                                                                    
C-----IS 2T DATA OK                                                                       
   95 IF(KREC(22)) 4,105,4                                                                
  105 IF(KREC(54)) 4,115,4                                                                
C                                                                                         
  115 RETURN                                                                              
C-----REJECT BRANCH                                                                       
    4 N=N-1                                                                               
      GO TO 100                                                                           
      END                                                                                 
      SUBROUTINE NDATA(M,D,N,KEY)                                                         
      COMMON /LABELS/ LABL,NLSC,NW,NLSCC,NWC                                              
      DIMENSION LABL(3,15),D(1),KREC(64),NW(3),NWC(3)                                     
      IAGE=3                                                                              
  100 CALL GETREC(KREC,64,20,KEY,N)                                                       
      IF(KEY) 7,8,7                                                                       
    7 RETURN                                                                              
    8 CONTINUE                                                                            
C-----TEST REC IN REQD GRP                                                                
      L1=NWC(1)                                                                           
      L2=NWC(2)                                                                           
      L3=NWC(3)                                                                           
      IF(LABL(1,L1)-KREC(11)) 4,15,4                                                      
   15 IF(LABL(2,L2)-KREC(1)) 4,25,4                                                       
   25 IF(LABL(3,L3)-KREC(33)) 4,35,4                                                      
C-----IS DAMS YEAR IN RANGE                                                               
   35 IYR=KREC(1)+IAGE                                                                    
      IF(IYR-59) 4,45,45                                                                  
   45 IF(IYR-68) 55,55,4                                                                  
   55 D(11)=KREC(IYR+22-58)                                                               
C-----IS PROGENY YEAR IN RANGE                                                            
      IYR=KREC(33)+IAGE                                                                   
      IF(IYR-59) 4,65,65                                                                  
   65 IF(IYR-68) 75,75,4                                                                  
   75 D(22)=KREC(IYR+54-58)                                                               
C-----WERE DOGS SCORES MADE                                                               
      IF(D(11)) 4,4,85                                                                    
   85 IF(D(22)) 4,4,95                                                                    
C-----IS 2T DATA OK                                                                       
   95 IF(KREC(22)) 4,105,4                                                                
  105 IF(KREC(54)) 4,115,4                                                                
C                                                                                         
  115 RETURN                                                                              
C-----REJECT BRANCH                                                                       
    4 N=N-1                                                                               
      GO TO 100                                                                           
      END                                                                                 
