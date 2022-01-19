      PROGRAM PREDOC                                                                      
C-----PRELIM GROUP INPUT FOR DAM/OFF CORRELATIONS                                         
C-----UNALTERED 3600 DECK                                                                 
C-----DIMENSIONED FOR 22 TRAITS DAM AND PROGENY                                           
      DIMENSION IFMT(20),JFMT(20),KFMT(20)                                                
      DIMENSION LCODE(10,20),NI(10)                                                       
      DIMENSION X(22),LEV(10),TOTAL(22),TSS(22),ESS(22)                                   
      DIMENSION RX(484),PR(253),PZ(231)                                                   
      DIMENSION EN(75),T(75,22)                                                           
      DIMENSION SP(15000)                                                                 
      COMMON EN,T,SP,RX,PR,PZ                                                             
      MAXSP=15000                                                                         
      NCR=60 $ NLP=61 $ NCP=62                                                            
      LUN=2                                                                               
      REWIND LUN                                                                          
      READ(NCR,1) NOT                                                                     
      WRITE(NLP,7) NOT                                                                    
    7 FORMAT(7H0NOT = ,I4)                                                                
    1 FORMAT(20I4)                                                                        
    5 FORMAT(1H ,20I4)                                                                    
      WRITE(NLP,8)                                                                        
    8 FORMAT(8H0FORMATS)                                                                  
      READ(NCR,2) IFMT                                                                    
      WRITE(NLP,4)IFMT                                                                    
      READ(NCR,2)JFMT                                                                     
      WRITE(NLP,4)JFMT                                                                    
      READ(NCR,2)KFMT                                                                     
      WRITE(NLP,4)KFMT                                                                    
    2 FORMAT(10A8)                                                                        
    4 FORMAT(1H ,10A8)                                                                    
      WRITE(NLP,9)                                                                        
    9 FORMAT(19H0FACTORS AND LEVELS)                                                      
      READ(NCR,1) NFAC,(NI(I),I=1,NFAC)                                                   
      WRITE(NLP,5) NFAC,(NI(I),I=1,NFAC)                                                  
      WRITE(NLP,6)                                                                        
    6 FORMAT(6H0CODES)                                                                    
      DO 3 I=1,NFAC                                                                       
      NII=NI(I)                                                                           
      READ(NCR,1)(LCODE(I,J),J=1,NII)                                                     
    3 WRITE(NLP,5)(LCODE(I,J),J=1,NII)                                                    
      READ(NCR,1)IRWDTA,IGPSUM                                                            
      WRITE(NLP,5)IRWDTA,IGPSUM                                                           
C     CALC NO OF GROUPS                                                                   
      NGR=NBSC(NI,NFAC)                                                                   
C     INITIALIZE ARRAYS                                                                   
      MOT=NOT*(NOT+1)/2                                                                   
      TEMP=0.0                                                                            
      DO 20 I=1,NGR                                                                       
      EN(I)=0.0                                                                           
      DO 21 J=1,NOT                                                                       
   21 T(I,J)=0.0                                                                          
      DO 20 J=1,MOT                                                                       
   20 CALL PUTSP(SP,TEMP,I,J,NGR,45,MAXSP)                                                
C-----READ RAW DATA                                                                       
      WRITE(NLP,10)                                                                       
   10 FORMAT(13H1UNCODED DATA)                                                            
      KOUNT1=0                                                                            
      KOUNT2=0                                                                            
   14 CALL INPUT(X,NOT,LEV,NFAC,IFMT,KEY)                                                 
      IF(KEY) 101,100,102                                                                 
  100 KOUNT2=KOUNT2+1                                                                     
      IF(IRWDTA) 201,200,201                                                              
  201 CONTINUE                                                                            
      WRITE(NLP,JFMT) (LEV(I),I=1,NFAC),(X(I),I=1,NOT)                                    
  200 CONTINUE                                                                            
C-----CODE DATA                                                                           
      DO 11 I=1,NFAC                                                                      
      NII=NI(I)                                                                           
      DO 12 J=1,NII                                                                       
      IF(LEV(I)-LCODE(I,J)) 12,11,12                                                      
   12 CONTINUE                                                                            
      WRITE(NLP,13) I                                                                     
   13 FORMAT(18H REJECT ON FACTOR ,I2)                                                    
      GO TO 14                                                                            
   11 LEV(I)=J                                                                            
C-----LOCATE SUBGROUP                                                                     
      IGR=MLOC(LEV,NI,NFAC)                                                               
C-----SUMMING                                                                             
      KOUNT1=KOUNT1+1                                                                     
      EN(IGR)=EN(IGR)+1.                                                                  
      DO 22 I=1,NOT                                                                       
      T(IGR,I)=T(IGR,I)+X(I)                                                              
      DO 22 J=I,NOT                                                                       
      CALL LOC(I,J,IR,NOT,NOT,1)                                                          
      TEMP=X(I)*X(J)+GETSP(SP,IGR,IR,NGR, 45,MAXSP)                                       
   22 CALL PUTSP(SP,TEMP,IGR,IR,NGR,45,MAXSP)                                             
C-----RETURN AND READ NEXT CARD                                                           
      GO TO 14                                                                            
C-----ERROR                                                                               
  102 WRITE(NLP,103) KOUNT1,KOUNT2                                                        
  103 FORMAT(30H0PARITY ERROR ON INPUT COUNTS ,2I5)                                       
      RETURN                                                                              
C-----EOF                                                                                 
  101 WRITE(NLP,104) KOUNT1,KOUNT2                                                        
  104 FORMAT(21H0EOF ON INPUT COUNTS ,2I5)                                                
C-----INITIALIZE FOR SUMMING OVER GROUPS                                                  
      EDF=0.0                                                                             
      TDF=0.0                                                                             
      DO 105 I=1,NOT                                                                      
      TOTAL(I)=0.0                                                                        
      TSS(I)=0.0                                                                          
  105 ESS(I)=0.0                                                                          
C-----PUNCH SUNMARY CARDS                                                                 
C-----GROUP SUMMARIES                                                                     
      WRITE(NLP,28)                                                                       
   28 FORMAT(16H1GROUP SUMMARIES)                                                         
      DO 30 I=1,NGR                                                                       
C-----INSERT TO OUTPUT GROUP SUMMARIES FOR NELSAV BY CALL TO NSTR                         
      PN=EN(I)                                                                            
      DO 400 J=1,NOT                                                                      
      DO 400 K=1,NOT                                                                      
      CALL LOC(J,K,IR,NOT,NOT,1)                                                          
      CALL LOC(J,K,IS,NOT,NOT,0)                                                          
      IF(PN) 401,401,402                                                                  
  401 RX(IS)=0.0                                                                          
      GO TO 400                                                                           
  402 RX(IS)=GETSP(SP,I,IR,NGR,45,MAXSP)-T(I,J)*T(I,K)/EN(I)                              
  400 CONTINUE                                                                            
      CALL RCALC(RX,PR,NOT)                                                               
      CALL ZOUT1(NOT,PR,PZ,NLP)                                                           
      CALL NSTR(PN,RX,PZ,NOT,LUN)                                                         
      WRITE(NLP,403) I,PN,RX(1),RX(2),RX(484),PR(2),PZ(1)                                 
  403 FORMAT(1H ,I4,6F20.4)                                                               
C-----END INSERT                                                                          
      IF(EN(I)) 40,40,41                                                                  
   40 DO 42 J=1,NOT                                                                       
   42 X(J)=0.0                                                                            
      GO TO 43                                                                            
   41 DO 32 J=1,NOT                                                                       
   32 X(J)=T(I,J)/EN(I)                                                                   
   43 CONTINUE                                                                            
      IF(IGPSUM) 203,202,203                                                              
  203 CONTINUE                                                                            
      WRITE(NLP,KFMT) I,EN(I),(X(J),J=1,NOT),(T(I,J),J=1,NOT)                             
  202 CONTINUE                                                                            
      DO 33 J=1,NOT                                                                       
      CALL LOC(J,J,IR,NOT,NOT,1)                                                          
      TOTAL(J)=TOTAL(J)+T(I,J)                                                            
      TSS(J)=TSS(J)+GETSP(SP,I,IR,NGR,45,MAXSP)                                           
      IF(EN(I)) 33,33,133                                                                 
  133 ESS(J)=ESS(J)+GETSP(SP,I,IR,NGR,45,MAXSP)-T(I,J)*T(I,K)/EN(I)                       
   33 CONTINUE                                                                            
      IF(EN(I)) 30,30,34                                                                  
   34 EDF=EDF+EN(I)-1                                                                     
   30 TDF=TDF+EN(I)                                                                       
C-----B/N GRP AOV                                                                         
      CALL BNWNAV(NGR,TSS,ESS,TOTAL, EDF,TDF,NOT,LUN)                                     
      END FILE LUN                                                                        
      REWIND LUN                                                                          
      RETURN                                                                              
      END                                                                                 
      SUBROUTINE PUTSP(SP,TEMP,IGR,IR,NGR,LUN,MAXSP)                                      
      DIMENSION SP(1)                                                                     
      JSP=NGR*(IR-1)+IGR                                                                  
      IF(JSP-MAXSP) 500,500,501                                                           
  500 SP(JSP)=TEMP                                                                        
    5 RETURN                                                                              
  501 CONTINUE                                                                            
      IF(UNIT,LUN) 501,2,3                                                                
    3 CALL Q8QERROR(0,18H0DISC WRITE ERROR.)                                              
    2 ISN=JSP-MAXSP                                                                       
      CALL WDISC(1,ISN,TEMP,LUN)                                                          
    4 CONTINUE                                                                            
      IF(UNIT,LUN) 4,5,3                                                                  
      END                                                                                 
      FUNCTION GETSP(SP,IGR,IR,NGR,LUN,MAXSP)                                             
      DIMENSION SP(1)                                                                     
      JSP=NGR*(IR-1)+IGR                                                                  
      IF(JSP-MAXSP) 500,500,501                                                           
  500 GETSP=SP(JSP)                                                                       
      RETURN                                                                              
  501 CONTINUE                                                                            
      IF(UNIT,LUN) 501,2,3                                                                
    3 CALL Q8QERROR(0,17H0DISC READ ERROR.)                                               
    2 ISN=JSP-MAXSP                                                                       
      CALL RDISC(U,ISN,TEMP,LUN)                                                          
    4 CONTINUE                                                                            
      IF(UNIT,LUN) 4,5,3                                                                  
    5 GETSP=TEMP                                                                          
      RETURN                                                                              
      END                                                                                 
