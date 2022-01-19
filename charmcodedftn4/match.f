      PROGRAM MATCH(TAPE10=/1000,TAPE11=/1000,TAPE12=/1000                                
     + ,TAPE14=/1000,TAPE13=/1000,TAPE15=/1000                                            
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----COMPARE TWO SORTED CHARM FILES (PRIMARY & SECONDARY READ FILES)                     
C-----AND GENERATE FOUR CHARM RESULT FILES                                                
C-----                        - PRIMARY MATCH - TAPE12                                    
C-----                        - SECONDARY MATCH - TAPE14                                  
C-----                        - PRIMARY NOT MATCHED - TAPE13                              
C-----                        - SECONDARY NOT MATCHED - TAPE15                            
C-----                                                                                    
C-----TWO ALTERNATIVE COLLATING SEQUENCES DISPLAY AND ASCII6                              
C-----UP TO 50 SORT KEYS EACH DEFINED ON SEPARATE DIRECTIVE CARD                          
C-----                                                                                    
C-----OFFSET MATCHING -- TAPE10 IS PRIMARY OR MASTER FILE                                 
C-----                                                                                    
C-----SEQUENCE CHECKING OF P AND S FILES                                                  
C-----        -CAN BE TURNED ON OR OFF FOR NORMAL MATCHING                                
C-----        -IS AUTOMATICALLY ON FOR OFFSET MATCHING                                    
C-----                                                                                    
      DIMENSION LITP(100),LITS(100),LLITP(100)                                            
      DIMENSION LA(8)                                                                     
      DIMENSION LKEYP(50),LKEYS(50)                                                       
      DIMENSION KEYP(50),KEYS(50)                                                         
      DIMENSION IBEG(50),ILEN(50),JBEG(50),IORD(50),ISEQ(50)                              
      DIMENSION LSTAR(1),LCOM(1),LOP(2),LAD(1)                                            
      DIMENSION ITM(8),JTM(8),KTM(8)                                                      
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /NXST/ LA,LALEN,IPOS,ILNG,ITYP,IEND,KTM                                      
      CALL DEFINE                                                                         
      LALEN=MACHCC                                                                        
      CALL MCHL(46,LSTAR,1)                                                               
      CALL MCH(40,LSTAR,1)                                                                
      CALL MCHL(46,LCOM,1)                                                                
      CALL MCH(47,LCOM,1)                                                                 
      CALL MCHL(46,LOP,19)                                                                
      CALL MCH(29,LOP,19)                                                                 
      CALL MCH(09,LOP,19)                                                                 
      CALL MCH(14,LOP,19)                                                                 
      CALL MCH(34,LOP,19)                                                                 
      CALL MCH(09,LOP,19)                                                                 
      CALL MCH(20,LOP,19)                                                                 
      CALL MCH(06,LOP,19)                                                                 
      CALL MCH(18,LOP,19)                                                                 
      CALL MCH(04,LOP,19)                                                                 
      CALL MCH(09,LOP,19)                                                                 
      CALL MCH(12,LOP,19)                                                                 
      CALL MCH(34,LOP,19)                                                                 
      CALL MCH(09,LOP,19)                                                                 
      CALL MCH(16,LOP,19)                                                                 
      CALL MCH(07,LOP,19)                                                                 
      CALL MCH(07,LOP,19)                                                                 
      CALL MCH(20,LOP,19)                                                                 
      CALL MCH(06,LOP,19)                                                                 
      CALL MCH(21,LOP,19)                                                                 
      CALL MCHL(46,LAD,6)                                                                 
      CALL MCH(29,LAD,6)                                                                  
      CALL MCH(09,LAD,6)                                                                  
      CALL MCH(02,LAD,6)                                                                  
      CALL MCH(29,LAD,6)                                                                  
      CALL MCH(09,LAD,6)                                                                  
      CALL MCH(05,LAD,6)                                                                  
C-----                                                                                    
      WRITE(LP,10)                                                                        
   10 FORMAT(10H0MATCH RUN)                                                               
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      REWIND 13                                                                           
      REWIND 14                                                                           
      REWIND 15                                                                           
      CALL LNREAD(LRECP,LRECWP,10)                                                        
      CALL LNREAD(LRECS,LRECWS,11)                                                        
      CALL LNWRIT(LRECP,LRECWP,12)                                                        
      CALL LNWRIT(LRECP,LRECWP,13)                                                        
      CALL LNWRIT(LRECS,LRECWS,14)                                                        
      CALL LNWRIT(LRECS,LRECWS,15)                                                        
      LASTP=LRECWP+1                                                                      
      LASTS=LRECWS+1                                                                      
C-----                                                                                    
C-----READ DIRECTIVES                                                                     
C-----                                                                                    
      ISEQSW=0                                                                            
      IOFFSW=0                                                                            
      K=0                                                                                 
    1 CALL READCD(LC,LA,IFLAG,MACHCD)                                                     
      IF(IFLAG) 3,4,999                                                                   
    4 CALL WRITLN(LP,LA,0,MACHCD)                                                         
C-----                                                                                    
C-----THIS SECTION CRACKS A DIRECTIVE AND STORES                                          
C-----                                                                                    
      IPOS=1                                                                              
      K=K+1                                                                               
      IEND=0                                                                              
      IF(NXSC(ITM,1,LSTAR,1,1,0)) 5,7,5                                                   
C-----                                                                                    
    5 WRITE(LP,109)                                                                       
  109 FORMAT(23H UNRECOGNISED DIRECTIVE)                                                  
      CALL JOBEND                                                                         
C-----                                                                                    
    7 I=NXSC(ITM,1,LOP,19,3,1)+1                                                          
      GO TO (5,310,311,312,999),I                                                         
C-----                                                                                    
  312 IOFFSW=1                                                                            
  311 ISEQSW=1                                                                            
      K=K-1                                                                               
      GO TO 1                                                                             
C-----                                                                                    
  310 IF(NXSC(ITM,1,LCOM,1,1,0)) 5,9,5                                                    
    9 IF(NXS(IBEG(K),0)) 5,5,13                                                           
   13 IF(NXSC(ITM,1,LCOM,1,1,0)) 5,14,5                                                   
   14 IF(NXS(ILEN(K),0)) 5,5,16                                                           
   16 IF(NXSC(ITM,1,LCOM,1,1,0)) 5,17,5                                                   
   17 IF(NXS(JBEG(K),0)) 5,5,19                                                           
   19 IF(NXSC(ITM,1,LCOM,1,1,0)) 5,20,5                                                   
   20 I=NXSC(ITM,1,LAD,6,2,1)+1                                                           
      IF(NXSC(ITM,1,LCOM,1,1,0)) 5,21,5                                                   
   21 GO TO (5,22,23,999),I                                                               
   22 IORD(K)=0                                                                           
      GO TO 25                                                                            
   23 IORD(K)=1                                                                           
   25 I=NXSC(ITM,1,LAD,6,2,1)+1                                                           
      GO TO (5,29,30,999),I                                                               
   29 ISEQ(K)=0                                                                           
      GO TO 32                                                                            
   30 ISEQ(K)=1                                                                           
C-----END DIRECTIVE CRACKING                                                              
   32 GO TO 1                                                                             
C-----                                                                                    
C-----                                                                                    
C-----THIS SECTION MATCHES RECORDS                                                        
C-----                                                                                    
C-----                                                                                    
C-----COUNTERS                                                                            
    3 K10=0                                                                               
      K11=0                                                                               
      K12=0                                                                               
      K13=0                                                                               
      K14=0                                                                               
      K15=0                                                                               
C-----SWITCHES                                                                            
      NEW10=1                                                                             
      NEW11=1                                                                             
      INITP=0                                                                             
      INITS=0                                                                             
      IDUPS=0                                                                             
      IEOFP=0                                                                             
      IEOFS=0                                                                             
C-----CALCULATE TOTAL KEY LENGTH IN CHARS                                                 
      KEYLEN=0                                                                            
      DO 270 KK=1,K                                                                       
  270 KEYLEN=KEYLEN+ILEN(KK)                                                              
C-----CHECK AT LEAST ONE DIRECTIVE PRESENT                                                
      IF(K) 999,140,130                                                                   
  140 WRITE(LP,141)                                                                       
  141 FORMAT(32H NO DIRECTIVES -- RUN TERMINATED)                                         
      CALL JOBEND                                                                         
C-----                                                                                    
C-----ERRORS BRANCH                                                                       
  999 CALL LOGIC(5HMATCH)                                                                 
      STOP                                                                                
C-----                                                                                    
C-----LOOP OVER RECIRDS                                                                   
C-----REMEMBER LAST KEYP AND LAST LITP                                                    
  130 CONTINUE                                                                            
      IF(INITP) 999,301,302                                                               
  302 CALL COPYC(KEYP,1,LKEYP,1,KEYLEN)                                                   
      DO 306 I=1,LASTP                                                                    
  306 LLITP(I)=LITP(I)                                                                    
  301 CONTINUE                                                                            
C-----PRIMARY READ                                                                        
      CALL READUN(10,LITP,IFLAG,LASTP)                                                    
      IF(IFLAG) 133,134,999                                                               
  134 K10=K10+1                                                                           
      NEW10=0                                                                             
C-----ASSEMBLE COMBINED KEYP                                                              
      CALL ORDKEY(IBEG,ILEN,IORD,ISEQ,K,LITP,LRECP,KEYP)                                  
      IF(ISEQSW)999,400,360                                                               
C-----SEQUENCE CHECK PRIMARY KEYS                                                         
  360 IF(INITP) 999,361,362                                                               
  361 INITP=1                                                                             
      GO TO 400                                                                           
  362 IF(IEOFP) 999,363,400                                                               
  363 IF(IJSTCM(KEYP,LKEYP,KEYLEN)) 370,380,390                                           
C-----SEQ ERROR PRIMARY                                                                   
  370 WRITE(LP,371)                                                                       
  371 FORMAT(49H SEQUENCE ERROR ON TAPE10 -- LAST RECORD READ WAS)                        
  403 CALL PRNTSI(LP,LITP(LASTP),LITP,LRECWP,0)                                           
      WRITE(LP,373)                                                                       
  373 FORMAT(15H RUN TERMINATED)                                                          
      CALL JOBEND                                                                         
C-----PRIMARY DUPLICATE                                                                   
  380 CONTINUE                                                                            
      IF(IOFFSW)999,390,401                                                               
  401 WRITE(LP,402)                                                                       
  402 FORMAT(65H DUPLICATE ON PRIMARY FILE IN OFFSET MODE -- LAST RECORD                  
     1 READ WAS)                                                                          
      GO TO 403                                                                           
C-----PRIMARY NON-DUPLICATE - PASS ON TO MATCH TEST                                       
  390 IF(IEOFS)999,400,170                                                                
C-----                                                                                    
  400 IF(NEW11)999,161,150                                                                
C-----                                                                                    
C-----SECONDARY READ                                                                      
  150 CALL READUN(11,LITS,IFLAG,LASTS)                                                    
      IF(IFLAG) 153,154,999                                                               
  154 K11=K11+1                                                                           
      NEW11=0                                                                             
C-----REMEMBER LAST KEYS                                                                  
      IF(INITS) 999,304,305                                                               
  305 CALL COPYC(KEYS,1,LKEYS,1,KEYLEN)                                                   
  304 CONTINUE                                                                            
C-----ASSEMBLE COMBINED KEYS                                                              
      CALL ORDKEY(JBEG,ILEN,IORD,ISEQ,K,LITS,LRECS,KEYS)                                  
C-----                                                                                    
C-----                                                                                    
C-----SEQUENCE TEST - COMPARE CURRENT AND PREVIOUS KEYS ON SAME FILE                      
C-----                                                                                    
C-----SEQUENCE CHECK SECONDARY KEYS                                                       
      IF(ISEQSW)999,161,1001                                                              
 1001 IF(INITS)999,461,462                                                                
  461 INITS=1                                                                             
      GO TO 161                                                                           
  462 IF(IEOFS) 999,410,170                                                               
  410 IF(IJSTCM(KEYS,LKEYS,KEYLEN)) 470,480,490                                           
C-----SEQUENCE ERROR SECONDARY                                                            
  470 WRITE(LP,471)                                                                       
  471 FORMAT(49H SEQUENCE ERROR ON TAPE11 -- LAST RECORD READ WAS)                        
  404 CALL PRNTSI(LP,LITS(LASTS),LITS,LRECWS,0)                                           
      WRITE(LP,373)                                                                       
      CALL JOBEND                                                                         
C-----SECONDARY DUPLICATE - MATCH OFFSET WITH LAST PRIMARY                                
  480 IDUPS=1                                                                             
      IF(K10-1)999,1002,600                                                               
C-----SECONDARY NON-DUPLICATE - MATCH WITH CURRENT PRIMARY                                
  490 IDUPS=0                                                                             
 1002 IF(IEOFP)999,161,190                                                                
C-----                                                                                    
C-----OFFSET MATCH TEST                                                                   
C-----COMPARE CURRENT SECONDARY KEY WITH PREVIOUS PRIMARY KEY                             
C-----ONLY IF CURRENT SECONDARY KEY A DUPLIVATE                                           
C-----                                                                                    
  600 IF(IOFFSW) 999,161,601                                                              
  601 IF(IDUPS) 999,999,602                                                               
  602 IF(IJSTCM(LKEYP,KEYS,KEYLEN)) 670,680,690                                           
C-----OFFSET LKEYP .LT. KEYS                                                              
C-----DO A CURRENT MATCH                                                                  
  670 CONTINUE                                                                            
      GO TO 161                                                                           
C-----OFFSET MATCH - WRITE LAST P AND S ONTO 12 AND 14                                    
C-----              - GET NEW S ONLY                                                      
  680 CALL WRITUN(12,LLITP,LASTP)                                                         
      K12=K12+1                                                                           
      CALL WRITUN(14,LITS,LASTS)                                                          
      K14=K14+1                                                                           
      NEW10=0                                                                             
      NEW11=1                                                                             
      GO TO 150                                                                           
C-----OFFSET LKEYP .GT. KEYS                                                              
C-----S DUP WITHOUT MATCHING PREVIOUS P , GET NEW S ONLY                                  
  690 GO TO 190                                                                           
C-----                                                                                    
C-----MATCH TEST -- COMPARE CURRENT P AND S KEYS                                          
C-----                                                                                    
  161 CONTINUE                                                                            
      IF(IEOFP) 999,162,190                                                               
  162 IF(IEOFS) 999,163,170                                                               
  163 IF(IJSTCM(KEYP,KEYS,KEYLEN)) 170,180,190                                            
C-----                                                                                    
C-----P LESS THAN S                                                                       
C-----REJECT P OMTO 13 AND GET NEW P FROM 10                                              
  170 CALL WRITUN(13,LITP,LASTP)                                                          
      K13=K13+1                                                                           
      NEW10=1                                                                             
      GO TO 130                                                                           
C-----                                                                                    
C-----P EQUALS S                                                                          
C-----ACCEPT P AND S ONTO 12 AND 14 -- GET NEW P AND S FROM 10 AND 11                     
  180 CALL WRITUN(12,LITP,LASTP)                                                          
      K12=K12+1                                                                           
      CALL WRITUN(14,LITS,LASTS)                                                          
      K14=K14+1                                                                           
      NEW10=1                                                                             
      NEW11=1                                                                             
      GO TO 130                                                                           
C-----                                                                                    
C-----P GREATER THAN S                                                                    
C-----REJECT S ONTO 15 AND GET NEW S FROM 11                                              
  190 CALL WRITUN(15,LITS,LASTS)                                                          
      K15=K15+1                                                                           
      NEW11=1                                                                             
      GO TO 150                                                                           
C-----                                                                                    
C-----                                                                                    
C-----END FILE ON 10 -- COPY 11 TO 15                                                     
  133 IF(IEOFP) 999,733,734                                                               
  733 CALL EOFR(10,K10)                                                                   
  734 CONTINUE                                                                            
C-----IS THIS A DOUBLE EOF                                                                
      IF(IEOFS) 999,221,212                                                               
C-----COMPLETE SEQUENCE CHECKING ON S FILE                                                
  221 IF(ISEQSW) 999,222,220                                                              
  220 IEOFP=1                                                                             
      IF(NEW11) 999,360,150                                                               
C-----WITHOUT SEQ CHECK                                                                   
  222 IF(NEW11) 999,200,201                                                               
  200 CALL WRITUN(15,LITS,LASTS)                                                          
      K15=K15+1                                                                           
  201 CALL READUN(11,LITS,IFLAG,LASTS)                                                    
      IF(IFLAG) 210,202,999                                                               
  202 K11=K11+1                                                                           
      GO TO 200                                                                           
C-----                                                                                    
C-----END FILE IN 11 - COPY 10 TO 13                                                      
  153 IF(IEOFS) 999,753,754                                                               
  753 CALL EOFR(11,K11)                                                                   
  754 CONTINUE                                                                            
C-----IS THIS A DOUBLE EOF                                                                
      IF(IEOFP) 999,231,212                                                               
C-----COMPLETE SEQUENCE CHECKING ON P FILE                                                
  231 IF(ISEQSW) 999,232,230                                                              
  230 IEOFS=1                                                                             
      IF(NEW10) 999,360,130                                                               
C-----WTTHOUT SEQUENCE CHECH                                                              
  232 IF(NEW10) 999,203,204                                                               
  203 CALL WRITUN(13,LITP,LASTP)                                                          
      K13=K13+1                                                                           
  204 CALL READUN(10,LITP,IFLAG,LASTP)                                                    
      IF(IFLAG) 211,205,999                                                               
  205 K10=K10+1                                                                           
      GO TO 203                                                                           
C-----                                                                                    
C-----ENDFILE ON BOTH 10 AND 11 -- TERMINATE RUN                                          
  210 CALL EOFR(11,K11)                                                                   
      GO TO 212                                                                           
  211 CALL EOFR(10,K10)                                                                   
  212 REWIND 10                                                                           
      REWIND 11                                                                           
      CALL EOFW(12,K12)                                                                   
      CALL EOFW(14,K14)                                                                   
      CALL EOFW(13,K13)                                                                   
      CALL EOFW(15,K15)                                                                   
      ENDFILE 12                                                                          
      END FILE 13                                                                         
      ENDFILE 14                                                                          
      END FILE 15                                                                         
      REWIND 12                                                                           
      REWIND 13                                                                           
      REWIND 14                                                                           
      REWIND 15                                                                           
C-----                                                                                    
      STOP                                                                                
      END                                                                                 
