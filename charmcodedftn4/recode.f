      PROGRAM RECODE(TAPE10=/1000,TAPE11=/1000,TAPE12=/1000                               
     + ,TAPE13=/1000                                                                      
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN R.E. 1977                                                                   
C---- SETS UP RECORDS FOR INPUT TO PROGRAM SHEET BY MULTIPLE RUNS                         
C---- THROUGH A SELECTING SECTION FOLLOWED BY REFORMATTING OF THE                         
C---- INCLUDED RECORDS. THE EXCLUDED RECORD FILE ON PASS I IS USED                        
C---- AS THE INPUT FILE FOR PASS (I+1).                                                   
      DIMENSION LIT(100),LOT(100)                                                         
      DIMENSION LA(8),IBEG(11),ILEN(11),LOPR(3),LDIR                                      
     .(50,36),LVAL(11),ITG(200),JTG(200),ISC(200),LSC(200),JSC(200),KSC(                  
     .200),KSTR(1000),ISTR(1000),IEM(200),LEM(200),JEM(200),KEM(200),IEM                  
     .TG(200),JEMTG(200),IB(200),IL(200),LENGTH(200)                                      
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LCR,LP,LCP,LTR,LTP,LPL,LSP                  
      COMMON/CONST/KONST(64)                                                              
      DATA (LOPR(I),I=1,3)/2HEQ,2HGT,2HLT/                                                
      DATA LSTAR,LCOM,LINC,LEXC,LEXCL,LFUN,LLB,LRB,LCD,LALL,IBLK/1H*,1H,                  
     .,1HI,1HE,2HEL,1HF,1H(,1H),4HCODE,3HALL,10H          /                               
      CALL DEFINE                                                                         
C---- BEGIN RUN                                                                           
      WRITE(LP,10)                                                                        
   10 FORMAT(11H0RECODE RUN)                                                              
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      REWIND 13                                                                           
C---- INITIALISE READ AND WRITE UNITS, DIRECTIVE BLOCK COUNTER, AND TAPE                  
C----  11 COUNTER                                                                         
      LUN=10                                                                              
      IUN=11                                                                              
      KUN=12                                                                              
      NBLK=0                                                                              
      N11=0                                                                               
C---- LOOP OVER BLOCK OF DIRECTIVES                                                       
C---- A BLOCK BEGINS WITH EITHER *ALL OR *CODE AND IS TERMINATED BY EOF                   
C---- OR BY A *CODE BELONGING TO THE NEXT BLOCK.                                          
C---- K IS THE SELECT DIRECTIVE COUNTER                                                   
C---- NOTE... *ALL DIRECTIVES ARE VALID ONLY IN THE FIRST BLOCK                           
 1000 K=0                                                                                 
      KCD=0                                                                               
      KALL=0                                                                              
      NEM=0                                                                               
      NCOP=0                                                                              
      MEM=0                                                                               
      MCOP=0                                                                              
      LMAX=1                                                                              
      IEM(1)=1                                                                            
      JEM(1)=1                                                                            
    1 CALL READCD(5,LA,IOF,MACHCD)                                                        
      IF(IOF)450,350,999                                                                  
C---- BEGIN CRACKING DIRECTIVE, CHECKING FOR TYPE LEGALITY                                
  350 IPOS=1                                                                              
      IF(NEXITM(LS,LA,IPOS))5,6,5                                                         
    5 CALL WRITLN(6,LA,0,MACHCD)                                                          
      WRITE(LP,109)                                                                       
  109 FORMAT(23H UNRECOGNISED DIRECTIVE)                                                  
      CALL JOBEND                                                                         
    6 IF(LS-LSTAR)5,354,5                                                                 
  354 IF(NEXITM(LD,LA,IPOS))355,5,5                                                       
C---- CHECK FOR *ALL DIRECTIVE                                                            
  355 IF(LD-LALL)367,351,367                                                              
  351 IF(LUN-10)999,352,353                                                               
  353 CALL WRITLN(6,LA,0,MACHCD)                                                          
      PRINT 2                                                                             
    2 FORMAT(42H *ALL DIRECTIVES NOT ALLOWED IN THIS BLOCK)                               
      CALL JOBEND                                                                         
C---- CRACK REMAINDER OF *ALL DIRECTIVE                                                   
  352 NEM=NEM+1                                                                           
      NCOP=NCOP+1                                                                         
      IF(NEXITM(LC,LA,IPOS))5,356,5                                                       
  356 IF(LC-LCOM)5,357,5                                                                  
  357 IF(NEXITM(ITARG,LA,IPOS))5,5,358                                                    
  358 IF(NEXITM(LC,LA,IPOS))5,359,5                                                       
  359 IF(LC-LCOM)5,360,5                                                                  
  360 IF(NEXITM(LX,LA,IPOS))5,361,364                                                     
C---- PICK UP CHARACTER STRING TO BE EMITTED GLOBALLY                                     
  361 NCOP=NCOP-1                                                                         
      IEMTG(NEM)=ITARG                                                                    
      IJ=IEM(NEM)                                                                         
      CALL DOLSTR(LA,IPOS-1,ISTR,IJ,NCH)                                                  
      IF(NCH)999,362,363                                                                  
  362 WRITE(LP,11)                                                                        
   11 FORMAT(21H N=0  NO VALID STRING)                                                    
      GO TO 5                                                                             
  363 IEM(NEM+1)=IEM(NEM)+NCH                                                             
      LEM(NEM)=NCH                                                                        
      LTARG=ITARG+NCH-1                                                                   
      IF(LTARG.GT.LMAX)LMAX=LTARG                                                         
      KALL=NEM+NCOP                                                                       
      KTOT=KALL+KCD                                                                       
      IB(KTOT)=ITARG                                                                      
      IL(KTOT)=NCH                                                                        
      CALL WRITLN(6,LA,0,MACHCD)                                                          
      GO TO 1                                                                             
C---- PICK UP BEGINNING POSITION AND LENGTH OF FIELD TO BE COPIED                         
  364 NEM=NEM-1                                                                           
      ITG(NCOP)=ITARG                                                                     
      ISC(NCOP)=LX                                                                        
      IF(NEXITM(LC,LA,IPOS))5,365,5                                                       
  365 IF(LC-LCOM)5,366,5                                                                  
  366 IF(NEXITM(LSC(NCOP),LA,IPOS))5,5,349                                                
  349 LTARG=ITARG+LSC(NCOP)-1                                                             
      IF(LTARG.GT.LMAX)LMAX=LTARG                                                         
      KALL=NEM+NCOP                                                                       
      KTOT=KALL+KCD                                                                       
      IB(KTOT)=ITARG                                                                      
      IL(KTOT)=LSC(NCOP)                                                                  
      CALL WRITLN(6,LA,0,MACHCD)                                                          
      GO TO 1                                                                             
C---- CHECK FOR *CODE DIRECTIVE                                                           
  367 IF(LD-LCD)380,368,380                                                               
  368 IF(K)999,369,451                                                                    
C---- CRACK REMAINDER OF *CODE DIRECTIVE                                                  
  369 MEM=MEM+1                                                                           
      MCOP=MCOP+1                                                                         
      IF(NEXITM(LC,LA,IPOS))5,370,5                                                       
  370 IF(LC-LCOM)5,371,5                                                                  
  371 IF(NEXITM(ITARG,LA,IPOS))5,5,372                                                    
  372 IF(NEXITM(LC,LA,IPOS))5,373,5                                                       
  373 IF(LC-LCOM)5,374,5                                                                  
  374 IF(NEXITM(LX,LA,IPOS))5,375,377                                                     
C---- PICK UP CHARACTER STRING TO BE EMITTED LOCALLY                                      
  375 MCOP=MCOP-1                                                                         
      JEMTG(MEM)=ITARG                                                                    
      IJ=JEM(MEM)                                                                         
      CALL DOLSTR(LA,IPOS-1,KSTR,IJ,NCH)                                                  
      IF(NCH)999,362,376                                                                  
  376 JEM(MEM+1)=JEM(MEM)+NCH                                                             
      KEM(MEM)=NCH                                                                        
      KCD=MEM+MCOP                                                                        
      KTOT=KALL+KCD                                                                       
      IB(KTOT)=ITARG                                                                      
      IL(KTOT)=NCH                                                                        
      LTARG=ITARG+NCH-1                                                                   
      IF(LTARG.GT.LMAX)LMAX=LTARG                                                         
      CALL WRITLN(6,LA,0,MACHCD)                                                          
      GO TO 1                                                                             
C---- PICK UP BEGINNING AND LENGTH OF FIELD TO BE COPIED LOCALLY                          
  377 MEM=MEM-1                                                                           
      JTG(MCOP)=ITARG                                                                     
      JSC(MCOP)=LX                                                                        
      IF(NEXITM(LC,LA,IPOS))5,378,5                                                       
  378 IF(LC-LCOM)5,379,5                                                                  
  379 IF(NEXITM(KSC(MCOP),LA,IPOS))5,5,348                                                
  348 KCD=MEM+MCOP                                                                        
      KTOT=KALL+KCD                                                                       
      IB(KTOT)=ITARG                                                                      
      IL(KTOT)=KSC(MCOP)                                                                  
      LTARG=ITARG+KSC(MCOP)-1                                                             
      IF(LTARG.GT.LMAX)LMAX=LTARG                                                         
      CALL WRITLN(6,LA,0,MACHCD)                                                          
      GO TO 1                                                                             
C---- CHECK LEGALITY OF SELECT DIRECTIVES                                                 
  380 IF(KALL)999,382,381                                                                 
  381 IF(LUN-10)999,385,353                                                               
  382 IF(KCD)999,383,385                                                                  
  383 PRINT 384                                                                           
  384 FORMAT(33H NO REFORMATTING DIRECTIVES FOUND)                                        
      CALL JOBEND                                                                         
  385 K=K+1                                                                               
      KSWF=0                                                                              
      IF(NEXITM(LC,LA,IPOS))5,9,5                                                         
    9 IF(LC-LCOM) 5,12,5                                                                  
C-----                                                                                    
   12 IF(NEXITM(LBEG,LA,IPOS)) 60,5,13                                                    
   13 IF(NEXITM(LC,LA,IPOS)) 5,14,5                                                       
   14 IF(LC-LCOM) 5,15,5                                                                  
   15 IF(NEXITM(LEN,LA,IPOS)) 5,5,16                                                      
C-----                                                                                    
   16 IF(NEXITM(LC,LA,IPOS)) 5,17,5                                                       
   17 IF(LC-LCOM) 5,18,5                                                                  
C-----OP -- EQ,GT,LT                                                                      
   18 IF(NEXITM(LOP,LA,IPOS)) 59,5,5                                                      
   59 IF(NEXITM(LC,LA,IPOS)) 5,58,5                                                       
   58 IF(LC-LCOM) 5,57,5                                                                  
C-----                                                                                    
C-----BRANCH FOR FUNCTION OF 2 OR MORE FIELDS                                             
   60 IF(LBEG-LFUN) 5,62,5                                                                
   62 IF(NEXITM(LBKT,LA,IPOS)) 5,63,5                                                     
   63 DO 64 I=1,11                                                                        
      IF(NEXITM(IBEG(I),LA,IPOS))5,5,66                                                   
   65 IF(NEXITM(LC,LA,IPOS))5,69,5                                                        
   69 IF(LC-LCOM)74,64,74                                                                 
   74 IF(LC-LRB)5,70,5                                                                    
   66 IF(NEXITM(LC,LA,IPOS)) 5,67,5                                                       
   67 IF(LC-LCOM) 5,68,5                                                                  
   68 IF(NEXITM(ILEN(I),LA,IPOS))5,5,65                                                   
   64 CONTINUE                                                                            
      GO TO 5                                                                             
C-----END BRACKETED SEQUENCE -- FUNCTION SWITCH ON                                        
   70 KSWF=1                                                                              
      KOUNTF=I                                                                            
C-----RETURN TO MAIN CRACKING SEQUENCE                                                    
      IF(KOUNTF)71,72,16                                                                  
   72 CALL WRITLN(6,LA,0,MACHCD)                                                          
      PRINT 73                                                                            
   73 FORMAT(42H NO PARAMETERS IN BRACKETS -- CARD SKIPPED)                               
      GO TO 1                                                                             
   71 GO TO 999                                                                           
C-----                                                                                    
C-----WHICH OPERATOR                                                                      
   57 DO 80 I=1,3                                                                         
      IF(LOP-LOPR(I)) 80,81,80                                                            
   80 CONTINUE                                                                            
      GO TO 5                                                                             
   81 LOPC=I                                                                              
      GO TO (85,86,86),LOPC                                                               
C-----EQUALS                                                                              
   85 DO 90 I=1,10                                                                        
   92 IF(NEXALF(LV,LA,IPOS))93,500,93                                                     
  500 IF(LV-10HCOLUMN 80 )5,503,5                                                         
  503 NVALS=I-1                                                                           
      GO TO 110                                                                           
   93 LVAL(I)=LV                                                                          
      IF(NEXITM(LC,LA,IPOS))5,91,5                                                        
   91 IF(LC-LCOM)502,90,502                                                               
  502 IF(LC-10HCOLUMN 80 )5,504,5                                                         
  504 NVALS=I                                                                             
      GO TO 110                                                                           
   90 CONTINUE                                                                            
  100 NVALS=I                                                                             
      GO TO 110                                                                           
C-----END OF DIRECTIVE  GO TO STORAGE SECTION                                             
   86 IF(NEXALF(LV,LA,IPOS))97,5,97                                                       
   97 LVAL(1)=LV                                                                          
      IF(NEXITM(LC,LA,IPOS))5,98,5                                                        
   98 IF(LC-LCOM)101,99,101                                                               
  101 IF(LC-10HCOLUMN 80 )5,95,5                                                          
   95 NVALS=1                                                                             
      GO TO 110                                                                           
C-----END OF DIRECTIVE FOR 1 VALUE                                                        
   99 IF(NEXITM(LOP2,LA,IPOS)) 94,5,5                                                     
   94 DO 102 I=2,3                                                                        
      IF(LOP2-LOPR(I)) 102,103,102                                                        
  102 CONTINUE                                                                            
      GO TO 5                                                                             
  103 LOPC2=I                                                                             
      IF(NEXITM(LC,LA,IPOS)) 5,104,5                                                      
  104 IF(LC-LCOM) 5,105,5                                                                 
  105 IF(NEXALF(LV,LA,IPOS))106,5,106                                                     
  106 LVAL(2)=LV                                                                          
      NVALS=2                                                                             
C-----END OF DIRECTIVE FOR 2 VALUES                                                       
      GO TO 110                                                                           
C-----                                                                                    
C-----END OF DIRECTIVES CRACKING SECTION                                                  
C-----                                                                                    
C-----                                                                                    
C-----ERRORS SECTION                                                                      
C-----                                                                                    
  999 CALL LOGIC(6HSELECT)                                                                
      STOP                                                                                
C-----                                                                                    
C-----                                                                                    
C-----THIS SECTION CONVERTS A DIRECTIVE FROM ONE TO TWO DIMENSIONAL                       
C-----STORAGE , TO STORE A LIST OF MULTIPLE DIRECTIVES , IN ARRAY LDIR(IRECT,ITE          
C-----LITERAL KEYWORDS ARE CONVERTED TO NUMERIC OP CODES IN THIS STEP                     
C-----                                                                                    
C-----K COUNTS DIRECTIVES                                                                 
C-----                                                                                    
  110 CONTINUE                                                                            
   19 IF(LD-LINC) 20,21,20                                                                
   21 LDIR(K,1)=0                                                                         
      LDIR(K,35)=0                                                                        
      GO TO 22                                                                            
   20 IF(LD-LEXC) 24,23,24                                                                
   23 LDIR(K,1)=1                                                                         
      LDIR(K,36)=0                                                                        
      GO TO 22                                                                            
   24 IF(LD-LEXCL) 5,25,5                                                                 
   25 LDIR(K,1)=1                                                                         
      LDIR(K,36)=1                                                                        
C-----                                                                                    
   22 LDIR(K,2)=KSWF                                                                      
      IF(KSWF) 999,120,121                                                                
  120 LDIR(K,4)=LBEG                                                                      
      LDIR(K,5)=LEN                                                                       
      GO TO 122                                                                           
  121 LDIR(K,3)=KOUNTF                                                                    
      DO 124 I=1,KOUNTF                                                                   
      LDIR(K,I*2+2)=IBEG(I)                                                               
  124 LDIR(K,I*2+3)=ILEN(I)                                                               
C-----                                                                                    
  122 LDIR(K,24)=LOPC                                                                     
      GO TO (125,126,126),LOPC                                                            
C-----                                                                                    
  125 DO 127 I=1,NVALS                                                                    
  127 LDIR(K,I+24)=LVAL(I)                                                                
      GO TO 128                                                                           
C-----                                                                                    
  126 LDIR(K,25)=LVAL(1)                                                                  
      GO TO (128,129),NVALS                                                               
  129 LDIR(K,26)=LOPC2                                                                    
      LDIR(K,27)=LVAL(2)                                                                  
C-----                                                                                    
  128 LDIR(K,35)=NVALS                                                                    
C-----                                                                                    
C-----END OF SECTION                                                                      
C-----                                                                                    
C-----READ ANOTHER DIRECTIVE                                                              
      CALL WRITLN(6,LA,0,MACHCD)                                                          
      GO TO 1                                                                             
C---- END OF DIRECTIVE BLOCK CRACKING                                                     
C---- CHECK FOR LEGALITY OF DIRECTIVES IN LAST BLOCK ( I.E. TERMINATED                    
C---- BY EOF)                                                                             
  450 IF(K)999,140,451                                                                    
  451 IF(KALL)999,452,454                                                                 
  452 IF(KCD)999,383,453                                                                  
  453 NBLK=NBLK+1                                                                         
      GO TO 455                                                                           
  454 IF(LUN-10)999,453,353                                                               
  455 LENGTH(NBLK)=LMAX                                                                   
      MREC=LMAX                                                                           
      IF(KTOT-1)383,470,456                                                               
C---- CHECK FOR OVERLAPPING TARGET FIELDS                                                 
  456 DO 469 KK=2,KTOT                                                                    
      LIML=IB(KK)                                                                         
      LIMR=LIML+IL(KK)-1                                                                  
      KKK=KK-1                                                                            
      DO 468 JK=1,KKK                                                                     
      KIML=IB(JK)                                                                         
      KIMR=KIML+IL(JK)-1                                                                  
      IF(KIML.GT.LIMR.OR.LIML.GT.KIMR)468,466                                             
  466 PRINT 467,KK,JK                                                                     
  467 FORMAT(44H0*** WARNING ***   TARGET FIELD IN DIRECTIVE,I3,27H OVER                  
     .LAPS THAT IN DIRECTIVE,I3)                                                          
  468 CONTINUE                                                                            
  469 CONTINUE                                                                            
C---- BEGIN READING RECORDS                                                               
  470 CALL LNREAD(LREC,LRECW,LUN)                                                         
      IF(LUN-10)999,471,472                                                               
  471 CALL LNWRIT(MREC,MRECW,11)                                                          
  472 CALL LNWRIT(LREC,LRECW,KUN)                                                         
      LAST=LRECW+1                                                                        
      LOST=MRECW+1                                                                        
C-----                                                                                    
C-----                                                                                    
C-----                                                                                    
C-----THIS SECTION SELECTS RECORDS FROM TAPE10 ACCORDING TO STORED DIRECTIVES             
C-----LIST , AND WRITES SELECTED RECORDS ON TAPE11                                        
C-----THERE ARE K DIRECTIVES                                                              
C-----                                                                                    
C-----TAPE10 COUNTER                                                                      
    3 K2=0                                                                                
C-----TAPE11 COUNTER                                                                      
      K3=0                                                                                
C-----TAPE12 COUNTER                                                                      
      K12=0                                                                               
C-----CHECK AT LEAST ONE DIRECTIVE PRESENT                                                
      IF(K)999,140,512                                                                    
  140 WRITE(LP,141)                                                                       
  141 FORMAT(32H NO DIRECTIVES -- RUN TERMINATED)                                         
      CALL JOBEND                                                                         
  512 IF(LUN-10)999,474,130                                                               
  474 KL1=NEM                                                                             
      KL2=NCOP                                                                            
C-----LOOP OVER RECORDS                                                                   
  130 CALL READUN(LUN,LIT,IFLAG,LAST)                                                     
      IF(IFLAG) 509,134,999                                                               
  134 K2=K2+1                                                                             
C-----LOOP OVER DIRECTIVES FOR THIS RECORD                                                
      DO 144 KK=1,K                                                                       
      LORD=0                                                                              
      LD=LDIR(KK,1)                                                                       
      LOPC=LDIR(KK,24)                                                                    
      KSWF=LDIR(KK,2)                                                                     
      KOUNTF=LDIR(KK,3)                                                                   
C-----                                                                                    
C-----                                                                                    
C-----EQ,GT,LT,OR BOTH                                                                    
      GO TO (151,152,153),LOPC                                                            
C-----EQUALS                                                                              
  151 NVALS=LDIR(KK,35)                                                                   
C----LOOP OVER LALUES EQ TO                                                               
      DO 155 IVAL=1,NVALS                                                                 
C-----TEST FOR FUNCTION                                                                   
      IF(KSWF) 999,159,157                                                                
C-----SINGLE FIELD                                                                        
  159 IF(LDIR(KK,IVAL+24)-IGETLB(LIT,LDIR(KK,5),LDIR(KK,4),LRECW))                        
     1     155,156,155                                                                    
C-----MULTIPLE FIELDS                                                                     
  157 DO 158 KF=1,KOUNTF                                                                  
      IBEG(KF)=LDIR(KK,KF*2+2)                                                            
  158 ILEN(KF)=LDIR(KK,KF*2+3)                                                            
C-----                                                                                    
      IF(LDIR(KK,IVAL+24)-MFF(IBEG,ILEN,KOUNTF,LIT,LREC)) 155,156,155                     
  155 CONTINUE                                                                            
C-----NO FOR ALL VALUES EXCLUDE IMMEDIATELY ON I NEXT DIRECTIVE ON E                      
      IF(LD)999,330,144                                                                   
C-----YES FOR ONE VALUE -- NEXT DIRECTIVE ON I EXCLUDE IMMEDIATELY ON E                   
  156 IF(LD) 999,144,230                                                                  
C-----                                                                                    
C-----GREATER THAN                                                                        
  152 NVALS=LDIR(KK,35)                                                                   
C-----TEST FOR FUNCTION                                                                   
  161 IF(KSWF) 999,162,163                                                                
C-----SINGLE                                                                              
  162 IF(IGETLB(LIT,LDIR(KK,5),LDIR(KK,4),LRECW)-LDIR(KK,LORD*2+25))                      
     1    165,165,166                                                                     
C-----MULTIPLE FIELDS                                                                     
  163 DO 168 KF=1,KOUNTF                                                                  
      IBEG(KF)=LDIR(KK,KF*2+2)                                                            
  168 ILEN(KF)=LDIR(KK,KF*2+3)                                                            
C-----                                                                                    
      IF(MFF(IBEG,ILEN,KOUNTF,LIT,LREC)-LDIR(KK,LORD*2+25)) 165,165,166                   
C-----                                                                                    
C-----NO -- EXCLUDE IMMED ON I , NEXT DIRECTIVE ON E                                      
  165 IF(LD)999,330,144                                                                   
C-----YES -- NEXT DIRECTIVE OR NEXT TEST ON I , EXCL IMMED ON E                           
  166 IF(NVALS.EQ.2.AND.LORD.EQ.0) 167,169                                                
  167 LORD=1                                                                              
      GO TO 181                                                                           
  169 IF(LD) 999,144,230                                                                  
C-----                                                                                    
C-----LESS THAN                                                                           
  153 NVALS=LDIR(KK,35)                                                                   
C-----TEST FOR FUNCTION                                                                   
  181 IF(KSWF) 999,182,183                                                                
C-----SINGLE                                                                              
  182 IF(IGETLB(LIT,LDIR(KK,5),LDIR(KK,4),LRECW)-LDIR(KK,LORD*2+25))                      
     1   186,185,185                                                                      
C-----MULTIPLE FIELDS                                                                     
  183 DO 188 KF=1,KOUNTF                                                                  
      IBEG(KF)=LDIR(KK,KF*2+2)                                                            
  188 ILEN(KF)=LDIR(KK,KF*2+3)                                                            
      IF(MFF(IBEG,ILEN,KOUNTF,LIT,LREC)-LDIR(KK,LORD*2+25)) 186,185,185                   
C-----NO -- EXCLUDE IMMED ON I , NEXT DIRECTIVE ON E                                      
  185 IF(LD)999,330,144                                                                   
C-----YES -- NEXT DIRECTIVE OR NEXT TEST ON I , EXCL IMMED ON E                           
  186 IF(NVALS.EQ.2.AND.LORD.EQ.0) 187,189                                                
  187 LORD=1                                                                              
      GO TO 161                                                                           
  189 IF(LD) 999,144,230                                                                  
C ----                                                                                    
C-----END OF DIRECTIVES LOOP                                                              
  144 CONTINUE                                                                            
C---- RE-FORMAT RECORDS INCLUDED BY SELECT DIRECTIVES                                     
C---- CLEAR OUTPUT RECORD                                                                 
      DO 473 J=1,MRECW                                                                    
  473 LOT(J)=IBLK                                                                         
C---- APPLY ANY *ALL DIRECTIVES, COPIES BEFORE EMITS                                      
      GO TO 479                                                                           
  475 IF(KL1)999,487,476                                                                  
  476 DO 478 NT=1,KL1                                                                     
      NCH=LEM(NT)                                                                         
      ITARG=IEMTG(NT)                                                                     
      ISRC=IEM(NT)                                                                        
      DO 477 I=1,NCH                                                                      
      J1=ISRC+I-1                                                                         
      J2=ITARG+I-1                                                                        
      CALL OUT(LOT,J2,IN(ISTR,J1))                                                        
  477 CONTINUE                                                                            
  478 CONTINUE                                                                            
      GO TO 487                                                                           
  479 IF(KL2)999,475,480                                                                  
  480 DO 482 NT=1,KL2                                                                     
      NCH=LSC(NT)                                                                         
      ITARG=ITG(NT)                                                                       
      ISRC=ISC(NT)                                                                        
      DO 481 I=1,NCH                                                                      
      J1=ISRC+I-1                                                                         
      J2=ITARG+I-1                                                                        
      CALL OUT(LOT,J2,IN(LIT,J1))                                                         
  481 CONTINUE                                                                            
  482 CONTINUE                                                                            
      GO TO 475                                                                           
C---- APPLY ANY *CODE DIRECTIVES, COPIES BEFORE EMITS                                     
  483 IF(MEM)999,491,484                                                                  
  484 DO 486 NT=1,MEM                                                                     
      NCH=KEM(NT)                                                                         
      ITARG=JEMTG(NT)                                                                     
      ISRC=JEM(NT)                                                                        
      DO 485 I=1,NCH                                                                      
      J1=ISRC+I-1                                                                         
      J2=ITARG+I-1                                                                        
      CALL OUT(LOT,J2,IN(KSTR,J1))                                                        
  485 CONTINUE                                                                            
  486 CONTINUE                                                                            
      GO TO 491                                                                           
  487 IF(MCOP)999,483,488                                                                 
  488 DO 490 NT=1,MCOP                                                                    
      NCH=KSC(NT)                                                                         
      ITARG=JTG(NT)                                                                       
      ISRC=JSC(NT)                                                                        
      DO 489 I=1,NCH                                                                      
      J1=ISRC+I-1                                                                         
      J2=ITARG+I-1                                                                        
      CALL OUT(LOT,J2,IN(LIT,J1))                                                         
  489 CONTINUE                                                                            
  490 CONTINUE                                                                            
      GO TO 483                                                                           
  491 LOT(LOST)=LIT(LAST)                                                                 
C-----WRITE NEW RECORD ON 11 -- IE INCLUDE                                                
      CALL WRITUN(11,LOT,LOST)                                                            
      K3=K3+1                                                                             
      N11=N11+1                                                                           
      GO TO 130                                                                           
C-----                                                                                    
C-----                                                                                    
C-----OPTIONAL PRINT ON EXCLUDE SECTION                                                   
  230 IF(LDIR(KK,36)) 999,330,231                                                         
  231 CALL PRNTSI(LP,LIT(LAST),LIT,LRECW,0)                                               
C-----                                                                                    
C-----WRITE EXCLUDED RECORD ON TAPE12                                                     
  330 CALL WRITUN(KUN,LIT,LAST)                                                           
      K12=K12+1                                                                           
      GO TO 130                                                                           
C-----                                                                                    
  509 IF(IOF)133,492,999                                                                  
C---- END OF BLOCK - PRINT RECORD SUMMARY                                                 
C---- INITIALISE VARIABLES FOR NEXT BLOCK                                                 
  492 CALL EOFR(LUN,K2)                                                                   
  493 FORMAT(22H END OF BLOCK ON UNIT ,I3,20H  --  RECORD COUNT =,I8)                     
      REWIND LUN                                                                          
      ENDFILE KUN                                                                         
      CALL EOFW(KUN,K12)                                                                  
      WRITE(LP,493) IUN,K3                                                                
      IODEV=NBLK-(NBLK/2)*2                                                               
      IF(IODEV)999,494,495                                                                
  494 LUN=13                                                                              
      KUN=12                                                                              
      GO TO 496                                                                           
  495 LUN=12                                                                              
      KUN=13                                                                              
  496 REWIND LUN                                                                          
      REWIND KUN                                                                          
      NEM=0                                                                               
      MEM=0                                                                               
      NCOP=0                                                                              
      MCOP=0                                                                              
      K=0                                                                                 
      KALL=0                                                                              
      KCD=0                                                                               
      IPOS=6                                                                              
      IEM(1)=1                                                                            
      JEM(1)=1                                                                            
      GO TO 369                                                                           
C---- CHECK FOR UNEQUAL RECORD LENGTHS                                                    
  133 NBLK1=NBLK-1                                                                        
      IF(NBLK1)999,499,497                                                                
  497 LFIX=LENGTH(1)                                                                      
      DO 498 I=1,NBLK1                                                                    
      IF(LFIX-LENGTH(I+1))510,498,510                                                     
  498 CONTINUE                                                                            
      GO TO 499                                                                           
  510 WRITE(LP,511)                                                                       
  511 FORMAT(62H ***  WARNING  ***   SELECTED RECORDS ARE OF DIFFERENT L                  
     .ENGTHS)                                                                             
C---- END OF FILE ON LUN                                                                  
C---- TERMINATE RUN NORMALLY                                                              
  499 CALL EOFR(LUN,K2)                                                                   
      REWIND LUN                                                                          
      ENDFILE KUN                                                                         
      REWIND KUN                                                                          
      CALL EOFW(KUN,K12)                                                                  
      WRITE(LP,493) IUN,K3                                                                
      ENDFILE 11                                                                          
      REWIND 11                                                                           
      CALL EOFW(11,N11)                                                                   
      STOP                                                                                
      END                                                                                 
