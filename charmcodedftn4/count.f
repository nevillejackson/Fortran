      PROGRAM COUNT(TAPE10=/1000,TAPE11,TAPE12=/1000                                      
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----COUNT (WITH CONTROL CHANGES) SORTED RECORDS FROM CHARM FILE TAPE10                  
C-----SUM AND MEAN SPECIFIED FIELDS                                                       
C-----STANDARD DEVIATION SPECIFIED FIELDS                                                 
C-----LIST ALL RECORDS OR JUST COUNTS & SUMS ON PRINT-FORMATTED TAPE11                    
C-----MAX FIELD WIDTH MACHC CHARS                                                         
C-----TAPE10   INPUT  CHARM-FILE                                                          
C---- TAPE11  OUTPUT  FORMATTED FOR PRINTING                                              
C---- TAPE12  OUTPUT  SUMMARY FILE FORMAT                                                 
C-----                                                                                    
      DIMENSION LIT(100)                                                                  
      DIMENSION LA(8)                                                                     
      DIMENSION LCNAM(50),LCBEG(50),LCLEN(50),LCORD(50),LCSEQ(50),                        
     1 LCSW(50),LCVAL(50,20)                                                              
      DIMENSION LSNAM(50),LSBEG(50),LSLEN(50),LSDEC(50)                                   
      DIMENSION LLNAM(50),LLBEG(50),LLLEN(50)                                             
      DIMENSION LLDEC(50)                                                                 
      DIMENSION LINE(50)                                                                  
      DIMENSION LFIELD(50),NFIELD(50)                                                     
      DIMENSION KOUNT(50),TOT(50,50),KTOT(50,50)                                          
      DIMENSION TOTM(50),KTOTM(50)                                                        
      DIMENSION KNFLD(50),KLFLD(50)                                                       
      DIMENSION JORD(50),JSEQ(50)                                                         
      DIMENSION SSQ(50,50),SSQM(50)                                                       
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LCR,LP,LCP,LTR,LTP,LPL,LSP                  
      COMMON /CONST/ KONST(64)                                                            
      COMMON /SUMFMT/ JFM(8),SFSW,LENSF                                                   
      DATA LSTAR,LHC,LHS,LHL,LHEQ,LHDO,LHA,LHD,LCOM                                       
     1   /1H*,1HC,1HS,1HL,2HEQ,2HDO,1HA,1HD,1H,/                                          
      DATA LHSF /2HSF/                                                                    
      DATA LOVL /7HOVERALL/                                                               
C-----                                                                                    
      CALL DEFINE                                                                         
      WRITE(LP,210)                                                                       
  210 FORMAT(10H0COUNT RUN)                                                               
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      LAST=LRECW+1                                                                        
C-----                                                                                    
C-----LOOP OVER DIRECTIVES                                                                
C-----                                                                                    
      KC=1                                                                                
      KS=1                                                                                
      KL=1                                                                                
      LLOPT=0                                                                             
      SFSW=0                                                                              
    1 CALL READCD(LCR,LA,IFLAG,MACHCD)                                                    
      IF(IFLAG) 3,4,999                                                                   
    4 CALL WRITLN(LP,LA,0,MACHCD)                                                         
C-----CRACK A DIRECTIVE                                                                   
      IPOS=1                                                                              
      IF(NEXITM(LS,LA,IPOS)) 5,6,5                                                        
    5 WRITE(LP,109)                                                                       
  109 FORMAT(23H UNRECOGNISED DIRECTIVE)                                                  
      CALL JOBEND                                                                         
    6 IF(LS-LSTAR) 5,7,5                                                                  
    7 IF(NEXITM(LD,LA,IPOS)) 8,5,5                                                        
    8 IF(LD-LHC) 9,10,9                                                                   
    9 IF(LD-LHS) 11,12,11                                                                 
   11 IF(LD-LHL) 15,14,15                                                                 
   15 IF(LD-LHSF) 5,16,5                                                                  
C-----                                                                                    
C-----CONTROL DIRECTIVE                                                                   
   10 IF(NEXITM(LC,LA,IPOS)) 5,19,5                                                       
   19 IF(LC-LCOM) 5,20,5                                                                  
   20 IF(NEXALF(LN,LA,IPOS)) 21,5,21                                                      
   21 LCNAM(KC)=LN                                                                        
      IF(NEXITM(LC,LA,IPOS)) 5,22,5                                                       
   22 IF(LC-LCOM) 5,23,5                                                                  
   23 IF(NEXITM(LN,LA,IPOS)) 5,5,24                                                       
   24 LCBEG(KC)=LN                                                                        
      IF(NEXITM(LC,LA,IPOS)) 5,25,5                                                       
   25 IF(LC-LCOM) 5,26,5                                                                  
   26 IF(NEXITM(LN,LA,IPOS)) 5,5,27                                                       
   27 LCLEN(KC)=LN                                                                        
      IF(NEXITM(LC,LA,IPOS)) 5,28,5                                                       
   28 IF(LC-LCOM) 5,29,5                                                                  
   29 IF(NEXITM(LN,LA,IPOS)) 30,5,5                                                       
   30 IF(LN-LHA) 40,41,40                                                                 
   41 LCORD(KC)=0                                                                         
      GO TO 42                                                                            
   40 IF(LN-LHD) 5,43,5                                                                   
   43 LCORD(KC)=1                                                                         
   42 CONTINUE                                                                            
      IF(NEXITM(LC,LA,IPOS)) 5,31,5                                                       
   31 IF(LC-LCOM) 5,32,5                                                                  
   32 IF(NEXITM(LN,LA,IPOS)) 33,5,5                                                       
   33 IF(LN-LHA) 45,46,45                                                                 
   46 LCSEQ(KC)=0                                                                         
      GO TO 47                                                                            
   45 IF(LN-LHD) 5,48,5                                                                   
   48 LCSEQ(KC)=1                                                                         
   47 CONTINUE                                                                            
      IF(NEXITM(LC,LA,IPOS)) 5,35,5                                                       
   35 IF(LC-LCOM) 36,37,36                                                                
   36 IF(LC-10HCOLUMN 80 ) 5,38,5                                                         
C-----DEFAULT BRANCH                                                                      
   38 LCSW(KC)=0                                                                          
      GO TO 50                                                                            
C-----                                                                                    
   37 IF(NEXITM(LN,LA,IPOS)) 39,5,5                                                       
   39 IF(LN-LHEQ) 51,52,51                                                                
C-----EQ BRANCH                                                                           
   52 LCSW(KC)=1                                                                          
      KCV=0                                                                               
   53 IF(NEXITM(LC,LA,IPOS)) 5,54,5                                                       
   54 IF(LC-LCOM) 55,56,55                                                                
   55 IF(LC-10HCOLUMN 80 ) 5,50,5                                                         
   56 IF(NEXALF(LV,LA,IPOS)) 57,5,57                                                      
   57 KCV=KCV+1                                                                           
      LCVAL(KC,KCV)=LV                                                                    
      GO TO 53                                                                            
C-----                                                                                    
   51 IF(LN-LHDO) 5,58,5                                                                  
C-----DO BRANCH                                                                           
   58 LCSW(KC)=2                                                                          
      DO 59 KCD=1,3                                                                       
      IF(NEXITM(LC,LA,IPOS)) 5,60,5                                                       
   60 IF(LC-LCOM) 5,61,5                                                                  
   61 IF(NEXITM(LV,LA,IPOS)) 5,5,62                                                       
   62 LCVAL(KC,KCD)=LV                                                                    
   59 CONTINUE                                                                            
      GO TO 50                                                                            
C-----                                                                                    
   50 KC=KC+1                                                                             
      GO TO 1                                                                             
C-----                                                                                    
C-----SUM DIRECTIVE BRANCH                                                                
   12 IF(NEXITM(LC,LA,IPOS)) 5,70,5                                                       
   70 IF(LC-LCOM) 5,71,5                                                                  
   71 IF(NEXALF(LN,LA,IPOS)) 72,5,72                                                      
   72 LSNAM(KS)=LN                                                                        
      IF(NEXITM(LC,LA,IPOS)) 5,73,5                                                       
   73 IF(LC-LCOM) 5,74,5                                                                  
   74 IF(NEXITM(LN,LA,IPOS)) 5,5,75                                                       
   75 LSBEG(KS)=LN                                                                        
      IF(NEXITM(LC,LA,IPOS)) 5,76,5                                                       
   76 IF(LC-LCOM) 5,77,5                                                                  
   77 IF(NEXITM(LN,LA,IPOS)) 5,5,78                                                       
   78 LSLEN(KS)=LN                                                                        
      IF(NEXITM(LC,LA,IPOS)) 5,79,5                                                       
   79 IF(LC-LCOM) 5,80,5                                                                  
   80 IF(NEXITM(LN,LA,IPOS)) 5,5,81                                                       
   81 LSDEC(KS)=LN                                                                        
      KS=KS+1                                                                             
      GO TO 1                                                                             
C-----                                                                                    
C-----LIST DIRECTIVE BRANCH                                                               
   14 IF(NEXITM(LC,LA,IPOS)) 5,82,5                                                       
   82 IF(LC-10HCOLUMN 80 ) 282,283,282                                                    
  283 KL=KL+1                                                                             
      LLOPT=1                                                                             
      GO TO 1                                                                             
  282 IF(LC-LCOM) 5,83,5                                                                  
   83 IF(NEXALF(LN,LA,IPOS)) 85,84,85                                                     
   84 IF(LN-10HCOLUMN 80 ) 5,86,5                                                         
   86 LLOPT=1                                                                             
      KL=KL+1                                                                             
      GO TO 1                                                                             
   85 LLNAM(KL)=LN                                                                        
      LLOPT=2                                                                             
      IF(NEXITM(LC,LA,IPOS)) 5,87,5                                                       
   87 IF(LC-LCOM) 5,88,5                                                                  
   88 IF(NEXITM(LN,LA,IPOS)) 5,5,89                                                       
   89 LLBEG(KL)=LN                                                                        
      IF(NEXITM(LC,LA,IPOS)) 5,90,5                                                       
   90 IF(LC-LCOM) 5,91,5                                                                  
   91 IF(NEXITM(LN,LA,IPOS)) 5,5,92                                                       
   92 LLLEN(KL)=LN                                                                        
      IF(NEXITM(LC,LA,IPOS)) 5,290,5                                                      
  290 IF(LC-10HCOLUMN 80 ) 292,291,292                                                    
  291 LLDEC(KL)=-100                                                                      
      GO TO 293                                                                           
  292 IF(LC-LCOM) 5,294,5                                                                 
  294 IF(NEXITM(LN,LA,IPOS)) 5,5,295                                                      
  295 LLDEC(KL)=LN                                                                        
  293 CONTINUE                                                                            
      KL=KL+1                                                                             
      GO TO 1                                                                             
C-----                                                                                    
C-----SUMMARY FILE DIRECTIVE BRANCH                                                       
   16 IF(NEXITM(LC,LA,IPOS)) 5,350,5                                                      
  350 IF(LC-LCOM) 5,351,5                                                                 
  351 IF(NEXITM(LENSF,LA,IPOS)) 5,5,352                                                   
  352 IF(NEXITM(LC,LA,IPOS)) 5,353,5                                                      
  353 IF(LC-LCOM) 5,354,5                                                                 
  354 CALL COPYC(LA,IPOS,JFM,1,MACHCC-10)                                                 
      SFSW=1                                                                              
      GO TO 1                                                                             
C-----                                                                                    
C-----END OF DIRECTIVE CRACKING                                                           
C-----                                                                                    
C-----PROSESS RECORDS                                                                     
C-----INIT                                                                                
    3 KC=KC-1                                                                             
      KS=KS-1                                                                             
      KL=KL-1                                                                             
      K10=0                                                                               
      K11=0                                                                               
      K12=0                                                                               
      KOUNTM=0                                                                            
      DO 221 MS=1,KS                                                                      
      TOTM(MS)=0.0                                                                        
      SSQM(MS)=0.                                                                         
  221 KTOTM(MS)=0                                                                         
      DO 200 MC=1,KC                                                                      
      KOUNT(MC)=0                                                                         
      DO 201 MS=1,KS                                                                      
      SSQ(MC,MS)=0.                                                                       
      TOT(MC,MS)=0.                                                                       
  201 KTOT(MC,MS)=0                                                                       
  200 CONTINUE                                                                            
      CALL HITCH(LCNAM,LSNAM,KOUNT,TOT,KTOT,KC,KS,ICON,ICHA,LFIELD,K11,                   
     1 LSDEC,KL,SSQ,K12)                                                                  
C-----HEADS FOR LIST PROSESSING                                                           
      IF(KS) 999,302,303                                                                  
  303 CONTINUE                                                                            
C-----RECORD LENGTH ON UNIT 12                                                            
      IF(SFSW) 360,360,361                                                                
  361 CALL LNWRIT(LENSF,LENSFW,12)                                                        
  360 CONTINUE                                                                            
C-----NEW PAGE                                                                            
      WRITE(11,184)                                                                       
  184 FORMAT(1H1)                                                                         
  302 CONTINUE                                                                            
      IF(KL) 999,180,181                                                                  
  181 IF(LLOPT-1) 180,182,183                                                             
C-----FULL LIST NO HEAD                                                                   
  182 WRITE(11,185)                                                                       
  185 FORMAT(1H ,4X,6HSEQ NO,5X,6HRECORD)                                                 
      K11=K11+2                                                                           
      GO TO 180                                                                           
C-----LIST SPEC ITEMS                                                                     
  183 CALL PRNTSI(11,6HSEQ NO,LLNAM,KL,1)                                                 
      K11=K11+2                                                                           
  180 CONTINUE                                                                            
C-----                                                                                    
      IF(KC+KS+KL) 999,99,100                                                             
  999 CALL LOGIC(5HCOUNT)                                                                 
      STOP                                                                                
C-----                                                                                    
   99 WRITE(LP,98)                                                                        
   98 FORMAT(32H NO DIRECTIVES -- RUN TERMINATED)                                         
      CALL JOBEND                                                                         
C-----                                                                                    
C-----LOOP OVER RECORDS                                                                   
  100 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 103,104,999                                                               
  104 K10=K10+1                                                                           
C-----LOOP OVER DIRECTIVES FOR THIS RECORD                                                
      IF(KC) 999,130,112                                                                  
C-----ASSEMBLE NEW ARRAY OF CONTROL FIELDS                                                
C-----     ONE WORD PER CONTROL FIELD                                                     
  112 DO 113 MC=1,KC                                                                      
C-----ORDFLD EXTRACTS FIELDS & TRANSPOSES TO DESC & ASCII6 IF REQD                        
C-----KNFLD() IS TRANSFORMED                                                              
C-----NFIELD() IS ORIGINAL                                                                
  113 CALL ORDFLD(LCBEG,LCLEN,LCORD,LCSEQ,MC,KC,LIT,LREC,KNFLD(MC),                       
     1   NFIELD(MC))                                                                      
      IF(K10-1) 999,165,166                                                               
  165 DO 167 MC=1,KC                                                                      
      KLFLD(MC)=KNFLD(MC)                                                                 
  167 LFIELD(MC)=NFIELD(MC)                                                               
C-----MATCH NEW FIELD ARRAY AGAINST OLD FIELD ARRAY                                       
  166 IF(KEYMAT(KNFLD,KLFLD,KC*MACHC))120,130,140                                         
C-----NEW .LT. LAST -- ERROR IN SORT                                                      
  120 WRITE(LP,121)                                                                       
  121 FORMAT(104H SORTED ORDER OF RECORDS ON TAPE10 NOT CONSISTENT WITH                   
     1CONTROL CHANGE DIRECTIVES -- LAST RECORD READ WAS)                                  
      CALL PRNTSI(11,LIT(LAST),LIT,LRECW,0)                                               
      CALL JOBEND                                                                         
C-----NEW .EQ. LAST                                                                       
C-----CONTINUE COUNTING , SUMMING , LISTING                                               
C-----WITHIN LOWEST LEVEL                                                                 
  130 ICON=KC                                                                             
C-----LIST PROCESSING                                                                     
      IF(KL) 999,170,171                                                                  
  171 IF(LLOPT-1) 170,172,173                                                             
C-----LIST FULL RECORD                                                                    
  172 CALL PRNTSI(11,LIT(LAST),LIT,LRECW,0)                                               
      K11=K11+1                                                                           
      GO TO 170                                                                           
C-----LIST SPEC ITEMS                                                                     
  173 DO 174 ML=1,KL                                                                      
C-----INSERT DECIMALS AND STRIP ZEROS                                                     
      JORD(ML)=0                                                                          
      JSEQ(ML)=1                                                                          
      CALL ORDFLD(LLBEG,LLLEN,JORD,JSEQ,ML,KL,LIT,LREC,KDUM,LINE(ML))                     
  174 CALL FHOL(LINE(ML),LLLEN(ML),LLDEC(ML))                                             
      CALL PRNTSI(11,LIT(LAST),LINE,KL,0)                                                 
C-----                                                                                    
  170 CONTINUE                                                                            
C-----                                                                                    
C-----                                                                                    
C-----COUNTING AND SUMMING                                                                
      KOUNTM=KOUNTM+1                                                                     
      DO 131 MC=1,ICON                                                                    
      KOUNT(MC)=KOUNT(MC)+1                                                               
      IF(KS) 999,131,133                                                                  
  133 DO 132 MS=1,KS                                                                      
      KIT=IGETRB(LIT,LSLEN(MS),LSBEG(MS),LRECW)                                           
      IF(ITYPE(IN(KIT,MACHC))-2) 132,134,132                                              
  134 KIT=INUM(KIT)                                                                       
      FIT=FXI(KIT,LSDEC(MS))                                                              
      SSQ(MC,MS)=SSQ(MC,MS)+FIT*FIT                                                       
      TOT(MC,MS)=TOT(MC,MS)+FIT                                                           
      KTOT(MC,MS)=KTOT(MC,MS)+1                                                           
      IF(MC-1) 999,220,132                                                                
  220 TOTM(MS)=TOTM(MS)+FIT                                                               
      KTOTM(MS)=KTOTM(MS)+1                                                               
      SSQM(MS)=SSQM(MS)+FIT*FIT                                                           
  132 CONTINUE                                                                            
  131 CONTINUE                                                                            
      GO TO 100                                                                           
C-----                                                                                    
C-----NEW .GT. LAST -- CONTROL CHANGE                                                     
C-----   WHICH FIELDS                                                                     
C-----                                                                                    
  140 DO 141 MC=1,KC                                                                      
C-----KNFLD() IS CURRENT TRANSFORMED FIELD                                                
C-----KLFLD() IS OLD TRANSFORMED FIELD                                                    
      IFIELD=KNFLD(MC)                                                                    
      JFIELD=KLFLD(MC)                                                                    
      IF(KEYMAT(IFIELD,JFIELD,MACHC)) 120,141,150                                         
  141 CONTINUE                                                                            
      GO TO 999                                                                           
C-----HIGH ORDER FIELD CONTROL CHANGE FOUND                                               
  150 ICON=MC-1                                                                           
C-----PRINT AND CLEAR ALL FIELDS FROM ICON+1 TO KC INCL                                   
      ICHA=ICON+1                                                                         
      CALL WITCH(LCNAM,LSNAM,KOUNT,TOT,KTOT,KC,KS,ICON,ICHA,LFIELD,K11,                   
     1 LSDEC,KL,SSQ,K12)                                                                  
C-----RE-INIT LFIELD                                                                      
      DO 160 MC=ICHA,KC                                                                   
      KLFLD(MC)=KNFLD(MC)                                                                 
  160 LFIELD(MC)=NFIELD(MC)                                                               
C-----REPEAT HEADING FOR LIST PROCESSING                                                  
      IF(KS) 999,301,300                                                                  
  300 CONTINUE                                                                            
      WRITE(11,194)                                                                       
  194 FORMAT(1H0)                                                                         
  301 CONTINUE                                                                            
      IF(KL) 999,190,191                                                                  
  191 IF(LLOPT-1) 190,192,193                                                             
  192 WRITE(11,185)                                                                       
      K11=K11+2                                                                           
      IF(LLOPT-2) 190,193,999                                                             
  193 CALL PRNTSI(11,6HSEQ NO,LLNAM,KL,1)                                                 
      K11=K11+2                                                                           
  190 CONTINUE                                                                            
C-----BACKSPACE OVER CURRENT RECORD AND REREAD                                            
  168 BACKSPACE 10                                                                        
      K10=K10-1                                                                           
      GO TO 100                                                                           
C-----                                                                                    
C-----EOF ON 10                                                                           
C-----PRINT LAST TOTALS                                                                   
  103 CONTINUE                                                                            
C-----LAST CC TOTALS                                                                      
      CALL WITCH(LCNAM,LSNAM,KOUNT,TOT,KTOT,KC,KS,   0,   1,LFIELD,K11,                   
     1 LSDEC,KL,SSQ,K12)                                                                  
C-----OVERALL TOTALS                                                                      
      KC=1                                                                                
      LCNAM(1)=LOVL                                                                       
      LFIELD(1)=LOVL                                                                      
      KOUNT(1)=KOUNTM                                                                     
      DO 230 MS=1,KS                                                                      
      TOT(1,MS)=TOTM(MS)                                                                  
      SSQ(1,MS)=SSQM(MS)                                                                  
  230 KTOT(1,MS)=KTOTM(MS)                                                                
      CALL WITCH(LCNAM,LSNAM,KOUNT,TOT,KTOT,KC,KS,  -1,   1,LFIELD,K11,                   
     1 LSDEC,KL,SSQ,K12)                                                                  
      CALL EOFR(10,K10)                                                                   
      REWIND 10                                                                           
      CALL EOFW(11,K11)                                                                   
      ENDFILE 11                                                                          
      REWIND 11                                                                           
      ENDFILE 12                                                                          
      CALL EOFW(12,K12)                                                                   
      REWIND 12                                                                           
      STOP                                                                                
      END                                                                                 
