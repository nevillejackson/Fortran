      PROGRAM TAB(TAPE10=/1000,TAPE11                                                     
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----FORM MULTI-COLUMN PAGE SIZED TABLES OF SORTED RECORDS FROM                          
C-----CHARM FILE TAPE10                                                                   
C-----CONTROL FIELD CHANGES TRIGGER SPACING,COLUMN HEADINGS ,PAGE HEADINGS                
C-----TABLES WRITTEN PAGEWISE ON PRINT-FORMATTED TAPE11                                   
C-----                                                                                    
      DIMENSION LSBEG(50),LSLEN(50),LCNAM(50),LCBEG(50),LCLEN(50),                        
     1 LPNAM(50),LPBEG(50),LPLEN(50),LLNAM(50),LLBEG(50),LLLEN(50),                       
     2 LTBEG(50),LTLEN(50),LTDEC(50),LTITLE(10),LTAB(20),LROW(20),                        
     3 LIT(100),LOT(100),                                                                 
     + LA(8),ITEM(20),IORD(50),ISEQ(50),                                                  
     4 LPAG(12),LPNL(50), LCOLN(12),LPAGV(12),                                            
     5 KEYP(50),KEYC(50),KEYS(50),KEYPO(50),KEYCO(50),KEYSO(50)                           
      DIMENSION LRULE(40),LLINE(20)                                                       
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      DATA LSTAR,LCOM,LCS,LCT,LCP,LHL,LRS,LTS,LPS                                         
     1 /1H*,1H,,2HCS,2HCT,2HCP,1HL,6HROWSET,6HTABSET,7HPAGESET/                           
      DATA LDSP /6HDSPACE/                                                                
      DATA LSP /5HSPACE/                                                                  
      DATA LRUL,LLIN /4HRULE,4HLINE/                                                      
C-----                                                                                    
      CALL DEFINE                                                                         
      WRITE(LP,210)                                                                       
  210 FORMAT(8H0TAB RUN)                                                                  
      REWIND 10                                                                           
      REWIND 11                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      LAST=LRECW+1                                                                        
C-----                                                                                    
C-----LOOP OVER DIRECTIVES                                                                
C-----                                                                                    
      KS=0                                                                                
      KT=0                                                                                
      KP=0                                                                                
      KL=0                                                                                
      KPS=0                                                                               
      KRS=0                                                                               
      KTS=0                                                                               
      IDSW=0                                                                              
      KRUL=0                                                                              
      KLIN=0                                                                              
      ISP=1                                                                               
C-----READ A DIRECTIVE                                                                    
    1 CALL READCD(LC,LA,IFLAG,MACHCD)                                                     
      IF(IFLAG) 3,4,999                                                                   
    4 CALL WRITLN(LP,LA,0,MACHCD)                                                         
C-----CRACK A DIRECTIVE                                                                   
      IPOS=1                                                                              
      IF(NEXITM(L,LA,IPOS)) 5,6,5                                                         
    5 WRITE(LP,199)                                                                       
  199 FORMAT(41H UNRECOGNIZED DIRECTIVE -- JOB TERMINATED)                                
      CALL JOBEND                                                                         
    6 IF(L-LSTAR) 5,7,5                                                                   
    7 IF(NEXITM(L,LA,IPOS)) 8,5,5                                                         
C-----DIRECTIVE TYPES                                                                     
    8 IF(L-LCS) 9,20,9                                                                    
    9 IF(L-LCT) 10,21,10                                                                  
   10 IF(L-LCP) 11,22,11                                                                  
   11 IF(L-LHL) 12,23,12                                                                  
   12 IF(L-LRS) 13,24,13                                                                  
   13 IF(L-LTS) 14,25,14                                                                  
   14 IF(L-LPS) 15,26,15                                                                  
   15 IF(L-LDSP) 16,27,16                                                                 
   16 IF(L-LRUL) 17,28,17                                                                 
   17 IF(L-LLIN) 18,29,18                                                                 
   18 IF(L-LSP) 5,1030,5                                                                  
C-----                                                                                    
C-----RULE DIRECTIVE                                                                      
   28 KRUL=KRUL+1                                                                         
      IF(KRUL-1) 999,1006,1000                                                            
 1006 KRULS=0                                                                             
 1000 IF(NEXITM(L,LA,IPOS)) 5,1001,5                                                      
 1001 IF(L-LCOM) 1002,1003,1002                                                           
 1002 IF(L-10HCOLUMN 80 ) 5,1004,5                                                        
 1003 IF(NEXITM(L,LA,IPOS)) 5,5,1005                                                      
 1005 KRULS=KRULS+1                                                                       
      LRULE(KRULS)=L                                                                      
      IF(KRULS-40) 1000,1,999                                                             
 1004 IF(KRULS) 999,5,1                                                                   
C-----                                                                                    
C-----LINE DIRECTIVE                                                                      
   29 KLIN=KLIN+1                                                                         
      IF(KLIN-1) 999,1016,1010                                                            
 1016 KLINS=0                                                                             
 1010 IF(NEXITM(L,LA,IPOS)) 5,1011,5                                                      
 1011 IF(L-LCOM) 1012,1013,1012                                                           
 1012 IF(L-10HCOLUMN 80 ) 5,1014,5                                                        
 1013 IF(NEXITM(L,LA,IPOS)) 5,5,1015                                                      
 1015 KLINS=KLINS+1                                                                       
      LLINE(KLINS)=L                                                                      
      IF(KLINS-20) 1010,1,999                                                             
 1014 IF(KLINS) 999,5,1                                                                   
C-----                                                                                    
C-----SPACE DIRECTIVE                                                                     
 1030 IF(NEXITM(L,LA,IPOS)) 5,1031,5                                                      
 1031 IF(L-LCOM) 5,1032,5                                                                 
 1032 IF(NEXITM(L,LA,IPOS)) 5,5,1033                                                      
 1033 ISP=L                                                                               
      GO TO 1                                                                             
C-----                                                                                    
C-----DOUBLE SPACE DIRECTIVE                                                              
   27 IDSW=1                                                                              
      GO TO 1                                                                             
C-----                                                                                    
C-----PAGESET DIRECTIVE                                                                   
   26 KPS=KPS+1                                                                           
      IF(KPS-1) 999,30,31                                                                 
C-----                                                                                    
   31 WRITE(LPERRS,32)                                                                    
   32 FORMAT(60H ONLY ONE DIRECTIVE OF THIS TYPE PERMITTED -- JOB TERMIN                  
     1ATED)                                                                               
      CALL JOBEND                                                                         
C-----                                                                                    
  999 CALL LOGIC(3HTAB)                                                                   
C-----                                                                                    
   30 IF(NEXITM(L,LA,IPOS)) 5,33,5                                                        
   33 IF(L-LCOM) 5,34,5                                                                   
   34 IF(NEXITM(L,LA,IPOS)) 5,5,35                                                        
   35 NPAGES=L                                                                            
      IF(NEXITM(L,LA,IPOS)) 5,36,5                                                        
   36 IF(L-LCOM) 5,37,5                                                                   
   37 CALL DOLSTR(LA,IPOS,LTITLE,1,LENT)                                                  
      IF(LENT) 999,5,38                                                                   
   38 GO TO 1                                                                             
C-----                                                                                    
C-----TABSET DIRECTIVE                                                                    
   25 KTS=KTS+1                                                                           
      IF(KTS-1) 999,40,31                                                                 
   40 IF(NEXITM(L,LA,IPOS)) 5,41,5                                                        
   41 IF(L-LCOM) 5,42,5                                                                   
   42 IF(NEXITM(L,LA,IPOS)) 5,5,43                                                        
   43 NCOLS=L                                                                             
      KTABS=0                                                                             
   44 IF(NEXITM(L,LA,IPOS)) 5,45,5                                                        
   45 IF(L-LCOM) 46,47,46                                                                 
   46 IF(L-10HCOLUMN 80 ) 5,48,5                                                          
   47 IF(NEXITM(L,LA,IPOS)) 5,5,49                                                        
   49 KTABS=KTABS+1                                                                       
      LTAB(KTABS)=L                                                                       
      IF(KTABS-20) 44,1,999                                                               
   48 IF(KTABS) 999,5,1                                                                   
C-----                                                                                    
C-----ROWSET DIRECTIVE                                                                    
   24 KRS=KRS+1                                                                           
      IF(KRS-1) 999,50,31                                                                 
   50 IF(NEXITM(L,LA,IPOS)) 5,51,5                                                        
   51 IF(L-LCOM) 5,52,5                                                                   
   52 IF(NEXITM(L,LA,IPOS)) 5,5,53                                                        
   53 NROWS=L                                                                             
      KROWS=0                                                                             
   54 IF(NEXITM(L,LA,IPOS)) 5,55,5                                                        
   55 IF(L-LCOM) 56,57,56                                                                 
   56 IF(L-10HCOLUMN 80 ) 5,58,5                                                          
   57 IF(NEXITM(L,LA,IPOS)) 5,5,59                                                        
   59 KROWS=KROWS+1                                                                       
      LROW(KROWS)=L                                                                       
      IF(KROWS-20) 54,1,999                                                               
   58 IF(KROWS) 999,5,1                                                                   
C-----                                                                                    
C-----LIST DIRECTIVE                                                                      
   23 KL=KL+1                                                                             
      IF(NEXITM(L,LA,IPOS)) 5,61,5                                                        
   61 IF(L-LCOM) 5,62,5                                                                   
   62 IF(NEXALF(L,LA,IPOS)) 63,5,63                                                       
   63 LLNAM(KL)=L                                                                         
      IF(NEXITM(L,LA,IPOS)) 5,64,5                                                        
   64 IF(L-LCOM) 5,65,5                                                                   
   65 IF(NEXITM(L,LA,IPOS)) 5,5,66                                                        
   66 LLBEG(KL)=L                                                                         
      IF(NEXITM(L,LA,IPOS)) 5,67,5                                                        
   67 IF(L-LCOM) 5,68,5                                                                   
   68 IF(NEXITM(L,LA,IPOS)) 5,5,69                                                        
   69 LLLEN(KL)=L                                                                         
      IF(NEXITM(L,LA,IPOS)) 5,70,5                                                        
   70 IF(L-LCOM) 5,71,5                                                                   
   71 IF(NEXITM(L,LA,IPOS)) 5,5,72                                                        
   72 LTBEG(KL)=L                                                                         
      IF(NEXITM(L,LA,IPOS)) 5,73,5                                                        
   73 IF(L-LCOM) 5,74,5                                                                   
   74 IF(NEXITM(L,LA,IPOS)) 5,5,75                                                        
   75 LTLEN(KL)=L                                                                         
      IF(NEXITM(L,LA,IPOS)) 5,76,5                                                        
   76 IF(L-LCOM) 77,78,77                                                                 
   77 IF(L-10HCOLUMN 80 ) 5,80,5                                                          
   80 LTDEC(KL)=-100                                                                      
      GO TO 1                                                                             
   78 IF(NEXITM(L,LA,IPOS)) 5,5,79                                                        
   79 LTDEC(KL)=L                                                                         
      GO TO 1                                                                             
C-----                                                                                    
C-----CONTROL PAGINATION DIRECTIVE                                                        
   22 KP=KP+1                                                                             
      LPNL(KP)=IPOS                                                                       
      IF(NEXITM(L,LA,IPOS)) 5,81,5                                                        
   81 IF(L-LCOM) 5,82,5                                                                   
   82 IF(NEXALF(L,LA,IPOS)) 83,5,83                                                       
   83 LPNAM(KP)=L                                                                         
      LPNL(KP)=IPOS-LPNL(KP)-1                                                            
      IF(NEXITM(L,LA,IPOS)) 5,84,5                                                        
   84 IF(L-LCOM) 5,85,5                                                                   
   85 IF(NEXITM(L,LA,IPOS)) 5,5,86                                                        
   86 LPBEG(KP)=L                                                                         
      IF(NEXITM(L,LA,IPOS)) 5,87,5                                                        
   87 IF(L-LCOM) 5,88,5                                                                   
   88 IF(NEXITM(L,LA,IPOS)) 5,5,89                                                        
   89 LPLEN(KP)=L                                                                         
      GO TO 1                                                                             
C-----                                                                                    
C-----CONTROL TABBING DIRECTIVE                                                           
   21 KT=KT+1                                                                             
      IF(NEXITM(L,LA,IPOS)) 5,91,5                                                        
   91 IF(L-LCOM) 5,92,5                                                                   
   92 IF(NEXALF(L,LA,IPOS)) 93,5,93                                                       
   93 LCNAM(KT)=L                                                                         
      IF(NEXITM(L,LA,IPOS)) 5,94,5                                                        
   94 IF(L-LCOM) 5,95,5                                                                   
   95 IF(NEXITM(L,LA,IPOS)) 5,5,96                                                        
   96 LCBEG(KT)=L                                                                         
      IF(NEXITM(L,LA,IPOS)) 5,97,5                                                        
   97 IF(L-LCOM) 5,98,5                                                                   
   98 IF(NEXITM(L,LA,IPOS)) 5,5,99                                                        
   99 LCLEN(KT)=L                                                                         
      GO TO 1                                                                             
C-----                                                                                    
C-----CONTROL SKIPPING DIRECTIVE                                                          
   20 KS=KS+1                                                                             
      IF(NEXITM(L,LA,IPOS)) 5,101,5                                                       
  101 IF(L-LCOM) 5,102,5                                                                  
  102 IF(NEXITM(L,LA,IPOS)) 5,5,106                                                       
  106 LSBEG(KS)=L                                                                         
      IF(NEXITM(L,LA,IPOS)) 5,107,5                                                       
  107 IF(L-LCOM) 5,108,5                                                                  
  108 IF(NEXITM(L,LA,IPOS)) 5,5,109                                                       
  109 LSLEN(KS)=L                                                                         
      GO TO 1                                                                             
C-----                                                                                    
C-----END DIRECTIVE CRACKING                                                              
C-----                                                                                    
C-----PROCESS RECORDS                                                                     
C-----                                                                                    
C-----INITIALIZE COUNTERS                                                                 
    3 CONTINUE                                                                            
      K10=0                                                                               
      K11=0                                                                               
      INIT=0                                                                              
      IEND=0                                                                              
      IHED=0                                                                              
C-----ERRORS                                                                              
      IF(KL) 999,110,111                                                                  
  110 WRITE(LP,112)                                                                       
  112 FORMAT(37H NO LIST DIRECTIVES -- RUN TERMINATED)                                    
      CALL JOBEND                                                                         
  111 IF(KS+KT+KP) 999,113,113                                                            
  113 IF(KPS-1) 114,115,999                                                               
  114 WRITE(LP,116)                                                                       
  116 FORMAT(39H NO PAGESET DIRECTIVE -- RUN TERMINATED)                                  
      CALL JOBEND                                                                         
  115 IF(KTS-1) 117,118,999                                                               
  117 WRITE(LP,119)                                                                       
  119 FORMAT(38H NO TABSET DIRECTIVE -- RUN TERMINATED)                                   
      CALL JOBEND                                                                         
  118 IF(KRS-1) 120,121,999                                                               
  120 WRITE(LP,122)                                                                       
  122 FORMAT(38H NO ROWSET DIRECTIVE -- RUN TERMINATED)                                   
      CALL JOBEND                                                                         
  121 CONTINUE                                                                            
C-----INITIALIZE PAGE                                                                     
      CALL PAGE(NROWS,NCOLS,0,0)                                                          
C-----SET PAGE HEAD                                                                       
      LENTV=0                                                                             
      IHCOL=(NCOLS-LENT)/2                                                                
      IF(KP) 999,136,137                                                                  
  137 DO 130 JP=1,KP                                                                      
      LENTV=LENTV+3+LPNL(KP)+LPLEN(KP)+1                                                  
  130 IHCOL=IHCOL-3-LPNL(KP)-LPLEN(KP)-1                                                  
  136 IF(IHCOL-1) 131,131,132                                                             
  131 IHCOL=2                                                                             
  132 IF(LENT+LENTV-NCOLS+15) 133,133,134                                                 
  134 WRITE(LP,135)                                                                       
  135 FORMAT(51H PAGE HEAD TOO LONG FOR PAGE WIDTH -- TWO ROWS USED)                      
      IHED=1                                                                              
      IHCOL=(NCOLS-LENT)/2                                                                
      IF(IHCOL-1) 140,140,141                                                             
  140 IHCOL=2                                                                             
  141 IF(LENT-NCOLS+15) 143,143,144                                                       
  144 WRITE(LP,135)                                                                       
      IHED=2                                                                              
  143 CONTINUE                                                                            
  133 CONTINUE                                                                            
      IF(IHED-1) 150,150,151                                                              
  151 CALL PAGE(LTITLE,30,1,IHCOL)                                                        
      CALL PAGE(LTITLE(4),LENT-30,2,IHCOL)                                                
      CALL PAGE(L,L,-1,0)                                                                 
      CALL PAGE(L,L,-2,0)                                                                 
      GO TO 153                                                                           
  150 CALL PAGE(LTITLE,LENT,1,IHCOL)                                                      
      CALL PAGE(L,L,-1,0)                                                                 
C-----SET LINE AND RULE CHARACTERS                                                        
      CALL MCHL(46,MLIN,1)                                                                
      CALL MCH(55,MLIN,1)                                                                 
      CALL MCHL(46,MRUL,1)                                                                
      CALL MCH(39,MRUL,1)                                                                 
      CALL MCHL(46,MBOTH,1)                                                               
      CALL MCH(38,MBOTH,1)                                                                
C-----GENERATE RULE ROWS AND MARK AS NOT ERASED                                           
      IF(KRULS) 999,1043,1044                                                             
 1044 DO 1040 KRU=1,KRULS                                                                 
      CALL PAGE(MRUL,1,-LRULE(KRU),0)                                                     
      DO 1040 KLI=2,NCOLS                                                                 
      IF(IEQ(LLINE,KLINS,KLI)) 999,1042,1041                                              
 1042 CALL PAGE(MRUL,1,LRULE(KRU),KLI)                                                    
      GO TO 1040                                                                          
 1041 CALL PAGE(MBOTH,1,LRULE(KRU),KLI)                                                   
 1040 CONTINUE                                                                            
 1043 CONTINUE                                                                            
C-----PAGE COUNT                                                                          
  153 CONTINUE                                                                            
      IPCNT=0                                                                             
C-----INITIALIZE OLD STRING                                                               
      READ(10)(LIT(I),I=1,LAST)                                                           
      DO 160 I=1,LAST                                                                     
  160 LOT(I)=LIT(I)                                                                       
      BACKSPACE10                                                                         
C-----LISTED COL NAMES PACKED AT CORRECT SPACING                                          
      CALL SETBLK(LCOLN,0,LTBEG(KL)+LTLEN(KL))                                            
      DO 300 JL=1,KL                                                                      
      CALL COPYC(LLNAM(JL),1,LCOLN,LTBEG(JL),MACHC)                                       
  300 CONTINUE                                                                            
C-----SET UP OLD CC KEYS                                                                  
      DO 301 J=1,50                                                                       
      IORD(J)=0                                                                           
  301 ISEQ(J)=1                                                                           
      CALL ORDINL(LPBEG,LPLEN,IORD,ISEQ,KP,LOT,LREC,KEYP)                                 
      CALL ORDINL(LCBEG,LCLEN,IORD,ISEQ,KT,LOT,LREC,KEYC)                                 
      CALL ORDINL(LSBEG,LSLEN,IORD,ISEQ,KS,LOT,LREC,KEYS)                                 
C-----                                                                                    
C-----PAGEWISE TABS -- CP OR FORCED                                                       
  400 IPCNT=IPCNT+1                                                                       
      IF(IPCNT-NPAGES) 402,402,403                                                        
  403 WRITE(LP,404)                                                                       
  404 FORMAT(38H PAGE LIMIT EXCEEDED -- RUN TERMINATED)                                   
      CALL JOBEND                                                                         
  402 CONTINUE                                                                            
      ICT=0                                                                               
C-----DOUBLE SPACE CONTROL                                                                
      IF(IDSW) 430,430,431                                                                
  431 CALL PAGE(L,L,0,-1)                                                                 
  430 CONTINUE                                                                            
C-----PAGE HEAD                                                                           
      ENCODE(10,401,LPAG) IPCNT                                                           
  401 FORMAT(4HPAGE,I3,3H   )                                                             
      CALL PAGE(LPAG,10,1,NCOLS-11)                                                       
C-----VARIABLE PAGE HEAD                                                                  
      IF(IHED-1) 420,421,422                                                              
  420 JCOL=IHCOL+LENT+1                                                                   
      IR=1                                                                                
      GO TO 423                                                                           
  421 JCOL=IHCOL+1                                                                        
      IR=2                                                                                
      GO TO 423                                                                           
  422 IR=3                                                                                
      JCOL=IHCOL+1                                                                        
  423 CONTINUE                                                                            
      JPOS1=1                                                                             
      CALL SETBLK(LPAGV,0,LENTV)                                                          
      DO 410 JP=1,KP                                                                      
      JPOS2=JPOS1+2                                                                       
      JPOS3=JPOS2+LPNL(JP)+1                                                              
      CALL COPYC(LPNAM(JP),1,LPAGV,JPOS2,LPNL(JP))                                        
      CALL COPYC(KONST(39),MACHC,LPAGV,JPOS1,1)                                           
      CALL COPYC(LIT,LPBEG(JP),LPAGV,JPOS3,LPLEN(JP))                                     
      JPOS1=JPOS3+LPLEN(JP)+1                                                             
  410 CONTINUE                                                                            
      CALL PAGE(LPAGV,LENTV,IR,JCOL)                                                      
C-----                                                                                    
C-----COLUMNWISE TABS -- CT OR FORCED                                                     
  500 ICT=ICT+1                                                                           
      IF(KT) 999,1200,1201                                                                
 1200 IR=IHED+4                                                                           
      GO TO 1202                                                                          
 1201 CONTINUE                                                                            
      IR=3+IHED                                                                           
      IF(IEQ(LRULE,KRULS,IR)) 999,1100,1101                                               
 1101 IR=IR+ISP                                                                           
 1100 CONTINUE                                                                            
C-----COLUMN HEAD                                                                         
C-----CONTROL NAMES VARIABLE VALUES                                                       
  502 DO 501 JT=1,KT                                                                      
      CALL PAGE(JRIGHT(LCNAM(JT)),MACHC,IR,LTAB(ICT))                                     
      CALL COPYC(LIT,LCBEG(JT),IVAL,1,LCLEN(JT))                                          
      CALL PAGE(IVAL,LCLEN(JT),IR,LTAB(ICT)+MACHC+1)                                      
      IR=IR+ISP                                                                           
      IF(IEQ(LRULE,KRULS,IR)) 999,1102,1103                                               
 1103 IR=IR+ISP                                                                           
 1102 CONTINUE                                                                            
  501 CONTINUE                                                                            
C-----LISTED NAMES - FIXED                                                                
 1202 CONTINUE                                                                            
      CALL PAGE(LCOLN,LTBEG(KL)+LTLEN(KL),IR,LTAB(ICT))                                   
      IR=IR+ISP                                                                           
      IF(IEQ(LRULE,KRULS,IR)) 999,1110,1111                                               
 1111 IR=IR+ISP                                                                           
 1110 CONTINUE                                                                            
      IF(INIT) 999,503,900                                                                
  503 INIT=1                                                                              
      GO TO 800                                                                           
C------                                                                                   
C-----ROWWISE TABS -- CT ONLY NOT FORCED                                                  
C-----SAME AS COLWISE TABS ONLY ICT AND IR REDIFINED                                      
C-----SEARCH FOR NEXT ROWTAB                                                              
  600 CONTINUE                                                                            
      L=IR+ISP                                                                            
      DO 601 JR=L,NROWS                                                                   
      DO 601 KR=1,KROWS                                                                   
      IF(LROW(KR)-JR)601,602,601                                                          
  601 CONTINUE                                                                            
C-----NO MORE ROWTABS EXIST -- NEXT COL                                                   
      IF(ICT-KTABS) 500,950,999                                                           
C-----ROWTAB FOUND                                                                        
  602 IR=JR                                                                               
      IF(IEQ(LRULE,KRULS,IR)) 999,1104,1105                                               
 1105 IR=IR+ISP                                                                           
 1104 CONTINUE                                                                            
      GO TO 502                                                                           
C-----                                                                                    
C-----SPACING CONTROL CHANGE                                                              
  700 IR=IR+ISP                                                                           
      IF(IEQ(LRULE,KRULS,IR)) 999,1106,1107                                               
 1107 IR=IR+ISP                                                                           
 1106 CONTINUE                                                                            
      GO TO 900                                                                           
C-----                                                                                    
C-----RECORD READING                                                                      
  800 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 803,804,999                                                               
  804 K10=K10+1                                                                           
C-----BUILD ITEM FROM LIT( ) FIELDS                                                       
      CALL SETBLK(ITEM,0,LTBEG(KL)+LTLEN(KL))                                             
      DO 805 JL=1,KL                                                                      
      CALL ORDFLD(LLBEG,LLLEN,IORD,ISEQ,JL,KL,LIT,LREC,KDUM,L)                            
      CALL FHOL(L,LLLEN(JL),LTDEC(JL))                                                    
      IF(LTDEC(JL)) 820,820,821                                                           
  821 LEN=LLLEN(JL)+1                                                                     
      GO TO 822                                                                           
  820 LEN=LLLEN(JL)                                                                       
  822 CALL COPYC(L,MACHC-LEN+1,ITEM,LTBEG(JL),LEN)                                        
  805 CONTINUE                                                                            
C-----SAVE OLD KEYS                                                                       
      DO 810 JP=1,KP                                                                      
  810 KEYPO(JP)=KEYP(JP)                                                                  
      DO 811 JT=1,KT                                                                      
  811 KEYCO(JT)=KEYC(JT)                                                                  
      DO 812 JS=1,KS                                                                      
  812 KEYSO(JS)=KEYS(JS)                                                                  
C-----EXTRACT NEW KEYS                                                                    
      CALL ORDINL(LPBEG,LPLEN,IORD,ISEQ,KP,LIT,LREC,KEYP)                                 
      CALL ORDINL(LCBEG,LCLEN,IORD,ISEQ,KT,LIT,LREC,KEYC)                                 
      CALL ORDINL(LSBEG,LSLEN,IORD,ISEQ,KS,LIT,LREC,KEYS)                                 
C-----TEST FOR CONTROL CHANGE                                                             
      IF(KEYMAT(KEYP,KEYPO,KP*MACHC)) 400,806,400                                         
  806 IF(KEYMAT(KEYC,KEYCO,KT*MACHC)) 600,807,600                                         
  807 IF(KEYMAT(KEYS,KEYSO,KS*MACHC)) 700,900,700                                         
C-----                                                                                    
C-----RECORD WRITING                                                                      
  900 IF(IR-NROWS) 901,901,902                                                            
  902 IF(ICT-KTABS) 500,950,999                                                           
  901 CALL PAGE(ITEM,LTBEG(KL)+LTLEN(KL),IR,LTAB(ICT))                                    
      IR=IR+ISP                                                                           
      IF(IEQ(LRULE,KRULS,IR)) 999,1108,1109                                               
 1109 IR=IR+ISP                                                                           
 1108 CONTINUE                                                                            
C-----SAVE OLD RECORD                                                                     
      DO 910 I=1,LAST                                                                     
  910 LOT(I)=LIT(I)                                                                       
      GO TO 800                                                                           
C-----                                                                                    
C-----LINE COLUMNS REGENERATED FOR EACH PAGE JUST BEFORE PRINTING                         
  950 CONTINUE                                                                            
      IF(KLINS) 999,1023,1024                                                             
 1024 LINSTT=4+IHED+KT*ISP                                                                
      DO 1020 KLI=1,KLINS                                                                 
      DO 1020 KRU=LINSTT,NROWS                                                            
      IF(IEQ(LRULE,KRULS,KRU)) 999,1022,1020                                              
 1022 CALL PAGE(MLIN,1,KRU,LLINE(KLI))                                                    
 1020 CONTINUE                                                                            
 1023 CONTINUE                                                                            
C-----PRINT OLD PAGE AND CLEAR UNMARKED ROWS                                              
      CALL PAGE(L,L,0,11)                                                                 
      K11=K11+NROWS                                                                       
      IF(IEND) 999,400,960                                                                
C-----                                                                                    
C-----END OF JOB                                                                          
  803 CALL EOFR(10,K10)                                                                   
      REWIND 10                                                                           
      IEND=1                                                                              
      GO TO 950                                                                           
  960 CALL EOFW(11,K11)                                                                   
      ENDFILE 11                                                                          
      REWIND 11                                                                           
      STOP                                                                                
      END                                                                                 
