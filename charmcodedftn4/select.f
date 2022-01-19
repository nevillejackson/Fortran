      PROGRAM SELECT(TAPE10=/1000,TAPE11=/1000,TAPE12=/1000                               
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----READ CHARM FILE (TAPE10) & MAKE CHARM FILE (TYAPE11) CONTAINING                     
C-----RECORDS SELECTED ON BASIS OF ONE OR MORE FIELDS IN RECORD TESTED                    
C-----AGAINST THOSE ON DIRECTIVES                                                         
C-----UP TO 50 DIRECTIVES ALLOWED                                                         
C-----RECORDS NOT SELECTED WRITTEN ON TAPE12                                              
C-----                                                                                    
      DIMENSION LIT(100)                                                                  
      DIMENSION LA(8)                                                                     
      DIMENSION LD(50),KSWF(50),IBEG(10,50),ILENG(10,50),LOP1(50),                        
     1 LOP2(50),LVAL(8,50),NVALS(50),LVAL1(8,50),LVAL2(8,50)                              
      DIMENSION ITM(8),JTM(8),KTM(8)                                                      
      DIMENSION LFUN(1),LLB(1),LRB(1),LOPR(2),LOP(1),LCOM(1),LSTAR(1)                     
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      COMMON/NXST/ LA,LALEN,IPOS,ILEN,ITYP,IEND,KTM                                       
      CALL DEFINE                                                                         
      LALEN=MACHCC                                                                        
      CALL MCHL(46,LSTAR,1)                                                               
      CALL MCH(40,LSTAR,1)                                                                
      CALL MCHL(46,LCOM,1)                                                                
      CALL MCH(47,LCOM,1)                                                                 
      CALL MCHL(46,LFUN,1)                                                                
      CALL MCH(07,LFUN,1)                                                                 
      CALL MCHL(46,LLB,1)                                                                 
      CALL MCH(42,LLB,1)                                                                  
      CALL MCHL(46,LRB,1)                                                                 
      CALL MCH(43,LRB,1)                                                                  
      CALL MCHL(46,LOPR,12)                                                               
      CALL MCH(30,LOPR,12)                                                                
      CALL MCH(09,LOPR,12)                                                                
      CALL MCH(06,LOPR,12)                                                                
      CALL MCH(18,LOPR,12)                                                                
      CALL MCH(30,LOPR,12)                                                                
      CALL MCH(09,LOPR,12)                                                                
      CALL MCH(08,LOPR,12)                                                                
      CALL MCH(21,LOPR,12)                                                                
      CALL MCH(30,LOPR,12)                                                                
      CALL MCH(09,LOPR,12)                                                                
      CALL MCH(13,LOPR,12)                                                                
      CALL MCH(21,LOPR,12)                                                                
      CALL MCHL(46,LOP,10)                                                                
      CALL MCH(29,LOP,10)                                                                 
      CALL MCH(09,LOP,10)                                                                 
      CALL MCH(10,LOP,10)                                                                 
      CALL MCH(29,LOP,10)                                                                 
      CALL MCH(09,LOP,10)                                                                 
      CALL MCH(06,LOP,10)                                                                 
      CALL MCH(30,LOP,10)                                                                 
      CALL MCH(09,LOP,10)                                                                 
      CALL MCH(06,LOP,10)                                                                 
      CALL MCH(13,LOP,10)                                                                 
C-----                                                                                    
      WRITE(LP,10)                                                                        
   10 FORMAT(11H0SELECT RUN)                                                              
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      CALL LNWRIT(LREC,LRECW,11)                                                          
      CALL LNWRIT(LREC,LRECW,12)                                                          
      LAST=LRECW+1                                                                        
C-----                                                                                    
C-----LOOP OVER DIRECTIVES                                                                
C-----                                                                                    
C-----DIRECTIVES COUNTER                                                                  
      K=0                                                                                 
    1 CALL READCD(LC,LA,IFLAG,MACHCD)                                                     
      IF(IFLAG) 3,4,999                                                                   
    4 CALL WRITLN(LP,LA,0,MACHCD)                                                         
C-----                                                                                    
C-----THIS SECTION CRACKS A DIRECTIVE                                                     
C-----                                                                                    
      IPOS=1                                                                              
      K=K+1                                                                               
      KSWF(K)=0                                                                           
      IEND=0                                                                              
      IF(NXSC(ITM,1,LSTAR,1,1,0)) 5,7,5                                                   
C-----                                                                                    
    5 WRITE(LP,109)                                                                       
  109 FORMAT(23H UNRECOGNISED DIRECTIVE)                                                  
      CALL JOBEND                                                                         
C-----                                                                                    
    7 I=NXSC(ITM,1,LOP,10,3,1) +1                                                         
      IF(NXSC(ITM,1,LCOM,1,1,0)) 5,8,5                                                    
    8 GO TO (5,6,6,6,999),I                                                               
    6 LD(K)=I-1                                                                           
C-----BEG OR F()                                                                          
      IF(NXS(ITM,0)) 60,5,14                                                              
C-----SINGLE FIELD                                                                        
   14 IBEG(1,K)=ITM(1)                                                                    
      IF(NXSC(ITM,1,LCOM,1,1,0)) 5,15,5                                                   
   15 IF(NXS(ITM,0)) 5,5,16                                                               
   16 ILENG(1,K)=ITM(1)                                                                   
      GO TO 17                                                                            
C-----MULTIPLE FIELDS                                                                     
   60 IPOS=IPOS-1                                                                         
      IF(NXSC(ITM,1,LFUN,1,1,0)) 5,62,5                                                   
   62 IF(NXSC(ITM,1,LLB,1,1,0)) 5,63,5                                                    
   63 DO 64 I=1,11                                                                        
      IF(NXS(IBEG(I,K),0)) 5,5,66                                                         
   66 IF(NXSC(ITM,1,LCOM,1,1,0)) 5,68,5                                                   
   68 IF(NXS(ILENG(I,K),0)) 5,5,65                                                        
   65 IF(NXSC(ITM,1,LCOM,1,1,0)) 74,64,74                                                 
   74 IPOS=IPOS-1                                                                         
      IF(NXSC(ITM,1,LRB,1,1,0)) 5,70,5                                                    
   64 CONTINUE                                                                            
      GO TO 5                                                                             
   70 KSWF(K)=I                                                                           
      IF(I) 999,5,17                                                                      
C-----                                                                                    
C-----OPERATOR                                                                            
   17 IF(NXSC(ITM,1,LCOM,1,1,0)) 5,18,5                                                   
   18 I=NXSC(ITM,1,LOPR,12,3,1) +1                                                        
      IF(NXSC(ITM,1,LCOM,1,1,0)) 5,19,5                                                   
   19 LOP1(K)=I-1                                                                         
      GO TO (5,85,86,86,999),I                                                            
C-----                                                                                    
C-----EQUALS                                                                              
   85 LPOS=1                                                                              
      DO 90 I=1,10                                                                        
      IF(NXS(ITM,1)) 93,500,93                                                            
C-----                                                                                    
  500 IF(IEND) 999,5,503                                                                  
  503 NVALS(K)=I-1                                                                        
      IF(NVALS(K)) 5,5,110                                                                
C-----                                                                                    
   93 CALL IHS(LVAL(1,K),ITM,ILEN,LPOS,MACHCC)                                            
      IF(NXSC(ITM,1,LCOM,1,1,0)) 91,90,91                                                 
   91 IF(IEND) 999,5,504                                                                  
   90 CONTINUE                                                                            
  504 NVALS(K)=I                                                                          
      IF(NVALS(K)) 5,5,110                                                                
C-----                                                                                    
C-----LT OR GT                                                                            
   86 IF(NXS(ITM,1)) 97,5,97                                                              
   97 CALL COPYC(ITM,1,LVAL1(1,K),1,ILEN)                                                 
      IF(NXSC(ITM,1,LCOM,1,1,0)) 101,99,101                                               
  101 IF(IEND) 999,5,95                                                                   
   95 NVALS(K)=1                                                                          
      GO TO 110                                                                           
C-----SECOND VALUE WITH GT AND LT                                                         
   99 I=NXSC(ITM,1,LOPR,12,3,1)+1                                                         
      IF(NXSC(ITM,1,LCOM,1,1,0)) 5,94,5                                                   
   94 LOP2(K)=I-1                                                                         
      IF(NXS(ITM,1)) 106,5,106                                                            
  106 CALL COPYC(ITM,1,LVAL2(1,K),1,ILEN)                                                 
      NVALS(K)=2                                                                          
      GO TO 110                                                                           
C-----                                                                                    
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
C-----                                                                                    
C-----K COUNTS DIRECTIVES                                                                 
C-----                                                                                    
  110 CONTINUE                                                                            
C-----                                                                                    
C-----END OF SECTION                                                                      
C-----                                                                                    
C-----READ ANOTHER DIRECTIVE                                                              
      GO TO 1                                                                             
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
      IF(K) 999,140,130                                                                   
  140 WRITE(LP,141)                                                                       
  141 FORMAT(32H NO DIRECTIVES -- RUN TERMINATED)                                         
      CALL JOBEND                                                                         
C-----LOOP OVER RECORDS                                                                   
  130 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 133,134,999                                                               
  134 K2=K2+1                                                                             
C-----LOOP OVER DIRECTIVES FOR THIS RECORD                                                
      DO 144 KK=1,K                                                                       
      LORD=0                                                                              
C-----                                                                                    
C-----                                                                                    
C-----EQ,GT,LT,OR BOTH                                                                    
      IF(LOP1(KK)-2) 151,152,153                                                          
C-----EQUALS                                                                              
  151 NV=NVALS(KK)                                                                        
C----LOOP OVER LALUES EQ TO                                                               
      DO 155 IVAL=1,NV                                                                    
C-----TEST FOR FUNCTION                                                                   
      IF(KSWF(KK)) 999,159,157                                                            
C-----SINGLE FIELD                                                                        
  159 CALL COPYC(LIT,IBEG(1,KK),JTM,1,ILENG(1,KK))                                        
      CALL JHS(LVAL(1,KK),MACHCC,IVAL,NVALS(KK),ITM,LENG)                                 
      IF(LENG-ILENG(1,KK)) 155,149,155                                                    
  149 IF(IJSTCM(ITM,JTM,LENG)) 155,156,155                                                
C-----MULTIPLE FIELDS                                                                     
  157 CALL MF(IBEG(1,KK),ILENG(1,KK),KSWF(KK),LIT,LREC,JTM,JTMLEN)                        
      CALL JHS(LVAL(1,KK),MACHCC,IVAL,NVALS(KK),ITM,LENG)                                 
      IF(LENG-JTMLEN) 155,148,155                                                         
  148 IF(IJSTCM(ITM,JTM,LENG)) 155,156,155                                                
  155 CONTINUE                                                                            
C-----NO FOR ALL VALUES EXCLUDE IMMEDIATELY ON I NEXT DIRECTIVE ON E                      
      IF(LD(KK)-1) 999,330,144                                                            
C-----YES FOR ONE VALUE -- NEXT DIRECTIVE ON I EXCLUDE IMMEDIATELY ON E                   
  156 IF(LD(KK)-1) 999,144,230                                                            
C-----                                                                                    
C-----GREATER THAN                                                                        
  152 CONTINUE                                                                            
C-----TEST FOR FUNCTION                                                                   
  161 IF(KSWF(KK)) 999,162,163                                                            
C-----SINGLE                                                                              
  162 CALL COPYC(LIT,IBEG(1,KK),JTM,1,ILENG(1,KK))                                        
      IF(LORD) 999,400,401                                                                
  400 IF(IJSTCM(JTM,LVAL1(1,KK),ILENG(1,KK)))165,165,166                                  
  401 IF(IJSTCM(JTM,LVAL2(1,KK),ILENG(1,KK)))165,165,166                                  
C-----MULTIPLE FIELDS                                                                     
  163 CALL MF(IBEG(1,KK),ILENG(1,KK),KSWF(KK),LIT,LREC,JTM,JTMLEN)                        
      IF(LORD) 999,402,403                                                                
  402 IF(IJSTCM(JTM,LVAL1(1,KK),JTMLEN)) 165,165,166                                      
  403 IF(IJSTCM(JTM,LVAL2(1,KK),JTMLEN)) 165,165,166                                      
C-----                                                                                    
C-----NO -- EXCLUDE IMMED ON I , NEXT DIRECTIVE ON E                                      
  165 IF(LD(KK)-1) 999,330,144                                                            
C-----YES -- NEXT DIRECTIVE OR NEXT TEST ON I , EXCL IMMED ON E                           
  166 IF(NVALS(KK).EQ.2.AND.LORD.EQ.0) GO TO 167                                          
  169 IF(LD(KK)-1) 999,144,230                                                            
  167 LORD=1                                                                              
      GO TO 181                                                                           
C-----                                                                                    
C-----LESS THAN                                                                           
  153 CONTINUE                                                                            
C-----TEST FOR FUNCTION                                                                   
  181 IF(KSWF(KK)) 999,182,183                                                            
C-----SINGLE                                                                              
  182 CALL COPYC(LIT,IBEG(1,KK),JTM,1,ILENG(1,KK))                                        
      IF(LORD) 999,404,405                                                                
  404 IF(IJSTCM(JTM,LVAL1(1,KK),ILENG(1,KK)))186,185,185                                  
  405 IF(IJSTCM(JTM,LVAL2(1,KK),ILENG(1,KK)))186,185,185                                  
C-----MULTIPLE FIELDS                                                                     
  183 CALL MF(IBEG(1,KK),ILENG(1,KK),KSWF(KK),LIT,LREC,JTM,JTMLEN)                        
      IF(LORD) 999,406,407                                                                
  406 IF(IJSTCM(JTM,LVAL1(1,KK),JTMLEN)) 186,185,185                                      
  407 IF(IJSTCM(JTM,LVAL2(1,KK),JTMLEN)) 186,185,185                                      
C-----NO -- EXCLUDE IMMED ON I , NEXT DIRECTIVE ON E                                      
  185 IF(LD(KK)-1) 999,330,144                                                            
C-----YES -- NEXT DIRECTIVE OR NEXT TEST ON I , EXCL IMMED ON E                           
  186 IF(NVALS(KK).EQ.2.AND.LORD.EQ.0) GO TO 187                                          
  189 IF(LD(KK)-1) 999,144,230                                                            
  187 LORD=1                                                                              
      GO TO 161                                                                           
C ----                                                                                    
C-----END OF DIRECTIVES LOOP                                                              
  144 CONTINUE                                                                            
C-----WRITE NEW RECORD ON 11 -- IE INCLUDE                                                
      CALL WRITUN(11,LIT,LAST)                                                            
      K3=K3+1                                                                             
      GO TO 130                                                                           
C-----                                                                                    
C-----                                                                                    
C-----OPTIONAL PRINT ON EXCLUDE SECTION                                                   
  230 IF(LD(KK)-2) 330,330,231                                                            
  231 CALL PRNTSI(LP,LIT(LAST),LIT,LRECW,0)                                               
C-----                                                                                    
C-----WRITE EXCLUDED RECORD ON TAPE12                                                     
  330 CALL WRITUN(12,LIT,LAST)                                                            
      K12=K12+1                                                                           
      GO TO 130                                                                           
C-----                                                                                    
C-----END FILE ON 10                                                                      
C-----TERMINATE RUN                                                                       
  133 CALL EOFR(10,K2)                                                                    
      REWIND 10                                                                           
      ENDFILE 11                                                                          
      REWIND 11                                                                           
      CALL EOFW(11,K3)                                                                    
      ENDFILE 12                                                                          
      REWIND 12                                                                           
      CALL EOFW(12,K12)                                                                   
      STOP                                                                                
      END                                                                                 
