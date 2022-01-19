      PROGRAM DUPCHK(TAPE10=/1000,TAPE11=/1000,TAPE12=/1000                               
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C---- WRITTEN R.E. 1982                                                                   
C-----SEPARATES A PRE-SORTED CHARM FILE INTO TWO CHARM                                    
C --- FILES ... TAPE11 - CONTAINS ALL UNIQUE RECORDS PLUS FIRST OCCURRING                 
C --- RECORD OF MULTIPLICATES.                                                            
C --- TAPE12 CONTAINS SECOND AND HIGHER OCCURRENCES OF MULTIPLICATES.                     
C --- RECORDS ARE COMPARED ON ONE OR MORE DEFINED KEY FIELDS, ON WHICH                    
C --- THE INPUT FILE (TAPE10) WOULD NORMALLY BE PRE-SORTED.                               
      DIMENSION LITA(100),LITB(100),LA(8)                                                 
      DIMENSION IBEG(50), ILEN(50), IORD(50), ISEQ(50),KEYA(50),KEYB(50)                  
      DIMENSION LSTAR(1), LCOM(1), LOP(1), LAD(1)                                         
      DIMENSION ITM(8),JTM(8),KTM(8)                                                      
      COMMON/MACH/MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                     
      COMMON/NXST/LA,LALEN,IPOS,ILNG,ITYP,IEND,KTM                                        
      CALL DEFINE                                                                         
      LALEN=MACHCC                                                                        
C --- SET UP LEGAL CHARACTER STRINGS                                                      
      CALL MCHL(46,LSTAR,1)                                                               
      CALL MCH(40,LSTAR,1)                                                                
      CALL MCHL(46,LCOM,1)                                                                
      CALL MCH(47,LCOM,1)                                                                 
      CALL MCHL(46,LOP,1)                                                                 
      CALL MCH(07,LOP,1)                                                                  
      CALL MCHL(46,LAD,6)                                                                 
      CALL MCH(29,LAD,6)                                                                  
      CALL MCH(09,LAD,6)                                                                  
      CALL MCH(02,LAD,6)                                                                  
      CALL MCH(29,LAD,6)                                                                  
      CALL MCH(09,LAD,6)                                                                  
      CALL MCH(05,LAD,6)                                                                  
C ---                                                                                     
      WRITE(LP,10)                                                                        
   10 FORMAT(11H0DUPCHK RUN)                                                              
      REWIND 10                                                                           
      REWIND 11                                                                           
      REWIND 12                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      CALL LNWRIT(LREC,LRECW,11)                                                          
      CALL LNWRIT(LREC,LRECW,12)                                                          
      LAST=LRECW+1                                                                        
C ---                                                                                     
C --- READ DIRECTIVES                                                                     
      K=0                                                                                 
    1 CALL READCD(LC,LA,IFLAG,MACHCD)                                                     
      IF(IFLAG)3,4,999                                                                    
C --- ERROR BRANCH                                                                        
  999 CALL LOGIC(6HDUPCHK)                                                                
      STOP                                                                                
C ---                                                                                     
    4 CALL WRITLN(LP,LA,0,MACHCD)                                                         
C --- CRACK DIRECTIVE AND STORE                                                           
      IPOS=1                                                                              
      K=K+1                                                                               
      IEND=0                                                                              
      IF(NXSC(ITM,1,LSTAR,1,1,0))5,7,5                                                    
    5 WRITE(LP,109)                                                                       
  109 FORMAT(23H UNRECOGNISED DIRECTIVE)                                                  
      CALL JOBEND                                                                         
    7 IF(NXSC(ITM,1,LOP,1,1,0))5,8,5                                                      
    8 IF(NXSC(ITM,1,LCOM,1,1,0))5,9,5                                                     
    9 IF(NXS(IBEG(K),0))5,5,13                                                            
   13 IF(NXSC(ITM,1,LCOM,1,1,0))5,14,5                                                    
   14 IF(NXS(ILEN(K),0))5,5,16                                                            
   16 IF(NXSC(ITM,1,LCOM,1,1,0))5,20,5                                                    
   20 I=NXSC(ITM,1,LAD,6,2,1)+1                                                           
      IF(NXSC(ITM,1,LCOM,1,1,0))5,21,5                                                    
   21 GO TO (5,22,23,999),I                                                               
   22 IORD(K)=0                                                                           
      GO TO 25                                                                            
   23 IORD(K)=1                                                                           
   25 I=NXSC(ITM,1,LAD,6,2,1)+1                                                           
      GO TO (5,29,30,999),I                                                               
   29 ISEQ(K)=0                                                                           
      GO TO 32                                                                            
   30 ISEQ(K)=1                                                                           
   32 GO TO 1                                                                             
C --- END DIRECTIVE CRACKING                                                              
C ---                                                                                     
C --- INITIALISE COUNTERS                                                                 
    3 K10=0                                                                               
      K11=0                                                                               
      K12=0                                                                               
C --- CALCULATE TOTAL KEY LENGTH                                                          
      KEYLEN=0                                                                            
      DO 270 KK=1,K                                                                       
  270 KEYLEN=KEYLEN+ILEN(KK)                                                              
      IF(K)999,140,130                                                                    
  140 WRITE(LP,141)                                                                       
  141 FORMAT(32H NO DIRECTIVES -- RUN TERMINATED)                                         
      CALL JOBEND                                                                         
C ---                                                                                     
C --- BEGIN READING RECORDS                                                               
  130 CALL READUN(10,LITA,IFLAG,LAST)                                                     
      IF(IFLAG)133,134,999                                                                
  134 K10=K10+1                                                                           
C --- ASSEMBLE COMBINED KEYA                                                              
      CALL ORDKEY(IBEG,ILEN,IORD,ISEQ,K,LITA,LREC,KEYA)                                   
      CALL WRITUN(11,LITA,LAST)                                                           
      K11=K11+1                                                                           
C --- LOOP OVER RECORDS                                                                   
  150 CALL READUN(10,LITB,IFLAG,LAST)                                                     
      IF(IFLAG)133,154,999                                                                
  154 K10=K10+1                                                                           
C --- ASSEMBLE COMBINED KEYB                                                              
      CALL ORDKEY(IBEG,ILEN,IORD,ISEQ,K,LITB,LREC,KEYB)                                   
C --- COMPARE KEYA WITH KEYB                                                              
      IF(IJSTCM(KEYA,KEYB,KEYLEN))160,165,161                                             
C --- SEQUENCE ERROR                                                                      
  161 WRITE(LP,162)                                                                       
  162 FORMAT(49H SEQUENCE ERROR ON TAPE10 -- LAST RECORD READ WAS)                        
      CALL PRNTSI(LP,LITB(LAST),LITB,LRECW,0)                                             
      WRITE(LP,170)                                                                       
  170 FORMAT(15H RUN TERMINATED)                                                          
      CALL JOBEND                                                                         
C --- UNIQUE OR FIRST-OCCURRING OF DUPLICATES                                             
  160 CALL WRITUN(11,LITB,LAST)                                                           
      K11=K11+1                                                                           
      GO TO 180                                                                           
C --- SECOND OR HIGHER OCCURRENCE OF DUPLICATES                                           
  165 CALL WRITUN(12,LITB,LAST)                                                           
      K12=K12+1                                                                           
C --- STORE LAST RECORD AND KEY                                                           
  180 CALL COPYC(KEYB,1,KEYA,1,KEYLEN)                                                    
      DO 185 I=1,LAST                                                                     
  185 LITA(I)=LITB(I)                                                                     
      GO TO 150                                                                           
C --- END OF FILE PROCESSING                                                              
  133 CALL EOFR(10,K10)                                                                   
      REWIND 10                                                                           
      CALL EOFW(11,K11)                                                                   
      CALL EOFW(12,K12)                                                                   
      ENDFILE 11                                                                          
      ENDFILE 12                                                                          
      REWIND 11                                                                           
      REWIND 12                                                                           
      STOP                                                                                
      END                                                                                 
