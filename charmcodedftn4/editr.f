      PROGRAM EDITR(TAPE10=/1000,TAPE11=/1000                                             
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----IN WHAT FOLLOWS LINE MEANS RECORD AND LINE NO MEANS SEQUENCE NO                     
C-----EDITS RECORDS BY WHOLE LINE AND INTRA-LINE EDITS                                    
C-----   *D,LN  DELETE LINE NO LN FOLLOW BY OPTIONAL INSERT                               
C-----   *I,LN  INSERT AFTER LINE NO LN                                                   
C-----   *S,LN,PARAMS                                                                     
C-----          INTRA-LINE EDIT ON LINE LN BY CALL TO SUB EDSUB                           
C-----   *L,LN  LIST LN ON OUTPUT                                                         
C-----                                                                                    
C-----RECORD LENGTH NOT LIMITED TO 80 CHARACTERS                                          
C-----ALL RECORDS MUST BE SAME LENGTH                                                     
C-----MAX 200 DIRECTIVES                                                                  
C----                                                                                     
C-----  PARAMS FOR INTRA LINE EDTTS                                                       
C-----   R(CN,STRING)                                                                     
C-----   D(CN,LENG)                                                                       
C-----   I(CN,STRING)                                                                     
C----                                                                                     
      DIMENSION LIT(100),LOT(100)                                                         
      DIMENSION LIST(200,9),LA(9)                                                         
      DIMENSION LDIR(4)                                                                   
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      DATA (LDIR(I),I=1,4)/1HD,1HI,1HS,1HL/                                               
      DATA LSTAR/1H*/                                                                     
      CALL DEFINE                                                                         
C-----                                                                                    
      WRITE(LP,15)                                                                        
   15 FORMAT(10H0EDITR RUN)                                                               
      CALL LNREAD(LREC,LRECW,10)                                                          
      CALL LNWRIT(LREC,LRECW,11)                                                          
      LAST=LRECW+1                                                                        
C-----                                                                                    
      N=0                                                                                 
      KSW=0                                                                               
C-----READ DIRECTIVES                                                                     
    1 CALL READCD(LC,LA,IFLAG,MACHCD)                                                     
      IF(IFLAG) 3,4,999                                                                   
    4 CALL WRITLN(LP,LA,0,MACHCD)                                                         
C-----NUMBER DIRECTIVES BY LINE NO REFERENCED                                             
      IPOS=4                                                                              
      IF(LINO(LA,MACHCD,LSTAR,IPOS,KEEP)) 6,7,8                                           
C-----REJECTED DIRECTIVES                                                                 
    7 WRITE(LP,9)                                                                         
    9 FORMAT(39H UNRECOGNIZED DIRECTIVE -- CARD SKIPPED)                                  
C-----SET FLAG TO REJECT DATA CARDS FOLLOWING UNRECOGNIZED DIRECTIVE                      
      KSW=1                                                                               
      GO TO 1                                                                             
C-----DATA CARDS                                                                          
    6 IF(KSW) 8,8,10                                                                      
   10 WRITE(LP,9)                                                                         
      GO TO 1                                                                             
C-----SORT ACCEPTED DIRECTIVES AND DATA CARDS                                             
    8 LLA=MACHCD+1                                                                        
      N=N+1                                                                               
      DO 80 I=1,LLA                                                                       
   80 LIST(N,I)=LA(I)                                                                     
      KSW=0                                                                               
      GO TO 1                                                                             
C-----                                                                                    
C-----END OF DIRECTIVES,RETRIEVE SORTED DIRECTIVES,INTERPRET AND PROCESS TAPE10           
C-----                                                                                    
    3 LAIR=0                                                                              
C-----SWITCH FOR NEW DIRECTIVE VERSUS DATA CARD                                           
C-----  LSW=1 -- DATA CARD LAST LA                                                        
C-----  LSW=0 -- DIRECTIVE CARD LAST LA                                                   
      LSW=0                                                                               
C-----SWITCH FOR REJECTED DIRECTIVE VERSUS OK                                             
      KSW=0                                                                               
C-----COUNTERS                                                                            
      K10=0                                                                               
      K11=0                                                                               
C-----                                                                                    
C-----LOOP OVER SORTED DIRECTIVES LIST                                                    
   20 LAIR=LAIR+1                                                                         
      IF(LAIR-N) 21,21,22                                                                 
C-----RETRIEVE FROM LIST INTO WORK ARRAY                                                  
   21 DO 23 I=1,LLA                                                                       
   23 LA(I)=LIST(LAIR,I)                                                                  
      IF(IGETLB(LA,1,1,MACHCD)-LSTAR) 32,31,32                                            
C-----DATA CARD , COPY LA TO TAPE11 OR SKIP OVER IF KSW=1                                 
C-----JOIN MULTIPLE CARD RECORDS IN LIST INTO ONE ARRAY LOT                               
C-----LAIR UPDATED ON RETURN TO POS OF LAST DATA CARD READ                                
   32 IF(KSW) 28,28,29                                                                    
   28 CALL JOKA(LIST,LOT,LAIR,LRECW,LREC80)                                               
      IF(LAIR-N) 19,19,22                                                                 
   19 LOT(LAST)=IHOL(LN)                                                                  
C-----PUTS SEQ NO OF EDITED RECORD IN CHARACTERS                                          
      CALL WRITUN(11,LOT,LAST)                                                            
      K11=K11+1                                                                           
      LSW=1                                                                               
      GO TO 20                                                                            
C-----UNRECOGNIZED OLD DIRECTIVE                                                          
   29 GO TO 20                                                                            
C-----NEW DIRECTIVE                                                                       
C-----READ TAPE10 DOWN TO LINE NO , KSW=0                                                 
   31 LN=LA(LLA)                                                                          
      LSW=0                                                                               
      KSW=0                                                                               
   24 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 33,34,999                                                                 
C-----CHECK IF LINE NO TO BE EDITED                                                       
   34 K10=K10+1                                                                           
      IF(INUM(LIT(LAST))-LN) 35,36,37                                                     
C-----DOES COMPARISON IN INTEGER FORM TO AVOID SORT PROBLEMS WITH LEADING BLANKS          
C----- WITH LEADING BLANKS                                                                
C-----COPY                                                                                
   35 CALL WRITUN(11,LIT,LAST)                                                            
      K11=K11+1                                                                           
      GO TO 24                                                                            
C-----LINE NO NOT FOUND                                                                   
   37 WRITE(LP,51)                                                                        
   51 FORMAT(44H SEQUENCE NUMBER ON DIRECTIVE NOT ON UNIT 10)                             
      CALL WRITLN(LP,LA,0,MACHCD)                                                         
      BACKSPACE 10                                                                        
      GO TO 20                                                                            
C-----                                                                                    
C-----EDIT THIS RECORD                                                                    
C-----INTERPRET DIRECTIVE FIRST                                                           
C-----                                                                                    
C-----LINE NO FOUND                                                                       
   36 IPOS=2                                                                              
      IF(NEXITM(ITM,LA,IPOS)) 40,38,39                                                    
C-----OP CODE UNRECOGNIZED IN LA                                                          
   38 CONTINUE                                                                            
   39 WRITE(LP,9)                                                                         
      CALL WRITLN(LP,LA,0,MACHCD)                                                         
      KSW=1                                                                               
      GO TO 20                                                                            
C-----CODE MNEMONIC                                                                       
   40 ND=NOD(ITM,LDIR,4)                                                                  
C-----BRANCH ON DIRECTIVE MNEMONIC                                                        
      GO TO (41,42,43,44),ND                                                              
C-----DELETION  DONT WRITE ON TAPE11                                                      
C-----CHECK FOR OPTIONAL INSERT                                                           
   41 LSW=0                                                                               
      GO TO 20                                                                            
C-----INSERTION  COPY NEXT DIRECTIVE CARDS TO 11 WITHOUT READING FROM 10                  
C-----WRITE CARD INSERTED AFTER ON 11 FIRST                                               
   42 CALL WRITUN(11,LIT,LAST)                                                            
      K11=K11+1                                                                           
      GO TO 20                                                                            
C-----LINK TO EDIT BY SUBROUTINE                                                          
   43 CALL EDSUB(LA,LIT,LREC ,IERR)                                                       
      IF(IERR) 74,73,74                                                                   
   74 WRITE(LP,9)                                                                         
C-----KSW FLAGS UNRECOGNIZED DIRECTIVE                                                    
      KSW=1                                                                               
      GO TO 20                                                                            
   73 CALL WRITUN(11,LIT,LAST)                                                            
      K11=K11+1                                                                           
      GO TO 20                                                                            
C-----PRINT SINGLE RECORD ON OUTPUT                                                       
   44 CALL PRNTSI(LP,LIT(LAST),LIT,LRECW,0)                                               
      CALL WRITUN(11,LIT,LAST)                                                            
      K11=K11+1                                                                           
      GO TO 20                                                                            
C-----NO MORE DIRECTIVES                                                                  
C-----FINISH COPYING 10 TO 11                                                             
   22 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 33,64,999                                                                 
   64 K10=K10+1                                                                           
      CALL WRITUN(11,LIT,LAST)                                                            
      K11=K11+1                                                                           
      GO TO 22                                                                            
C-----EOF ON TAPE10                                                                       
   33 CALL EOFR(10,K10)                                                                   
      REWIND 10                                                                           
      ENDFILE 11                                                                          
      REWIND 11                                                                           
      CALL EOFW(11,K11)                                                                   
C-----CHECK IF LAIR = N                                                                   
      IF(LAIR-N) 70,71,71                                                                 
   70 WRITE(LP,72)                                                                        
   72 FORMAT(28H SOME DIRECTIVES UNPROCESSED)                                             
   71 STOP                                                                                
  999 CALL LOGIC(5HEDITR)                                                                 
      END                                                                                 
