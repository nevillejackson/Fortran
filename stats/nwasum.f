      PROGRAM NWASUM(TAPE10,TAPE11,INPUT,OUTPUT,TAPE5=INPUT,                              
     1   TAPE6=OUTPUT)                                                                    
C-----CALCULATES CARD COUNTS, TOTALS, AND MEANS FOR CROSS-CLASSIFIED                      
C-----DATA                                                                                
C----- NFAC=NUMBER OF FACTORS                                                             
C----- NOT=NUMBER OF TRAITS                                                               
C----- MAXNTY=MAXIMUM NUMBER OF WAYS TABULATED(<OR=NFAC)                                  
C----- MT=PRINT COUNTS AND MEANS (1), COUNTS AND TOTALS (2),                              
C----- COUNTS, MEANS AND TOTALS (3)                                                       
C-----OUTPUT, FOR EACH TRAIT, IS ALL POSSIBLE 0,1,2,.......,MAXNTY                        
C----- -WAY TABLES OF COUNTS, TOTALS AND MEANS.                                           
C-----                                                                                    
      DIMENSION IHEAD(8)                                                                  
      COMMON A(1)                                                                         
      EQUIVALENCE(IHEAD,A)                                                                
      LPROG=MEMORYS(0)                                                                    
      PRINT 10                                                                            
   10 FORMAT(11H0NWASUM RUN)                                                              
      PRINT 1,LPROG                                                                       
    1 FORMAT(9H LPROG = ,I8,8H DECIMAL)                                                   
C-----READ HEADING CARD                                                                   
      CALL REMARK(23HREADING PARAMETER CARDS)                                             
      READ 90,IHEAD                                                                       
   90 FORMAT(8A10)                                                                        
      PRINT 91,IHEAD                                                                      
   91 FORMAT(1H1,8A10)                                                                    
C-----READ SIZE PARAMETER CARD                                                            
      READ 92,NFAC,NOT,MAXNTY,MT,NTF,MAXLEV                                               
   92 FORMAT(16I5)                                                                        
      PRINT 93,NFAC,NOT,MAXNTY,MT,NTF,MAXLEV                                              
   93 FORMAT(8HONFAC = ,I3,8H  NOT = ,I3,11H  MAXNTY = ,I3,7H  MT = ,                     
     1 I1,8H  NTF = ,I3,11H  MAXLEV = ,I3)                                                
C-----CALCULATE ARRAY LENGTHS IN COMMON                                                   
      I1=1                                                                                
      I2=I1+8                                                                             
      I3=I2+NFAC                                                                          
      I4=I3+NFAC                                                                          
      I5=I4+NFAC*MAXLEV                                                                   
      I6=I5+NFAC                                                                          
      I7=I6+NFAC                                                                          
      I8=I7+NFAC                                                                          
      I9=I9+NFAC                                                                          
      I10=I9+NOT                                                                          
      I11=I10+NOT                                                                         
      I12=I11+NOT                                                                         
      I13=I12+NOT                                                                         
      I14=I13+NOT                                                                         
      I15=I14+NOT                                                                         
      I16=I15+NOT                                                                         
      IELL=0                                                                              
      KNT=0                                                                               
      INTY=-1                                                                             
 2001 INTY=INTY+1                                                                         
      IF(INTY-MAXNTY) 2003,2003,2002                                                      
 2003 NTMAX=ICOMB(NFAC,INTY)                                                              
      DO 2004 INT=1,NTMAX                                                                 
      IELF=IELL+1                                                                         
      IELL=IELL+IELNO(NFAC,INT,INTY,NI)                                                   
      DO 2004 IEL=IELF,IELL                                                               
      KNT=KNT+1                                                                           
 2004 CONTINUE                                                                            
      GO TO 2001                                                                          
 2002 I17=I16+KNT*NOT                                                                     
      I18=I17+KNT*NOT                                                                     
      PRINT 2,I18                                                                         
    2 FORMAT(9H LCOMN = ,I8,8H DECIMAL)                                                   
C-----CALC LENGTH OF PGM + COMMON                                                         
      LTOTL=LPROG+I18                                                                     
      PRINT 3,LTOTL                                                                       
    3 FORMAT(9H0LTOTL = ,I8,8H DECIMAL)                                                   
      CALL MEMORYS(LTOTL)                                                                 
C-----                                                                                    
      CALL NWASUB(A(I1),A(I2),A(I3),A(I4),A(I5),A(I6),A(I7),A(I8),A(I9),                  
     1 A(I10),A(I11),A(I12),A(I13),A(I14),A(I15),A(I16),A(I17),NFAC,NOT,                  
     2 MAXNTY,MT,NTF,MAXLEV,KNT)                                                          
      REWIND 10                                                                           
      ENDFILE 11                                                                          
      REWIND 11                                                                           
      STOP                                                                                
      END                                                                                 
      SUBROUTINE NWASUB(IHEAD,NI,LFAC,LEVCOD,LEVC,LEVN,IFN,NK,LOT,X,MIS                   
     1 ,NIS,LW,LD,FMT,K,T,NFAC,NOT,MAXNTY,MT,NTF,MAXLEV,KNT)                              
      DIMENSION IHEAD(8)                                                                  
      DIMENSION NI(1),LFAC(1),LEVCOD(NFAC,1)                                              
     1 ,LEVC(1),LEVN(1),IFN(1),NK(1)                                                      
     2 ,LOT(1),X(1),MIS(1),NIS(1)                                                         
     3 ,LW(1),LD(1),FMT(1)                                                                
     4 ,K(KNT,1),T(KNT,1)                                                                 
      INTEGER X                                                                           
      DIMENSION LIT(37)                                                                   
      KSEQ=0                                                                              
C-----                                                                                    
C-----READ FACTOR PARAMETER CARDS                                                         
      DO 70 I=1,NFAC                                                                      
      READ 94,LFAC(I),NLEV,(LEVCOD(I,J),J=1,NLEV)                                         
   94 FORMAT(A10,I10,6A10/(8A10))                                                         
      NI(I)=NLEV                                                                          
      PRINT 95,I,LFAC(I),NLEV,(LEVCOD(I,J),J=1,NLEV)                                      
   95 FORMAT(9H0FOR I = ,I3,2X,10HLFAC(I) = ,A10,2X,8HNI(I) = ,I3,2X,                     
     1 14HLEVCOD(I,J) = ,6A10/(1H ,13A10))                                                
   70 CONTINUE                                                                            
C-----READ TRAIT PARAMETER CARDS                                                          
      DO 71 I=1,NOT                                                                       
      READ 96,LOT(I),MIS(I),NIS(I),LW(I),LD(I)                                            
   96 FORMAT(3A10,2I5)                                                                    
      PRINT 97,I,LOT(I),MIS(I),NIS(I),LW(I),LD(I)                                         
   97 FORMAT(9H0FOR I = ,I3,2X,                                                           
     1 9HLOT(I) = ,A10,2X,                                                                
     2 9HMIS(I) = ,A10,2X,                                                                
     2 9HNIS(I) = ,A10,2X,                                                                
     3 8HLW(I) = ,I5,2X,8HLD(I) = ,I5)                                                    
   71 CONTINUE                                                                            
C-----                                                                                    
C-----INITIALIZE K( ) AND T( ) ARRAYS                                                     
C-----                                                                                    
      CALL REMARK(12HINITIALIZING)                                                        
      IELL=0                                                                              
      INTY=-1                                                                             
C-----CHOOSE A TABLE TYPE FROM 0-WAY TO MAXNTY-WAY                                        
    1 INTY=INTY+1                                                                         
      IF(INTY-MAXNTY) 3,3,2                                                               
    3 NTMAX=ICOMB(NFAC,INTY)                                                              
C-----CHOOSE A TABLE WITHIN TYPE INTY                                                     
      DO 4 INT=1,NTMAX                                                                    
C-----CHOOSE AN ELEMENT WITHIN TABLE INT                                                  
      IELF=IELL+1                                                                         
      IELL=IELL+IELNO(NFAC,INT,INTY,NI)                                                   
      DO 4 IEL=IELF,IELL                                                                  
C-----CHOOSE A TRAIT                                                                      
      DO 4 J=1,NOT                                                                        
      K(IEL,J)=0                                                                          
      T(IEL,J)=0.0                                                                        
    4 CONTINUE                                                                            
      GO TO 1                                                                             
    2 CONTINUE                                                                            
C-----                                                                                    
      PRINT 1000,IEL                                                                      
 1000 FORMAT(21HO LENGTH OF K AND T = ,I8)                                                
C-----SETUP VARIABLE FORMATS FOR DECODING X()                                             
      DO 43 J=1,NOT                                                                       
      LD(J)=LD(J)+10-LW(J)                                                                
   43 ENCODE(7,42,FMT(J))LD(J)                                                            
   42 FORMAT(5H(F10.,I1,1H))                                                              
C-----                                                                                    
C-----READ DATA CARDS AND BUILD TABLES OF COUNTS AND TOTALS                               
C-----                                                                                    
      CALL REMARK(42HREADING DATA CARDS -- COUNTING AND SUMMING)                          
C-----HEADING FOR DATA CARDS LIST                                                         
      PRINT 100,(I,I=1,NFAC),(I,I=1,NOT)                                                  
  100 FORMAT(58H0LISTING OF LEVEL CODES AND TRAITS AS READ FROM DATA CAR                  
     1DS/7H    CC ,25I5)                                                                  
      KEY=0                                                                               
      KOUNT=0                                                                             
C-----READ FACTOR LEVEL CODES AND TRAITS FROM CARD                                        
   10  CALL DATA(NFAC,NOT,LEVC,X,KEY)                                                     
      KOUNT=KOUNT+1                                                                       
C-----CHECK END OF DATA                                                                   
      IF(KEY) 50,51,50                                                                    
   51 INTY=-1                                                                             
      IF(KOUNT.GT.10) GOTO 1004                                                           
      PRINT 101,KOUNT,(LEVC(I),I=1,NFAC),(X(I),I=1,NOT)                                   
  101 FORMAT(1H ,I6,1X,25A5)                                                              
C-----CHECK ALL LEVEL CODES DEFINED                                                       
 1004 DO 417 I=1,NFAC                                                                     
      NJ=NI(I)                                                                            
      DO 17 J=1,NJ                                                                        
      IF(LEVC(I)-LEVCOD(I,J)) 17,417,17                                                   
   17 CONTINUE                                                                            
      PRINT 19,LEVC(I),I,KOUNT                                                            
   19 FORMAT(12HOLEVEL CODE ,A10,12H  FACTOR NO ,I3,10H  CARD NO ,I6,                     
     1 29H  NOT DEFINED -- CARD SKIPPED)                                                  
      GO TO 10                                                                            
  417 CONTINUE                                                                            
C-----CHOOSE A TABLE TYPE FROM 0-WAY TO MAXNTY-WAY                                        
   11 INTY=INTY+1                                                                         
      IF(INTY-MAXNTY) 13,13,10                                                            
   13 NTMAX=ICOMB(NFAC,INTY)                                                              
C-----CHOOSE A TABLE WITHIN TYPE INTY                                                     
      DO 14 INT=1,NTMAX                                                                   
C-----CONVERT DATA LEVEL CODES LEVC() TO LEVEL NUMBERS LEVN() BY                          
C----- MATCHING WITH LEVCOD()                                                             
      IF(INTY) 53,53,52                                                                   
   53 LEML=1                                                                              
      GO TO 54                                                                            
   52 DO 15 NWAY=1,INTY                                                                   
C-----DECIDE WHICH FACTOR NO CORRESPONDS TO NWAY                                          
      IFNN=INTFAC(NFAC,INTY,INT,NWAY)                                                     
C-----EXTRACT FACTOR LEVEL CODE FROM THIS CARD                                            
      LEVCEX=LEVC(IFNN)                                                                   
C-----MAP INTO LEVEL NO BY MATCHING WITH LEVCOD                                           
      NJ=NI(IFNN)                                                                         
      NK(NWAY)=NJ                                                                         
      DO 16 J=1,NJ                                                                        
      IF(LEVCEX-LEVCOD(IFNN,J)) 16,18,16                                                  
   18 LEVN(NWAY)=J                                                                        
      GO TO 15                                                                            
   16 CONTINUE                                                                            
      PRINT 102,NWAY,LEVCEX                                                               
  102 FORMAT(25H0ERROR IN NWASUM  NWAY = ,I4,11H  LEVCEX = ,A10)                          
      STOP                                                                                
   15 CONTINUE                                                                            
C-----FIND SUBSCRIPT NO OF ELEMENT WITHIN K() AND T() ARRAYS                              
      LEML=LEMLOC(NFAC,NI,INT,INTY,LEVN,NK)                                               
C-----INCREMENT COUNTS AND TOTALS                                                         
   54 DO 12 J=1,NOT                                                                       
C-----TEST FOR MISSING DATA                                                               
      IF(X(J).EQ.MIS(J).OR.X(J).EQ.NIS(J))GOTO 12                                         
   40 DECODE(10,FMT(J),X(J)) XX                                                           
      K(LEML,J)=K(LEML,J)+1                                                               
      T(LEML,J)=T(LEML,J)+XX                                                              
   12 CONTINUE                                                                            
   14 CONTINUE                                                                            
      GO TO 11                                                                            
   50 CONTINUE                                                                            
      CALL EOFR(10,KOUNT)                                                                 
      REWIND 10                                                                           
C-----                                                                                    
C-----CALCULATE MEANS AND PRINT TABLES                                                    
C-----                                                                                    
      CALL REMARK(22HAVERAGING AND PRINTING)                                              
      IELL=0                                                                              
      INTY=-1                                                                             
C-----CHOOSE A TABLE TYPE FROM 0-WAY TO MAXNTY-WAY                                        
   21 INTY=INTY+1                                                                         
      CALL SETBLK(LIT,0,360)                                                              
      ITM=IHOL(INTY)                                                                      
      ITM=IPUTR(LIT,5,1,360,ITM)                                                          
      IF(INTY-MAXNTY) 23,23,20                                                            
   23 NTMAX=ICOMB(NFAC,INTY)                                                              
C-----CHOOSE A TABLE WITHIN TYPE INTY                                                     
      DO 24 INT=1,NTMAX                                                                   
      CALL SETBLK(LIT,6,360)                                                              
      ITM=IHOL(INT)                                                                       
      ITM=IPUTR(LIT,5,6,360,ITM)                                                          
      PRINT 200,INTY,INT                                                                  
  200 FORMAT(1H0,I3,16H - WAY TABLE NO ,I3)                                               
C-----CALC RANGE OF SUBSCRIPTS WITHIN THIS TABLE                                          
      IELF=IELL+1                                                                         
      IELL=IELL+IELNO(NFAC,INT,INTY,NI)                                                   
C-----CALC FACTORS IN EACH WAY OF TABLE INT                                               
      IF(INTY) 63,63,62                                                                   
C-----SPECIAL PRINTOUT FOR 0-WAY TABLES                                                   
   63 GO TO (308,309,310),MT                                                              
  308 PRINT 201                                                                           
      GO TO 311                                                                           
  309 PRINT 202                                                                           
      GO TO 311                                                                           
  310 PRINT 203                                                                           
  311 DO 64 J=1,NOT                                                                       
      ENUM=T(1,J)                                                                         
      DEN=K(1,J)                                                                          
      AVE=DIVF(ENUM,DEN)                                                                  
      ITM=IHOL(J)                                                                         
      ITM=IPUTR(LIT,5,311,360,ITM)                                                        
      ITM=IHOL(1)                                                                         
      ITM=IPUTR(LIT,5,316,360,ITM)                                                        
      ITM=IPUTR(LIT,10,321,360,LOT(J))                                                    
      ITM=IHOL(K(IEL,J))                                                                  
      ITM=IPUTR(LIT,10,331,360,ITM)                                                       
      ITM=IXF(AVE,4)                                                                      
      ITM=IHOL(ITM)                                                                       
      ITM=IPUTR(LIT,10,341,360,ITM)                                                       
      ITM=IXF(T(IEL,J),4)                                                                 
      ITM=IHOL(ITM)                                                                       
      ITM=IPUTR(LIT,10,351,360,ITM)                                                       
      KSEQ=KSEQ+1                                                                         
      WRITE(11)(LIT(I),I=1,36),KSEQ                                                       
      GO TO (312,313,314),MT                                                              
  312 PRINT 204,LOT(J),K(1,J),AVE                                                         
      GO TO 64                                                                            
  313 PRINT 205,LOT(J),K(1,J),T(1,J)                                                      
      GO TO 64                                                                            
  314 PRINT 206,LOT(J),K(1,J),AVE,T(1,J)                                                  
   64 CONTINUE                                                                            
      GO TO 24                                                                            
C-----ALL HIGHER TABLES - SET UP FACTOR NUMBER TO WAY NUMBER MAPPINGS                     
   62 DO 25 NWAY=1,INTY                                                                   
      IFN(NWAY)=INTFAC(NFAC,INTY,INT,NWAY)                                                
      II=IFN(NWAY)                                                                        
      LW(NWAY)=LFAC(II)                                                                   
      LD(NWAY)=NI(II)                                                                     
      ITM=IHOL(IFN(NWAY))                                                                 
      ITM=IPUTR(LIT,5,11+(NWAY-1)*5,360,ITM)                                              
      ITM=IPUTR(LIT,10,61+(NWAY-1)*10,360,LW(NWAY))                                       
      ITM=IHOL(LD(NWAY))                                                                  
      ITM=IPUTR(LIT,5,161+(NWAY-1)*5,360,ITM)                                             
   25 CONTINUE                                                                            
C-----HEADINGS                                                                            
      GO TO (300,301,302),MT                                                              
  300 PRINT 201,(LW(NWAY),NWAY=1,INTY)                                                    
  201 FORMAT(32H0     TRAIT   COUNT         MEAN,1X,                                      
     1 10A10/(1H ,33X,10A10))                                                             
      GO TO 303                                                                           
  301 PRINT 202,(LW(NWAY),NWAY=1,INTY)                                                    
  202 FORMAT(32H0     TRAIT   COUNT        TOTAL,1X,                                      
     1 10A10/(1H ,33X,10A10))                                                             
      GO TO 303                                                                           
  302 PRINT 203,(LW(NWAY),NWAY=1,INTY)                                                    
  203 FORMAT(47H0     TRAIT   COUNT         MEAN          TOTAL,1X,                       
     1 8A10/(1H ,48X,8A10))                                                               
  303 CONTINUE                                                                            
C-----DATA - CHOOSE A TRAIT                                                               
      DO 27 J=1,NOT                                                                       
C-----SET LEVEL COUNTERS FOR EACH WAY                                                     
      DO 125 NWAY=2,INTY                                                                  
  125 LEVN(NWAY)=1                                                                        
      LEVN (1)=0                                                                          
C-----PRINT OUT IN ORDER STORED                                                           
      DO 26 IEL=IELF,IELL                                                                 
C-----VARIABLE DEPTH NESTED LOOPS CHOOSE LEVEL NOS OF EACH FACTOR                         
C----- CORRESPONDING TO IEL                                                               
C-----WAY-1 VARIES LEVELS FASTEST                                                         
      NWAY=1                                                                              
C-----NEXT LEVEL OF WAY-NWAY                                                              
   30 LEVN(NWAY)=LEVN(NWAY)+1                                                             
      IF(LEVN(NWAY)-LD(NWAY))29,29,28                                                     
C-----ON OVERFLOW RESET LEVN TO 1 AND CARRY 1 TO NEXT NWAY UNLESS LAST                    
   28 LEVN(NWAY)=1                                                                        
      NWAY=NWAY+1                                                                         
      IF(NWAY-INTY) 30,30,26                                                              
C-----CONVERT LEVEL NUMBERS TO LEVEL CODES                                                
   29 DO 31 N=1,INTY                                                                      
      NN=LEVN(N)                                                                          
      IFNN=INTFAC(NFAC,INTY,INT,N)                                                        
      ITM=IPUTR(LIT,10,211+(N-1)*10,360,LEVCOD(IFNN,NN))                                  
   31 LEVC(N)=LEVCOD(IFNN,NN)                                                             
C-----MEANS                                                                               
      ENUM=T(IEL,J)                                                                       
      DEN=K(IEL,J)                                                                        
      AVE=DIVF(ENUM,DEN)                                                                  
      ITM=IHOL(J)                                                                         
      ITM=IPUTR(LIT,5,311,360,ITM)                                                        
      ITM=IHOL(IEL)                                                                       
      ITM=IPUTR(LIT,5,316,360,ITM)                                                        
      ITM=IPUTR(LIT,10,321,360,LOT(J))                                                    
      ITM=IHOL(K(IEL,J))                                                                  
      ITM=IPUTR(LIT,10,331,360,ITM)                                                       
      ITM=IXF(AVE,4)                                                                      
      ITM=IHOL(ITM)                                                                       
      ITM=IPUTR(LIT,10,341,360,ITM)                                                       
      ITM=IXF(T(IEL,J),4)                                                                 
      ITM=IHOL(ITM)                                                                       
      ITM=IPUTR(LIT,10,351,360,ITM)                                                       
      KSEQ=KSEQ+1                                                                         
      WRITE(11)(LIT(I),I=1,36),KSEQ                                                       
C-----PRINTOUT                                                                            
      GO TO (304,305,306),MT                                                              
  304 PRINT 204,LOT(J),K(IEL,J),AVE,(LEVC(N),N=1,INTY)                                    
  204 FORMAT(1H ,A10,I8,F13.4,1X,10A10/(1H ,33X,10A10))                                   
      GO TO 307                                                                           
  305 PRINT 205,LOT(J),K(IEL,J),T(IEL,J),(LEVC(N),N=1,INTY)                               
  205 FORMAT(1H ,A10,I8,F13.3,1X,10A10/(1H ,33X,10A10))                                   
      GO TO 307                                                                           
  306 PRINT 206,LOT(J),K(IEL,J),AVE,T(IEL,J),(LEVC(N),N=1,INTY)                           
  206 FORMAT(1H ,A10,I8,F13.4,F15.3,1X,8A10/(1H ,48X,8A10))                               
  307 CONTINUE                                                                            
C-----RETURN TO WAY-1 AFTER EVERY PRINT LINE                                              
   26 CONTINUE                                                                            
      PRINT 207                                                                           
  207 FORMAT(1H )                                                                         
   27 CONTINUE                                                                            
   24 CONTINUE                                                                            
      GO TO 21                                                                            
   20 CONTINUE                                                                            
      ENDFILE 11                                                                          
      CALL EOFW(11,KSEQ)                                                                  
      REWIND 11                                                                           
      STOP                                                                                
      END                                                                                 
