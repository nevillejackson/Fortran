      PROGRAM CALC(TAPE10=/1000,TAPE11=/1000                                              
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----PERFORM SIMPLE ARITHMETIC CALCULATIONS ON SPECIFIED FIELDS OF                       
C-----EVERY RECORD OF A CHARM FILE                                                        
      DIMENSION LIT(100),LOT(100)                                                         
C-----LINK TO SUBROUTINE BXA FOR COMPLEX CALCULATIONS AND EDITING                         
      DIMENSION LA(8)                                                                     
      DIMENSION LOPC(50),CONST(50),LBEG1(50),LEN1(50),NDEC1(50),                          
     1   LBEG2(50),LEN2(50),NDEC2(50)                                                     
      DIMENSION ITM(8),KTM(8),LOP(2),LPC(1),LEO(1)                                        
      DIMENSION LSTAR(1),LCOM(1),LMIN(1)                                                  
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON/CONST/ KONST(64)                                                             
      COMMON /NXST/ LA,LALEN,IPOS,ILEN,ITYP,IEND,KTM                                      
      COMMON /N/ NA,NB,NC,ND                                                              
      COMMON /NAM/ NAMA(100),NAMB(100),NAMC(100),NAMD(100)                                
      COMMON /BEG/ IBEGA(100),IBEGB(100),IBEGC(100),IBEGD(100)                            
      COMMON /LEN/ LENA(100),LENB(100),LENC(100),LEND(100)                                
      COMMON /DEC/ IDECA(100),IDECB(100)                                                  
      CALL DEFINE                                                                         
      LALEN=MACHCC                                                                        
      CALL MCHL(46,LSTAR,1)                                                               
      CALL MCH(40,LSTAR,1)                                                                
      CALL MCHL(46,LCOM,1)                                                                
      CALL MCH(47,LCOM,1)                                                                 
      CALL MCHL(46,LMIN,1)                                                                
      CALL MCH(39,LMIN,1)                                                                 
      CALL MCHL(46,LOP,20)                                                                
      CALL MCH(30,LOP,20)                                                                 
      CALL MCH(09,LOP,20)                                                                 
      CALL MCH(04,LOP,20)                                                                 
      CALL MCH(14,LOP,20)                                                                 
      CALL MCH(30,LOP,20)                                                                 
      CALL MCH(09,LOP,20)                                                                 
      CALL MCH(04,LOP,20)                                                                 
      CALL MCH(02,LOP,20)                                                                 
      CALL MCH(29,LOP,20)                                                                 
      CALL MCH(09,LOP,20)                                                                 
      CALL MCH(02,LOP,20)                                                                 
      CALL MCH(29,LOP,20)                                                                 
      CALL MCH(09,LOP,20)                                                                 
      CALL MCH(03,LOP,20)                                                                 
      CALL MCH(29,LOP,20)                                                                 
      CALL MCH(09,LOP,20)                                                                 
      CALL MCH(04,LOP,20)                                                                 
      CALL MCH(29,LOP,20)                                                                 
      CALL MCH(09,LOP,20)                                                                 
      CALL MCH(05,LOP,20)                                                                 
      CALL MCHL(46,LPC,6)                                                                 
      CALL MCH(29,LPC,6)                                                                  
      CALL MCH(09,LPC,6)                                                                  
      CALL MCH(48,LPC,6)                                                                  
      CALL MCH(29,LPC,6)                                                                  
      CALL MCH(09,LPC,6)                                                                  
      CALL MCH(47,LPC,6)                                                                  
      CALL MCHL(46,LEO,6)                                                                 
      CALL MCH(29,LEO,6)                                                                  
      CALL MCH(09,LEO,6)                                                                  
      CALL MCH(06,LEO,6)                                                                  
      CALL MCH(29,LEO,6)                                                                  
      CALL MCH(09,LEO,6)                                                                  
      CALL MCH(16,LEO,6)                                                                  
C-----                                                                                    
      WRITE(LP,10)                                                                        
   10 FORMAT(9H0CALC RUN)                                                                 
      REWIND 10                                                                           
      REWIND 11                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      LAST=LRECW+1                                                                        
      MREC=LREC+1                                                                         
C-----                                                                                    
C-----READ DIRECTIVES                                                                     
C-----                                                                                    
      K=0                                                                                 
C-----K COUNTS  CM AND CA DIRECTIVES                                                      
      NA=0                                                                                
      NB=0                                                                                
      NC=0                                                                                
      ND=0                                                                                
    1 CALL READCD(LC,LA,IFLAG,MACHCD)                                                     
      IF(IFLAG) 3,4,999                                                                   
    4 CALL WRITLN(LP,LA,0,MACHCD)                                                         
C-----                                                                                    
C-----THIS SECTION CRACKS A DIRECTIVE AND STORES ITS CONTENTS                             
C-----                                                                                    
      IPOS=1                                                                              
      K=K+1                                                                               
C-----                                                                                    
      IEND=0                                                                              
C-----                                                                                    
      IF(NXSC(ITM,1,LSTAR,1,1,0)) 5,6,5                                                   
C-----                                                                                    
    5 WRITE(LP,109)                                                                       
  109 FORMAT(23H UNRECOGNIZED DIRECTIVE)                                                  
      CALL JOBEND                                                                         
C-----                                                                                    
    6 I=NXSC(ITM,1,LOP,20,6,1)+1                                                          
      IF(NXSC(ITM,1,LCOM,1,1,0)) 5,7,5                                                    
    7 GO TO (5,110,110,111,111,111,111,999),I                                             
C-----CM OR CA BRANCH                                                                     
  110 LOPC(K)=I-1                                                                         
      IF(NXSC(LWHOL,0,LMIN,1,1,0))12,120,12                                               
   12 IF(ITYP)5,5,14                                                                      
  120 IF(NXS(LWHOL,0)) 5,5,122                                                            
  122 ISGN=-1                                                                             
      GO TO 13                                                                            
   14 ISGN=1                                                                              
   13 I=NXSC(ITM,1,LPC,6,2,1)+1                                                           
      GO TO (5,16,18,999),I                                                               
   18 CONST(K)=LWHOL*ISGN                                                                 
      GO TO 17                                                                            
   16 IF(NXSC(LFRAC,0,LCOM,1,1,0)) 19,18,19                                               
   19 IF(ITYP) 5,5,20                                                                     
   20 DECML=FRACT(LFRAC,LDEC)                                                             
      DECML=DECML/10.0**(ILEN-LDEC)                                                       
      CONST(K)=(LWHOL+DECML)*ISGN                                                         
      IF(NXSC(ITM,1,LCOM,1,1,0)) 5,17,5                                                   
C-----                                                                                    
   17 IF(NXS(LBEG1(K),0)) 5,5,21                                                          
   21 IF(NXSC(ITM,1,LCOM,1,1,0)) 5,23,5                                                   
   23 IF(NXS(LEN1(K),0)) 5,5,24                                                           
   24 IF(NXSC(ITM,1,LCOM,1,1,0)) 5,26,5                                                   
   26 IF(NXS(NDEC1(K),0)) 5,5,27                                                          
   27 IF(NXSC(ITM,1,LCOM,1,1,0)) 5,29,5                                                   
   29 I=NXSC(LBEG2(K),0,LEO,6,2,1) +1                                                     
      GO TO (31,32,33,999),I                                                              
   31 IF(ITYP) 5,5,32                                                                     
   32 IF(NXSC(ITM,1,LCOM,1,1,0)) 5,35,5                                                   
   35 IF(NXS(LEN2(K),0)) 5,5,33                                                           
   33 IF(NXSC(ITM,1,LCOM,1,1,0)) 5,37,5                                                   
   37 IF(NXS(NDEC2(K),0)) 5,5,38                                                          
C-----                                                                                    
   38 GO TO (233,231,232,999),I                                                           
  233 IF(LBEG2(K)+LEN2(K)-MREC) 1,1,234                                                   
  234 MREC=LBEG2(K)+LEN2(K)                                                               
      GO TO 1                                                                             
  231 LBEG2(K)=MREC                                                                       
      MREC=MREC+LEN2(K)                                                                   
      GO TO 1                                                                             
  232 LBEG2(K)=LBEG1(K)                                                                   
      LEN2(K)=LEN1(K)                                                                     
      GO TO 1                                                                             
C-----CALSUB BRANCH                                                                       
  111 K=K-1                                                                               
      L=I-2                                                                               
      GO TO (5,41,42,43,44,999),L                                                         
C-----A  FLOATING INPUT                                                                   
   41 NA=NA+1                                                                             
      IF(NXS(NAMA(NA),1))50,5,50                                                          
   50 IF(NXSC(ITM,1,LCOM,1,1,0))5,51,5                                                    
   51 IF(NXS(IBEGA(NA),0))5,5,52                                                          
   52 IF(NXSC(ITM,1,LCOM,1,1,0))5,53,5                                                    
   53 IF(NXS(LENA(NA),0))5,5,54                                                           
   54 IF(NXSC(ITM,1,LCOM,1,1,0))5,55,5                                                    
   55 IF(NXS(IDECA(NA),0))5,5,45                                                          
C-----B  FLOATING OUTPUT                                                                  
   42 NB=NB+1                                                                             
      IF(NXS(NAMB(NB),1))60,5,60                                                          
   60 IF(NXSC(ITM,1,LCOM,1,1,0))5,61,5                                                    
   61 IF(NXS(IBEGB(NB),0))5,5,62                                                          
   62 IF(NXSC(ITM,1,LCOM,1,1,0))5,63,5                                                    
   63 IF(NXS(LENB(NB),0))5,5,64                                                           
   64 IF(NXSC(ITM,1,LCOM,1,1,0))5,65,5                                                    
   65 IF(NXS(IDECB(NB),0))5,5,45                                                          
C-----C  HOLLERITH INPUT                                                                  
   43 NC=NC+1                                                                             
      IF(NXS(NAMC(NC),1))70,5,70                                                          
   70 IF(NXSC(ITM,1,LCOM,1,1,0))5,71,5                                                    
   71 IF(NXS(IBEGC(NC),0))5,5,72                                                          
   72 IF(NXSC(ITM,1,LCOM,1,1,0))5,73,5                                                    
   73 IF(NXS(LENC(NC),0))5,5,45                                                           
C-----D  HOLLERITH OUTPUT                                                                 
   44 ND=ND+1                                                                             
      IF(NXS(NAMD(ND),1))80,5,80                                                          
   80 IF(NXSC(ITM,1,LCOM,1,1,0))5,81,5                                                    
   81 IF(NXS(IBEGD(ND),0))5,5,82                                                          
   82 IF(NXSC(ITM,1,LCOM,1,1,0))5,83,5                                                    
   83 IF(NXS(LEND(ND),0))5,5,45                                                           
C-----                                                                                    
   45 GO TO 1                                                                             
C-----                                                                                    
C-----                                                                                    
C-----READ RECORDS AND PROCESS DIRECTIVES FOR EACH RECORD                                 
C-----                                                                                    
C-----INITIALIZE                                                                          
    3 K10=0                                                                               
      K11=0                                                                               
      MREC=MREC-1                                                                         
      MRECW=LWORDS(MREC)                                                                  
      CALL CSINIT(LOT,MREC,MRECW)                                                         
      MAST=MRECW+1                                                                        
      CALL LNWRIT(MREC,MRECW,11)                                                          
      WRITE(LP,180) MREC,MRECW                                                            
  180 FORMAT(21H NEW RECORD LENGTH = ,I5,14H CHARACTERS = ,I5,6H WORDS)                   
C-----CHECK AT LEAST ONE DIRECTIVE PRESENT                                                
      IF(K+NA+NB+NC+ND)999,140,130                                                        
  140 WRITE(LP,141)                                                                       
  141 FORMAT(32H NO DIRECTIVES -- RUN TERMINATED)                                         
      CALL JOBEND                                                                         
C-----                                                                                    
C-----ERRORS                                                                              
  999 CALL LOGIC(4HCALC)                                                                  
      STOP                                                                                
C-----                                                                                    
C-----LOOP OVER RECORDS                                                                   
  130 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 133,134,999                                                               
  134 K10=K10+1                                                                           
C-----INITIALIZE LOT                                                                      
      DO 149 I=1,LRECW                                                                    
  149 LOT(I)=LIT(I)                                                                       
      CALL SETBLK(LOT,LREC,MRECW*MACHC)                                                   
C-----LOOP OVER CA AND CM DIRECTIVES FOR THIS RECORD                                      
      IF(K)999,200,148                                                                    
  148 DO 150 KK=1,K                                                                       
C-----CM OR CA BRANCH                                                                     
  151 CALL COPYC(LIT,LBEG1(KK),ITM,1,LEN1(KK))                                            
      IF(ITYPE(IN(ITM,LEN1(KK)))-2) 160,161,160                                           
  160 LDIF=LEN1(KK)-LEN2(KK)                                                              
      IF(LDIF) 162,163,164                                                                
  162 IDIF=0                                                                              
      GO TO 165                                                                           
  163 IDIF=0                                                                              
      LDIF=0                                                                              
      GO TO 165                                                                           
  164 IDIF=LDIF                                                                           
      LDIF=0                                                                              
      GO TO 165                                                                           
  165 CALL COPYC(ITM,1+IDIF,LOT,LBEG2(KK)-LDIF,LEN2(KK)+LDIF)                             
      GO TO 150                                                                           
  161 KIT=LNUM(ITM,LEN1(KK))                                                              
      FIT=FXI(KIT,NDEC1(KK))                                                              
      IF(LOPC(KK)-2) 152,154,999                                                          
  154 FOT=FIT+CONST(KK)                                                                   
      GO TO 155                                                                           
  152 FOT=FIT*CONST(KK)                                                                   
  155 KOT=IXF(FOT,NDEC2(KK))                                                              
      CALL LHOLZ(KOT,ITM,LEN2(KK))                                                        
      CALL COPYC(ITM,1,LOT,LBEG2(KK),LEN2(KK))                                            
  150 CONTINUE                                                                            
C-----LOOP OVER BXA DIRECTIVES FOR THIS RECORD                                            
  200 IF(NA+NB+NC+ND)999,201,147                                                          
  147 CALL CALSUB(LOT,MREC,MRECW)                                                         
  201 CONTINUE                                                                            
C-----                                                                                    
      LOT(MAST)=LIT(LAST)                                                                 
      CALL WRITUN(11,LOT,MAST)                                                            
      K11=K11+1                                                                           
      GO TO 130                                                                           
C-----                                                                                    
C-----END RUN                                                                             
  133 CALL EOFR(10,K10)                                                                   
      REWIND 10                                                                           
      ENDFILE 11                                                                          
      CALL EOFW(11,K11)                                                                   
      REWIND 11                                                                           
      STOP                                                                                
      END                                                                                 
