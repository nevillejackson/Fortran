      SUBROUTINE WITCH(LCNAM,LSNAM,KOUNT,TOT,KTOT,KC,KS,ICON,ICHA,LFIELD                  
     1 ,K11,LSDEC,KL,SSQ,K12)                                                             
C-----WRITTEN N.J. 1977                                                                   
C-----HANDLES OUTPUT FOR PROGRAM COUNT                                                    
C-----WRITES OUT TOTALS ON CONTROL CHANGE                                                 
C-----REINITIALIZES COUNTERS AFTER PRINTING                                               
      DIMENSION LCNAM(1),LSNAM(1),KOUNT(1),LFIELD(1)                                      
      DIMENSION TOT(50,1),KTOT(50,1)                                                      
      DIMENSION SSQ(50,1),LINEQ(11)                                                       
      DIMENSION LSDEC(1)                                                                  
      DIMENSION LINE(11),LINES(11),LINEM(11)                                              
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /SUMFMT/ JFM(8),SFSW,LENSF                                                   
      DATA IH1,IH2 /10HCONTROL FI,3HELD/                                                  
      DATA JH1,JH2 /10HRECORD COU,2HNT/                                                   
      DATA ICNT,ITOT,IAVE /4H CNT,4H TOT,4H AVE/                                          
      DATA ICOUNT /10H     COUNT/                                                         
      DATA ISTD /4H  SD/                                                                  
      CALL SETBLK(IBLK,0,MACHC)                                                           
C-----CHOOSE FORMAT                                                                       
      IHED=0                                                                              
   49 IF(KS) 999,50,51                                                                    
C-----                                                                                    
C-----NO SUMS FORMAT                                                                      
C-----REPEAT HEADINGS IF LISTING OR IF NEW PAGE OR AT START                               
   50 IF(KC-6) 58,58,51                                                                   
   58 IF(KL) 999,52,59                                                                    
   59 WRITE(11,47)                                                                        
   47 FORMAT(1H )                                                                         
      GO TO 53                                                                            
   52 IF(NL-59) 54,55,55                                                                  
C-----PAGESKIP                                                                            
   55 WRITE(11,56)                                                                        
   56 FORMAT(1H1)                                                                         
      NL=0                                                                                
C-----HEAD                                                                                
   53 WRITE(11,57)(JRIGHT(LCNAM(I)),ICOUNT,I=1,KC)                                        
   57 FORMAT(1H ,11A11,A10)                                                               
      K11=K11+1                                                                           
      IF(IHED) 999,54,100                                                                 
C-----CONTROL FIELDS AND COUNTS                                                           
   54 IF(ICON) 60,61,61                                                                   
   61 DO 62 IC=1,ICON                                                                     
   62 LINE(IC)=IBLK                                                                       
C-----                                                                                    
   60 DO 63 IC=ICHA,KC                                                                    
      LINE(IC)=IHOL(KOUNT(IC))                                                            
   63 KOUNT(IC)=0                                                                         
C-----                                                                                    
      WRITE(11,57)(LFIELD(I),LINE(I),I=1,KC)                                              
      K11=K11+1                                                                           
      NL=NL+1                                                                             
      IF(ICHA-KC) 70,71,999                                                               
   70 WRITE(11,47)                                                                        
      NL=NL+1                                                                             
   71 CONTINUE                                                                            
C-----                                                                                    
      GO TO 100                                                                           
C-----PRINT ALL HEADINGS AND CONTROL FIELDS                                               
C-----                                                                                    
C-----SUMS FORMAT                                                                         
   51 IF(IHED) 999,48,100                                                                 
   48 CONTINUE                                                                            
      IF(KC-11) 10,10,11                                                                  
   11 WRITE(LP,13)                                                                        
   13 FORMAT(40H TOO MANY CONTROL FIELDS FOR THIS FORMAT)                                 
      CALL JOBEND                                                                         
   10 IF(KC) 14,14,15                                                                     
   14 WRITE(LP,12)                                                                        
   12 FORMAT(18H NO CONTROL FIELDS)                                                       
      CALL JOBEND                                                                         
   15 WRITE(11,1)(JRIGHT(LCNAM(I)),I=1,KC)                                                
    1 FORMAT(1H0,15X,10A11,A10)                                                           
      K11=K11+1                                                                           
C-----SKIP IF NO CONTROL FIELD -- IE ON OVERALL TOTALS                                    
      IF(ICON) 30,31,31                                                                   
   31 WRITE(11,2) IH1,IH2,(LFIELD(I),I=1,KC)                                              
    2 FORMAT(1H ,A10,A4,1X,10A11,A10)                                                     
      K11=K11+1                                                                           
C-----                                                                                    
C-----PAD OUTPUT LINE WITH BLANKS                                                         
      DO 20 IC=1,ICON                                                                     
      LINES(IC)=IBLK                                                                      
      LINEM(IC)=IBLK                                                                      
      LINEQ(IC)=IBLK                                                                      
   20 LINE(IC)=IBLK                                                                       
C-----PRINT RECORD COUNTS                                                                 
   30 DO 21 IC=ICHA,KC                                                                    
      LINE(IC)=IHOL(KOUNT(IC))                                                            
   21 KOUNT(IC)=0                                                                         
      WRITE(11,2)JH1,JH2,(LINE(I),I=1,KC)                                                 
C-----SUMS                                                                                
      IF(KS) 100,100,23                                                                   
   23 DO 24 IS=1,KS                                                                       
      DO 25 IC=ICHA,KC                                                                    
      LINE(IC)=IHOL(KTOT(IC,IS))                                                          
      LINES(IC)=IHOLXF(TOT(IC,IS),LSDEC(IS))                                              
      IF(KTOT(IC,IS)-1) 40,43,41                                                          
   40 LINEM(IC)=IBLK                                                                      
      AVE=0.0                                                                             
      IPRNT=0                                                                             
   43 LINEQ(IC)=IBLK                                                                      
      SD=0.0                                                                              
      IF(KTOT(IC,IS)-1)42,41,41                                                           
   41 KNT=KTOT(IC,IS)                                                                     
      AVE=TOT(IC,IS)/FLOAT(KNT)                                                           
      SUM=TOT(IC,IS)                                                                      
      LINEM(IC)=IHOLXF(AVE,LSDEC(IS)+1)                                                   
      IPRNT=1                                                                             
      IF(KNT-1)42,42,44                                                                   
   44 XTERM=SSQ(IC,IS)-TOT(IC,IS)*TOT(IC,IS)/FLOAT(KNT)                                   
      IF(XTERM)75,75,76                                                                   
   75 SD=0.0                                                                              
      GO TO 77                                                                            
   76 SD=SQRT(XTERM/FLOAT(KNT-1))                                                         
   77 CONTINUE                                                                            
      LINEQ(IC)=IHOLXF(SD,LSDEC(IS)+3)                                                    
   42 KTOT(IC,IS)=0                                                                       
      TOT(IC,IS)=0                                                                        
      SSQ(IC,IS)=0.0                                                                      
   25 CONTINUE                                                                            
      IF(ICON) 65,66,66                                                                   
   66 IF(IPRNT)999,65,69                                                                  
   69 K12=K12+1                                                                           
      IF(SFSW) 65,65,361                                                                  
  361 CONTINUE                                                                            
      WRITE(12,JFM)(LFIELD(I),I=1,KC),IS,KNT,SUM,AVE,SD,K12                               
C-----LFIELD() IS CONTROL FIELD(S)                                                        
C-----ITN      IS TRAIT NO                                                                
C-----KNT      IS COUNT                                                                   
C-----SUM      IS TOTAL                                                                   
C-----AVE      IS MEAN                                                                    
C-----SD       IS STANDARD DEVIATION                                                      
C-----K12      IS SEQUENCE NO                                                             
   65 CONTINUE                                                                            
      WRITE(11,2) LSNAM(IS),ICNT,(LINE(I),I=1,KC)                                         
      WRITE(11,2) IBLK,ITOT,(LINES(I),I=1,KC)                                             
      WRITE(11,2) IBLK,IAVE,(LINEM(I),I=1,KC)                                             
      WRITE(11,2) IBLK,ISTD,(LINEQ(I),I=1,KC)                                             
      K11=K11+4                                                                           
   24 CONTINUE                                                                            
C-----                                                                                    
  100 RETURN                                                                              
      ENTRY HITCH                                                                         
      NL=59                                                                               
      IHED=1                                                                              
      GO TO 49                                                                            
  999 CALL LOGIC(5HWITCH)                                                                 
      STOP                                                                                
      END                                                                                 
