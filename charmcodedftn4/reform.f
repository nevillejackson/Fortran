      PROGRAM REFORM(TAPE10=/1000,TAPE11=/1000                                            
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN R.E. 1977                                                                   
C-----                                                                                    
C-----REFORMS CHARM FILE RECORDS FROM TAPE10 TO TAPE11                                    
C-----ALL FIELDS TO BE TRANSFERRED MUST BE SPECIFIED. I.E. ONLY FIELDS                    
C-----SPECIFIED ON DIRECTIVES WILL BE WRITTEN ON TAPE11.                                  
C-----DIRECTIVES AVAILABLE ARE ...                                                        
C          *C,NBEG,IBEG,LEN   --- COPY                                                    
C          *E,NBEG,$.....$    --- EMIT                                                    
C-----MAXIMUM OF 200 DIRECTIVES.FIELDS TO BE COPIED FROM TAPE10 CAN                       
C-----BE ANY LENGTH UP TO MAX RECORD LENGTH (1000 CHARACTERS)                             
      DIMENSION LITI(100),LITO(100)                                                       
      DIMENSION IDIR(8),IPAR(200,4),IVAL(1000),IS(200)                                    
      DIMENSION LOP(1),LCOM(1),LSTAR(1)                                                   
      DIMENSION ITM(8),KTM(8)                                                             
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      COMMON /NXST/ IDIR,IDLEN,IPOS,ILEN,ITYP,IEND,KTM                                    
      CALL DEFINE                                                                         
      IDLEN=MACHCC                                                                        
      CALL MCHL(46,LSTAR,1)                                                               
      CALL MCH(40,LSTAR,1)                                                                
      CALL MCHL(46,LCOM,1)                                                                
      CALL MCH(47,LCOM,1)                                                                 
      CALL MCHL(46,LOP,6)                                                                 
      CALL MCH (29,LOP,6)                                                                 
      CALL MCH (09,LOP,6)                                                                 
      CALL MCH (04,LOP,6)                                                                 
      CALL MCH (29,LOP,6)                                                                 
      CALL MCH (09,LOP,6)                                                                 
      CALL MCH (06,LOP,6)                                                                 
C-----                                                                                    
      WRITE(LP,10)                                                                        
   10 FORMAT(11H0REFORM RUN)                                                              
      REWIND 10                                                                           
      REWIND 11                                                                           
      CALL LNREAD(LRECI,LRECIW,10)                                                        
      LASTI=LRECIW+1                                                                      
C-----READ, COUNT AND CRACK DIRECTIVES. SET UP PARAMETER ARRAY TO                         
C-----CONTROL RE-FORMATTING.                                                              
      K=0                                                                                 
      LMAX=1                                                                              
      IS(1)=1                                                                             
      NEM=0                                                                               
  100 CALL READCD(LC,IDIR,IFLAG,MACHCD)                                                   
      IF(IFLAG) 200,11,999                                                                
   11 CALL WRITLN(LP,IDIR,0,MACHCD)                                                       
      IPOS=1                                                                              
      K=K+1                                                                               
      IEND=0                                                                              
      IF(NXSC(ITM,1,LSTAR,1,1,0)) 9,13,9                                                  
C-----                                                                                    
    9 WRITE(LP,3)                                                                         
    3 FORMAT(37H INVALID DIRECTIVE --- RUN TERMINATED)                                    
      CALL JOBEND                                                                         
C-----                                                                                    
   13 I=NXSC(ITM,1,LOP,6,2,1)+1                                                           
      IF(NXSC(ITM,1,LCOM,1,1,0)) 9,14,9                                                   
   14 GO TO (9,17,16,999),I                                                               
   16 IPAR(K,4)=1                                                                         
      GO TO 18                                                                            
   17 IPAR(K,4)=0                                                                         
   18 IF(NXS(IPAR(K,3),0)) 9,9,22                                                         
   22 IF(NXSC(ITM,1,LCOM,1,1,0)) 9,26,9                                                   
C-----BRANCH TO SELECTIVELY CRACK *C OR *E                                                
   26 IF(IPAR(K,4))29,23,29                                                               
C-----*C BRANCH                                                                           
   23 IF(NXS(IPAR(K,2),0)) 9,9,25                                                         
   25 IF(NXSC(ITM,1,LCOM,1,1,0)) 9,27,9                                                   
   27 IF(NXS(IPAR(K,1),0)) 9,9,31                                                         
C-----*E BRANCH                                                                           
   29 NEM=NEM+1                                                                           
      IJ=IS(NEM)                                                                          
      CALL DOLSTR(IDIR,IPOS,IVAL,IJ,LEN)                                                  
      IF(LEN)999,30,52                                                                    
   30 WRITE(LP,51)                                                                        
   51 FORMAT(21H N=0  NO VALID STRING)                                                    
      GO TO 9                                                                             
   52 IPAR(K,1)=LEN                                                                       
      IS(NEM+1)=IS(NEM)+LEN                                                               
   31 LTARG=IPAR(K,3)+IPAR(K,1)-1                                                         
      IF(LTARG.GT.LMAX)LMAX=LTARG                                                         
      GO TO 100                                                                           
C-----END OF DIRECTIVE PROCESSING                                                         
  200 IF(K)999,32,33                                                                      
  999 CALL LOGIC(6HREFORM)                                                                
      STOP                                                                                
   32 WRITE(LP,4)                                                                         
    4 FORMAT(33H NO DIRECTIVES --- RUN TERMINATED)                                        
      CALL JOBEND                                                                         
   33 IF(K-1)999,60,50                                                                    
C-----CHECK FOR OVERLAPPING TARGET FIELDS                                                 
   50 DO 150 KK=2,K                                                                       
      LIML=IPAR(KK,3)                                                                     
      LIMR=LIML+IPAR(KK,1)-1                                                              
      KKK=KK-1                                                                            
      DO 149 JK=1,KKK                                                                     
      KIML=IPAR(JK,3)                                                                     
      KIMR=KIML+IPAR(JK,1)-1                                                              
      IF(KIML.GT.LIMR.OR.LIML.GT.KIMR) GO TO 149                                          
  147 WRITE(LP,148) KK,JK                                                                 
  148 FORMAT(48H ***** WARNING *****   TARGET FIELD IN DIRECTIVE,I3,27H                   
     .OVERLAPS THAT IN DIRECTIVE,I3)                                                      
  149 CONTINUE                                                                            
  150 CONTINUE                                                                            
C-----END OVERLAP CHECKING                                                                
C-----INITIALISE RECORD COUNTERS AND BEGIN PROCESSING RECORDS                             
   60 KI=0                                                                                
      KO=0                                                                                
      LRECO=LMAX                                                                          
      CALL LNWRIT(LRECO,LRECOW,11)                                                        
      LASTO=LRECOW+1                                                                      
  500 CALL READUN(10,LITI,IFLAG,LASTI)                                                    
      IF(IFLAG) 250,35,999                                                                
   35 KI=KI+1                                                                             
C-----CLEAR OUTPUT RECORD                                                                 
      CALL SETBLK(LITO,0,LRECOW*MACHC)                                                    
C-----APPLY EACH DIRECTIVE IN TURN                                                        
      NEM=0                                                                               
      DO 300 KK=1,K                                                                       
      LEN=IPAR(KK,1)                                                                      
      NBEG=IPAR(KK,3)                                                                     
C-----BRANCH DEPENDING ON WHETHER *C OR *E                                                
      IF(IPAR(KK,4))40,39,40                                                              
   39 IBEG=IPAR(KK,2)                                                                     
      DO 43 I=1,LEN                                                                       
      J1=IBEG+I-1                                                                         
      J2=NBEG+I-1                                                                         
   43 CALL OUT(LITO,J2,IN(LITI,J1))                                                       
      GO TO 300                                                                           
   40 NEM=NEM+1                                                                           
      JK=IS(NEM)                                                                          
      DO 44 I=1,LEN                                                                       
      J2=NBEG+I-1                                                                         
      J1=JK+I-1                                                                           
   44 CALL OUT(LITO,J2,IN(IVAL,J1))                                                       
  300 CONTINUE                                                                            
      LITO(LASTO)=LITI(LASTI)                                                             
      KO=KO+1                                                                             
      CALL WRITUN(11,LITO,LASTO)                                                          
      GO TO 500                                                                           
C-----END OF RECORD WRITING                                                               
C-----TERMINATE RUN                                                                       
  250 CALL EOFR(10,KI)                                                                    
      CALL EOFW(11,KO)                                                                    
      ENDFILE 11                                                                          
      REWIND 10                                                                           
      REWIND 11                                                                           
      STOP                                                                                
      END                                                                                 
