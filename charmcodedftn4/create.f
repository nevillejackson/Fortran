      PROGRAM CREATE(TAPE10=/1000,TAPE11=/1000                                            
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----CONVERTS CARD IMAGE FILE (TAPE10) TO CHARM-FILE (TAPE11)                            
C-----IF THE SECOND DIRECTIVE (VARIABLE FORMAT) IS MISSING , THEN                         
C-----A FORMATTED READ OCCURS BY CALL TO READUN                                           
C-----  IE. - 'A' FORMAT OR NO CONVERSION - TEXT FILE ASSUMED                             
C-----OTHERWISE , A VARIABLE FORMAT READ OCCURS BY CALL TO READVF                         
      DIMENSION LIT(100)                                                                  
      DIMENSION IFM(8)                                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      CALL DEFINE                                                                         
      WRITE(LP,10)                                                                        
   10 FORMAT(11H0CREATE RUN)                                                              
      REWIND 10                                                                           
      REWIND 11                                                                           
      CALL LENGET(LREC,LRECW)                                                             
      CALL LNWRIT(LREC,LRECW,11)                                                          
      CALL READCD(LC,IFM,IFLAG,MACHCD)                                                    
C-----                                                                                    
      LAST=LRECW+1                                                                        
      K=0                                                                                 
      IF(IFLAG) 5,2,999                                                                   
C-----IF NO FORMAT TREAT TAPE10 AS TEXT OR CARDIMAGE FILE                                 
    5 CONTINUE                                                                            
    6 CALL READUN(10,LIT,JFLAG,LRECW)                                                     
      IF(JFLAG) 3,4,999                                                                   
C-----IF FORMAT , USE IT TO READ TAPE10                                                   
    2 CALL READVF(10,LIT,JFLAG,LRECW,IFM)                                                 
      IF(JFLAG) 3,4,999                                                                   
    4 K=K+1                                                                               
      LIT(LAST)=IHOL(K)                                                                   
      CALL WRITUN(11,LIT,LAST)                                                            
      IF(IFLAG) 6,2,999                                                                   
    3 CALL EOFR(10,K)                                                                     
      ENDFILE 11                                                                          
      REWIND 10                                                                           
      REWIND 11                                                                           
      CALL EOFW(11,K)                                                                     
      STOP                                                                                
  999 CALL LOGIC(6HCREATE)                                                                
      END                                                                                 
