      PROGRAM LIST(TAPE10=/1000                                                           
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN N.J. 1977                                                                   
C-----CHARM TO PRINT-FORMATTED CONVERSION . SEQ NOS LISTED                                
C-----LISTS FILE ON OUTPUT. REQUIRES USER-PROVIDED FORMAT.                                
      DIMENSION LIT(100)                                                                  
      DIMENSION IFM(8)                                                                    
      DIMENSION ITM(8),KTM(8)                                                             
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON/CONST/ KONST(64)                                                             
      COMMON/NXST/IFM,LALEN,IPOS,ILNG,ITYP,IEND,KTM                                       
      CALL DEFINE                                                                         
      LALEN=MACHCC                                                                        
      CALL MCHL(46,LSTAR,1)                                                               
      CALL MCH(40,LSTAR,1)                                                                
      WRITE(LP,10)                                                                        
   10 FORMAT(9H0LIST RUN)                                                                 
      REWIND 10                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      CALL FMGET(IFM)                                                                     
      IEND=0                                                                              
      IPOS=1                                                                              
      IF(NXSC(ITM,1,LSTAR,1,1,0)) 5,6,5                                                   
    6 CALL RASTER(IFM,LP)                                                                 
      CALL COPYC(IFM,2,IFM,1,MACHCC)                                                      
    5 CONTINUE                                                                            
C-----                                                                                    
      LAST=LRECW+1                                                                        
      K=0                                                                                 
    2 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 3,4,999                                                                   
    4 K=K+1                                                                               
      WRITE(LP,IFM) LIT(LAST), (LIT(I),I=1,LRECW)                                         
      GO TO 2                                                                             
    3 CALL EOFR(10,K)                                                                     
      CALL EOFW(LP,K)                                                                     
      REWIND 10                                                                           
      STOP                                                                                
  999 CALL LOGIC(4HLIST)                                                                  
      STOP                                                                                
      END                                                                                 
