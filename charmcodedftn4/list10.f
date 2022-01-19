      PROGRAM LIST10(TAPE10=/1000                                                         
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----CHARM TO PRINT-FORMATTED CONVERSION . SEQ NOS LISTED                                
C-----LISTS FIRST NREC RECORDS ON OUTPUT. ASSUMES (10A10) FORMAT.                         
      DIMENSION LIT(100)                                                                  
      DIMENSION IFM(8)                                                                    
      DIMENSION ITM(8),KTM(8)                                                             
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON/CONST/ KONST(64)                                                             
      COMMON/NXST/IFM,LALEN,IPOS,ILNG,ITYP,IEND,KTM                                       
      DATA IFM/32H*(1X,A10,4X,10A10,(/,15X,10A10))/                                       
      DATA NREC/10/                                                                       
      CALL DEFINE                                                                         
      LALEN=MACHCC                                                                        
      CALL MCHL(46,LSTAR,1)                                                               
      CALL MCH(40,LSTAR,1)                                                                
      WRITE(LP,10)                                                                        
   10 FORMAT(11H0LIST10 RUN)                                                              
      REWIND 10                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
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
      IF(K.GT.NREC)GO TO 8                                                                
      WRITE(LP,IFM) LIT(LAST), (LIT(I),I=1,LRECW)                                         
      GO TO 2                                                                             
    3 CALL EOFR(10,K)                                                                     
      GO TO 9                                                                             
    8 K=K-1                                                                               
    9 CALL EOFW(LP,K)                                                                     
      REWIND 10                                                                           
      STOP                                                                                
  999 CALL LOGIC(6HLIST10)                                                                
      STOP                                                                                
      END                                                                                 
