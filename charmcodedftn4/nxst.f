      FUNCTION NXST(KARD,KLEN,IPOS,ITM,ILEN,IALF,IEND)                                    
C-----WRITTEN N.J. 1978                                                                   
C-----NEXT STRING FROM A CARD IMAGE ARRAY                                                 
C----SEE FUNCTION NXSTCM FOR DEFINITION OF PARAMETERS                                     
C-----NXST RETURNS ITYP                                                                   
      DIMENSION KARD(1),ITM(1)                                                            
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      IF(IEND) 999,150,151                                                                
  150 CONTINUE                                                                            
C-----                                                                                    
      LIMIT=IN(KONST(44),MACHC)                                                           
      JPOS=1                                                                              
      ITY=1                                                                               
  100 ICH=IN(KARD,IPOS)                                                                   
      IF(JPOS-1) 999,20,30                                                                
C-----                                                                                    
C-----FIRST CHAR                                                                          
   20 IF(ITY-8) 120,30,999                                                                
  120 ITY=ITYPE(ICH)                                                                      
      GO TO (1,2,3,4,5,6,7),ITY                                                           
C-----ALPH A-Z                                                                            
    1 ITYP=-1                                                                             
      GO TO 10                                                                            
C-----NUMERIC                                                                             
    2 IF(IALF) 999,102,202                                                                
  102 ITYP=+1                                                                             
      GO TO 10                                                                            
  202 ITYP=-1                                                                             
      GO TO 10                                                                            
C-----SPECIAL +,-,=                                                                       
    3 GO TO 5                                                                             
    4 GO TO 5                                                                             
    5 ITYP=0                                                                              
      CALL OUT(ITM,JPOS,ICH)                                                              
      IPOS=IPOS+1                                                                         
      JPOS=JPOS+1                                                                         
      GO TO 200                                                                           
C-----BLANK                                                                               
    6 IPOS=IPOS+1                                                                         
      IF(IPOS-KLEN) 100,100,50                                                            
C-----SPECIAL UNRECOGNIZED                                                                
    7 IF(IALF) 999,5,207                                                                  
  207 IF(IJCHCM(ICH,LIMIT)) 5,8,5                                                         
C-----$ STRING - DONT STORE $                                                             
    8 IPOS=IPOS+1                                                                         
      ITY=8                                                                               
      ITYP=+1                                                                             
      IF(IPOS-KLEN) 100,100,50                                                            
C-----                                                                                    
C-----STORE THIS CHAR                                                                     
   10 CALL OUT(ITM,JPOS,ICH)                                                              
      IPOS=IPOS+1                                                                         
      JPOS=JPOS+1                                                                         
      IF(IPOS-KLEN) 60,60,50                                                              
   60 IF(JPOS-KLEN) 100,100,90                                                            
C-----                                                                                    
C-----NOT FIRST CHAR                                                                      
C-----END STRING BY TEST OF CHANGE OF TYPE (OR$ IF IALF=1)                                
C-----NUM WITHIN WORDS ALLOWED                                                            
   30 JTY=ITYPE(ICH)                                                                      
      IF(IALF) 999,130,230                                                                
  130 GO TO (11,12),ITY                                                                   
  230 IF(IJCHCM(ICH,LIMIT)) 31,32,31                                                      
   32 JTY=8                                                                               
   31 GO TO (11,11,999,999,999,999,999,13),ITY                                            
C-----                                                                                    
C-----ALPHA STRING (OR NUM TREATED AS ALPHA )                                             
   11 GO TO (21,21,22,22,22,22,22,22),JTY                                                 
C-----NUM 0-9 ( TREATED AS NUMERIC )                                                      
   12 GO TO (23,21,23,23,23,23,23,23),JTY                                                 
C-----$ STRING                                                                            
   13 GO TO (21,21,21,21,21,21,21,24),JTY                                                 
C-----CARRY ON                                                                            
   21 GO TO 10                                                                            
C-----TERMINATE ALPHA                                                                     
   22 GO TO 200                                                                           
C-----TERMINATE NUMERIC                                                                   
   23 ITM(1)=LNUM(ITM,JPOS-1)                                                             
      GO TO 200                                                                           
C-----TERMINATE $ STRING                                                                  
   24 IPOS=IPOS+1                                                                         
      GO TO 200                                                                           
C-----                                                                                    
C-----ABNORMAL TERMINATION                                                                
   50 IPOS=IPOS-1                                                                         
   90 IF(IALF) 999,91,92                                                                  
   92 GO TO (222,222,999,999,999,25,999,224),ITY                                          
   91 GO TO (222,223,999,999,999,25,999),ITY                                              
C-----ABN.TERM.ALPHA                                                                      
  222 GO TO 25                                                                            
C-----ABN.TERM.NUM.                                                                       
  223 ITM(1)=LNUM(ITM,JPOS-1)                                                             
      GO TO 25                                                                            
C-----ABN.TERM.$ STRING                                                                   
  224 IPOS=IPOS+1                                                                         
      GO TO 25                                                                            
C-----ABN.TERM.BLANK                                                                      
   25 IEND=1                                                                              
      ILEN=JPOS-1                                                                         
      NXST=0                                                                              
      RETURN                                                                              
C-----                                                                                    
C-----NORMAL TERMINATION                                                                  
  200 IEND=0                                                                              
      ILEN=JPOS-1                                                                         
      NXST=ITYP                                                                           
      RETURN                                                                              
C-----                                                                                    
  151 WRITE(LP,152)                                                                       
  152 FORMAT(41H0ATTEMPT TO READ PAST END OF STRING ARRAY)                                
  999 CALL LOGIC(4HNXST)                                                                  
      RETURN                                                                              
      END                                                                                 
