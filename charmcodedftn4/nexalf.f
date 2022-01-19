      FUNCTION NEXALF(ITEM,KARD,IPOS)                                                     
C-----WRITTEN N.J. 1977                                                                   
      DIMENSION KARD(1)                                                                   
C-----RETURNS -VE FOR ALPHA-NUM OR NUM STRING , NOT ENCLOSED                              
C-----          0 FOR SPECIAL CHAR ( OTHER THAN $ )                                       
C-----        +VE FOR STRING ENCLOSED IN $-$                                              
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      LIMIT=IN(KONST(44),MACHC)                                                           
C-----                                                                                    
      JPOS=1                                                                              
      ITY=1                                                                               
      CALL SETBLK(ITM,0,MACHC)                                                            
  100 ICH=IN(KARD,IPOS)                                                                   
      IF(JPOS-1) 999,20,30                                                                
C-----                                                                                    
C-----FIRST CHARACTER                                                                     
   20 IF(ITY-8) 120,30,999                                                                
  120 ITY=ITYPE(ICH)                                                                      
      GO TO (1,2,3,4,5,6,7),ITY                                                           
C-----ALPH OR NUM                                                                         
    1 GO TO 2                                                                             
    2 NEXALF=-1                                                                           
C-----STORE THIS CHAR                                                                     
   10 CALL OUT(ITM,JPOS,ICH)                                                              
      IPOS=IPOS+1                                                                         
      IF(IPOS-80) 60,60,50                                                                
   60 JPOS=JPOS+1                                                                         
      IF(JPOS-MACHC) 40,40,90                                                             
   40 GO TO 100                                                                           
C-----SPECIAL +,-,=                                                                       
    3 GO TO 5                                                                             
    4 GO TO 5                                                                             
    5 NEXALF=0                                                                            
      CALL OUT(ITM,JPOS,ICH)                                                              
      IPOS=IPOS+1                                                                         
      GO TO 200                                                                           
C-----BLANK                                                                               
    6 IPOS=IPOS+1                                                                         
      IF(IPOS-80) 100,100,50                                                              
C-----SPECIAL UNRECOGNIZED                                                                
    7 IF(ICH-LIMIT) 5,8,5                                                                 
C-----$ STRING - DONT STORE $                                                             
    8 IPOS=IPOS+1                                                                         
      ITY=8                                                                               
      NEXALF=+1                                                                           
      IF(IPOS-80) 100,100,50                                                              
C-----                                                                                    
C-----NOT FIRST CHARACTER                                                                 
C-----END STRING BY TEST OF CHANGE OF TYPE OR $                                           
   30 JTY=ITYPE(ICH)                                                                      
      IF(ICH-LIMIT) 31,32,31                                                              
   32 JTY=8                                                                               
   31 GO TO (11,11,999,999,999,999,999,12),ITY                                            
C-----ALPHA - NUM STRING                                                                  
   11 GO TO (21,21,22,22,22,22,22,22),JTY                                                 
C----- $ STRING                                                                           
   12 GO TO (21,21,21,21,21,21,21,23),JTY                                                 
C-----CARRY ON                                                                            
   21 GO TO 10                                                                            
C-----TERMINATE ALPHA-NUM STRING                                                          
   22 GO TO 200                                                                           
C-----TERMINATE $ STRING                                                                  
   23 IPOS=IPOS+1                                                                         
      GO TO 200                                                                           
C-----                                                                                    
C-----ABNORMAL TERM                                                                       
   50 IPOS=IPOS-1                                                                         
   90 GO TO (22,22,999,999,999,24,999,23),ITY                                             
   24 NEXALF=0                                                                            
      ITEM=10HCOLUMN 80                                                                   
      RETURN                                                                              
C-----NORMAL TERM                                                                         
  200 ITEM=ITM                                                                            
      RETURN                                                                              
C-----                                                                                    
  999 CALL LOGIC(6HNEXALF)                                                                
      END                                                                                 
