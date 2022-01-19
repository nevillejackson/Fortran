      FUNCTION NEXITM(ITEM,KARD,IPOS)                                                     
C-----WRITTEN N.J. 1977                                                                   
      DIMENSION KARD(1)                                                                   
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C-----RETURNS -VE FOR ALPHA STRING ( LEFT JUST , BLANK FILL)                              
C-----          0 FOR SPECIAL CHAR (LEFT JUST, BLANK FILL 1 CHAR )                        
C-----                OR NO MORE ITEMS                                                    
C-----        +VE FOR INTEGER ( DECODED)                                                  
      JPOS=1                                                                              
      CALL SETBLK(ITM,0,MACHC)                                                            
  100 ICH=IN(KARD,IPOS)                                                                   
      IF(JPOS-1) 999,20,30                                                                
C-----FIRST CHARACTER                                                                     
   20 ITY=ITYPE(ICH)                                                                      
      GO TO (1,2,3,4,5,6,7),ITY                                                           
C-----ALPH A-Z                                                                            
    1 NEXITM=-1                                                                           
C-----STORE THIS CHAR                                                                     
   10 CALL OUT(ITM,JPOS,ICH)                                                              
      IPOS=IPOS+1                                                                         
      IF(IPOS-80) 60,60,50                                                                
   60 JPOS=JPOS+1                                                                         
      IF(JPOS-MACHC) 40,40,90                                                             
   40 GO TO 100                                                                           
C-----NUMERIC 0-9                                                                         
    2 NEXITM=+1                                                                           
      GO TO 10                                                                            
C-----SPECIAL                                                                             
    3 GO TO 7                                                                             
    4 GO TO 7                                                                             
    5 GO TO 7                                                                             
    7 NEXITM=0                                                                            
      CALL OUT(ITM,JPOS,ICH)                                                              
      IPOS=IPOS+1                                                                         
      GO TO 200                                                                           
C-----BLANK                                                                               
    6 IPOS=IPOS+1                                                                         
      IF(IPOS-80) 100,100,50                                                              
C-----NOT FIRST CHARACTER                                                                 
C-----END STRING BY TEST OF CHANGE OF TYPE                                                
C-----NUM WITHIN WORDS ALLOWED                                                            
   30 JTY=ITYPE(ICH)                                                                      
      GO TO (11,12),ITY                                                                   
C-----ALPH A-Z                                                                            
   11 GO TO (21,21,22,22,22,22,22),JTY                                                    
C-----NUM 0-9                                                                             
   12 GO TO (23,21,23,23,23,23,23),JTY                                                    
C-----CARRY ON                                                                            
   21 GO TO 10                                                                            
C-----TERMINATE ALPHA                                                                     
   22 GO TO 200                                                                           
C-----TERMINATE NUMERIC -- RIGHT SHIFT                                                    
   23 JPOS=JPOS-1                                                                         
      DO 70 I=1,JPOS                                                                      
   70 CALL OUT(ITM,MACHC-I+1,IN(ITM,JPOS-I+1))                                            
C-----BLANK FILL FROM LEFT                                                                
      JREM=MACHC-JPOS                                                                     
      IF(JREM) 999,72,73                                                                  
   73 CALL SETBLK(ITM,0,JREM)                                                             
C-----DECODE TO I10                                                                       
   72 ITM=INUM(ITM)                                                                       
      GO TO 200                                                                           
C-----ABNORMAL TERM                                                                       
   50 IPOS=IPOS-1                                                                         
   90 GO TO (22,23,999,999,999,24,999),ITY                                                
   24 NEXITM=0                                                                            
      ITEM=10HCOLUMN 80                                                                   
      RETURN                                                                              
C-----                                                                                    
  999 CALL LOGIC(6HNEXITM)                                                                
C-----                                                                                    
  200 ITEM=ITM                                                                            
      RETURN                                                                              
      END                                                                                 
