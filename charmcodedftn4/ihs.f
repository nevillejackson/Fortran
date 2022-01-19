      SUBROUTINE IHS(LHSTR,ITM,ILEN,LPOS,LMAX)                                            
      DIMENSION LHSTR(1),ITM(1),LENHOL(1)                                                 
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C-----WRITTEN N.J. 1978                                                                   
C-----INSERTS A STRING INTO A MULTI-ITEM H-STRING AT POSITION LPOS.                       
C-----LENGTH AND H ADDED AUTOMATICALLY.                                                   
C-----LHSTR() - MULTI ITEM STRING OF FORM N1HS1N2HS2.....                                 
C-----ITM() - SINGLE ITEM STRING TO BE INSERTED                                           
C-----ILEN  - LENGTH OF SINGLE ITEM STRING - INTEGER                                      
C-----LPOS  - CHAR POSITION TO INSERT STRING - INCREMENTED ON RETURN                      
C-----LMAX  - UPPER LIMIT OF LPOS - IN CHARS                                              
C-----                                                                                    
      LENLEN=ALOG10(FLOAT(ILEN))+1.                                                       
      CALL LHOL(ILEN,LENHOL,LENLEN)                                                       
      CALL COPYC(LENHOL,1,LHSTR,LPOS,LENLEN)                                              
      LPOS=LPOS+LENLEN                                                                    
      IF(LPOS-LMAX) 1,1,999                                                               
    1 CALL OUT(LHSTR,LPOS,IN(KONST(9),MACHC))                                             
      LPOS=LPOS+1                                                                         
      IF(LPOS-LMAX) 2,2,999                                                               
    2 CALL COPYC(ITM,1,LHSTR,LPOS,ILEN)                                                   
      LPOS=LPOS+ILEN                                                                      
      IF(LPOS-LMAX) 3,3,999                                                               
  999 CALL LOGIC(3HIHS)                                                                   
    3 RETURN                                                                              
      END                                                                                 
