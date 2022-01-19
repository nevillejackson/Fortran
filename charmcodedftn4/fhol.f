      SUBROUTINE FHOL(WORD,LEN,NDEC)                                                      
C-----WRITTEN N.J. 1977                                                                   
C-----INSERTS A PERIOD IN A HOLLERITH WORD FOR PRINTING A CODED NUMBER                    
C-----     IN FLOATING POINT FORM                                                         
C-----WORD MUST BE RIGHT JUSTIFIED AND BLANK FILLED                                       
C-----DOES NOT INSERT PERIOD IF LAST CHAR IN WORD NON-NUMERIC                             
C-----                       OR IF LEN = MACHC                                            
C-----                       OR IF NDEC = ZERO                                            
C-----STRIPS LEADING ZEROS                                                                
C-----      EXCEPT WHEN LAST CHAR NON-NUMERIC                                             
      INTEGER WORD                                                                        
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      IF(NDEC) 5,7,7                                                                      
    7 CONTINUE                                                                            
      IF(ITYPE(IN(WORD,MACHC))-2) 5,2,5                                                   
    2 IB=MACHC-LEN+1                                                                      
      IF(IB-1) 1,1,3                                                                      
    3 IL=LEN-NDEC                                                                         
      IF(IL-MACHC) 4,1,1                                                                  
    4 IF(NDEC) 5,1,6                                                                      
    6 CALL COPYC(WORD,IB,WORD,IB-1,IL)                                                    
      ID=MACHC-NDEC                                                                       
      CALL OUT(WORD,ID,IN(KONST(48),MACHC))                                               
    1 CALL LZSTRP(WORD,IB-1,LEN+1)                                                        
    5 RETURN                                                                              
      END                                                                                 
