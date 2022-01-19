      FUNCTION LINO(LA,LENW,LSTAR,IPOS,KEEP)                                              
C-----WRITTEN N.J. 1977                                                                   
C-----DECODES LINE NO FROM DIRECTIVE CARDS FIR PGM EDITR                                  
      DIMENSION LA(1)                                                                     
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C-----RETURNS -1 IF DATA CARD                                                             
C-----         0 IF UNRECOGNIZABLE                                                        
C-----        +1 IF DIRECTIVE                                                             
C-----KEEP = LINE NO OF LAST DIRECTIVE ON CALL = LA(9) ON RETURN                          
C-----                                                                                    
C-----TEST FOR *                                                                          
      IF(IGETLB(LA,1,1,LENW)-LSTAR) 2,1,2                                                 
C-----DIRECTIVE CARD                                                                      
    1 LINO=1                                                                              
C-----FIND AND TEST LINE NO                                                               
      IF(NEXITM(KEEP,LA,IPOS)) 4,4,3                                                      
C-----LINE NO ALPH OR SPECIAL\                                                            
    4 LINO=0                                                                              
C-----LINE NO NUMERIC                                                                     
    3 LA(MACHCD+1)=KEEP                                                                   
      RETURN                                                                              
C-----DATA CARD                                                                           
    2 LINO=-1                                                                             
C-----SET LINE NO EQ TO LAST DIRECTIVE                                                    
      GO TO 3                                                                             
      END                                                                                 
