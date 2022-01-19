      FUNCTION IGET(KARD,KLEN,KBEG,KMAX)                                                  
C-----WRITTEN N.J. 1977                                                                   
      DIMENSION KARD(1)                                                                   
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C-----RETURNS KLEN CHARACTERS FROM A CARD OR LINE IMAGE IN ARRAY KARD                     
C-----STARTING AT THE KBEG TH CHARACTER AND RETURNS AS AN INTEGER WORD                    
C-----ENTRY POINTS SELECT LEFT OR RIGHT JUSTIFIED , HOLLERITH BLANK OR                    
C-----BINARY ZERO FILL , IN INTEGER WORD RETURNED                                         
C-----KMAX IS MAX LENGTH OF CARD OR LINE IN MACHC CHARACTER WORDS                         
C-----MAX VALUE OF KLEN IS MACHC CHARACTERS                                               
C-----                                                                                    
C-----LEFT BLANK                                                                          
      ENTRY IGETLB                                                                        
      IJUST=-1                                                                            
      IFILL=-1                                                                            
      GO TO 1                                                                             
C-----LEFT ZERO                                                                           
      ENTRY IGETLZ                                                                        
      IJUST=-1                                                                            
      IFILL=+1                                                                            
      GO TO 1                                                                             
C-----RIGHT BLANK                                                                         
      ENTRY IGETRB                                                                        
      IJUST=+1                                                                            
      IFILL=-1                                                                            
      GO TO 1                                                                             
C-----RIGHT ZERO                                                                          
      ENTRY IGETRZ                                                                        
      IJUST=+1                                                                            
      IFILL=+1                                                                            
C-----CHECK BOUNDS ERRORS                                                                 
    1 KEND=KBEG+KLEN-1                                                                    
      IF(KLEN-MACHC) 21,21,3                                                              
   21 IF(KBEG-1) 3,22,22                                                                  
   22 IF(KEND-KMAX*MACHC) 2,2,3                                                           
    3 WRITE(LP,4)                                                                         
    4 FORMAT(21H0BOUNDS ERROR IN IGET)                                                    
      CALL JOBEND                                                                         
C-----FIND WORD(S) CONTAINING COLS KBEG TO KEND                                           
    2 K1=(KBEG-1)/MACHC+1                                                                 
      K2=(KEND-1)/MACHC+1                                                                 
C-----CONSTRUCT BLANK FILLED WORD                                                         
      CALL MCHR(46,MBLK,MACHC)                                                            
C-----ONE OR TWO WORD PROBLEM \                                                           
      IF(K1-K2) 5,6,7                                                                     
C-----LOGIC ERROR                                                                         
    7 WRITE(LP,8)                                                                         
    8 FORMAT(20H0LOGIC ERROR IN IGET)                                                     
      CALL LOGIC(4HIGET)                                                                  
C-----ONE WORD PROBLEM                                                                    
    6 KOPY=KARD(K1)                                                                       
      M1=MOD(KBEG,MACHC)                                                                  
      M2=MOD(KEND,MACHC)                                                                  
      IF(M1.EQ.0) M1=MACHC                                                                
      IF(M2.EQ.0) M2=MACHC                                                                
C-----LEFT OR RIGHT JUSTIFIED \                                                           
      IF(IJUST) 9,7,10                                                                    
C-----LEFT JUSTIFIED                                                                      
    9 M3=MACHB*(M1-1)                                                                     
      KOPY=SHIFT(KOPY,M3)                                                                 
C-----MAKE MASK OF KLEN*MACHB BITS SET TO ONE AT LEFT OF WORD                             
      M4=KLEN*MACHB                                                                       
      MSKL=MASK(M4)                                                                       
C-----LOGICAL PRODUCT MASKS RIGHTMOST BITS TO ZERO                                        
      KOPY=AND(MSKL,KOPY)                                                                 
C-----BLANK OR BINARY ZERO FILL \                                                         
      IF(IFILL) 11,7,12                                                                   
C-----MAKE MASK OG (MACHC-KLEN) BLANKS                                                    
   11 MSKB=AND(MBLK,COMPL(MSKL))                                                          
C-----LOGICAL SUM CONVERTS RIGHTMOST BINARY ZERO BITS TO HOLLERITH BLANKS                 
      KOPY=OR(MSKB,KOPY)                                                                  
   12 IGET=KOPY                                                                           
      GO TO 100                                                                           
C-----RIGHT JUSTIFIED                                                                     
   10 M3=MACHB*(M2-MACHC)                                                                 
      KOPY=SHIFT(KOPY,M3)                                                                 
C-----MAKE MASK OF KLEN*MACHB BITS SET TO ONE AT RIGHT OF WORD                            
      M4=MACHB*MACHC-(KLEN*MACHB)                                                         
      MSKL=MASK(M4)                                                                       
      MSKR=COMPL(MSKL)                                                                    
C-----LOGICAL PRODUCT MASKS LEFTMOST BITS TO ZERO                                         
      KOPY=AND(MSKR,KOPY)                                                                 
C-----BLANK OR ZERO FILL \                                                                
      IF(IFILL) 13,7,14                                                                   
C-----BLANK FILL                                                                          
C-----MAKE MASK OF (MACHC-KLEN) BLANKS                                                    
   13 MSKB=AND(MBLK,COMPL(MSKR))                                                          
C-----LOGICAL SUM CONVERTS LEFTMOST BINARY ZERO BITS TO HOLLERITH BLANKS                  
      KOPY=OR(MSKB,KOPY)                                                                  
   14 IGET=KOPY                                                                           
      GO TO 100                                                                           
C-----TWO WORD PROBLEM                                                                    
    5 KOPY=KARD(K1)                                                                       
      KOPZ=KARD(K2)                                                                       
      M1=MOD(KBEG,MACHC)                                                                  
      M2=MACHC                                                                            
      M3=1                                                                                
      M4=MOD(KEND,MACHC)                                                                  
      IF(M1.EQ.0) M1=MACHC                                                                
      IF(M2.EQ.0) M2=MACHC                                                                
C-----FIRST WORD OF TWO WORD PROBLEM                                                      
C-----ALREADY RIGHT JUSTIFIED                                                             
C-----MAKE MASK OF M2-M1+1 BITS SET TO ONE AT RIGHT OF WORD                               
      M5=MACHB*MACHC-(M2-M1+1)*MACHB                                                      
      MSKL=MASK(M5)                                                                       
      MSKR=COMPL(MSKL)                                                                    
C-----LOGICAL PRODUCT MASKS LEFTMOST BITS TO ZERO                                         
      KOPY=AND(MSKR,KOPY)                                                                 
C-----SECOND WORD OF TWO WORD PROBLEM                                                     
C-----RIGHT JUSTIFY BEFORE MASKING                                                        
      M5=MACHB*(M4-MACHC)                                                                 
      KOPZ=SHIFT(KOPZ,M5)                                                                 
C-----MAKE MASK OF M4-M3+1 BITS SET TO ONE AT RIGHT OF WORD                               
      M6=MACHB*MACHC-(M4-M3+1)*MACHB                                                      
      MSKL=MASK(M6)                                                                       
      MSKR=COMPL(MSKL)                                                                    
C-----LOGICAL PRODUCT MASKS LEFTMOST BITS TO ZERO                                         
      KOPZ=AND(MSKR,KOPZ)                                                                 
C-----COMBINE TWO WORDS                                                                   
C-----LEFT SHIFT UPPER WORD BY LENGTH OF LOWER WORD                                       
      KOPY=SHIFT(KOPY,MACHB*(M4-M3+1))                                                    
C-----LOGICAL SUM ADDS UPPER TO LOWER WORD                                                
      KOPY=OR(KOPY,KOPZ)                                                                  
C-----TEST TYPE OF JUSTIFY AND FILL                                                       
C-----LEFT OR RIGHT JUSTIFY \                                                             
      IF(IJUST) 15,7,16                                                                   
C-----LEFT JUSTIFY                                                                        
   15 M5=-MACHB*(M2-M1+1+M4-M3+1)+MACHB*MACHC                                             
      KOPY=SHIFT(KOPY,M5)                                                                 
C-----OTHER BITS ALREADY ZERO HERE                                                        
C-----BLANK OR ZERO FILL \                                                                
      IF(IFILL) 17,7,18                                                                   
C-----BLANK FILL                                                                          
C-----MAKE MASK OF (MACHC-KLEN) BLANKS                                                    
   17 MSKL=MASK(KLEN*MACHB)                                                               
      MSKB=AND(MBLK,COMPL(MSKL))                                                          
C-----LOGICAL SUM CONVERTS RIGHTMOST BIN ZERO BITS TO HOLLERITH BLANKS                    
      KOPY=OR(MSKB,KOPY)                                                                  
C-----ZERO FILL                                                                           
   18 IGET=KOPY                                                                           
      GO TO 100                                                                           
C-----RIGHT JUSTIFY   OK AS IS                                                            
C-----BLANK OR ZERO FILL \                                                                
   16 IF(IFILL) 19,7,20                                                                   
C-----MAKE MASK OF (MACHC-KLEN) BLANKS                                                    
   19 MSKL=MASK(MACHB*MACHC-KLEN*MACHB)                                                   
      MSKR=COMPL(MSKL)                                                                    
      MSKB=AND(MBLK,COMPL(MSKR))                                                          
C-----LOGICAL SUM CONVERTS LEFTMOST BINARY ZERO BITS TO HOLLERITH BLANKS                  
      KOPY=OR(MSKB,KOPY)                                                                  
C-----ZERO FILL                                                                           
   20 IGET=KOPY                                                                           
  100 RETURN                                                                              
      END                                                                                 
