      FUNCTION IPUT(KARD,KLEN,KBEG,KMAX,KOPY)                                             
C-----WRITTEN N.J. 1975                                                                   
      DIMENSION KARD(1)                                                                   
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C----KOPY IS DESTROYED BUT IS PRESERVED IN IPUT                                           
C----- LEFT OR RIGHT JUSTIFIED  -- IGNORE FILL -- SET TO ZERO                             
C-----LEFT                                                                                
      ENTRY IPUTL                                                                         
      IJUST=-1                                                                            
      GO TO 1                                                                             
C-----RIGHT                                                                               
      ENTRY IPUTR                                                                         
      IJUST=+1                                                                            
      GO TO 1                                                                             
C-----CHECK BOUNDS ERRORS                                                                 
    1 IPUT=KOPY                                                                           
      KEND=KBEG+KLEN-1                                                                    
      IF(KLEN-MACHC) 21,21,3                                                              
   21 IF(KBEG-1) 3,22,22                                                                  
   22 IF(KEND-KMAX*MACHC) 2,2,3                                                           
    3 WRITE(LP,4)                                                                         
    4 FORMAT(21H0BOUNDS ERROR IN IPUT)                                                    
      CALL JOBEND                                                                         
C-----FIND WORD(S) CONTAINING COLS KBEG TO KEND                                           
C-----FIRST WORD NO                                                                       
    2 K1=(KBEG-1)/MACHC+1                                                                 
C-----SECOND WORD NO                                                                      
      K2=(KEND-1)/MACHC+1                                                                 
C-----ONE OR TWO WORD PROBLEM\                                                            
      IF(K1-K2) 5,6,7                                                                     
C-----LOGIC ERROR                                                                         
    7 WRITE(LP,8)                                                                         
    8 FORMAT(20H0LOGIC ERROR IN IPUT)                                                     
      CALL LOGIC(4HIPUT)                                                                  
C-----ONE WORD PROBLEM                                                                    
C-----INSERT INTO KARD(K1) IN CHAR POSITIONS M1 TO M2                                     
    6 M1=MOD(KBEG,MACHC)                                                                  
      M2=MOD(KEND,MACHC)                                                                  
      IF(M1.EQ.0) M1=MACHC                                                                
      IF(M2.EQ.0) M2=MACHC                                                                
C-----JUSTIFY KOPY FROM L OR R TO (M1,M2) POSITION                                        
      IF(IJUST) 9,7,10                                                                    
C-----FROM L TO (M1,M2)                                                                   
    9 M3=(1-M1)*MACHB                                                                     
      GO TO 11                                                                            
C-----FROM R TO (M1,M2)                                                                   
   10 M3=(MACHC-M2)*MACHB                                                                 
C-----SHIFT TO (M1,M2) POSITION                                                           
   11 KOPY=SHIFT(KOPY,M3)                                                                 
C-----MASK UNWANTED PARTS OF KOPY TO ZERO                                                 
C-----KLEN CHARACTER MASK                                                                 
      MSK1=MASK((M1-1)*MACHB)                                                             
C-----M2 CHARACTER MASK                                                                   
      MSK2=MASK((M1+KLEN-1)*MACHB)                                                        
C-----LOGICAL PRODUCT OF COMPL MSK1 WITH MSK2                                             
      MSK=AND(COMPL(MSK1),MSK2)                                                           
C-----LOGICAL PRODUCT MASKS KOPY                                                          
      KOPY=AND(KOPY,MSK)                                                                  
C-----MASK KARD(K1) WITH COMPL OF MSK                                                     
      KARD(K1)=AND(KARD(K1),COMPL(MSK))                                                   
C-----INSERT KOPY IN MASKED PART OF KARD(K1) BY LOGICAL OR                                
      KARD(K1)=OR(KARD(K1),KOPY)                                                          
      GO TO 20                                                                            
C-----                                                                                    
C-----TWO WORD PROBLEM                                                                    
C-----INSERT INTO KARD(K1) IN POSITIONS (M1,M2) AND KARD(K2) IN POSITIONS (M3,M4          
    5 M1=MOD(KBEG,MACHC)                                                                  
      M2=MACHC                                                                            
      M3=1                                                                                
      M4=MOD(KEND,MACHC)                                                                  
      IF(M1.EQ.0) M1=MACHC                                                                
      IF(M4.EQ.0) M4=MACHC                                                                
C-----BREAK KOPY INTO TWO WORDS                                                           
C-----M1 TO M2 R JUST AND M3 TO M4 L JUST                                                 
      IF(IJUST) 15,7,16                                                                   
C-----FROM L TO (M1,M2)  R SHIFT -VE                                                      
   15 M5=(1-M1)*MACHB                                                                     
C-----FROM L TO (M3,M4) - L SHIFT +VE                                                     
      M6=(M2-M1+1)*MACHB                                                                  
      GO TO 17                                                                            
C-----                                                                                    
C-----FROM R TO (M1,M2) R SHIFT -VE                                                       
   16 M5=-(M4-M3+1)*MACHB                                                                 
C-----FROM R TO (M3,M4) L SHIFT +VE                                                       
      M6=MACHB*MACHC-(M4-M3+1)*MACHB                                                      
C-----MAKE TWO WORDS FROM KOPY                                                            
C-----2ND WORD                                                                            
   17 KOPZ=SHIFT(KOPY,M6)                                                                 
C-----1ST WORD                                                                            
      KOPY=SHIFT(KOPY,M5)                                                                 
C-----MASK UNWANTED PARTS OF KOPY AND KOPZ TO ZERO                                        
      MSK1=MASK(MACHB*MACHC-(M2-M1+1)*MACHB)                                              
C-----LOGICAL PRODUCT MASKS KOPY                                                          
      KOPY=AND(KOPY,COMPL(MSK1))                                                          
C-----                                                                                    
      MSK2=MASK((M4-M3+1)*MACHB)                                                          
C-----LOGICAL PRODUCT MASKS KOPZ                                                          
      KOPZ=AND(KOPZ,MSK2)                                                                 
C-----MASK KARD(K1) AND KARD(K2) WITH COMPL OF COMPL MSK1 AND MSK2                        
      KARD(K1)=AND(KARD(K1),MSK1)                                                         
      KARD(K2)=AND(KARD(K2),COMPL(MSK2))                                                  
C-----INSERT KOPY AND KOPZ IN MASKED PART OF K1 AND K2 BY LOGICAL OR                      
      KARD(K1)=OR(KARD(K1),KOPY)                                                          
      KARD(K2)=OR(KARD(K2),KOPZ)                                                          
C-----RETURN                                                                              
   20 RETURN                                                                              
      END                                                                                 
