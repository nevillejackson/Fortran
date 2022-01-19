      SUBROUTINE EDSUB(LA,LIT,LREC ,IERR)                                                 
C-----WRITTEN N.J. 1977                                                                   
C-----PERFORMS INTRA-LINE EDITS FOR EDITR PROGRAM                                         
      DIMENSION LA(1),LIT(1),LSTRNG(8)                                                    
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      IPOS=4                                                                              
      IERR=0                                                                              
      IF(NEXITM(LN,LA,IPOS)) 1,1,2                                                        
    1 IERR=1                                                                              
      RETURN                                                                              
    2 IF(NEXITM(LSP,LA,IPOS)) 1,3,1                                                       
    3 IF(NEXITM(MNEM,LA,IPOS)) 4,1,1                                                      
    4 IF(NEXITM(LLB,LA,IPOS)) 1,5,1                                                       
    5 IF(NEXITM(LCOL,LA,IPOS)) 1,1,6                                                      
    6 IF(NEXITM(LSP,LA,IPOS)) 1,7,1                                                       
    7 JPOS=IPOS                                                                           
      IF(NEXALF(LSTRNG,LA,IPOS)) 8,1,18                                                   
    8 LEN=IPOS-JPOS                                                                       
      GO TO 20                                                                            
   18 LEN=IPOS-JPOS-2                                                                     
   20 CONTINUE                                                                            
      IF(NEXITM(LRB,LA,IPOS)) 1,9,1                                                       
    9 IF(MNEM-1HR) 10,11,10                                                               
   10 IF(MNEM-1HD) 12,13,12                                                               
   12 IF(MNEM-1HI) 14,15,14                                                               
   14 IF(MNEM-1HB) 1,17,1                                                                 
C-----BLANK BRANCH                                                                        
   17 CALL COPYC(LSTRNG,1,LSTRNG,MACHC-LEN+1,LEN)                                         
      CALL SETBLK(LSTRNG,0,MACHC-LEN)                                                     
      LSTRNG=INUM(LSTRNG)                                                                 
      CALL SETBLK(LIT,LCOL-1,LCOL-1+LSTRNG)                                               
      GO TO 50                                                                            
C-----REPLACE BRANCH                                                                      
   11 CONTINUE                                                                            
      CALL DELETE(LIT,LCOL,LEN,LREC)                                                      
      CALL INSERT(LIT,LCOL-1,LSTRNG,LEN,LREC)                                             
      GO TO 50                                                                            
C-----DELETE BRANCH                                                                       
   13 CALL COPYC(LSTRNG,1,LSTRNG,MACHC-LEN+1,LEN)                                         
      CALL SETBLK(LSTRNG,0,MACHC-LEN)                                                     
      LSTRNG=INUM(LSTRNG)                                                                 
      CALL DELETE(LIT,LCOL,LSTRNG,LREC)                                                   
      GO TO 50                                                                            
C-----INSERT BRANCH                                                                       
   15 CONTINUE                                                                            
      CALL INSERT(LIT,LCOL-1,LSTRNG,LEN,LREC)                                             
      GO TO 50                                                                            
C-----MORE THAN ONE EDIT PER LINE ?                                                       
   50 IF(NEXITM(LSP,LA,IPOS)) 1,51,1                                                      
   51 IF(LSP-1H,) 52,3,52                                                                 
   52 IF(LSP-10HCOLUMN 80 ) 1,53,1                                                        
   53 RETURN                                                                              
      END                                                                                 
