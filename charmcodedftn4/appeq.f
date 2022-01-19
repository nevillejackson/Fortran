      SUBROUTINE APPEQ(IARR,IB,LB,NT1,NT2,NT3,IV1,IV2,IST1,IST3,IS1,IS3,                  
     .LS1,LS3,IS2,LS2,NB,ICL,IRW,IPGE,LAST)                                               
      DIMENSION IARR(1),IV1(1),IV2(1),IST1(1),IST3(1),IS1(1),LS1(1),IS3(                  
     .1),LS3(1),IS2(1),LS2(1)                                                             
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
C---- WRITTEN R.E. 1977                                                                   
C---- USED BY PROGRAM SHEET                                                               
C---- DECIDES CHARACTER STRING TO BE PRINTED AS A RESULT OF A *EQUIV                      
C---- DIRECTIVE AND WRITES THIS STRING INTO IPAGE ARRAY                                   
      IF(NT1)20,20,10                                                                     
   10 DO 11 IT=1,NT1                                                                      
      ISTR=IV1(IT)                                                                        
      IBEG=IS1(IT)                                                                        
      ILEN=LS1(IT)                                                                        
      KM=MATCHF(IARR,IB,ISTR,LB)                                                          
      IF(KM)11,11,12                                                                      
   11 CONTINUE                                                                            
      GO TO 20                                                                            
   12 IRW=IRW+NB+1                                                                        
      CALL COPAGE(ICL,IRW,IPGE,IST1,IBEG,ILEN)                                            
      RETURN                                                                              
   20 IF(NT2)30,30,21                                                                     
   21 DO 22 IT=1,NT2                                                                      
      ISTR=IV2(IT)                                                                        
      IBEG=IS2(IT)                                                                        
      ILEN=LS2(IT)                                                                        
      KM=MATCHF(IARR,IB,ISTR,LB)                                                          
      IF(KM)22,22,23                                                                      
   22 CONTINUE                                                                            
      GO TO 30                                                                            
   23 IRW=IRW+NB+1                                                                        
      CALL COPAGE(ICL,IRW,IPGE,IARR,IBEG,ILEN)                                            
      RETURN                                                                              
   30 IF(NT3)40,40,31                                                                     
   31 IRW=IRW+NB+1                                                                        
      CALL COPAGE(ICL,IRW,IPGE,IST3,IS3,LS3)                                              
      RETURN                                                                              
   40 WRITE(LP,1) IB,LB,(IARR(I),I=1,LAST)                                                
    1 FORMAT(66H NO ALTERNATIVE FIELD FOUND IN *EQUIV DIRECTIVE ... RUN                   
     .TERMINATED,/,28H TRIGGERING FIELD BEGINS COL,I4,7H LENGTH,I4,19H O                  
     .N THE DATA RECORD,/,1H ,12A10)                                                      
      STOP                                                                                
      END                                                                                 
