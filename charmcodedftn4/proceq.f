      SUBROUTINE PROCEQ(IDIR,IPOS,NT1,NT2,NT3,IVAL1,IVAL2,ISTR1,ISTR3,I1                  
     .,I3,L1,L3,IBEG,ILEN,IBOM)                                                           
C-----WRITTEN R.E. 1977                                                                   
C-----CRACKS REMAINDER OF *EQUIV DIRECTIVE FOR PROGRAM SHEET AND STORES                   
C-----CUMULATIVE COUNTS OF EACH TYPE, VALUES TO BE MATCHED AGAINST THE                    
C-----TRIGGER FIELD AND THE FIELDS TO BE SUBSTITUTED FOR HEADINGS.                        
C-----AN ERROR CONDITION IS RETURNED WITH IBOM=1. OTHERWISE, IBOM=0.                      
      DIMENSION IDIR(1),ISTR1(1),ISTR3(1),IVAL1(1),IVAL2(1),IBEG(1),ILEN                  
     .(1),I1(1),I3(1),L1(1),L3(1)                                                         
      COMMON/MACH/MACHB,MACHC,MACHCD,MACHCC,LCR,LP,LCP,LTR,LTP,LPL,LSP                    
      DATA LEQ,LDOL,LLP,LRP,LCOM/1H=,1H$,1H(,1H),1H,/                                     
      IBOM=0                                                                              
 1000 IF(NEXITM(LC,IDIR,IPOS))9,20,9                                                      
    9 IBOM=1                                                                              
      RETURN                                                                              
   20 IF(LC-LCOM)21,23,21                                                                 
   21 IF(LC-10HCOLUMN 80 )9,22,9                                                          
   22 RETURN                                                                              
   23 JPOS=IPOS                                                                           
      IF(NEXALF(ICHS,IDIR,IPOS))50,9,50                                                   
   24 IPOS=JPOS+1                                                                         
C --- SUB-DIRECTIVE TYPE 3                                                                
   25 NT3=NT3+1                                                                           
      IPOS=IPOS-1                                                                         
      K3=I3(NT3)                                                                          
      CALL DOLSTR(IDIR,IPOS,ISTR3,K3,NCH)                                                 
      IF(NCH)9,9,26                                                                       
   26 I3(NT3+1)=I3(NT3)+NCH                                                               
      L3(NT3)=NCH                                                                         
      IPOS=IPOS+1                                                                         
      IF(IPOS-80)1000,1000,22                                                             
   50 IF(NEXITM(IEQ,IDIR,IPOS))9,51,9                                                     
   51 IF(IEQ-LEQ)24,52,24                                                                 
   52 IF(NEXITM(LD,IDIR,IPOS))9,53,9                                                      
   53 IF(LD-LDOL)60,54,60                                                                 
C --- SUB-DIRECTIVE TYPE 1                                                                
   54 IPOS=IPOS-1                                                                         
      NT1=NT1+1                                                                           
      K1=I1(NT1)                                                                          
      CALL DOLSTR(IDIR,IPOS,ISTR1,K1,NCH)                                                 
      IF(NCH)9,9,55                                                                       
   55 I1(NT1+1)=I1(NT1)+NCH                                                               
      L1(NT1)=NCH                                                                         
      IVAL1(NT1)=ICHS                                                                     
      IPOS=IPOS+1                                                                         
      IF(IPOS-80)1000,1000,22                                                             
   60 IF(LD-LLP)9,61,9                                                                    
C --- SUB-DIRECTIVE TYPE2                                                                 
   61 NT2=NT2+1                                                                           
      IVAL2(NT2)=ICHS                                                                     
      IF(NEXITM(NUM,IDIR,IPOS))9,9,62                                                     
   62 IBEG(NT2)=NUM                                                                       
      IF(NEXITM(LC,IDIR,IPOS))9,63,9                                                      
   63 IF(LC-LCOM)9,64,9                                                                   
   64 IF(NEXITM(NUM,IDIR,IPOS))9,9,65                                                     
   65 ILEN(NT2)=NUM                                                                       
      IF(NEXITM(LB,IDIR,IPOS))9,66,9                                                      
   66 IF(LB-LRP)9,67,9                                                                    
   67 GO TO 1000                                                                          
      END                                                                                 
