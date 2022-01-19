      SUBROUTINE CALSUB(LOT,MREC,MRECW)                                                   
C-----WRITTEN N.J. 1977                                                                   
C-----MODIFIED N.J. 1981                                                                  
      DIMENSION IC(100),ID(100),ITA(100),ITB(100)                                         
      DIMENSION LOT(1)                                                                    
      DIMENSION IA(100),IB(100),A(100),B(100)                                             
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      COMMON /N/ NA,NB,NC,ND                                                              
      COMMON /NAM/ NAMA(100),NAMB(100),NAMC(100),NAMD(100)                                
      COMMON /BEG/ IBEGA(100),IBEGB(100),IBEGC(100),IBEGD(100)                            
      COMMON /LEN/ LENA(100),LENB(100),LENC(100),LEND(100)                                
      COMMON /DEC/ IDECA(100),IDECB(100)                                                  
C-----INSERT CALCULATIONS HERE                                                            
C-----INPUT FIELDS -- COPY,DECODE AND FLOAT                                               
      IF(NA) 999,21,31                                                                    
   31 DO 1 I=1,NA                                                                         
      ITA(I)=0                                                                            
      CALL COPYC(LOT,IBEGA(I),IA(I),1,LENA(I))                                            
      IF(ITYPE(IN(IA(I),LENA(I)))-2) 160,161,160                                          
  160 ITA(I)=1                                                                            
      IF(LENA(I)-MACHC) 140,141,996                                                       
  140 CALL SETBLK(IA   ,(I-1)*MACHC+LENA(I),I*MACHC)                                      
  141 CONTINUE                                                                            
      GO TO 1                                                                             
  161 IA(I)=LNUM(IA(I),LENA(I))                                                           
      A(I)=FXI(IA(I),IDECA(I))                                                            
    1 CONTINUE                                                                            
   21 IF(NC) 999,41,51                                                                    
   51 DO 11 I=1,NC                                                                        
      CALL COPYC(LOT,IBEGC(I),IC(I),1,LENC(I))                                            
      IF(LENC(2)-MACHC) 150,11,994                                                        
  150 CALL SETBLK(IC   ,(I-1)*MACHC+LENC(I),I*MACHC)                                      
   11 CONTINUE                                                                            
C-----CALCULATE IN FLOATING POINT                                                         
   41 CALL BXA(A,B,IA,IB,IC,ID,ITA,ITB)                                                   
C-----OUTPUT FIELDS -- FIX , ENCODE AND COPY                                              
      IF(NB) 999,22,42                                                                    
   42 DO 2 I=1,NB                                                                         
      IF(ITB(I)-1) 15,15,998                                                              
   15 IF(ITB(I)) 998,163,164                                                              
  164 CALL COPYC(IB(I),1,LOT,IBEGB(I),LENB(I))                                            
      GO TO 2                                                                             
  163 IB(I)=IXF(B(I),IDECB(I))                                                            
      CALL LHOLZ(IB(I),IA(I),LENB(I))                                                     
      CALL COPYC(IA(I),1,LOT,IBEGB(I),LENB(I))                                            
    2 CONTINUE                                                                            
   22 IF(ND) 999,43,52                                                                    
   52 DO 12 I=1,ND                                                                        
   12 CALL COPYC(ID(I),1,LOT,IBEGD(I),LEND(I))                                            
   43 RETURN                                                                              
      ENTRY CSINIT                                                                        
C-----INSERT INITIALIZATION HERE                                                          
C-----RECORD LENGTH INCREMENTED                                                           
      IF(NB) 999,180,181                                                                  
  181 DO 3 I=1,NB                                                                         
    3 MREC=MAX0(IBEGB(I)+LENB(I)-1,MREC)                                                  
  180 IF(ND) 999,182,183                                                                  
  183 DO 4 I=1,ND                                                                         
    4 MREC=MAX0(IBEGD(I)+LEND(I)-1,MREC)                                                  
  182 CONTINUE                                                                            
      MRECW=LWORDS(MREC)                                                                  
      RETURN                                                                              
  994 WRITE(LP,993) I,MACHC                                                               
  993 FORMAT(10H0FIELD IC(,I3,34H) EXCEEDS COMPUTER WORD LENGTH OF ,                      
     1 I3,11H CHARACTERS)                                                                 
      CALL JOBEND                                                                         
  996 WRITE(LP,995) I,MACHC                                                               
  995 FORMAT(10H0FIELD IA(,I3,34H) EXCEEDS COMPUTER WORD LENGTH OF ,                      
     1 I3,32H CHARACTERS AND IS NOT DECODABLE)                                            
      CALL JOBEND                                                                         
  998 WRITE(LP,997)                                                                       
  997 FORMAT(16H0ITB NOT DEFINED)                                                         
      CALL JOBEND                                                                         
  999 CALL LOGIC(6HCALSUB)                                                                
      END                                                                                 
