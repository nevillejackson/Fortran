      SUBROUTINE DOLSTR(KARD,I,LARD,KK,N)                                                 
C-----WRITTEN R.E. 1977                                                                   
C --- SEARCHES CHARACTER ARRAY BEGINNING AT KARD(I) FOR STRING ENCLOSED BY $'S.           
C --- IF STRING IS FOUND, RETURNED IN LARD BEGINNING AT LARD(K)                           
C --- LENGTH OF STRING IS RETURNED IN N. IF NO STRING IS FOUND OR IF COL 80 IS            
C --- REACHED BEFORE TERMINATING $, N=0 IS RETURNED.                                      
      DIMENSION KARD(1),LARD(1)                                                           
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
      K=KK                                                                                
      K=K-1                                                                               
      N=0                                                                                 
      IF(I.GE.MACHCC) GO TO 150                                                           
      J=I                                                                                 
  100 IF(IJCHCM(IN(KARD,J),IN(KONST(44),MACHC))) 301,300,301                              
  301 CONTINUE                                                                            
      J=J+1                                                                               
      IF(J.LE.MACHCC) GO TO 100                                                           
  150 J=MACHCC                                                                            
      N=0                                                                                 
  200 I=J                                                                                 
      RETURN                                                                              
  300 J=J+1                                                                               
      IF(IJCHCM(IN(KARD,J),IN(KONST(44),MACHC))) 201,200,201                              
  201 CONTINUE                                                                            
      IF(J.GE.MACHCC)GO TO 150                                                            
      N=N+1                                                                               
      K=K+1                                                                               
      CALL OUT(LARD,K,IN(KARD,J))                                                         
      GO TO 300                                                                           
      END                                                                                 
