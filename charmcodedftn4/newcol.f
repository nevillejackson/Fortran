      SUBROUTINE NEWCOL(IARRC,KACR,COLKNT,ROWKNT,PGEKNT,KD,NB3,NB4,NB5,                   
     .NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,LTRIG,NT1CH,NT2CH,NT3CH,IVAL1C                  
     .H,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,LAST,NB1)                   
      DIMENSION ITRIG(15,10),LTRIG(15,10),IARRC(1),KD(1),NB3(1),NB5(1),                   
     .ICOL(1),IHD1(1),KPOS1(1),NCHD1(1),KEQ(1),IVAL1CH(1),IVAL2CH(1),                     
     .ISTR1CH(1),ISTR3CH(1),I1CH(1),I3CH(1),L1CH(1),L3CH(1),IBCH(1),                      
     .ILCH(1)                                                                             
      INTEGER PGEKNT,ROWKNT,COLKNT                                                        
C---- WRITTEN R.E. 1977                                                                   
C---- USED BY PROGRAM SHEET TO OPEN A NEW COLUMN                                          
C---- COLKNT IS INCREMENTED PRIOR TO CALLING NEWCOL                                       
      IF(KACR)11,11,2                                                                     
   11 ROWKNT=1                                                                            
      K=KD(10)                                                                            
      IF(K.EQ.0)GO TO 2                                                                   
      DO 1 J=1,K                                                                          
    1 ROWKNT=ROWKNT+NB3(J)                                                                
      ROWKNT=ROWKNT+K                                                                     
    2 K=KD(1)                                                                             
      IF(K.EQ.0)GO TO 3                                                                   
      ROWKNT=ROWKNT+NB4+1                                                                 
    3 K=KD(11)                                                                            
      IF(K) 4,6,4                                                                         
    4 DO 5 J=1,K                                                                          
      ROWKNT=ROWKNT+NB5(J)+1                                                              
    5 CALL COPAGE(ICOL(COLKNT),ROWKNT,PGEKNT,IHD1,KPOS1(J),NCHD1(J))                      
    6 K=KD(2)                                                                             
      IF(K) 7,10,7                                                                        
    7 IF(KEQ(2))8,8,9                                                                     
    8 ROWKNT=ROWKNT+NB6+1                                                                 
      CALL COPAGE(ICOL(COLKNT),ROWKNT,PGEKNT,IARRC,ITRIG(2,1),LTRIG(2,1)                  
     .)                                                                                   
      GO TO 10                                                                            
    9 CALL APPEQ(IARRC,ITRIG(2,1),LTRIG(2,1),NT1CH,NT2CH,NT3CH,IVAL1CH,                   
     .IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,NB6,                          
     .ICOL(COLKNT),ROWKNT,PGEKNT,LAST)                                                    
   10 ROWKNT=ROWKNT+NB1                                                                   
      RETURN                                                                              
      END                                                                                 
