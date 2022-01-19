      SUBROUTINE NEWPGE(IARRP,IARRC,KACR,COLKNT,ROWKNT,PGEKNT,MXR,JBLK,                   
     .MAXCOL,KD,NB3,NB4,NB5,NB6,IFH,IHD,KPOS,NCHD,KEQ,ITH,ITRIG,LTRIG,                    
     .ICOL,IHD1,KPOS1,NCHD1,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH,                    
     .ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,NT1CH,NT2CH,NT3CH,IVAL1CH,                    
     .IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,LAST,NB1)                     
      DIMENSION ITRIG(15,10),LTRIG(15,10),IARRP(1),IARRC(1),JBLK(1),KD(1                  
     .),NB3(1),NB5(1),IFH(1),IHD(1),KPOS(1),NCHD(1),KEQ(1),ICOL(1),                       
     .IHD1(1),KPOS1(1),NCHD1(1),IVAL1PH(1),IVAL2PH(1),ISTR1PH(1),                         
     .ISTR3PH(1),I1PH(1),I3PH(1),L1PH(1),L3PH(1),IBPH(1),ILPH(1),IVAL1CH                  
     .(1),IVAL2CH(1),ISTR1CH(1),ISTR3CH(1),I1CH(1),I3CH(1),L1CH(1),                       
     .L3CH(1),IBCH(1),ILCH(1)                                                             
      INTEGER PGEKNT,ROWKNT,COLKNT                                                        
      COMMON/MACH/MACHB,MACHC,MACHCD,MACHCC,LCR,LP,LCP,LT,LTP,LPL,LSP                     
C---- WRITTEN R.E. 1977                                                                   
C---- USED BY PROGRAM SHEET TO OPEN A NEW PAGE                                            
C---- PGEKNT IS INCREMENTED PRIOR TO CALLING NEWPGE                                       
      KACR=0                                                                              
      ROWKNT=1                                                                            
      COLKNT=1                                                                            
C---- CLEAR THE PAGE                                                                      
      LENCLR=MACHC*LWORDS(MAXCOL)                                                         
      DO 20 J=1,MXR                                                                       
   20 CALL COPAGE(1,J,PGEKNT,JBLK,1,LENCLR)                                               
C---- WRITE PAGE HEADINGS, ETC.                                                           
      K=KD(10)                                                                            
      IF(K.EQ.0)GO TO 22                                                                  
      DO 21 J=1,K                                                                         
      ROWKNT=ROWKNT+NB3(J)+1                                                              
   21 CALL COPAGE(IFH(J),ROWKNT,PGEKNT,IHD,KPOS(J),NCHD(J))                               
   22 K=KD(1)                                                                             
      IF(K.EQ.0)GO TO 23                                                                  
      IF(KEQ(1))28,28,29                                                                  
   28 ROWKNT=ROWKNT+NB4+1                                                                 
      CALL COPAGE(ITH,ROWKNT,PGEKNT,IARRP,ITRIG(1,1),LTRIG(1,1))                          
      GO TO 23                                                                            
   29 CALL APPEQ(IARRP,ITRIG(1,1),LTRIG(1,1),NT1PH,NT2PH,NT3PH,IVAL1PH,                   
     .IVAL2PH,ISTR1PH,ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,NB4,ITH,                      
     .ROWKNT,PGEKNT,LAST)                                                                 
   23 K=KD(11)                                                                            
      IF(K) 24,26,24                                                                      
   24 DO 25 J=1,K                                                                         
      ROWKNT=ROWKNT+NB5(J)+1                                                              
   25 CALL COPAGE(ICOL(COLKNT),ROWKNT,PGEKNT,IHD1,KPOS1(J),NCHD1(J))                      
   26 K=KD(2)                                                                             
      IF(K) 27,30,27                                                                      
   27 IF(KEQ(2))38,38,39                                                                  
   38 ROWKNT=ROWKNT+NB6+1                                                                 
      CALL COPAGE(ICOL(COLKNT),ROWKNT,PGEKNT,IARRC,ITRIG(2,1),LTRIG(2,1)                  
     .)                                                                                   
      GO TO 30                                                                            
   39 CALL APPEQ(IARRC,ITRIG(2,1),LTRIG(2,1),NT1CH,NT2CH,NT3CH,IVAL1CH,                   
     .IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,NB6,                          
     .ICOL(COLKNT),ROWKNT,PGEKNT,LAST)                                                    
   30 ROWKNT=ROWKNT+NB1                                                                   
      RETURN                                                                              
      END                                                                                 
