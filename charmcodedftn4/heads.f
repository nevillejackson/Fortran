      SUBROUTINE HEADS(IND,IARR,KACR,COLKNT,ROWKNT,PGEKNT,LARD,KARD,NLDP                  
     .,NLDC,NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,                     
     .LTRIG,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,                  
     .L1CH,L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH                  
     .,ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                     
     .KPOS,NCHD,ITH,NB1)                                                                  
      DIMENSION ITRIG(15,10),LTRIG(15,10),LARD(1),KARD(1),KD(1),NB3(1),                   
     .NB5(1),ICOL(1),IHD1(1),KPOS1(1),NCHD1(1),KEQ(1),IVAL1CH(1),                         
     .IVAL2CH(1),ISTR1CH(1),ISTR3CH(1),I1CH(1),I3CH(1),L1CH(1),L3CH(1),                   
     .IBCH(1),ILCH(1),IVAL1PH(1),IVAL2PH(1),ISTR1PH(1),ISTR3PH(1),I1PH(1                  
     .),I3PH(1),L1PH(1),L3PH(1),IBPH(1),ILPH(1),JBLK(1),IFH(1),IHD(1),                    
     .KPOS(1),NCHD(1),IARR(1)                                                             
      INTEGER PGEKNT,ROWKNT,COLKNT                                                        
C---- WRITTEN R.E. 1977                                                                   
C---- WRITES HEADINGS FOR PROGRAM SHEET                                                   
      IF(IND-10) 10,1,10                                                                  
C---- FPH BRANCH                                                                          
    1 K=KD(10)                                                                            
      IF(K)99,99,2                                                                        
    2 DO 3 J=1,K                                                                          
      ROWKNT=ROWKNT+NB3(J)+1                                                              
    3 CALL COPAGE(IFH(J),ROWKNT,PGEKNT,IARR,KPOS(J),NCHD(J))                              
      GO TO 99                                                                            
   10 IF(IND-11) 20,11,20                                                                 
C---- FCH BRANCH                                                                          
   11 K=KD(11)                                                                            
      IF(K)99,99,12                                                                       
   12 DO 13 J=1,K                                                                         
      ROWKNT=ROWKNT+NB5(J)+1                                                              
   13 CALL COPAGE(ICOL(COLKNT),ROWKNT,PGEKNT,IARR,KPOS1(J),NCHD1(J))                      
      GO TO 99                                                                            
   20 IF(IND-1) 30,21,30                                                                  
C---- TPH BRANCH                                                                          
   21 K=KD(1)                                                                             
      IF(K)99,99,22                                                                       
   22 IF(KACR.GT.0)ROWKNT=KACR                                                            
      ROWKNT=ROWKNT+NB4+1                                                                 
      IF(ROWKNT-MXR)23,25,25                                                              
   23 IF(KEQ(1))24,24,26                                                                  
   24 CALL COPAGE(ITH,ROWKNT,PGEKNT,IARR,ITRIG(1,1),LTRIG(1,1))                           
      GO TO 99                                                                            
   25       CALL MAXROW(KACR,ROWKNT,COLKNT,PGEKNT,LARD,KARD,NLDP,NLDC,                    
     .NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,LTRIG,                     
     .NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,                   
     .L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH,                      
     .ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                      
     .KPOS,NCHD,ITH,NB1)                                                                  
      GO TO 99                                                                            
   26 ROWKNT=ROWKNT-NB4-1                                                                 
      IF(NLDP)27,27,28                                                                    
   27 CALL APPEQ(LARD,ITRIG(1,1),LTRIG(1,1),NT1PH,NT2PH,NT3PH,IVAL1PH,                    
     .IVAL2PH,ISTR1PH,ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,NB4,ITH,                      
     .ROWKNT,PGEKNT,LAST)                                                                 
      GO TO 99                                                                            
   28 CALL APPEQ(KARD,ITRIG(1,1),LTRIG(1,1),NT1PH,NT2PH,NT3PH,IVAL1PH,                    
     .IVAL2PH,ISTR1PH,ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,NB4,ITH,                      
     .ROWKNT,PGEKNT,LAST)                                                                 
      GO TO 99                                                                            
   30 IF(IND-2) 99,31,99                                                                  
C---- TCH BRANCH                                                                          
   31 K=KD(2)                                                                             
      IF(K)99,99,32                                                                       
   32 ROWKNT=ROWKNT+NB6+1                                                                 
      IF(ROWKNT-MXR)33,36,36                                                              
   33 IF(KEQ(2))34,34,35                                                                  
   34 CALL COPAGE(ICOL(COLKNT),ROWKNT,PGEKNT,IARR,ITRIG(2,1),LTRIG(2,1))                  
      GO TO 99                                                                            
   35 ROWKNT=ROWKNT-NB6-1                                                                 
      IF(NLDC)37,37,38                                                                    
   37 CALL APPEQ(LARD,ITRIG(2,1),LTRIG(2,1),NT1CH,NT2CH,NT3CH,IVAL1CH,                    
     .IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,NB6,                          
     .ICOL(COLKNT),ROWKNT,PGEKNT,LAST)                                                    
      GO TO 99                                                                            
   38 CALL APPEQ(KARD,ITRIG(2,1),LTRIG(2,1),NT1CH,NT2CH,NT3CH,IVAL1CH,                    
     .IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,NB6,                          
     .ICOL(COLKNT),ROWKNT,PGEKNT,LAST)                                                    
      GO TO 99                                                                            
   36       CALL MAXROW(KACR,ROWKNT,COLKNT,PGEKNT,LARD,KARD,NLDP,NLDC,                    
     .NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,LTRIG,                     
     .NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,                   
     .L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH,                      
     .ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                      
     .KPOS,NCHD,ITH,NB1)                                                                  
   99 RETURN                                                                              
      END                                                                                 
