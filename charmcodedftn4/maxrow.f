      SUBROUTINE MAXROW(KACR,ROWKNT,COLKNT,PGEKNT,LARD,KARD,NLDP,NLDC,                    
     .NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,LTRIG,                     
     .NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,                   
     .L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH,                      
     .ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                      
     .KPOS,NCHD,ITH,NB1)                                                                  
      DIMENSION ITRIG(15,10),LTRIG(15,10),LARD(1),KARD(1),KD(1),NB3(1),                   
     .NB5(1),ICOL(1),IHD1(1),KPOS1(1),NCHD1(1),KEQ(1),IVAL1CH(1),                         
     .IVAL2CH(1),ISTR1CH(1),ISTR3CH(1),I1CH(1),I3CH(1),L1CH(1),L3CH(1),                   
     .IBCH(1),ILCH(1),IVAL1PH(1),IVAL2PH(1),ISTR1PH(1),ISTR3PH(1),I1PH(1                  
     .),I3PH(1),L1PH(1),L3PH(1),IBPH(1),ILPH(1),JBLK(1),IFH(1),IHD(1),                    
     .KPOS(1),NCHD(1)                                                                     
      INTEGER PGEKNT,ROWKNT,COLKNT                                                        
C---- WRITTEN R.E. 1977                                                                   
C---- USED BY PROGRAM SHEET                                                               
C---- DECIDES AND INITIATES ACTION TO BE TAKEN WHEN MXR IS EXCEEDED                       
      ROWKNT=KACR                                                                         
      COLKNT=COLKNT+1                                                                     
      IF(COLKNT-NCOLS)10,10,15                                                            
   10 IF(NLDC)11,11,12                                                                    
   11 CALL NEWCOL(LARD,KACR,COLKNT,ROWKNT,PGEKNT,KD,NB3,NB4,NB5,NB6,ICOL                  
     .,IHD1,KPOS1,NCHD1,KEQ,ITRIG,LTRIG,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2C                  
     .H,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,LAST,NB1)                           
      GO TO 13                                                                            
   12 CALL NEWCOL(KARD,KACR,COLKNT,ROWKNT,PGEKNT,KD,NB3,NB4,NB5,NB6,ICOL                  
     .,IHD1,KPOS1,NCHD1,KEQ,ITRIG,LTRIG,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2C                  
     .H,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,LAST,NB1)                           
   13 RETURN                                                                              
   15 PGEKNT=PGEKNT+1                                                                     
      IF(NLDP)16,16,20                                                                    
   16 IF(NLDC)17,17,18                                                                    
   17 CALL NEWPGE(LARD,LARD,KACR,COLKNT,ROWKNT,PGEKNT,MXR,JBLK,MAXCOL,                    
     .KD,NB3,NB4,NB5,NB6,IFH,IHD,KPOS,NCHD,KEQ,ITH,ITRIG,LTRIG,ICOL,IHD1                  
     .,KPOS1,NCHD1,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH,ISTR3PH,                     
     .I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,                    
     .ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,LAST,NB1)                             
      GO TO 19                                                                            
   18 CALL NEWPGE(LARD,KARD,KACR,COLKNT,ROWKNT,PGEKNT,MXR,JBLK,MAXCOL,                    
     .KD,NB3,NB4,NB5,NB6,IFH,IHD,KPOS,NCHD,KEQ,ITH,ITRIG,LTRIG,ICOL,IHD1                  
     .,KPOS1,NCHD1,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH,ISTR3PH,                     
     .I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,                    
     .ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,LAST,NB1)                             
   19 RETURN                                                                              
   20 IF(NLDC)21,21,22                                                                    
   21 CALL NEWPGE(KARD,LARD,KACR,COLKNT,ROWKNT,PGEKNT,MXR,JBLK,MAXCOL,                    
     .KD,NB3,NB4,NB5,NB6,IFH,IHD,KPOS,NCHD,KEQ,ITH,ITRIG,LTRIG,ICOL,IHD1                  
     .,KPOS1,NCHD1,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH,ISTR3PH,                     
     .I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,                    
     .ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,LAST,NB1)                             
      GO TO 23                                                                            
   22 CALL NEWPGE(KARD,KARD,KACR,COLKNT,ROWKNT,PGEKNT,MXR,JBLK,MAXCOL,                    
     .KD,NB3,NB4,NB5,NB6,IFH,IHD,KPOS,NCHD,KEQ,ITH,ITRIG,LTRIG,ICOL,IHD1                  
     .,KPOS1,NCHD1,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH,ISTR3PH,                     
     .I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,                    
     .ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,LAST,NB1)                             
   23 RETURN                                                                              
      END                                                                                 
