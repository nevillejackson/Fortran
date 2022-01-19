      PROGRAM SHEET(TAPE10=/1000,TAPE11=/1000                                             
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C---- WRITTEN R.E. 1977                                                                   
C---- PROGRAM TO PRODUCE MULTI-PAGED STRUCTURED OUTPUT CONTROLLED WITH                    
C---- USER-SPECIFIED DIRECTIVES.                                                          
C---- DATA IS ON TAPE 10, DIRECTIVES ON TAPE 5, STRUCTURED PAGE WRITTEN                   
C---- ON TAPE 11 WHICH WOULD USUALLY BE DISPOSED TO OUTPUT.                               
      DIMENSION LARD(100),KARD(100)                                                       
      DIMENSION IDIR(8),KD(15),ITRIG(15,10),                                              
     .LTRIG(15,10),JBLK(14),NB2(10),NB3(10),NB5(10),ICOL(10),KPOS(10),                    
     .IHD(100),NCHD(10),ITH(10),IFH(10),IHD1(100),KPOS1(10),NCHD1(10),                    
     .KSTR(100),IHOL(12),IVAL1PH(50),IVAL2PH(50),ISTR1PH(100),ISTR3PH(10                  
     .0),I1PH(50),I3PH(50),L1PH(50),L3PH(50),IBPH(50),ILPH(50),IVAL1CH(5                  
     .0),IVAL2CH(50),ISTR1CH(100),ISTR3CH(100),I1CH(50),I3CH(50),L1CH(50                  
     .),L3CH(50), IBCH(50),ILCH(50),KEQ(2),NLPC(10),NLCC(10)                              
      INTEGER PGEKNT,ROWKNT,COLKNT                                                        
      LEVEL 2, IPAGE                                                                      
      COMMON/ONE/IPAGE(14,70,80)                                                          
      COMMON / TWO / ISSW(15,10)                                                          
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LCR,LP,LCP,LT,LTP,LPL,LSP                   
      DATA (IHOL(I),I=1,12)/3HTPH,3HTCH,3HTPC,3HTCC,3HTBL,3HPFX,3HCOL,3H                  
     .ROW,4HDATA,3HFPH,3HFCH,5HEQUIV/                                                     
      DATA LSTAR,LCOM,LR,LNZ,LDL,LMIN/1H*,1H,,1HR,2HNZ,1H$,1H-/                           
      DATA NDT,MAXR,NTG,MAXCL/11,58,6,80/                                                 
      DATA JBLK /14*1H /                                                                  
      CALL DEFINE                                                                         
      WRITE(LP,10)                                                                        
   10 FORMAT(10H0SHEET RUN)                                                               
      REWIND 10                                                                           
      REWIND 11                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      LAST=LRECW+1                                                                        
C---- INITIALISE DIRECTIVE COUNTERS                                                       
      DO 8 I=1,NDT                                                                        
    8 KD(I)=0                                                                             
C---- INITIALISE HEADING POINTERS                                                         
      KPOS(1)=1                                                                           
      KPOS1(1)=1                                                                          
C---- BEGIN READING DIRECTIVES - ANY INVALID DIRECTIVE TERMINATES JOB                     
   11 CALL DIRDRT(5,6,IDIR,IOF)                                                           
      IF(IOF)12,12,100                                                                    
   12 IPOS=1                                                                              
      IF(NEXITM(LS,IDIR,IPOS))9,13,9                                                      
    6 WRITE(LP,7)                                                                         
    9 WRITE(LP,3)                                                                         
    3 FORMAT(37H INVALID DIRECTIVE ... RUN TERMINATED)                                    
    7 FORMAT(53H THIS DIRECTIVE EXCEEDS MAXIMUM ALLOWED FOR THIS TYPE)                    
      CALL JOBEND                                                                         
   13 IF(LS-LSTAR)9,14,9                                                                  
C---- IDENTIFY DIRECTIVE TYPE AND BRANCH ACCORDINGLY                                      
   14 IF(NEXITM(LOP,IDIR,IPOS))15,9,9                                                     
   15 DO 16 J=1,NDT                                                                       
      IF(LOP-IHOL(J))16,17,16                                                             
   16 CONTINUE                                                                            
      GO TO 9                                                                             
   17 GO TO (620,700,660,740,560,540,500,520,580,600,680)J                                
C---- *COL DIRECTIVE                                                                      
  500 KD(J)=KD(J)+1                                                                       
      IF(KD(J).GT.1)GO TO 6                                                               
      IPOS=6                                                                              
      IF(NEXITM(NCOLS,IDIR,IPOS))9,9,501                                                  
  501 DO 505 I=1,NCOLS                                                                    
      IF(NEXITM(LC,IDIR,IPOS))9,502,9                                                     
  502 IF(LC-LCOM)9,503,9                                                                  
  503 IF(NEXITM(K,IDIR,IPOS))9,9,504                                                      
  504 ICOL(I)=K                                                                           
  505 CONTINUE                                                                            
      IF(NEXITM(LC,IDIR,IPOS))9,506,9                                                     
  506 IF(LC-LCOM)507,508,507                                                              
  507 MAXCOL=MAXCL                                                                        
      GO TO 11                                                                            
  508 IF(NEXITM(MAXCOL,IDIR,IPOS))9,9,509                                                 
  509 IF(MAXCOL-140)11,11,510                                                             
  510 WRITE(LP,511)                                                                       
  511 FORMAT(25H IPAGE DIMENSION EXCEEDED)                                                
      GO TO 9                                                                             
C---- *ROW DIRECTIVE                                                                      
  520 KD(J)=KD(J)+1                                                                       
      IF(KD(J).GT.1)GO TO 6                                                               
      IPOS=6                                                                              
      IF(NEXITM(NB1,IDIR,IPOS))9,9,521                                                    
  521 IF(NEXITM(LC,IDIR,IPOS))9,522,9                                                     
  522 IF(LC-LCOM)9,523,9                                                                  
  523 IF(NEXITM(NBBR,IDIR,IPOS))9,9,524                                                   
  524 IF(NEXITM(LC,IDIR,IPOS))9,525,9                                                     
  525 IF(LC-LCOM)526,527,526                                                              
  527 IF(NEXITM(MXR,IDIR,IPOS))9,9,528                                                    
  528 IF(MXR-70)11,11,510                                                                 
  526 MXR=MAXR                                                                            
      GO TO 11                                                                            
C---- *PFX DIRECTIVE                                                                      
  540 KD(J)=KD(J)+1                                                                       
      IF(KD(J).GT.1)GO TO 6                                                               
      IPOS=6                                                                              
      IF(NEXITM(IPX,IDIR,IPOS))9,9,541                                                    
  541 ITRIG(J,1)=IPX                                                                      
      IF(NEXITM(LC,IDIR,IPOS))9,542,9                                                     
  542 IF(LC-LCOM)9,543,9                                                                  
  543 IF(NEXITM(LPX,IDIR,IPOS))9,9,544                                                    
  544 LTRIG(J,1)=LPX                                                                      
      IF(NEXITM(LC,IDIR,IPOS))9,545,9                                                     
  545 IF(LC-LCOM)546,547,546                                                              
  546 INDPX=0                                                                             
      GO TO 11                                                                            
  547 IF(NEXITM(KR,IDIR,IPOS))548,9,9                                                     
  548 IF(KR-LR)9,549,9                                                                    
  549 INDPX=1                                                                             
      GO TO 11                                                                            
C---- *TBL DIRECTIVE                                                                      
  560 KD(J)=KD(J)+1                                                                       
      NTR=KD(J)                                                                           
      IPOS=6                                                                              
      IF(NEXITM(ITR,IDIR,IPOS))9,9,561                                                    
  561 ITRIG(J,NTR)=ITR                                                                    
      IF(NEXITM(LC,IDIR,IPOS))9,562,9                                                     
  562 IF(LC-LCOM)9,563,9                                                                  
  563 IF(NEXITM(LTR,IDIR,IPOS))9,9,564                                                    
  564 LTRIG(J,NTR)=LTR                                                                    
      IF(NEXITM(LC,IDIR,IPOS))9,565,9                                                     
  565 IF(LC-LCOM)9,566,9                                                                  
  566 IF(NEXITM(NB,IDIR,IPOS))9,9,567                                                     
  567 NB2(NTR)=NB                                                                         
      GO TO 11                                                                            
C---- *DATA DIRECTIVE                                                                     
  580 KD(J)=KD(J)+1                                                                       
      IF(KD(J).GT.1)GO TO 6                                                               
      IPOS=7                                                                              
      IF(NEXITM(IDA,IDIR,IPOS))9,9,581                                                    
  581 IF(NEXITM(LC,IDIR,IPOS))9,582,9                                                     
  582 IF(LC-LCOM)9,583,9                                                                  
  583 IF(NEXITM(LDA,IDIR,IPOS))9,9,584                                                    
  584 GO TO 11                                                                            
C---- *FPH DIRECTIVE                                                                      
  600 KD(J)=KD(J)+1                                                                       
      K=KD(J)                                                                             
      IPOS=6                                                                              
      IF(NEXITM(LC,IDIR,IPOS))9,601,609                                                   
  601 IF(LC-LMIN)9,610,9                                                                  
  610 IF(NEXITM(NB,IDIR,IPOS))9,9,611                                                     
  611 NB3(K)=-NB                                                                          
      GO TO 612                                                                           
  609 NB3(K)=LC                                                                           
  612 IF(NEXITM(LC,IDIR,IPOS))9,602,9                                                     
  602 IF(LC-LCOM)9,603,9                                                                  
  603 IF(NEXITM(IC,IDIR,IPOS))9,9,604                                                     
  604 IFH(K)=IC                                                                           
      IF(NEXITM(LC,IDIR,IPOS))9,605,9                                                     
  605 IF(LC-LCOM)9,606,9                                                                  
  606 KP=KPOS(K)                                                                          
      CALL DOLSTR(IDIR,IPOS,IHD,KP,NCH)                                                   
      IF(NCH)607,607,608                                                                  
  607 WRITE(LP,4)                                                                         
    4 FORMAT(21H N=0  NO VALID STRING)                                                    
      GO TO 9                                                                             
  608 KPOS(K+1)=KPOS(K)+NCH                                                               
      NCHD(K)=NCH                                                                         
      GO TO 11                                                                            
C---- *TPH DIRECTIVE                                                                      
  620 KD(J)=KD(J)+1                                                                       
      K=KD(J)                                                                             
      IF(K.GT.1)GO TO 6                                                                   
      IPOS=6                                                                              
      IF(NEXITM(LC,IDIR,IPOS))9,616,619                                                   
  616 IF(LC-LMIN)9,617,9                                                                  
  617 IF(NEXITM(NB4,IDIR,IPOS))9,9,618                                                    
  618 NB4=-NB4                                                                            
      GO TO 621                                                                           
  619 NB4=LC                                                                              
  621 IF(NEXITM(LC,IDIR,IPOS))9,622,9                                                     
  622 IF(LC-LCOM)9,623,9                                                                  
  623 IF(NEXITM(ITH,IDIR,IPOS))9,9,624                                                    
  624 IF(NEXITM(LC,IDIR,IPOS))9,625,9                                                     
  625 IF(LC-LCOM)9,626,9                                                                  
  626 IF(NEXITM(ITR,IDIR,IPOS))9,9,627                                                    
  627 ITRIG(J,K)=ITR                                                                      
      IF(NEXITM(LC,IDIR,IPOS))9,628,9                                                     
  628 IF(LC-LCOM)9,629,9                                                                  
  629 IF(NEXITM(LTR,IDIR,IPOS))9,9,630                                                    
  630 LTRIG(J,K)=LTR                                                                      
      IF(NEXITM(LC,IDIR,IPOS))9,631,9                                                     
  631 IF(LC-LCOM)632,633,632                                                              
  632 INDNZ=0                                                                             
      GO TO 642                                                                           
  633 IF(NEXITM(KNZ,IDIR,IPOS))634,9,9                                                    
  634 IF(KNZ-LNZ)9,635,9                                                                  
  635 INDNZ=1                                                                             
  642 KEQ(J)=0                                                                            
      NT1PH=0                                                                             
      NT2PH=0                                                                             
      NT3PH=0                                                                             
      I1PH(1)=1                                                                           
      I3PH(1)=1                                                                           
  636 CALL DIRDRT(5,6,IDIR,IOF)                                                           
      IF(IOF)637,637,100                                                                  
  637 IPOS=1                                                                              
      IF(NEXITM(LS,IDIR,IPOS))9,638,9                                                     
  638 IF(LS-LSTAR)9,639,9                                                                 
  639 IF(NEXITM(LOP,IDIR,IPOS))640,9,9                                                    
  640 IF(LOP-IHOL(12))15,641,15                                                           
  641 KEQ(J)=KEQ(J)+1                                                                     
      CALL PROCEQ(IDIR,IPOS,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH,                    
     .ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,IBOM)                                         
      IF(IBOM)9,636,9                                                                     
C---- *TPC DIRECTIVE                                                                      
  660 KD(J)=KD(J)+1                                                                       
      K=KD(J)                                                                             
      IPOS=6                                                                              
      IF(NEXITM(ITR,IDIR,IPOS))9,9,661                                                    
  661 ITRIG(J,K)=ITR                                                                      
      IF(NEXITM(LC,IDIR,IPOS))9,662,9                                                     
  662 IF(LC-LCOM)9,663,9                                                                  
  663 IF(NEXITM(LTR,IDIR,IPOS))9,9,664                                                    
  664 LTRIG(J,K)=LTR                                                                      
      IF(NEXITM(LC,IDIR,IPOS))9,665,9                                                     
  665 IF(LC-LCOM)666,667,666                                                              
  666 NLPC(K)=0                                                                           
      GO TO 11                                                                            
  667 IF(NEXITM(NLPC(K),IDIR,IPOS))9,9,11                                                 
C---- *FCH DIRECTIVE                                                                      
  680 KD(J)=KD(J)+1                                                                       
      K=KD(J)                                                                             
      IPOS=6                                                                              
      IF(NEXITM(NB,IDIR,IPOS))9,9,681                                                     
  681 NB5(K)=NB                                                                           
      IF(NEXITM(LC,IDIR,IPOS))9,682,9                                                     
  682 IF(LC-LCOM)9,683,9                                                                  
  683 KP=KPOS1(K)                                                                         
      CALL DOLSTR(IDIR,IPOS,IHD1,KP,NCH)                                                  
      IF(NCH)684,684,685                                                                  
  684 WRITE(LP,4)                                                                         
      GO TO 9                                                                             
  685 KPOS1(K+1)=KPOS1(K)+NCH                                                             
      NCHD1(K)=NCH                                                                        
      GO TO 11                                                                            
C---- TCH DIRECTIVE                                                                       
  700 KD(J)=KD(J)+1                                                                       
      K=KD(J)                                                                             
      IF(K.GT.1)GO TO 6                                                                   
      IPOS=6                                                                              
      IF(NEXITM(LC,IDIR,IPOS))9,696,699                                                   
  696 IF(LC-LMIN)9,697,9                                                                  
  697 IF(NEXITM(NB6,IDIR,IPOS))9,9,698                                                    
  698 NB6=-NB6                                                                            
      GO TO 701                                                                           
  699 NB6=LC                                                                              
  701 IF(NEXITM(LC,IDIR,IPOS))9,702,9                                                     
  702 IF(LC-LCOM)9,703,9                                                                  
  703 IF(NEXITM(ITR,IDIR,IPOS))9,9,704                                                    
  704 ITRIG(J,K)=ITR                                                                      
      IF(NEXITM(LC,IDIR,IPOS))9,705,9                                                     
  705 IF(LC-LCOM)9,706,9                                                                  
  706 IF(NEXITM(LTR,IDIR,IPOS))9,9,707                                                    
  707 LTRIG(J,K)=LTR                                                                      
      KEQ(J)=0                                                                            
      NT1CH=0                                                                             
      NT2CH=0                                                                             
      NT3CH=0                                                                             
      I1CH(1)=1                                                                           
      I3CH(1)=1                                                                           
  708 CALL DIRDRT(5,6,IDIR,IOF)                                                           
      IF(IOF)709,709,100                                                                  
  709 IPOS=1                                                                              
      IF(NEXITM(LS,IDIR,IPOS))9,710,9                                                     
  710 IF(LS-LSTAR)9,711,9                                                                 
  711 IF(NEXITM(LOP,IDIR,IPOS))712,9,9                                                    
  712 IF(LOP-IHOL(12))15,713,15                                                           
  713 KEQ(J)=KEQ(J)+1                                                                     
      CALL PROCEQ(IDIR,IPOS,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,                    
     .ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,IBOM)                                         
      IF(IBOM)9,708,9                                                                     
C---- *TCC DIRECTIVE                                                                      
  740 KD(J)=KD(J)+1                                                                       
      K=KD(J)                                                                             
      IPOS=6                                                                              
      IF(NEXITM(ITR,IDIR,IPOS))9,9,741                                                    
  741 ITRIG(J,K)=ITR                                                                      
      IF(NEXITM(LC,IDIR,IPOS))9,742,9                                                     
  742 IF(LC-LCOM)9,743,9                                                                  
  743 IF(NEXITM(LTR,IDIR,IPOS))9,9,744                                                    
  744 LTRIG(J,K)=LTR                                                                      
      IF(NEXITM(LC,IDIR,IPOS))9,745,9                                                     
  745 IF(LC-LCOM)746,747,746                                                              
  746 NLCC(K)=0                                                                           
      GO TO 11                                                                            
  747 IF(NEXITM(NLCC(K),IDIR,IPOS))9,9,11                                                 
C---- END OF DIRECTIVE READING AND CRACKING                                               
C---- CHECK ON PRESENCE OF COMPULSORY DIRECTIVES                                          
  100 IBOM=0                                                                              
      IF(KD(7).LT.1)CALL MISDIR(4H*COL,IBOM)                                              
      IF(KD(8).LT.1)CALL MISDIR(4H*ROW,IBOM)                                              
      IF(KD(6).LT.1)CALL MISDIR(4H*PFX,IBOM)                                              
      IF(KD(9).LT.1)CALL MISDIR(5H*DATA,IBOM)                                             
      IF(IBOM.GT.0)GO TO 2000                                                             
C---- READ FIRST DATA RECORD, WRITE HEADINGS, ETC.                                        
      PGEKNT=0                                                                            
      ROWKNT=0                                                                            
      COLKNT=0                                                                            
      LENCLR=MACHC*LWORDS(MAXCOL)                                                         
  800 CALL READUN(10,LARD,IFLAG,LAST)                                                     
      IF(IFLAG) 998,799,999                                                               
  799 KDKNT=1                                                                             
      LDKNT=1                                                                             
      PGEKNT=PGEKNT+1                                                                     
      CALL NEWPGE(LARD,LARD,KACR,COLKNT,ROWKNT,PGEKNT,MXR,JBLK,MAXCOL,                    
     .KD,NB3,NB4,NB5,NB6,IFH,IHD,KPOS,NCHD,KEQ,ITH,ITRIG,LTRIG,ICOL,IHD1                  
     .,KPOS1,NCHD1,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH,ISTR3PH,                     
     .I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,                    
     .ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,L3CH,IBCH,ILCH,LAST,NB1)                             
C---- WRITE FIRST DATA RECORD                                                             
  820 ROWKNT=ROWKNT+1                                                                     
      CALL COPAGE(ICOL(COLKNT),ROWKNT,PGEKNT,LARD,IDA,LDA)                                
C---- BEGIN MAJOR LOOP TO READ AND PROCESS ALL DATA RECORDS                               
 1000 CALL READUN(10,KARD,IFLAG,LAST)                                                     
      IF(IFLAG) 1001,900,999                                                              
C---- CHECK FOR CHANGE IN ANY TRIGGER FIELD                                               
C---- SET SWITCH ON FOR ACTIVE TRIGGERS                                                   
  900 INDTR=0                                                                             
      INCPFX=0                                                                            
      NLDP=0                                                                              
      NLDC=0                                                                              
      KDKNT=KDKNT+1                                                                       
      DO 910 ITG=1,NTG                                                                    
      K=KD(ITG)                                                                           
      IF(K)999,910,901                                                                    
  901 KK=1                                                                                
  902 IBEG=ITRIG(ITG,KK)                                                                  
      ILEN=LTRIG(ITG,KK)                                                                  
      ISSW(ITG,KK)=0                                                                      
      CALL COPYC(LARD,IBEG,KSTR,1,ILEN)                                                   
      KM=MATCHF(KARD,IBEG,KSTR,ILEN)                                                      
      IF(KM)904,904,903                                                                   
  903 IF(K-1)910,910,905                                                                  
  905 KK=KK+1                                                                             
      IF(KK-K)902,902,910                                                                 
  904 ISSW(ITG,KK)=1                                                                      
      INDTR=1                                                                             
  910 CONTINUE                                                                            
C---- BRANCH ACCORDING TO TRIGGER OPERATIVE ACTIVITY STATUS                               
      IF(INDTR)999,911,912                                                                
C---- NO TRIGGER ACTION NEEDED - CHECK DUPLICATE PREFIX STATUS                            
C---- AND WRITE DATA RECORD                                                               
  911 DO 890 ITG=1,5                                                                      
      IF(INDTRG(ITG,KD(ITG),KORD))999,890,891                                             
  890 CONTINUE                                                                            
      ROWKNT=ROWKNT+NBBR+1                                                                
      GO TO 892                                                                           
  891 ROWKNT=ROWKNT+1                                                                     
  892 IF(ROWKNT-MXR)913,913,914                                                           
  914       CALL MAXROW(KACR,ROWKNT,COLKNT,PGEKNT,LARD,KARD,NLDP,NLDC,                    
     .NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,LTRIG,                     
     .NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,                   
     .L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH,                      
     .ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                      
     .KPOS,NCHD,ITH,NB1)                                                                  
      INCPFX=1                                                                            
      ROWKNT=ROWKNT+1                                                                     
  913 IF(INDPX)999,915,916                                                                
  915 IF(INCPFX)999,919,916                                                               
  919 LEN=IPX-IDA                                                                         
      CALL COPAGE(ICOL(COLKNT),ROWKNT,PGEKNT,KARD,IDA,LEN)                                
      CALL COPAGE(ICOL(COLKNT)+LEN+LPX,ROWKNT,PGEKNT,KARD,IDA+LEN+LPX,LD                  
     .A-LEN-LPX)                                                                          
      LDKNT=LDKNT+1                                                                       
      GO TO 917                                                                           
  916 CALL COPAGE(ICOL(COLKNT),ROWKNT,PGEKNT,KARD,IDA,LDA)                                
      LDKNT=LDKNT+1                                                                       
  917 DO 918 I=1,LAST                                                                     
  918 LARD(I)=KARD(I)                                                                     
      GO TO 1000                                                                          
C---- A TRIGGER OPERATIVE IS ACTIVE                                                       
C---- IDENTIFY ACTIVE TRIGGERS AND CARRY OUT APPROPRIATE ACTIONS                          
C---- TPH BRANCH                                                                          
  912 IF(INDTRG(1,KD(1),LORD))999,105,50                                                  
   50 NLDP=1                                                                              
      IF(INDTRG(3,KD(3),KORD))999,61,51                                                   
   61 CALL MISDIR(4H*TPC,IBOM)                                                            
      GO TO 2000                                                                          
   51 KACR=0                                                                              
      IF(NLPC(KORD))999,62,55                                                             
   62 IF(INDNZ)999,52,53                                                                  
   52 CALL PRNTPG(PGEKNT,MXR,MAXCOL)                                                      
      PGEKNT=0                                                                            
   53 PGEKNT=PGEKNT+1                                                                     
      ROWKNT=1                                                                            
      COLKNT=1                                                                            
      KACR=0                                                                              
      DO 54 J=1,MXR                                                                       
   54 CALL COPAGE(1,J,PGEKNT,JBLK,1,LENCLR)                                               
            CALL HEADS( 10, IHD,KACR,COLKNT,ROWKNT,PGEKNT,LARD,KARD,NLDP                  
     .,NLDC,NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,                     
     .LTRIG,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,                  
     .L1CH,L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH                  
     .,ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                     
     .KPOS,NCHD,ITH,NB1)                                                                  
      GO TO 49                                                                            
   55 CALL SPACE(NLINS,KACR,MXR,MAXCOL,JBLK,PGEKNT)                                       
      IF(NLINS-NLPC(KORD))62,63,63                                                        
   63 COLKNT=1                                                                            
      ROWKNT=KACR                                                                         
      IF(INDTRG(5,KD(5),K))999,49,64                                                      
   64 ROWKNT=ROWKNT+NB2(K)                                                                
      KACR=ROWKNT                                                                         
   49       CALL HEADS(  1,KARD,KACR,COLKNT,ROWKNT,PGEKNT,LARD,KARD,NLDP                  
     .,NLDC,NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,                     
     .LTRIG,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,                  
     .L1CH,L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH                  
     .,ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                     
     .KPOS,NCHD,ITH,NB1)                                                                  
      COLKNT=1                                                                            
   56       CALL HEADS( 11,IHD1,KACR,COLKNT,ROWKNT,PGEKNT,LARD,KARD,NLDP                  
     .,NLDC,NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,                     
     .LTRIG,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,                  
     .L1CH,L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH                  
     .,ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                     
     .KPOS,NCHD,ITH,NB1)                                                                  
   57 IF(INDTRG(2,KD(2),LORD))999,58,59                                                   
   58       CALL HEADS(  2,LARD,KACR,COLKNT,ROWKNT,PGEKNT,LARD,KARD,NLDP                  
     .,NLDC,NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,                     
     .LTRIG,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,                  
     .L1CH,L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH                  
     .,ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                     
     .KPOS,NCHD,ITH,NB1)                                                                  
      GO TO 60                                                                            
   59 NLDC=1                                                                              
            CALL HEADS(  2,KARD,KACR,COLKNT,ROWKNT,PGEKNT,LARD,KARD,NLDP                  
     .,NLDC,NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,                     
     .LTRIG,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,                  
     .L1CH,L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH                  
     .,ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                     
     .KPOS,NCHD,ITH,NB1)                                                                  
   60 INCPFX=1                                                                            
      ROWKNT=ROWKNT+NB1                                                                   
      GO TO 911                                                                           
C---- TPC BRANCH                                                                          
  105 IF(INDTRG(3,KD(3),KORD))999,150,101                                                 
  101 IF(INDTRG(1,KD(1),LORD))999,106,50                                                  
  106 IF(NLPC(KORD))999,102,107                                                           
  107 CALL SPACE(NLINS,KACR,MXR,MAXCOL,JBLK,PGEKNT)                                       
      IF(NLINS-NLPC(KORD))102,108,108                                                     
  108 COLKNT=1                                                                            
      ROWKNT=KACR                                                                         
      IF(INDTRG(5,KD(5),K))999,56,109                                                     
  109 ROWKNT=ROWKNT+NB2(K)                                                                
      KACR=ROWKNT                                                                         
      GO TO 56                                                                            
  102 PGEKNT=PGEKNT+1                                                                     
      COLKNT=1                                                                            
      ROWKNT=1                                                                            
      KACR=0                                                                              
      DO 103 J=1,MXR                                                                      
  103 CALL COPAGE(1,J,PGEKNT,JBLK,1,LENCLR)                                               
            CALL HEADS( 10, IHD,KACR,COLKNT,ROWKNT,PGEKNT,LARD,KARD,NLDP                  
     .,NLDC,NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,                     
     .LTRIG,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,                  
     .L1CH,L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH                  
     .,ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                     
     .KPOS,NCHD,ITH,NB1)                                                                  
            CALL HEADS(  1,LARD,KACR,COLKNT,ROWKNT,PGEKNT,LARD,KARD,NLDP                  
     .,NLDC,NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,                     
     .LTRIG,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,                  
     .L1CH,L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH                  
     .,ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                     
     .KPOS,NCHD,ITH,NB1)                                                                  
      GO TO 56                                                                            
C---- TCH BRANCH                                                                          
  150 IF(INDTRG(2,KD(2),LORD))999,200,151                                                 
  151 NLDC=1                                                                              
      IF(INDTRG(1,KD(1),LORD))999,152,50                                                  
  152 IF(INDTRG(3,KD(3),KORD))999,153,101                                                 
  153 IF(INDTRG(4,KD(4),IORD))999,165,161                                                 
  161 IF(COLKNT-NCOLS)154,162,162                                                         
  162 IF(NLCC(IORD))999,154,163                                                           
  163 CALL SPACE(NLINS,KACR,MXR,MAXCOL,JBLK,PGEKNT)                                       
      IF(NLINS-NLCC(IORD))154,164,164                                                     
  164 COLKNT=0                                                                            
      ROWKNT=KACR                                                                         
      IF(INDTRG(5,KD(5),K))999,154,167                                                    
  167 ROWKNT=ROWKNT+NB2(K)                                                                
      KACR=ROWKNT                                                                         
  154       CALL MAXROW(KACR,ROWKNT,COLKNT,PGEKNT,LARD,KARD,NLDP,NLDC,                    
     .NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,LTRIG,                     
     .NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,L1CH,                   
     .L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH,                      
     .ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                      
     .KPOS,NCHD,ITH,NB1)                                                                  
      GO TO 160                                                                           
  165 IF(INDTRG(5,KD(5),K))999,155,166                                                    
  166 ROWKNT=ROWKNT+NB2(K)                                                                
  155       CALL HEADS(  2,KARD,KACR,COLKNT,ROWKNT,PGEKNT,LARD,KARD,NLDP                  
     .,NLDC,NCOLS,KD,NB3,NB4,NB5,NB6,ICOL,IHD1,KPOS1,NCHD1,KEQ,ITRIG,                     
     .LTRIG,NT1CH,NT2CH,NT3CH,IVAL1CH,IVAL2CH,ISTR1CH,ISTR3CH,I1CH,I3CH,                  
     .L1CH,L3CH,IBCH,ILCH,LAST,NT1PH,NT2PH,NT3PH,IVAL1PH,IVAL2PH,ISTR1PH                  
     .,ISTR3PH,I1PH,I3PH,L1PH,L3PH,IBPH,ILPH,MXR,JBLK,MAXCOL,IFH,IHD,                     
     .KPOS,NCHD,ITH,NB1)                                                                  
      ROWKNT=ROWKNT+NB1                                                                   
  160 INCPFX=1                                                                            
      GO TO 911                                                                           
C---- TCC BRANCH                                                                          
  200 IF(INDTRG(4,KD(4),IORD))999,250,201                                                 
  201 IF(INDTRG(1,KD(1),LORD))999,202,50                                                  
  202 IF(INDTRG(3,KD(3),KORD))999,203,101                                                 
  203 IF(INDTRG(2,KD(2),LORD))999,161,205                                                 
  205 NLDC=1                                                                              
      GO TO 161                                                                           
C---- TBL BRANCH                                                                          
  250 IF(INDTRG(5,KD(5),K))999,300,253                                                    
  253 ROWKNT=ROWKNT+NB2(K)                                                                
      INCPFX=1                                                                            
      GO TO 911                                                                           
C---- PFX BRANCH                                                                          
  300 IF(INDTRG(6,KD(6),K))999,911,301                                                    
  301 INCPFX=1                                                                            
      GO TO 911                                                                           
C---- END OF DATA PROCESSING                                                              
 1001 CALL PRNTPG(PGEKNT,MXR,MAXCOL)                                                      
      CALL EOFR(10,KDKNT)                                                                 
      CALL EOFW(11,LDKNT)                                                                 
      REWIND 10                                                                           
      REWIND 11                                                                           
      STOP                                                                                
C---- NO DATA FOUND                                                                       
  998 PRINT 21                                                                            
   21 FORMAT(19H NO DATA ON TAPE 10)                                                      
      GO TO 2000                                                                          
C---- ERRORS BRANCH                                                                       
  999 CALL LOGIC(5HSHEET)                                                                 
 2000 CONTINUE                                                                            
      CALL JOBEND                                                                         
      END                                                                                 
