      PROGRAM RANK(TAPE10=/1000,TAPE11=/1000,TAPE12=/1000                                 
     + ,TAPE13=/1000,TAPE14=/1000,TAPE15=/1000                                            
     + ,INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT)                                            
C-----WRITTEN R.E. 1977                                                                   
C-----RANKS DISPLAY CODE RECORDS IN A SPECIFIED ORDER ON A SINGLE                         
C-----NUMERIC FIELD.                                                                      
C-----MACHINE DEPENDENT ROUTINE - CYBER 76 VERSION                                        
      DIMENSION LIT(100)                                                                  
      DIMENSION IDIR(8)                                                                   
      DIMENSION LSTAR(1),LCOM(1),LOP(1)                                                   
      DIMENSION ITM(8),JTM(8),KTM(8)                                                      
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LCR,LP,LCP,LTR,LTP,LPL,LSP                  
      COMMON/NXST/IDIR,IDLEN,IPOS,ILEN,ITYP,IEND,KTM                                      
      CALL DEFINE                                                                         
      IDLEN=MACHCC                                                                        
      CALL MCHL(46,LSTAR,1)                                                               
      CALL MCH(40,LSTAR,1)                                                                
      CALL MCHL(46,LCOM,1)                                                                
      CALL MCH(47,LCOM,1)                                                                 
      CALL MCHL(46,LOP,6)                                                                 
      CALL MCH(29,LOP,6)                                                                  
      CALL MCH(09,LOP,6)                                                                  
      CALL MCH(02,LOP,6)                                                                  
      CALL MCH(29,LOP,6)                                                                  
      CALL MCH(09,LOP,6)                                                                  
      CALL MCH(05,LOP,6)                                                                  
      WRITE(LP,10)                                                                        
   10 FORMAT(9H0RANK RUN)                                                                 
      REWIND 10                                                                           
      REWIND 11                                                                           
      CALL LNREAD(LREC,LRECW,10)                                                          
      CALL LNWRIT(LREC,LRECW,11)                                                          
      LAST=LRECW+1                                                                        
      MREC=LAST*MACHC                                                                     
C-----READ, PRINT AND CRACK THE DIRECTIVE                                                 
      K=0                                                                                 
    1 CALL READCD(LCR,IDIR,IFLAG,MACHCD)                                                  
      IF(IFLAG) 30,15,48                                                                  
   15 CALL WRITLN(LP,IDIR,0,MACHCD)                                                       
      IPOS=1                                                                              
      K=K+1                                                                               
      IEND=0                                                                              
      IF(NXSC(ITM,1,LSTAR,1,1,0))9,16,9                                                   
    9 WRITE(LP,12)                                                                        
   12 FORMAT(42H UNRECOGNIZED DIRECTIVE ... RUN TERMINATED)                               
   13 CONTINUE                                                                            
      CALL JOBEND                                                                         
   16 I=NXSC(ITM,1,LOP,6,2,1) +1                                                          
      IF(NXSC(ITM,1,LCOM,1,1,0))9,17,9                                                    
   17 GO TO (9,20,21,48),I                                                                
   20 LCOL=0                                                                              
      GO TO 22                                                                            
   21 LCOL=1                                                                              
   22 IF(NXS(IBEG,0))9,9,23                                                               
   23 IF(NXSC(ITM,1,LCOM,1,1,0))9,24,9                                                    
   24 IF(NXS(LEN,0))9,9,28                                                                
   28 GO TO 1                                                                             
   30 IF(K-1) 31,32,31                                                                    
   31 WRITE(LP,14)                                                                        
   14 FORMAT(53H EXACTLY ONE DIRECTIVE IS REQUIRED ... RUN TERMINATED)                    
      GO TO 13                                                                            
C-----READ RECORDS AND ALLOCATE THEM TO EITHER POSITIVE OR NEGATIVE FILE                  
   32 NPOS=0                                                                              
      NNEG=0                                                                              
      KNT=0                                                                               
   33 CALL READUN(10,LIT,IFLAG,LAST)                                                      
      IF(IFLAG) 41,34,48                                                                  
   34 KNT=KNT+1                                                                           
      J=IBEG-1                                                                            
   35 J=J+1                                                                               
      IF(J-(IBEG+LEN-1)) 36,36,38                                                         
   36 K=ITYPE(IN(LIT,J))                                                                  
      IF(K-6)37,35,38                                                                     
   37 IF(K-4)38,39,38                                                                     
C-----WRITE POSITIVES ON TAPE12                                                           
   38 NPOS=NPOS+1                                                                         
      CALL WRITUN(12,LIT,LAST)                                                            
      GO TO 40                                                                            
C-----WRITE NEGATIVES ON TAPE13                                                           
   39 NNEG=NNEG+1                                                                         
      CALL WRITUN(13,LIT,LAST)                                                            
   40 GO TO 33                                                                            
   41 CALL EOFR(10,KNT)                                                                   
      ENDFILE 12                                                                          
      ENDFILE 13                                                                          
      CALL EOFW(12,NPOS)                                                                  
      CALL EOFW(13,NNEG)                                                                  
      REWIND 12                                                                           
      REWIND 13                                                                           
      REWIND 10                                                                           
      IF(NPOS)48,42,43                                                                    
   42 IF(NNEG)48,44,45                                                                    
   43 IF(NNEG)48,46,47                                                                    
C-----NO DATA FOUND                                                                       
   44 WRITE(LP,8)                                                                         
    8 FORMAT(34H NO DATA FOUND ON TAPE12 OR TAPE13)                                       
      GO TO 13                                                                            
   48 CALL LOGIC(4HRANK)                                                                  
      GO TO 13                                                                            
C-----ALL VALUES NEGATIVE                                                                 
   45 CALL SMSORT(MREC)                                                                   
      CALL SMFILE("SORT","FORMATTED",13,"REWIND")                                         
      CALL SMFILE("OUTPUT","FORMATTED",11,"NONE")                                         
      KNT=NNEG                                                                            
      IF(LCOL)48,49,50                                                                    
   49 CALL SMKEY(IBEG,1,LEN,0,"DISPLAY","ASCII6","D")                                     
      GO TO 51                                                                            
   50 CALL SMKEY(IBEG,1,LEN,0,"DISPLAY","ASCII6","A")                                     
   51 CALL SMEND                                                                          
      GO TO 62                                                                            
C-----ALL VALUES POSITIVE                                                                 
   46 CALL SMSORT(MREC)                                                                   
      CALL SMFILE("SORT","FORMATTED",12,"REWIND")                                         
      CALL SMFILE("OUTPUT","FORMATTED",11,"NONE")                                         
      KNT=NPOS                                                                            
      IF(LCOL)48,50,49                                                                    
C-----MIXED POSITIVE AND NEGATIVE VALUES                                                  
   47 CALL SMSORT(MREC)                                                                   
      CALL SMFILE("SORT","FORMATTED",12,"REWIND")                                         
      CALL SMFILE("OUTPUT","FORMATTED",14,"REWIND")                                       
      IF(LCOL)48,52,53                                                                    
   52 CALL SMKEY(IBEG,1,LEN,0,"DISPLAY","ASCII6","A")                                     
      GO TO 54                                                                            
   53 CALL SMKEY(IBEG,1,LEN,0,"DISPLAY","ASCII6","D")                                     
   54 CALL SMEND                                                                          
      CALL SMSORT(MREC)                                                                   
      CALL SMFILE("SORT","FORMATTED",13,"REWIND")                                         
      CALL SMFILE("OUTPUT","FORMATTED",15,"REWIND")                                       
      IF(LCOL)48,55,56                                                                    
   55 CALL SMKEY(IBEG,1,LEN,0,"DISPLAY","ASCII6","D")                                     
      GO TO 57                                                                            
   56 CALL SMKEY(IBEG,1,LEN,0,"DISPLAY","ASCII6","A")                                     
   57 CALL SMEND                                                                          
C-----JOIN THE SEPARATELY SORTED FILES TOGETHER ON TAPE11                                 
      KNT=0                                                                               
      IF(LCOL)48,58,59                                                                    
   58 DO 60 J=1,NNEG                                                                      
      CALL READUN(15,LIT,IFLAG,LAST)                                                      
      IF(IFLAG.NE.0)GO TO 48                                                              
      KNT=KNT+1                                                                           
   60 CALL WRITUN(11,LIT,LAST)                                                            
   61 IF(LCOL)48,59,62                                                                    
   59 DO 63 J=1,NPOS                                                                      
      CALL READUN(14,LIT,IFLAG,LAST)                                                      
      IF(IFLAG.NE.0)GO TO 48                                                              
      KNT=KNT+1                                                                           
   63 CALL WRITUN(11,LIT,LAST)                                                            
      IF(LCOL)48,62,58                                                                    
   62 ENDFILE 11                                                                          
      CALL EOFW(11,KNT)                                                                   
      REWIND 11                                                                           
      STOP                                                                                
      END                                                                                 
