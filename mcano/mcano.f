      PROGRAM MCANO
c----- units 5=stdin, 6=stdout
      DIMENSION XBAR(37),STD(37),CANR(37),CHISQ(37),NDF(37)             
      DIMENSION RX(1400),R(800),COEFL(1400),COEFR(1400)                 
      character*6 pr
    2 FORMAT(27H1CANONICAL CORRELATION ....,a//
     1'   NO. OF OBSERVATIONS',8X,I4/
     2'   NO. OF LEFT HAND VARIABLES',I5/
     3'   NO. OF RIGHT HAND VARIABLES',I4/)                                               
    3 FORMAT(6H0MEANS/(8F15.5))                                         
    4 FORMAT(20H0STANDARD DEVIATIONS/(8F15.5))                          
    5 FORMAT(25H0CORRELATION COEFFICIENTS)                              
    6 FORMAT(4H0ROW,I3/(10F12.5))                                       
    7 FORMAT(1H0//12H   NUMBER OF,7X,7HLARGEST,7X,13HCORRESPONDING,31X,7
     1HDEGREES/13H  EIGENVALUES,5X,10HEIGENVALUE,7X,9HCANONICAL,7X,6HLAM
     2BDA,5X,10HCHI-SQUARE,7X,2HOF/4X,7HREMOVED,7X,9HREMAINING,7X,11HCOR
     3RELATION,32X,7HFREEDOM/)                                          
    8 FORMAT(1H ,I7,F19.5,F16.5,2F14.5,5X,I5)                           
    9 FORMAT(1H0/22H CANONICAL CORRELATION,F12.5)                       
   10 FORMAT(39H0  COEFFICIENTS FOR LEFT HAND VARIABLES/(8F15.5))       
   11 FORMAT(40H0  COEFFICIENTS FOR RIGHT HAND VARIABLES/(8F15.5))      
C                                                                       
      NCR=5                                                             
      NLP=6                                                             
c     pr  - heading
c     n   - no of observations
c     mp  - no of LH variates
c     mq  - no of RH variates - mp>=mq
c     io  - input options  -1=read stats (call statin in corre)
c                           1=data in core
c                           0=read observations (call data in corre)
  100 READ(NCR,*,end=210) PR,N,MP,MQ,IO                                     
      WRITE(NLP,2) PR,N,MP,MQ                                       
      IF(N) 210,210,110                                                 
  110 M=MP+MQ                                                           
      X=0.0                                                             
      CALL CORRE(N,M,IO,X,XBAR,STD,RX,R,CANR,CHISQ,COEFL)               
      WRITE(NLP,3) (XBAR(I),I=1,M)                                      
      WRITE(NLP,4) (STD(I),I=1,M)                                       
      WRITE(NLP,5)                                                      
      DO 160 I=1,M                                                      
      DO 150 J=1,M                                                      
      IF(I-J) 120,130,130                                               
  120 L=I+(J*J-J)/2                                                     
      GO TO 140                                                         
  130 L=J+(I*I-I)/2                                                     
  140 CANR(J)=R(L)                                                      
  150 CONTINUE                                                          
  160 WRITE(NLP,6) I,(CANR(J),J=1,M)                                    
      CALL CANOR(N,MP,MQ,R,XBAR,STD,CANR,CHISQ,NDF,COEFR,COEFL,RX)      
      WRITE(NLP,7)                                                      
      DO 170 I=1,MQ                                                     
      N1=I-1                                                            
      IF(XBAR(I)) 165,165,170                                           
  165 MM=N1                                                             
      GO TO 175                                                         
  170 WRITE(NLP,8) N1,XBAR(I),CANR(I),STD(I),CHISQ(I),NDF(I)            
      MM=MQ                                                             
  175 N1=0                                                              
      N2=0                                                              
      DO 200 I=1,MM                                                     
      WRITE(NLP,9) CANR(I)                                              
      DO 180 J=1,MP                                                     
      N1=N1+1                                                           
  180 XBAR(J)=COEFL(N1)                                                 
      WRITE(NLP,10) (XBAR(J),J=1,MP)                                    
      DO 190 J=1,MQ                                                     
      N2=N2+1                                                           
  190 XBAR(J)=COEFR(N2)                                                 
      WRITE(NLP,11) (XBAR(J),J=1,MQ)                                    
  200 CONTINUE                                                          
      GO TO 100                                                         
  210 STOP                                                              
      END                                                               
