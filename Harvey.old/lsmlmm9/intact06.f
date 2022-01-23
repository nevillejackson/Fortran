*TEXT                                                                                     
      SUBROUTINE INTACT (ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,DK150010          
     1EFF1,EFF2,IM,NCLN,NOM)                                            DK150020          
C     -------------------------------------------                       DK150030          
C     SUBROUTINE WHICH CHECKS FOR THE PRESENCE OF CONSTANTS FOR AN      DK150040          
C     INTERACTION AND SETS ONES, ZEROS AND MINUS ONES INTO THE X ARRAY  DK150050          
C     ------------------------------------------------------            DK150060          
      DIMENSION INT1(30),INT2(30),X(106),NMC(30),MSCL(100),NCL(20),EFF1(DK150080          
     150),EFF2(50),IM(90),NCLN(40)                                      DK150090          
      N=ME1*100+ME2                                                     DK150100          
      INT=ME2*100+ME1                                                   DK150110          
      J=0                                                               DK150120          
      M=0                                                               DK150130          
    1 M=M+1                                                             DK150140          
      IF (M.GT.N2F) GO TO 9                                             DK150150          
      I=INT1(M)*100+INT2(M)                                             DK150160          
      IF (N.EQ.I) GO TO 2                                               DK150170          
      IF (INT.EQ.I) GO TO 8                                             DK150180          
      GO TO 1                                                           DK150190          
    8 J=1                                                               DK150200          
    2 INT=M                                                             DK150210          
      IF (ME1.GT.NOM) GO TO 30                                          DK150220          
      N1=NCL(ME1)                                                       DK150230          
      GO TO 31                                                          DK150240          
   30 I=ME1-NOM                                                         DK150250          
      N1=NCLN(I)                                                        DK150260          
   31 IF (ME2.GT.NOM) GO TO 32                                          DK150270          
      N2=NCL(ME2)                                                       DK150280          
      GO TO 33                                                          DK150290          
   32 I=ME2-NOM                                                         DK150300          
      N2=NCLN(I)                                                        DK150310          
   33 N=NOT+M-1                                                         DK150320          
      IB=IM(N)+1                                                        DK150330          
      MS1=0                                                             DK150340          
      N=M-1                                                             DK150350          
      IF (N.EQ.0) GO TO 14                                              DK150360          
      DO 11 I=1,N                                                       DK150370          
   11 MS1=MS1+NMC(I)                                                    DK150380          
   14 IF (J.EQ.1) GO TO 3                                               DK150390          
      NCL1=N1                                                           DK150400          
      NCL2=N2                                                           DK150410          
      N1=NI                                                             DK150420          
      N2=NJ                                                             DK150430          
      GO TO 12                                                          DK150440          
    3 NCL1=N2                                                           DK150450          
      NCL2=N1                                                           DK150460          
      N1=NJ                                                             DK150470          
      N2=NI                                                             DK150480          
   12 L=NCL1-1                                                          DK150490          
      M=NCL2-1                                                          DK150500          
      NCEL=L*M-NMC(INT)                                                 DK150510          
      NEND=IB+NCEL-1                                                    DK150520          
      DO 15 I=IB,NEND                                                   DK150530          
   15 X(I)=0.                                                           DK150540          
      DO 16 I=1,L                                                       DK150550          
   16 EFF1(I)=0.                                                        DK150560          
      DO 17 I=1,M                                                       DK150570          
   17 EFF2(I)=0.                                                        DK150580          
      IF (N1.EQ.NCL1) GO TO 18                                          DK150590          
      EFF1(N1)=1.                                                       DK150600          
      GO TO 19                                                          DK150610          
   18 DO 20 I=1,L                                                       DK150620          
   20 EFF1(I)=-1.                                                       DK150630          
   19 IF (N2.EQ.NCL2) GO TO 21                                          DK150640          
      EFF2(N2)=1.                                                       DK150650          
      GO TO 22                                                          DK150660          
   21 DO 23 I=1,M                                                       DK150670          
   23 EFF2(I)=-1.                                                       DK150680          
   22 DO 24 I=1,L                                                       DK150690          
      DO 25 J=1,M                                                       DK150700          
      K=I*100+J                                                         DK150710          
      IF (NMC(INT).EQ.0) GO TO 26                                       DK150720          
      K1=MS1+NMC(INT)                                                   DK150730          
   27 MS1=MS1+1                                                         DK150740          
      IF (MS1.GT.K1) GO TO 26                                           DK150750          
      IF (K-MSCL(MS1)) 27,25,27                                         DK150760          
   26 X(IB)=EFF1(I)*EFF2(J)                                             DK150770          
      IB =IB+1                                                          DK150780          
   25 CONTINUE                                                          DK150790          
   24 CONTINUE                                                          DK150800          
    9 RETURN                                                            DK150810          
      END                                                               DK150820          
*ENDTEXT                                                                                  
