*TEXT                                                                                     
      SUBROUTINE INTACT(ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,                   
     1EFF1,EFF2,IM,NCLN,NOM)                                                              
C     SUBROUTINE WHICH CHECKS FOR THE PRESENCE OF CONSTANTS FOR AN      DEC17000          
C     INTERACTION AND SETS ONES, ZEROS AND MINUS ONES INTO THE X ARRAY  DEC17001          
C     ------------------------------------------------------            DEC17002          
      DIMENSION INT1(30),INT2(30),X(106),NMC(30),MSCL(100),NCL(20),EFF1(DEC17005          
     150),EFF2(50),IM(90),NCLN(40)                                      DEC17006          
      N=ME1*100+ME2                                                     DEC17007          
      INT=ME2*100+ME1                                                   DEC17008          
      J=0                                                               DEC17009          
      M=0                                                               DEC17010          
    1 M=M+1                                                             DEC17011          
      IF (M.GT.N2F) GO TO 9                                             DEC17012          
      I=INT1(M)*100+INT2(M)                                             DEC17013          
      IF (N.EQ.I) GO TO 2                                               DEC17014          
      IF (INT.EQ.I) GO TO 8                                             DEC17015          
      GO TO 1                                                           DEC17016          
    8 J=1                                                               DEC17017          
    2 INT=M                                                             DEC17018          
      IF (ME1.GT.NOM) GO TO 30                                          DEC17019          
      N1=NCL(ME1)                                                       DEC17020          
      GO TO 31                                                          DEC17021          
   30 I=ME1-NOM                                                         DEC17022          
      N1=NCLN(I)                                                        DEC17023          
   31 IF (ME2.GT.NOM) GO TO 32                                          DEC17024          
      N2=NCL(ME2)                                                       DEC17025          
      GO TO 33                                                          DEC17026          
   32 I=ME2-NOM                                                         DEC17027          
      N2=NCLN(I)                                                        DEC17028          
   33 N=NOT+M-1                                                         DEC17029          
      IB=IM(N)+1                                                        DEC17030          
      MS1=0                                                             DEC17031          
      N=M-1                                                             DEC17032          
      IF (N.EQ.0) GO TO 14                                              DEC17033          
      DO 11 I=1,N                                                       DEC17034          
   11 MS1=MS1+NMC(I)                                                    DEC17035          
   14 IF (J.EQ.1) GO TO 3                                               DEC17036          
      NCL1=N1                                                           DEC17037          
      NCL2=N2                                                           DEC17038          
      N1=NI                                                             DEC17039          
      N2=NJ                                                             DEC17040          
      GO TO 12                                                          DEC17041          
    3 NCL1=N2                                                           DEC17042          
      NCL2=N1                                                           DEC17043          
      N1=NJ                                                             DEC17044          
      N2=NI                                                             DEC17045          
   12 L=NCL1-1                                                          DEC17046          
      M=NCL2-1                                                          DEC17047          
      NCEL=L*M-NMC(INT)                                                 DEC17048          
      NEND=IB+NCEL-1                                                    DEC17049          
      DO 15 I=IB,NEND                                                   DEC17050          
   15 X(I)=0.                                                           DEC17051          
      DO 16 I=1,L                                                       DEC17052          
   16 EFF1(I)=0.                                                        DEC17053          
      DO 17 I=1,M                                                       DEC17054          
   17 EFF2(I)=0.                                                        DEC17055          
      IF (N1.EQ.NCL1) GO TO 18                                          DEC17056          
      EFF1(N1)=1.                                                       DEC17057          
      GO TO 19                                                          DEC17058          
   18 DO 20 I=1,L                                                       DEC17059          
   20 EFF1(I)=-1.                                                       DEC17060          
   19 IF (N2.EQ.NCL2) GO TO 21                                          DEC17061          
      EFF2(N2)=1.                                                       DEC17062          
      GO TO 22                                                          DEC17063          
   21 DO 23 I=1,M                                                       DEC17064          
   23 EFF2(I)=-1.                                                       DEC17065          
   22 DO 24 I=1,L                                                       DEC17066          
      DO 25 J=1,M                                                       DEC17067          
      K=I*100+J                                                         DEC17068          
      IF (NMC(INT).EQ.0) GO TO 26                                       DEC17069          
      K1=MS1+NMC(INT)                                                   DEC17070          
   27 MS1=MS1+1                                                         DEC17071          
      IF (MS1.GT.K1) GO TO 26                                           DEC17072          
      IF (K-MSCL(MS1)) 27,25,27                                         DEC17073          
   26 X(IB)=EFF1(I)*EFF2(J)                                             DEC17074          
      IB =IB+1                                                          DEC17075          
   25 CONTINUE                                                          DEC17076          
   24 CONTINUE                                                          DEC17077          
    9 RETURN                                                            DEC17078          
      END                                                               DEC17079          
*ENDTEXT                                                                                  
