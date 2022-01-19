*TEXT                                                                                     
      SUBROUTINE INTACT(ME1,ME2,NI,NJ,N2F,NOT,INT1,INT2,X,NMC,MSCL,NCL,   200001          
     1EFF1,EFF2,IM,NCLN,NOM)                                              200002          
C     SUBROUTINE WHICH CHECKS FOR THE PRESENCE OF CONSTANTS FOR AN        200003          
C     INTERACTION AND SETS ONES, ZEROS AND MINUS ONES INTO THE X ARRAY    200004          
C     ------------------------------------------------------              200005          
      DIMENSION INT1(1),INT2(2),X(1),NMC(1),MSCL(1),NCL(1),EFF1(1),       200006          
     1 EFF2(1),IM(1),NCLN(1)                                              200007          
      N=ME1*1000+ME2                                                      200008          
      INT=ME2*1000+ME1                                                    200009          
      J=0                                                                 200010          
      M=0                                                                 200011          
    1 M=M+1                                                               200012          
      IF (M.GT.N2F) GO TO 9                                               200013          
      I=INT1(M)*1000+INT2(M)                                              200014          
      IF (N.EQ.I) GO TO 2                                                 200015          
      IF (INT.EQ.I) GO TO 8                                               200016          
      GO TO 1                                                             200017          
    8 J=1                                                                 200018          
    2 INT=M                                                               200019          
      IF (ME1.GT.NOM) GO TO 30                                            200020          
      N1=NCL(ME1)                                                         200021          
      GO TO 31                                                            200022          
   30 I=ME1-NOM                                                           200023          
      N1=NCLN(I)                                                          200024          
   31 IF (ME2.GT.NOM) GO TO 32                                            200025          
      N2=NCL(ME2)                                                         200026          
      GO TO 33                                                            200027          
   32 I=ME2-NOM                                                           200028          
      N2=NCLN(I)                                                          200029          
   33 N=NOT+M-1                                                           200030          
      IB=IM(N)+1                                                          200031          
      MS1=0                                                               200032          
      N=M-1                                                               200033          
      IF (N.EQ.0) GO TO 14                                                200034          
      DO 11 I=1,N                                                         200035          
   11 MS1=MS1+NMC(I)                                                      200036          
   14 IF (J.EQ.1) GO TO 3                                                 200037          
      NCL1=N1                                                             200038          
      NCL2=N2                                                             200039          
      N1=NI                                                               200040          
      N2=NJ                                                               200041          
      GO TO 12                                                            200042          
    3 NCL1=N2                                                             200043          
      NCL2=N1                                                             200044          
      N1=NJ                                                               200045          
      N2=NI                                                               200046          
   12 L=NCL1-1                                                            200047          
      M=NCL2-1                                                            200048          
      NCEL=L*M-NMC(INT)                                                   200049          
      NEND=IB+NCEL-1                                                      200050          
      DO 15 I=IB,NEND                                                     200051          
   15 X(I)=0.                                                             200052          
      DO 16 I=1,L                                                         200053          
   16 EFF1(I)=0.                                                          200054          
      DO 17 I=1,M                                                         200055          
   17 EFF2(I)=0.                                                          200056          
      IF (N1.EQ.NCL1) GO TO 18                                            200057          
      EFF1(N1)=1.                                                         200058          
      GO TO 19                                                            200059          
   18 DO 20 I=1,L                                                         200060          
   20 EFF1(I)=-1.                                                         200061          
   19 IF (N2.EQ.NCL2) GO TO 21                                            200062          
      EFF2(N2)=1.                                                         200063          
      GO TO 22                                                            200064          
   21 DO 23 I=1,M                                                         200065          
   23 EFF2(I)=-1.                                                         200066          
   22 DO 24 I=1,L                                                         200067          
      DO 25 J=1,M                                                         200068          
      K=I*1000+J                                                          200069          
      IF (NMC(INT).EQ.0) GO TO 26                                         200070          
      K1=MS1+NMC(INT)                                                     200071          
   27 MS1=MS1+1                                                           200072          
      IF (MS1.GT.K1) GO TO 26                                             200073          
      IF (K-MSCL(MS1)) 27,25,27                                           200074          
   26 X(IB)=EFF1(I)*EFF2(J)                                               200075          
      IB =IB+1                                                            200076          
   25 CONTINUE                                                            200077          
   24 CONTINUE                                                            200078          
    9 RETURN                                                              200079          
      END                                                                 200080          
*ENDTEXT                                                                                  
