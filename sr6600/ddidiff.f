      SUBROUTINE DDIDIFF(N,X,Y,D,MAXDIFF,ISUPP)                         DDIVD002          
      DOUBLEPRECISION X,Y,D                                             DDIVD003          
      DIMENSION X(1),Y(1),D(6,1),IVAR(6),IFORMAT(5)                     DDIVD004          
C                                                                       DDIVD005          
C         DIVIDED DIFFERENCES UP TO 6TH ORDER ARE FORMED FROM N PAIRS   DDIVD006          
C     X,Y AND PRINTED OUT IN STANDARD DIFFERENCE TABLE FORMAT WITH      DDIVD007          
C     FLOATING POINT REPRESENTATION E18.11.                             DDIVD008          
C         N  IS AN INTEGER SPECIFYING NUMBER OF PAIRS X,Y               DDIVD009          
C         X,Y MUST BE DIMENSIONED (.GE.N) EXTERNALLY                    DDIVD010          
C         MAXDIFF IS THE NUMBER OF DIFFERENCES PRINTED AND IS .LE.6     DDIVD011          
C         ISUPP   =0 IF PRINT OUT IS TO BE SUPPRESSED AND =1 OTHERWISE  DDIVD012          
C         D(6,N) ARE THE DIFFERENCES AND MUST BE DIMENSIONED EXTERNALLY DDIVD013          
C                WITH DIMENSION D(6,N1)  WITH N1.GE.N .                 DDIVD014          
C     THE CENTRAL DIFFERENCES                                           DDIVD015          
C                             CDELTA(2*L ) Y(M)   =D(2*L ,M)            DDIVD016          
C                             CDELTA(2L+1)Y(M+1/2)=D(2L+1,M)            DDIVD017          
C     FDELTA(2*L )Y(M)=D(2*L ,M+L)  BDELTA(2*L )Y(M)=D(2*L ,M-L)        DDIVD018          
C     FDELTA(2L+1)Y(M)=D(2L+1,M+L)  BDELTA(2L-1)Y(M)=D(2L-1,M-L)        DDIVD019          
C                                                                       DDIVD020          
      NN=N-1                                                            DDIVD021          
      I=1                                                               DDIVD022          
31    IF(I.GT.NN) GOTO 39                                               DDIVD023          
      D(1,I)=(Y(I+1)-Y(I))/(X(I+1)-X(I))                                DDIVD024          
      I=I+1                                                             DDIVD025          
      GOTO 31                                                           DDIVD026          
39    CONTINUE                                                          DDIVD027          
      J=2                                                               DDIVD028          
61    IF(J.GT.6) GOTO 69                                                DDIVD029          
      IMIN=J/2+1                                                        DDIVD030          
      JJ=J-J/2                                                          DDIVD031          
      IMAX=N-JJ                                                         DDIVD032          
      JP=J-2*(J/2)                                                      DDIVD033          
      I=IMIN                                                            DDIVD034          
111   IF(I.GT.IMAX) GOTO 119                                            DDIVD035          
      IN=I+JP                                                           DDIVD036          
      ID2=I+JJ                                                          DDIVD037          
      ID1=I-J/2                                                         DDIVD038          
      D(J,I)=(D(J-1,IN)-D(J-1,IN-1))/(X(ID2)-X(ID1))                    DDIVD039          
      I=I+1                                                             DDIVD040          
      GOTO 111                                                          DDIVD041          
119   CONTINUE                                                          DDIVD042          
      J=J+1                                                             DDIVD043          
      GOTO 61                                                           DDIVD044          
69    CONTINUE                                                          DDIVD045          
      IF(ISUPP.EQ.0) GOTO 490                                           DDIVD046          
      WRITE(61,200)                                                     DDIVD047          
200   FORMAT(9X,1HX,19X,1HY)                                            DDIVD048          
      IVAR(1)=4H,1(1                                                    DDIVD049          
      IVAR(2)=4H,2(1                                                    DDIVD050          
      IVAR(3)=4H,3(1                                                    DDIVD051          
      IVAR(4)=4H,4(1                                                    DDIVD052          
      IVAR(5)=4H,5(1                                                    DDIVD053          
      IVAR(6)=4H,6(1                                                    DDIVD054          
      IFORMAT(1)=8H(1H+,33X                                             DDIVD055          
      IFORMAT(2)=IVAR(MAXDIFF)                                          DDIVD056          
      IFORMAT(3)=4H2X,1                                                 DDIVD057          
      IFORMAT(4)=4HHD,I                                                 DDIVD058          
      IFORMAT(5)=3H2))                                                  DDIVD059          
C                   IFORMAT=(34X,*(12X,1HD,I2))                         DDIVD060          
      WRITE(61,IFORMAT)(J,J=1,MAXDIFF)                                  DDIVD061          
      I=1                                                               DDIVD062          
331   IF(I.GT.N) GOTO 339                                               DDIVD063          
      JMAX=I-1                                                          DDIVD064          
      IF(.NOT.(JMAX.GT.MAXDIFF/2)) GOTO 370                             DDIVD065          
      JMAX=MAXDIFF/2                                                    DDIVD066          
370   IF(.NOT.(JMAX.GT.N-I)) GOTO 390                                   DDIVD067          
      JMAX=N-I                                                          DDIVD068          
390   KMAX=I                                                            DDIVD069          
      IF(.NOT.(KMAX.GT.MAXDIFF-MAXDIFF/2)) GOTO 420                     DDIVD070          
      KMAX=MAXDIFF-MAXDIFF/2                                            DDIVD071          
420   IF(.NOT.(KMAX.GT.N-I)) GOTO 440                                   DDIVD072          
      KMAX=N-I                                                          DDIVD073          
440   WRITE(61,450) X(I),Y(I),(D(2*J,I),J=1,JMAX)                       DDIVD074          
450   FORMAT(1H0,D18.11,2X,D18.11,3X,3(12X,D18.11))                     DDIVD075          
      WRITE(61,470)(D(2*K-1,I),K=1,KMAX)                                DDIVD076          
470   FORMAT(1H0,26X,3(12X,D18.11))                                     DDIVD077          
      I=I+1                                                             DDIVD078          
      GOTO 331                                                          DDIVD079          
339   CONTINUE                                                          DDIVD080          
490   RETURN                                                            DDIVD081          
      END                                                               DDIVD082          
