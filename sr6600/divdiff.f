      SUBROUTINE DIVDIFF(N,X,Y,D,MAXDIFF,ISUPP)                         DIVDI002          
      REAL X,Y,D                                                        DIVDI003          
      DIMENSION X(1),Y(1),D(6,1),IVAR(6),IFORMAT(5)                     DIVDI004          
C                                                                       DIVDI005          
C         DIVIDED DIFFERENCES UP TO 6TH ORDER ARE FORMED FROM N PAIRS   DIVDI006          
C     X,Y AND PRINTED OUT IN STANDARD DIFFERENCE TABLE FORMAT WITH      DIVDI007          
C     FLOATING POINT REPRESENTATION E18.11.                             DIVDI008          
C         N  IS AN INTEGER SPECIFYING NUMBER OF PAIRS X,Y               DIVDI009          
C         X,Y MUST BE DIMENSIONED (.GE.N) EXTERNALLY                    DIVDI010          
C         MAXDIFF IS THE NUMBER OF DIFFERENCES PRINTED AND IS .LE.6     DIVDI011          
C         ISUPP   =0 IF PRINT OUT IS TO BE SUPPRESSED AND =1 OTHERWISE  DIVDI012          
C         D(6,N) ARE THE DIFFERENCES AND MUST BE DIMENSIONED EXTERNALLY DIVDI013          
C                WITH DIMENSION D(6,N1)  WITH N1.GE.N .                 DIVDI014          
C     THE CENTRAL DIFFERENCES                                           DIVDI015          
C                             CDELTA(2*L ) Y(M)   =D(2*L ,M)            DIVDI016          
C                             CDELTA(2L+1)Y(M+1/2)=D(2L+1,M)            DIVDI017          
C     FDELTA(2*L )Y(M)=D(2*L ,M+L)  BDELTA(2*L )Y(M)=D(2*L ,M-L)        DIVDI018          
C     FDELTA(2L+1)Y(M)=D(2L+1,M+L)  BDELTA(2L-1)Y(M)=D(2L-1,M-L)        DIVDI019          
C                                                                       DIVDI020          
      NN=N-1                                                            DIVDI021          
      I=1                                                               DIVDI022          
31    IF(I.GT.NN) GOTO 39                                               DIVDI023          
      D(1,I)=(Y(I+1)-Y(I))/(X(I+1)-X(I))                                DIVDI024          
      I=I+1                                                             DIVDI025          
      GOTO 31                                                           DIVDI026          
39    CONTINUE                                                          DIVDI027          
      J=2                                                               DIVDI028          
61    IF(J.GT.6) GOTO 69                                                DIVDI029          
      IMIN=J/2+1                                                        DIVDI030          
      JJ=J-J/2                                                          DIVDI031          
      IMAX=N-JJ                                                         DIVDI032          
      JP=J-2*(J/2)                                                      DIVDI033          
      I=IMIN                                                            DIVDI034          
111   IF(I.GT.IMAX) GOTO 119                                            DIVDI035          
      IN=I+JP                                                           DIVDI036          
      ID2=I+JJ                                                          DIVDI037          
      ID1=I-J/2                                                         DIVDI038          
      D(J,I)=(D(J-1,IN)-D(J-1,IN-1))/(X(ID2)-X(ID1))                    DIVDI039          
      I=I+1                                                             DIVDI040          
      GOTO 111                                                          DIVDI041          
119   CONTINUE                                                          DIVDI042          
      J=J+1                                                             DIVDI043          
      GOTO 61                                                           DIVDI044          
69    CONTINUE                                                          DIVDI045          
      IF(ISUPP.EQ.0) GOTO 490                                           DIVDI046          
      WRITE(61,200)                                                     DIVDI047          
200   FORMAT(9X,1HX,19X,1HY)                                            DIVDI048          
      IVAR(1)=4H,1(1                                                    DIVDI049          
      IVAR(2)=4H,2(1                                                    DIVDI050          
      IVAR(3)=4H,3(1                                                    DIVDI051          
      IVAR(4)=4H,4(1                                                    DIVDI052          
      IVAR(5)=4H,5(1                                                    DIVDI053          
      IVAR(6)=4H,6(1                                                    DIVDI054          
      IFORMAT(1)=8H(1H+,33X                                             DIVDI055          
      IFORMAT(2)=IVAR(MAXDIFF)                                          DIVDI056          
      IFORMAT(3)=4H2X,1                                                 DIVDI057          
      IFORMAT(4)=4HHD,I                                                 DIVDI058          
      IFORMAT(5)=3H2))                                                  DIVDI059          
C                   IFORMAT=(34X,*(12X,1HD,I2))                         DIVDI060          
      WRITE(61,IFORMAT)(J,J=1,MAXDIFF)                                  DIVDI061          
      I=1                                                               DIVDI062          
331   IF(I.GT.N) GOTO 339                                               DIVDI063          
      JMAX=I-1                                                          DIVDI064          
      IF(.NOT.(JMAX.GT.MAXDIFF/2)) GOTO 370                             DIVDI065          
      JMAX=MAXDIFF/2                                                    DIVDI066          
370   IF(.NOT.(JMAX.GT.N-I)) GOTO 390                                   DIVDI067          
      JMAX=N-I                                                          DIVDI068          
390   KMAX=I                                                            DIVDI069          
      IF(.NOT.(KMAX.GT.MAXDIFF-MAXDIFF/2)) GOTO 420                     DIVDI070          
      KMAX=MAXDIFF-MAXDIFF/2                                            DIVDI071          
420   IF(.NOT.(KMAX.GT.N-I)) GOTO 440                                   DIVDI072          
      KMAX=N-I                                                          DIVDI073          
440   WRITE(61,450) X(I),Y(I),(D(2*J,I),J=1,JMAX)                       DIVDI074          
450   FORMAT(1H0,E18.11,2X,E18.11,3X,3(12X,E18.11))                     DIVDI075          
      WRITE(61,470)(D(2*K-1,I),K=1,KMAX)                                DIVDI076          
470   FORMAT(1H0,26X,3(12X,E18.11))                                     DIVDI077          
      I=I+1                                                             DIVDI078          
      GOTO 331                                                          DIVDI079          
339   CONTINUE                                                          DIVDI080          
490   RETURN                                                            DIVDI081          
      END                                                               DIVDI082          
