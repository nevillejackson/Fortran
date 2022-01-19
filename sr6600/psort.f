      SUBROUTINE PSORT(A,N,IND,NI)                                      PSORT002          
      INTEGER P                                                         PSORT003          
      DIMENSION A(N),IND(NI),INDU(16),INDL(16),IU(16),IL(16)            PSORT004          
C     CACM ALGORITH 410,  J.M. CHAMBERS, 15 JULY 1970                   PSORT005          
C PARAMETERS TO CHAM HAVE THE FOLLOWING MEANING                         PSORT006          
C A     ARRAY TO BE SORTED                                              PSORT007          
C N     NUMBER OF ELEMENTS IN A                                         PSORT008          
C IND   ARRAY OF INDICES IN ASCENDING ORDER                             PSORT009          
C NI    NUMBER OF ELEMENTS IN IND                                       PSORT010          
      JL=1                                                              PSORT011          
      JU=NI                                                             PSORT012          
      INDL(1)=1                                                         PSORT013          
      INDU(1)=NI                                                        PSORT014          
C ARRAYS INDL, INDU KEEP ACCOUNT OF THE PORTION OF IND RELATED TO THE   PSORT015          
C CURRENT SEGMENT OF DATA BEING ORDERED.                                PSORT016          
      I=1                                                               PSORT017          
      J=N                                                               PSORT018          
      M=1                                                               PSORT019          
100   IF(.NOT.(I.LT.J)) GOTO 570                                        PSORT020          
C FIRST ORDER A(I),A(J),A((I+J)/2), AND USE MEDIAN TO SPLIT THE DATA    PSORT021          
110   K=I                                                               PSORT022          
      IJ=(I+J)/2                                                        PSORT023          
      T=A(IJ)                                                           PSORT024          
      IF(.NOT.(A(I).GT.T)) GOTO 180                                     PSORT025          
      A(IJ)=A(I)                                                        PSORT026          
      A(I)=T                                                            PSORT027          
      T=A(IJ)                                                           PSORT028          
180   L=J                                                               PSORT029          
      IF(.NOT.(A(J).LT.T)) GOTO 300                                     PSORT030          
      A(IJ)=A(J)                                                        PSORT031          
      A(J)=T                                                            PSORT032          
      T=A(IJ)                                                           PSORT033          
      IF(.NOT.(A(I).GT.T)) GOTO 300                                     PSORT034          
      A(IJ)=A(I)                                                        PSORT035          
      A(I)=T                                                            PSORT036          
      T=A(IJ)                                                           PSORT037          
      GOTO 300                                                          PSORT038          
280   A(L)=A(K)                                                         PSORT039          
      A(K)=TT                                                           PSORT040          
300   L=L-1                                                             PSORT041          
      IF(.NOT.(A(L).LE.T)) GOTO 300                                     PSORT042          
      TT=A(L)                                                           PSORT043          
C SPLIT THE DATA INTO A(I TO L).LT.T, A(K TO J).GT.T                    PSORT044          
330   K=K+1                                                             PSORT045          
      IF(.NOT.(A(K).GE.T)) GOTO 330                                     PSORT046          
      IF(.NOT.(K.GT.L)) GOTO 280                                        PSORT047          
      INDL(M)=JL                                                        PSORT048          
      INDU(M)=JU                                                        PSORT049          
      P=M                                                               PSORT050          
      M=M+1                                                             PSORT051          
C SPLIT THE LARGER OF THE SEGMENTS                                      PSORT052          
      IF(.NOT.(L-I.GT.J-K)) GOTO 480                                    PSORT053          
      IL(P)=I                                                           PSORT054          
      IU(P)=L                                                           PSORT055          
      I=K                                                               PSORT056          
C SKIP ALL SEGMENTS NOT CORRESPONDING TO AN ENTRY IN IND                PSORT057          
440   IF(.NOT.(JL.LE.JU)) GOTO 570                                      PSORT058          
      IF(.NOT.(IND(JL).LT.I)) GOTO 650                                  PSORT059          
      JL=JL+1                                                           PSORT060          
      GOTO 440                                                          PSORT061          
480   IL(P)=K                                                           PSORT062          
      IU(P)=J                                                           PSORT063          
      J=L                                                               PSORT064          
510   IF(.NOT.(JL.LE.JU)) GOTO 570                                      PSORT065          
      IF(.NOT.(IND(JU).GT.J)) GOTO 550                                  PSORT066          
      JU=JU-1                                                           PSORT067          
      GOTO 510                                                          PSORT068          
550   INDL(P)=JU+1                                                      PSORT069          
      GOTO 660                                                          PSORT070          
570   M=M-1                                                             PSORT071          
      IF(M.EQ.0) GOTO 790                                               PSORT072          
      I=IL(M)                                                           PSORT073          
      J=IU(M)                                                           PSORT074          
      JL=INDL(M)                                                        PSORT075          
      JU=INDU(M)                                                        PSORT076          
      IF(JL.LE.JU) GOTO 660                                             PSORT077          
      GOTO 570                                                          PSORT078          
650   INDU(P)=JL-1                                                      PSORT079          
660   IF(.NOT.(J-I.LE.10)) GOTO 110                                     PSORT080          
      IF(.NOT.(I.NE.1)) GOTO 100                                        PSORT081          
      I=I-1                                                             PSORT082          
690   I=I+1                                                             PSORT083          
      IF(.NOT.(I.NE.J)) GOTO 570                                        PSORT084          
      T=A(I+1)                                                          PSORT085          
      IF(.NOT.(A(I).GT.T)) GOTO 690                                     PSORT086          
      K=I                                                               PSORT087          
740   A(K+1)=A(K)                                                       PSORT088          
      K=K-1                                                             PSORT089          
      IF(.NOT.(T.GE.A(K))) GOTO 740                                     PSORT090          
      A(K+1)=T                                                          PSORT091          
      GOTO 690                                                          PSORT092          
790   RETURN                                                            PSORT093          
      END                                                               PSORT094          
