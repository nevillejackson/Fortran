      SUBROUTINE IMXINV(IA,N,ID,IRR)                                    IMXIN002          
      DIMENSION IA(30,30),IB(30,30),IC(30)                              IMXIN003          
C     METHOD OF ROSSER TO REDUCE A TO UNIT MATRIX COLUMN BY COLUMN.     IMXIN004          
C     IA IS A MATRIX OF INTEGER ELEMENTS,ON EXIT THE INVERSE IS GIVEN BYIMXIN005          
C     THE ELEMENTS OF IA,DIVIDED BY ID ( THE DETERMINANT OF A).         IMXIN006          
C     IRR =1 OR 2 ON EXIT MEANS THE MATRIX IS SINGULAR,OR THE METHOD HASIMXIN007          
C     BROKEN DOWN                                                       IMXIN008          
C     INITIALISE WITH IB=THE UNIT MATRIX.                               IMXIN009          
      I=1                                                               IMXIN010          
21    IF(I.GT.N) GOTO 29                                                IMXIN011          
      J=1                                                               IMXIN012          
31    IF(J.GT.N) GOTO 39                                                IMXIN013          
      IB(I,J)=0                                                         IMXIN014          
      J=J+1                                                             IMXIN015          
      GOTO 31                                                           IMXIN016          
39    CONTINUE                                                          IMXIN017          
      IB(I,I)=1                                                         IMXIN018          
C     IF WILL CONTAIN THE DETERMINANT OF A(N-1),AND ID THAT OF A(N-1)*A.IMXIN019          
      I=I+1                                                             IMXIN020          
      GOTO 21                                                           IMXIN021          
29    CONTINUE                                                          IMXIN022          
      IF=1                                                              IMXIN023          
      ID=1                                                              IMXIN024          
      NN=N-1                                                            IMXIN025          
      J=1                                                               IMXIN026          
91    IF(J.GT.NN) GOTO 99                                               IMXIN027          
C     FIND LARGEST ELEMENT IN COL J                                     IMXIN028          
C     AND THE NEXT LARGEST ELEMENT,FROM ROW J ONWARDS.                  IMXIN029          
100   MX=0                                                              IMXIN030          
      MAX=0                                                             IMXIN031          
      NAX=0                                                             IMXIN032          
      I=1                                                               IMXIN033          
131   IF(I.GT.N) GOTO 139                                               IMXIN034          
      IF(IABS(IA(I,J))-MAX) 210,210,150                                 IMXIN035          
150   IF(MX-J) 180,160,160                                              IMXIN036          
160   NX=MX                                                             IMXIN037          
      NAX=MAX                                                           IMXIN038          
180   MAX=IABS(IA(I,J))                                                 IMXIN039          
      MX=I                                                              IMXIN040          
      GOTO 250                                                          IMXIN041          
210   IF(I-J) 250,220,220                                               IMXIN042          
220   IF(IABS(IA(I,J))-NAX) 250,250,230                                 IMXIN043          
230   NAX=IABS(IA(I,J))                                                 IMXIN044          
      NX=I                                                              IMXIN045          
250   CONTINUE                                                          IMXIN046          
      I=I+1                                                             IMXIN047          
      GOTO 131                                                          IMXIN048          
139   CONTINUE                                                          IMXIN049          
      IF(MAX) 270,270,290                                               IMXIN050          
C     MATRIX SINGULAR ,IRR=1                                            IMXIN051          
270   IRR=1                                                             IMXIN052          
      GOTO 840                                                          IMXIN053          
290   IF(NAX) 350,350,300                                               IMXIN054          
C     REDUCE ROW MX  BY ROW NX IN IA AND IB ARRAYS.                     IMXIN055          
300   IS=IA(MX,J)/MAX*IA(NX,J)/NAX                                      IMXIN056          
      K=1                                                               IMXIN057          
311   IF(K.GT.N) GOTO 319                                               IMXIN058          
      IA(MX,K)=IA(MX,K)-IS*IA(NX,K)                                     IMXIN059          
      IB(MX,K)=IB(MX,K)-IS*IB(NX,K)                                     IMXIN060          
      K=K+1                                                             IMXIN061          
      GOTO 311                                                          IMXIN062          
319   CONTINUE                                                          IMXIN063          
      GOTO 100                                                          IMXIN064          
350   IF(MX-J) 360,380,380                                              IMXIN065          
C     ALL ELEMENTS FROM ROW J ONWARDS ZERO,PROCESS FAILED.              IMXIN066          
360   IRR=2                                                             IMXIN067          
      GOTO 840                                                          IMXIN068          
C     REDUCE ALL ELEMENTS ABOVE ROW J TO ZERO,FOR COL J.                IMXIN069          
380   JJ=J-1                                                            IMXIN070          
      IF(JJ) 580,580,400                                                IMXIN071          
400   I=1                                                               IMXIN072          
401   IF(I.GT.JJ) GOTO 409                                              IMXIN073          
      IF(IA(I,J)) 420,570,420                                           IMXIN074          
C     FIND HCD OF IA(I,J) AND IA(MX,J)                                  IMXIN075          
420   NAX=IABS(IA(I,J))                                                 IMXIN076          
430   IF(MAX-(MAX/NAX)*NAX) 440,510,440                                 IMXIN077          
440   IS=MAX-NAX                                                        IMXIN078          
      IF(NAX-IS) 460,460,480                                            IMXIN079          
460   MAX=IS                                                            IMXIN080          
      GOTO 430                                                          IMXIN081          
480   MAX=NAX                                                           IMXIN082          
      NAX=IS                                                            IMXIN083          
      GOTO 430                                                          IMXIN084          
C     SUBTRACT ROW MX(SCALED) FROM ROW I                                IMXIN085          
510   MAX=IA(MX,J)/NAX                                                  IMXIN086          
      NAX=IA(I,J)/NAX                                                   IMXIN087          
      K=1                                                               IMXIN088          
531   IF(K.GT.N) GOTO 539                                               IMXIN089          
      IA(I,K)=IA(I,K)*MAX-IA(MX,K)*NAX                                  IMXIN090          
      IB(I,K)=IB(I,K)*MAX-IB(MX,K)*NAX                                  IMXIN091          
      K=K+1                                                             IMXIN092          
      GOTO 531                                                          IMXIN093          
539   CONTINUE                                                          IMXIN094          
      IF=IF*MAX                                                         IMXIN095          
570   CONTINUE                                                          IMXIN096          
C     MAKE NON ZERO ELEMENT THE DIAGONAL                                IMXIN097          
      I=I+1                                                             IMXIN098          
      GOTO 401                                                          IMXIN099          
409   CONTINUE                                                          IMXIN100          
580   IF(MX-J) 360,670,590                                              IMXIN101          
590   K=1                                                               IMXIN102          
591   IF(K.GT.N) GOTO 599                                               IMXIN103          
      IS=IA(MX,K)                                                       IMXIN104          
      IA(MX,K)=IA(J,K)                                                  IMXIN105          
      IA(J,K)=IS                                                        IMXIN106          
      IS=IB(MX,K)                                                       IMXIN107          
      IB(MX,K)=IB(J,K)                                                  IMXIN108          
      IB(J,K)=IS                                                        IMXIN109          
      K=K+1                                                             IMXIN110          
      GOTO 591                                                          IMXIN111          
599   CONTINUE                                                          IMXIN112          
      IF=-IF                                                            IMXIN113          
670   CONTINUE                                                          IMXIN114          
C     FORM INVERSE OF IA  (NOW A(N-1)*A)                                IMXIN115          
      J=J+1                                                             IMXIN116          
      GOTO 91                                                           IMXIN117          
99    CONTINUE                                                          IMXIN118          
      I=1                                                               IMXIN119          
681   IF(I.GT.N) GOTO 689                                               IMXIN120          
      ID=ID*IA(I,I)                                                     IMXIN121          
      I=I+1                                                             IMXIN122          
      GOTO 681                                                          IMXIN123          
689   CONTINUE                                                          IMXIN124          
      I=1                                                               IMXIN125          
701   IF(I.GT.NN) GOTO 709                                              IMXIN126          
      IA(I,I)=ID/IA(I,I)                                                IMXIN127          
      IA(I,N)=-IA(I,N)*IA(I,I)/IA(N,N)                                  IMXIN128          
      I=I+1                                                             IMXIN129          
      GOTO 701                                                          IMXIN130          
709   CONTINUE                                                          IMXIN131          
      IA(N,N)=ID/IA(N,N)                                                IMXIN132          
C     MULTIPLY IA*IB TO GET INVERSE,A(-1)=(A(N-1)*A)(-1)*A(N-1)/ID.     IMXIN133          
C     REDUCING THE DETERMINANT ID AND THE ELEMENTS BY FACTOR IF.        IMXIN134          
      I=1                                                               IMXIN135          
741   IF(I.GT.N) GOTO 749                                               IMXIN136          
      J=1                                                               IMXIN137          
751   IF(J.GT.N) GOTO 759                                               IMXIN138          
      IC(J)=0                                                           IMXIN139          
      K=1                                                               IMXIN140          
771   IF(K.GT.N) GOTO 779                                               IMXIN141          
      IC(J)=IC(J)+IA(I,K)*IB(K,J)                                       IMXIN142          
      K=K+1                                                             IMXIN143          
      GOTO 771                                                          IMXIN144          
779   CONTINUE                                                          IMXIN145          
      J=J+1                                                             IMXIN146          
      GOTO 751                                                          IMXIN147          
759   CONTINUE                                                          IMXIN148          
      J=1                                                               IMXIN149          
791   IF(J.GT.N) GOTO 799                                               IMXIN150          
      IA(I,J)=IC(J)/IF                                                  IMXIN151          
      J=J+1                                                             IMXIN152          
      GOTO 791                                                          IMXIN153          
799   CONTINUE                                                          IMXIN154          
      I=I+1                                                             IMXIN155          
      GOTO 741                                                          IMXIN156          
749   CONTINUE                                                          IMXIN157          
      ID=ID/IF                                                          IMXIN158          
C     NORMAL EXIT                                                       IMXIN159          
      IRR=0                                                             IMXIN160          
840   RETURN                                                            IMXIN161          
      END                                                               IMXIN162          
