      SUBROUTINE INTLAG(X,Y,N,M,ARG,A,AA,L)                             INTLA002          
      DIMENSION X(1),Y(1),V(100),W(100)                                 INTLA003          
      A=ARG                                                             INTLA004          
      IF(N-M) 110,120,40                                                INTLA005          
40    K=M/2+1                                                           INTLA006          
      I=K                                                               INTLA007          
      IMAX=N-M/2+1                                                      INTLA008          
60    IF(A-X(I)) 90,70,70                                               INTLA009          
70    I=I+1                                                             INTLA010          
      IF(I-IMAX) 60,60,120                                              INTLA011          
90    K=I-K                                                             INTLA012          
      GOTO 130                                                          INTLA013          
110   M=N                                                               INTLA014          
120   K=N-M                                                             INTLA015          
130   IN=1                                                              INTLA016          
131   IF(IN.GT.M) GOTO 139                                              INTLA017          
      S=1.0                                                             INTLA018          
      W(IN)=S                                                           INTLA019          
      V(IN)=0.0                                                         INTLA020          
      J=1                                                               INTLA021          
161   IF(J.GT.M) GOTO 169                                               INTLA022          
      IF(J-IN) 180,310,180                                              INTLA023          
180   JJ=K+J                                                            INTLA024          
      W(IN)=W(IN)*(A-X(JJ))                                             INTLA025          
      NN=K+IN                                                           INTLA026          
      S=S*(X(NN)-X(JJ))                                                 INTLA027          
      KQZ001=L                                                          INTLA028          
      IF(KQZ001.LT.1) KQZ001=1                                          INTLA029          
      IF(KQZ001.GT.2) KQZ001=2                                          INTLA030          
      GOTO(310,230),KQZ001                                              INTLA031          
230   T=1.0                                                             INTLA032          
      I=1                                                               INTLA033          
241   IF(I.GT.M) GOTO 249                                               INTLA034          
      IF(I-J) 260,290,260                                               INTLA035          
260   IF(I-IN) 270,290,270                                              INTLA036          
270   II=K+I                                                            INTLA037          
      T=T*(A-X(II))                                                     INTLA038          
290   CONTINUE                                                          INTLA039          
      I=I+1                                                             INTLA040          
      GOTO 241                                                          INTLA041          
249   CONTINUE                                                          INTLA042          
      V(IN)=V(IN)+T                                                     INTLA043          
310   CONTINUE                                                          INTLA044          
      J=J+1                                                             INTLA045          
      GOTO 161                                                          INTLA046          
169   CONTINUE                                                          INTLA047          
      W(IN)=W(IN)/S                                                     INTLA048          
      V(IN)=V(IN)/S                                                     INTLA049          
      IN=IN+1                                                           INTLA050          
      GOTO 131                                                          INTLA051          
139   CONTINUE                                                          INTLA052          
      AA=0.0                                                            INTLA053          
      A=AA                                                              INTLA054          
      I=1                                                               INTLA055          
351   IF(I.GT.M) GOTO 359                                               INTLA056          
      II=K+I                                                            INTLA057          
      A=A+W(I)*Y(II)                                                    INTLA058          
      AA=AA+V(I)*Y(II)                                                  INTLA059          
      I=I+1                                                             INTLA060          
      GOTO 351                                                          INTLA061          
359   CONTINUE                                                          INTLA062          
      RETURN                                                            INTLA063          
      END                                                               INTLA064          
