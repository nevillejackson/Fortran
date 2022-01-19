      SUBROUTINE SYMMPY(A,B,C,N)                                        SYMMP002          
      DIMENSION A(1000),B(1000),C(1000),D(50)                           SYMMP003          
C     FORMS C=A*B, WHERE A,B,AND C ARE SYMMETRIC N*N MATRICES,WITH ONLY SYMMP004          
C     THE LOWER HALVES STORED COLUMNWISE IN N(N+1)/2 STORAGE WORDS.     SYMMP005          
C     C CAN BE THE SAME ARRAY AS A. D IS A TEMPORARY ARRAY OF N WORDS.  SYMMP006          
      ICOL=0                                                            SYMMP007          
      I=1                                                               SYMMP008          
31    IF(I.GT.N) GOTO 39                                                SYMMP009          
      JCOL=0                                                            SYMMP010          
      J=1                                                               SYMMP011          
51    IF(J.GT.I) GOTO 59                                                SYMMP012          
      D(J)=0.                                                           SYMMP013          
      J=J+1                                                             SYMMP014          
      GOTO 51                                                           SYMMP015          
59    CONTINUE                                                          SYMMP016          
      J=1                                                               SYMMP017          
71    IF(J.GT.I) GOTO 79                                                SYMMP018          
      KCOL=J                                                            SYMMP019          
      KROW=I                                                            SYMMP020          
      K=1                                                               SYMMP021          
101   IF(K.GT.J) GOTO 109                                               SYMMP022          
      D(J)=D(J)+A(KROW)*B(KCOL)                                         SYMMP023          
      KROW=KROW+N-K                                                     SYMMP024          
      KCOL=KCOL+N-K                                                     SYMMP025          
      K=K+1                                                             SYMMP026          
      GOTO 101                                                          SYMMP027          
109   CONTINUE                                                          SYMMP028          
      IF(I-J) 200,200,150                                               SYMMP029          
150   KK=J+1                                                            SYMMP030          
      K=KK                                                              SYMMP031          
161   IF(K.GT.I) GOTO 169                                               SYMMP032          
      KCOL=JCOL+K                                                       SYMMP033          
      D(J)=D(J)+A(KROW)*B(KCOL)                                         SYMMP034          
      KROW=KROW+N-K                                                     SYMMP035          
      K=K+1                                                             SYMMP036          
      GOTO 161                                                          SYMMP037          
169   CONTINUE                                                          SYMMP038          
200   IF(I-N) 210,260,260                                               SYMMP039          
210   KK=I+1                                                            SYMMP040          
      K=KK                                                              SYMMP041          
221   IF(K.GT.N) GOTO 229                                               SYMMP042          
      KROW=ICOL+K                                                       SYMMP043          
      KCOL=JCOL+K                                                       SYMMP044          
      D(J)=D(J)+A(KROW)*B(KCOL)                                         SYMMP045          
      K=K+1                                                             SYMMP046          
      GOTO 221                                                          SYMMP047          
229   CONTINUE                                                          SYMMP048          
260   JCOL=JCOL+N-J                                                     SYMMP049          
      J=J+1                                                             SYMMP050          
      GOTO 71                                                           SYMMP051          
79    CONTINUE                                                          SYMMP052          
      JCOL=I                                                            SYMMP053          
      J=1                                                               SYMMP054          
281   IF(J.GT.I) GOTO 289                                               SYMMP055          
      C(JCOL)=D(J)                                                      SYMMP056          
      JCOL=JCOL+N-J                                                     SYMMP057          
      J=J+1                                                             SYMMP058          
      GOTO 281                                                          SYMMP059          
289   CONTINUE                                                          SYMMP060          
      ICOL=ICOL+N-I                                                     SYMMP061          
      I=I+1                                                             SYMMP062          
      GOTO 31                                                           SYMMP063          
39    CONTINUE                                                          SYMMP064          
      RETURN                                                            SYMMP065          
      END                                                               SYMMP066          
