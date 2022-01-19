      SUBROUTINE ABSINV(A,N,DET,EPS,ITER,IRR)                           ABSIN002          
      DIMENSION A(30,30),F(30,30),E(30,30)                              ABSIN003          
C     FORMS A INITIAL APPROXIMATION TO THE INVERSE (D) OF MATRIX A      ABSIN004          
C     THEN USES  D(M)=D(M-1)(2I-A*D(M-1)) TO ITERATE FOR AN ABSOLUTE    ABSIN005          
C     VALUE (ERROR LESS THAN EPS)                                       ABSIN006          
C     ITER GIVES A MAXIMUM FOR THE NUMBER OF ITERATIONS                 ABSIN007          
C     IRR IS NON ZERO IF THE MATRIX A IS SINGULAR.                      ABSIN008          
C     STORE MATRIX -A IN F                                              ABSIN009          
      IT=0                                                              ABSIN010          
      SS=1.0E90                                                         ABSIN011          
      I=1                                                               ABSIN012          
41    IF(I.GT.N) GOTO 49                                                ABSIN013          
      J=1                                                               ABSIN014          
51    IF(J.GT.N) GOTO 59                                                ABSIN015          
      F(I,J)=-A(I,J)                                                    ABSIN016          
C     INVERT A USING MATINV TO OBTAIN D(O) IN A                         ABSIN017          
      J=J+1                                                             ABSIN018          
      GOTO 51                                                           ABSIN019          
59    CONTINUE                                                          ABSIN020          
      I=I+1                                                             ABSIN021          
      GOTO 41                                                           ABSIN022          
49    CONTINUE                                                          ABSIN023          
      CALL MATINV(A,N,E,0,DET,IRR)                                      ABSIN024          
      IF(IRR) 210,90,210                                                ABSIN025          
C     FORM 2I-A*D(M-1)  IN E                                            ABSIN026          
90    CALL MATMPA(F,A,E,N,N,N)                                          ABSIN027          
      S=0.                                                              ABSIN028          
      I=1                                                               ABSIN029          
111   IF(I.GT.N) GOTO 119                                               ABSIN030          
      E(I,I)=2.0+E(I,I)                                                 ABSIN031          
      S=ABS(E(I,I)-1.0)+S                                               ABSIN032          
      I=I+1                                                             ABSIN033          
      GOTO 111                                                          ABSIN034          
119   CONTINUE                                                          ABSIN035          
      IF(S-EPS) 210,210,150                                             ABSIN036          
150   IF(S-10.0*SS) 160,160,200                                         ABSIN037          
160   SS=S                                                              ABSIN038          
C     FORM D(M) IN A                                                    ABSIN039          
      CALL MATMPA(A,E,A,N,N,N)                                          ABSIN040          
      IT=IT+1                                                           ABSIN041          
      IF(IT-ITER) 90,90,200                                             ABSIN042          
C     TOO MANY ITERATIONS, OR NOT CONVERGING.                           ABSIN043          
200   IRR=2                                                             ABSIN044          
C     ACCURATE INVERSE FOUND, OR GIVEN UP.                              ABSIN045          
210   CONTINUE                                                          ABSIN046          
      RETURN                                                            ABSIN047          
      END                                                               ABSIN048          
