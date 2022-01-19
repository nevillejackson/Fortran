      SUBROUTINE BESMNUZ(REZ,AIMZ,NU,BESIR,BESII,BESKR,BESKI)           BESSM002          
      REAL NU                                                           BESSM003          
      DIMENSION BESIR(1),BESII(1),BESKR(1),BESKI(1)                     BESSM004          
C   WRITTEN BY G. KEADY, C.S.I.R.O., MELBOURNE, DECEMBER, 1967          BESSM005          
C   MUST BE USED IN CONJUNCTION WITH C3 CSIR GAMMA AND C3 CSIR BESSNUZ  BESSM006          
C   EVALUATION OF MODIFIED BESSEL FUNCTION, REAL ORDER, COMPLEX ARGUMENTBESSM007          
C   NU IS (REAL) ORDER OF BESSEL FUNCTION                               BESSM008          
C   REZ IS THE REAL PART OF THE ARGUMENT Z                              BESSM009          
C   AIMZ IS THE IMAGINARY PART OF THE ARGUMENT Z                        BESSM010          
C   BESIR (M+1) RETURNS THE REAL PART OF THE BESSEL FUNCTION I          BESSM011          
C   BESII(M+1) RETURNS THE IMAG PART OF THE BESSEL FN I                 BESSM012          
C   BESKR(M+1) RETURNS THE REAL PART  OF THE BESSEL FN K                BESSM013          
C   BESKI (M+1) RETURNS THE IMAG PART OF THE BESSEL FN K                BESSM014          
      INU=NU                                                            BESSM015          
      I=IABS(INU)+1                                                     BESSM016          
      IF(.NOT.(ABS(REZ).LT.1.E-20)) GOTO 70                             BESSM017          
      ARGZ=SIGN(1.570796327,AIMZ)                                       BESSM018          
      GOTO 90                                                           BESSM019          
70    ARGZ=ATAN(AIMZ/REZ)                                               BESSM020          
      ARGZ=ARGZ+SIGN(3.141592654,ARGZ)*(SIGN(0.5,REZ)-0.5)              BESSM021          
90    PIT=1.570796327 $ A=PIT*NU                                        BESSM022          
      IF(.NOT.(ARGZ.LE.0.)) GOTO 300                                    BESSM023          
      X=-AIMZ $ Y=REZ                                                   BESSM024          
      RE=COS(-A) $ AIM=SIN(-A)                                          BESSM025          
      CALL BESSNUZ(X,Y,NU,BESIR,BESII,BESKR,BESKI)                      BESSM026          
170   BESJR=BESIR(I) $ BESJI=BESII(I)                                   BESSM027          
      BESYR=BESKR(I) $ BESYI=BESKI(I)                                   BESSM028          
      BESIR(I)=RE*BESJR-AIM*BESJI                                       BESSM029          
      BESII(I)=RE*BESJI+AIM*BESJR                                       BESSM030          
      BESKR(I)=PIT*(-RE*BESJI+AIM*BESJR-RE*BESYR-AIM*BESYI)             BESSM031          
      BESKI(I)=PIT*(RE*BESJR+AIM*BESJI-RE*BESYI+AIM*BESYR)              BESSM032          
      I=I-1 $ B=RE $ RE=-AIM $ AIM=B                                    BESSM033          
      IF(I.LE.0) GOTO 480                                               BESSM034          
      GOTO 170                                                          BESSM035          
300   X=AIMZ $ Y=-REZ                                                   BESSM036          
      RE=COS(A) $ AIM=SIN(A)                                            BESSM037          
      CALL BESSNUZ(X,Y,NU,BESIR,BESII,BESKR,BESKI)                      BESSM038          
350   BESJR=BESIR(I) $ BESJI=BESII(I)                                   BESSM039          
      BESYR=BESKR(I) $ BESYI=BESKI(I)                                   BESSM040          
      BESIR(I)=RE*BESJR-AIM*BESJI                                       BESSM041          
      BESII(I)=RE*BESJI+AIM*BESJR                                       BESSM042          
      BESKR(I)=PIT*(RE*BESJI-AIM*BESJR-RE*BESYR-AIM*BESYI)              BESSM043          
      BESKI(I)=PIT*(-RE*BESJR-AIM*BESJI-RE*BESYI+AIM*BESYR)             BESSM044          
      I=I-1 $ B=RE $ RE=AIM $ AIM=-B                                    BESSM045          
      IF(.NOT.(I.LE.0)) GOTO 350                                        BESSM046          
480   CONTINUE                                                          BESSM047          
      RETURN                                                            BESSM048          
      END                                                               BESSM049          
