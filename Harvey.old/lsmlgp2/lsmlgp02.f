*TEXT                                                                                     
      SUBROUTINE LSMLGP(                                                  040001          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    040002          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   040003          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       040004          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      040005          
     5EFF2,NOS,X,NMAC,IED)                                                                
C     LEAST SQUARES AND MAXIMUM LIKELIHOOD GENERAL PURPOSE PROGRAM        040007          
C     FORTRAN IV VERSION, CONVERTED TO FORTRAN IV FROM SCATRAN BY         040008          
C                       CHARLES GASKINS AND WALTER HARVEY                 040009          
C     LSMLGP DRIVER DECK                                                  040010          
C     --------------------------------------------------                  040011          
      DIMENSION ARRAY(1),SSCPR(1),SSS(1),RHM(1),TOT(1),TOT2(1),TOT3(1),   040012          
     1LAB1(1),LAB2(1),LAB3(1),LAB4(1),LITY(1),TRED(1),YM(1),IM(1),MS(1),  040013          
     2IPL(1),NSP(1),NND(1),XP(1),YP(1),NDC(1),NMI(1),MEN(1),NCL(1),LME(1  040014          
     3),IBEG(1),IDEN(1),LIT(1),NEN(1),NCLN(1),LNE(1),NDEN(1),NLIT(1),     040015          
     4NMA(1),NMC(1),INT1(1),INT2(1),MSCL(1),NBEG(1),NEGX(1),LOGE(1),      040016          
     5LQC(1),NREGP(1),LGTX(1),JBEG(1),NDECX(1),LITR(1),XM(1),NEGY(1),     040017          
     6LNY(1),LHY(1),KBEG(1),NDECY(1),IC(1),EFF1(1),EFF2(1),NOS(1),X(1),   040018          
     7NMAC(1),IED(1)                                                                      
      DIMENSION NEQ(10)                                                                   
      COMMON /CMBLK1/NEQ                                                  040021          
      COMMON /CMBLK2/MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,      040022          
     1 MXNMJC,MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,     040023          
     2 MXK9,MXNOS,MXNED ,MXI2                                             040024          
      COMMON /CMBLK3/IJOB,NAB,NLHM,NRHM,NMEA,NME,NNEA,NNE,N2F,NPR,IRAN,M  040025          
     1POP,LIOP,IAD,REP,LGT,IEI,IE,I309,NR1,NW,NS2,MULL,NOM,NON,ML,MLB,NS  040026          
     2,NOT,DF,KPUT,SDF,EDF,WK,MN2,NCDS,MATX,NCPR,NSME,NCAS,NLC,RR         040027          
     3,IN,NED                                                             040028          
      COMMON /CMBLK4/NCC,NMJC,ICN1,NMIC,NCD,ML3,INTV,K1,ICOD,ISW2,ISW3,   040029          
     1L,L7                                                                040030          
      DATA (IBLK=6H      )                                                040031          
    1 CONTINUE                                                            040032          
      KPUT=0                                                              040033          
      MULL=0                                                              040034          
      NSME=0                                                              040035          
      NCAS=0                                                              040036          
      I309=0                                                              040037          
      CALL PARAM(                                                         040040          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    040041          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   040042          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       040043          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      040044          
     5EFF2,NOS,X,NMAC,IED)                                                                
      IF (MULL.EQ.1) GO TO 900                                            040046          
      CALL MAIN1 (                                                        040047          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    040048          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   040049          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       040050          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      040051          
     5EFF2,NOS,X,NMAC,IED)                                                                
      IF (MULL.EQ.1) GO TO 900                                            040053          
      IF (LIOP.EQ.6.OR.KPUT.EQ.1) GO TO 900                               040054          
      CALL LSCOMB(                                                        040055          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    040056          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   040057          
     3NCLN,LNE,NDEN,NLIT,NMA,NMC,INT1,INT2,MSCL,NBEG,NEGX,LOGE,LQC,       040058          
     4NREGP,LGTX,JBEG,NDECX,LITR,XM,NEGY,LNY,LHY,KBEG,NDECY,IC,EFF1,      040059          
     5EFF2,NOS,X,NMAC,IED)                                                                
  900 CONTINUE                                                            040061          
      IF (MULL.EQ.0) GO TO 913                                            040062          
  910 CALL READIN(IC,IN,NTRA,IBLK,1,80,1)                                 040063          
      IF (NTRA.EQ.1) GO TO 910                                            040064          
  913 IF(MPOP-1) 914,1,915                                                040065          
  915 REWIND IN                                                           040066          
      GO TO 1                                                             040067          
  914 RETURN                                                              040068          
      END                                                                 040069          
*ENDTEXT                                                                                  
