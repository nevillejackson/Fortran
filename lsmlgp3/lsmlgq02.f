*TEXT                                                                                     
      SUBROUTINE LSMLGQ(                                                  020001          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    020002          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   020003          
     3NCLN,LNE,A)                                                         020004          
C-----                                                                    020005          
C-----THIS SUBROUTINE DOES NOTHING BUT IS NECESSARY BECAUSE THE COMPILER  020006          
C-----CANNOT PROCESS AN ARGUMENT LIST MORE COMPLEX THAN THAT IN THE CALL                  
C-----LSMLGP STATEMENT                                                                    
C-----                                                                    020009          
      DIMENSION A(1)                                                      020010          
      DIMENSION ARRAY(1),SSCPR(1),SSS(1),RHM(1),TOT(1),TOT2(1),TOT3(1),   020011          
     1LAB1(1),LAB2(1),LAB3(1),LAB4(1),LITY(1),TRED(1),YM(1),IM(1),MS(1),  020012          
     2IPL(1),NSP(1),NND(1),XP(1),YP(1),NDC(1),NMI(1),MEN(1),NCL(1),LME(1  020013          
     3),IBEG(1),IDEN(1),LIT(1),NEN(1),NCLN(1),LNE(1)                      020014          
      COMMON/SUBS/ INDEN,INLIT,INMA,INMC,IINT1,IINT2,IMSCL,INBEG,INEGX,   020015          
     1ILOGE,ILQC,INREGP,ILGTX,IJBEG,INDECX,ILITR,IXM,INEGY,ILNY,ILHY,     020016          
     2IKBEG,INDECY,IIC,IEFF1,IEFF2,INOS,IX,INMAC,IIED                     020017          
C-----THE FOLLOWING STATEMENT IS NECESSARY TO OVERCOME A COMPILER BUG     020018          
C-----ENCOUNTERED ON USING OPT = 1 OR 2                                   020019          
      I=1                                                                 020020          
      CALL LSMLGP(                                                        020021          
     1ARRAY,SSCPR,SSS,RHM,TOT,TOT2,TOT3,LAB1,LAB2,LAB3,LAB4,LITY,TRED,    020022          
     2YM,IM,MS,IPL,NSP,NND,XP,YP,NDC,NMI,MEN,NCL,LME,IBEG,IDEN,LIT,NEN,   020023          
     3NCLN,LNE,A(INDEN),A(INLIT),A(INMA),A(INMC),A(IINT1),                020024          
     6 A(IINT2),A(IMSCL),A(INBEG),A(INEGX),A(ILOGE),A(ILQC),A(INREGP),    020025          
     7 A(ILGTX),A(IJBEG),A(INDECX),A(ILITR),A(IXM),A(INEGY),A(ILNY),      020026          
     8 A(ILHY),A(IKBEG),A(INDECY),A(IIC),A(IEFF1),A(IEFF2),A(INOS),       020027          
     9 A(IX),A(INMAC),A(IIED))                                            020028          
C-----THE FOLLOWING STATEMENT IS NECESSARY TO OVERCOME A COMPILER BUG     020029          
C-----ENCOUNTERED ON USING OPT = 1 OR 2                                   020030          
      I=I+1                                                               020031          
      RETURN                                                              020032          
      END                                                                 020033          
*ENDTEXT                                                                                  
