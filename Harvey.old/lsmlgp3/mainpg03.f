*TEXT                                                                                     
      PROGRAM MAINPG                                                      010001          
      COMMON A(12000)                                                                     
      COMMON /CMBLK2/MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,      010003          
     1 MXNMJC,MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,     010004          
     2 MXK9,MXNOS,MXNED ,MXI2                                             010005          
      COMMON/SUBS/ INDEN,INLIT,INMA,INMC,IINT1,IINT2,IMSCL,INBEG,INEGX,   010006          
     1ILOGE,ILQC,INREGP,ILGTX,IJBEG,INDECX,ILITR,IXM,INEGY,ILNY,ILHY,     010007          
     2IKBEG,INDECY,IIC,IEFF1,IEFF2,INOS,IX,INMAC,IIED                     010008          
      WRITE(6,6)                                                          010009          
    6 FORMAT(1H1)                                                         010010          
C-----                                                                    010011          
C----- READ IN MAX SIZES OF ARRAYS                                        010012          
C----                                                                     010013          
      READ (5,2)  MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,MXNMJC,  010014          
     1MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,MXK9,MXNOS,  010015          
     2 MXNED ,MXI2                                                        010016          
    2 FORMAT(21I3)                                                        010017          
      WRITE(6,3)  MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,MXNMJC,  010018          
     1MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,MXK9,MXNOS,  010019          
     2 MXNED ,MXI2                                                        010020          
    3 FORMAT(10H0MXNLHM = ,I4/10H MXNRHM = ,I4/10H MXNCAS = ,I4/          010021          
     1 10H MXNCF  = ,I4/10H MXMN2P = ,I4/10H MXNS2  = ,I4/10H MXNSME = ,  010022          
     2 I4/10H MXNMJC = ,I4/10H MXNMIC = ,I4/10H MXME   = ,I4/             010023          
     3 10H MXMECL = ,I4/10H MXNE   = ,I4/10H MXNECL = ,I4/10H MX2F   = ,  010024          
     4 I4/10H MX2FMS = ,I4/10H MXNPR  = ,I4/10H MXNCD  = ,I4/             010025          
     5 10H MXK9   = ,I4/10H MXNOS  = ,I4/10H MXNED  = ,I4/                010026          
     6 10H MXI2   = ,I4)                                                  010027          
C-----                                                                    010028          
C----- CALCULATE STARTING ADDRESS OF EACH ARRAY IN BLANK COMMON           010029          
C----                                                                     010030          
      IARRAY=1                                                            010031          
      ISSCPR=IARRAY+MXNLHM*(MXNLHM+1)/2 + MXNLHM*MXNRHM + MXI2            010032          
      ISSS=ISSCPR+MAX0(MXNRHM*(MXNRHM+1)/2,5)                             010033          
      IRHM=ISSS+MAX0(MXNRHM*(MXNRHM+1)/2,MXI2+MXNSME*MXNRHM*5,MXNOS)      010034          
      ITOT=IRHM+MAX0(MXNLHM*MXNRHM,MXI2*(MXI2+1)+MXI2*MXNRHM)             010035          
      ITOT2=ITOT+MXNLHM+MXNRHM                                            010036          
      ITOT3=ITOT2+MAX0(MXNLHM+MXNRHM,MXNCF)                               010037          
      ILAB1=ITOT3+MXNLHM+MXNRHM                                           010038          
      ILAB2=ILAB1+MXNLHM                                                  010039          
      ILAB3=ILAB2+MXNLHM                                                  010040          
      ILAB4=ILAB3+MXNLHM                                                  010041          
      ILITY=ILAB4+MXNLHM                                                  010042          
      ITRED=ILITY+MXNRHM                                                  010043          
      IYM=ITRED+MXNRHM                                                    010044          
      IIM=IYM+MXNRHM                                                      010045          
      IMS=IIM+MXMN2P                                                      010046          
      IIPL=IMS+MXNS2                                                      010047          
      INSP=IIPL+MXNSME                                                    010048          
      INND=INSP+MXNSME                                                    010049          
      IXP=INND+MXNSME                                                     010050          
      IYP=IXP+MXNCAS                                                      010051          
      INDC=IYP+MXNRHM                                                     010052          
      INMI=INDC+MXNMJC                                                    010053          
      IMEN=INMI+MXNMIC                                                    010054          
      INCL=IMEN+MXME                                                      010055          
      ILME=INCL+MXME                                                      010056          
      IIBEG=ILME+MXME                                                     010057          
      IIDEN=IIBEG+MXME                                                    010058          
      ILIT=IIDEN+MXMECL                                                   010059          
      INEN=ILIT+MXME                                                      010060          
      INCLN=INEN+MXNE                                                     010061          
      ILNE=INCLN+MXNE                                                     010062          
      INDEN=ILNE+MXNE                                                     010063          
      INLIT=INDEN+MXNECL                                                  010064          
      INMA=INLIT+MXNE                                                     010065          
      INMC=INMA+MXNE                                                      010066          
      IINT1=INMC+MX2F                                                     010067          
      IINT2=IINT1+MX2F                                                    010068          
      IMSCL=IINT2+MX2F                                                    010069          
      INBEG=IMSCL+MX2FMS                                                  010070          
      INEGX=INBEG+MXNE                                                    010071          
      ILOGE=INEGX+MXNPR                                                   010072          
      ILQC=ILOGE+MXNPR                                                    010073          
      INREGP=ILQC+MXNPR                                                   010074          
      ILGTX=INREGP+MXNPR                                                  010075          
      IJBEG=ILGTX+MXNPR                                                   010076          
      INDECX=IJBEG+MAX0(MXNPR,MXNCF)                                                      
      ILITR=INDECX+MXNPR                                                  010078          
      IXM=ILITR+MXNPR                                                     010079          
      INEGY=IXM+MXNPR                                                     010080          
      ILNY=INEGY+MXNRHM                                                   010081          
      ILHY=ILNY+MXNRHM                                                    010082          
      IKBEG=ILHY+MXNRHM                                                   010083          
      INDECY=IKBEG+MXNRHM                                                 010084          
      IIC=INDECY+MXNRHM                                                   010085          
      IEFF1=IIC+MXNCD*80                                                  010086          
      IEFF2=IEFF1+MXK9                                                    010087          
      INOS=IEFF2+MXK9                                                     010088          
      IX=INOS+MXNOS                                                       010089          
      INMAC=IX+MXNLHM+MXNRHM                                              010090          
      IIED=INMAC+MXNE                                                                     
C-----                                                                    010094          
C----- CALCULATE LENGTH OF COMMON                                         010095          
C-----                                                                    010096          
      LCOMN=IIED+MXNED-1                                                                  
      WRITE(6,4) LCOMN,LCOMN                                              010098          
    4 FORMAT(10H0LCOMN  = ,I8,13H (DECIMAL) = ,O8,8H (OCTAL))             010099          
C-----                                                                    010100          
C----- TEST CORE SIZE NOT EXCEEDED                                        010101          
C-----                                                                    010102          
      IF(LCOMN-12000) 10,10,11                                                            
   11 WRITE(6,12)                                                         010105          
   12 FORMAT(27H0CORE REQUIREMENT TOO GREAT)                              010106          
      STOP                                                                010107          
   10 CONTINUE                                                            010108          
C-----                                                                    010109          
      CALL LSMLGQ(                                                        010110          
     1 A(IARRAY),A(ISSCPR),A(ISSS),A(IRHM),A(ITOT),A(ITOT2),A(ITOT3),     010111          
     2 A(ILAB1),A(ILAB2),A(ILAB3),A(ILAB4),A(ILITY),A(ITRED),A(IYM),      010112          
     3 A(IIM),A(IMS),A(IIPL),A(INSP),A(INND),A(IXP),A(IYP),A(INDC),       010113          
     4 A(INMI),A(IMEN),A(INCL),A(ILME),A(IIBEG),A(IIDEN),A(ILIT),A(INEN)  010114          
     5,A(INCLN),A(ILNE),A)                                                010115          
      STOP                                                                010116          
      END                                                                 010117          
*ENDTEXT                                                                                  
