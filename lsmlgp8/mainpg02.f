      PROGRAM MAINPG(INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT,                               
     1 TAPE10=/1000,TAPE11=130)                                                           
C-----CYBER 835 VERSION -- N.J. -- JUNE,1984                                              
      COMMON A(100000)                                                                    
      COMMON /CMBLK2/MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,           3          
     1 MXNMJC,MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,          4          
     2 MXK9,MXNOS,MXNED ,MXI2                                                  5          
      COMMON/SUBS/ INDEN,INLIT,INMA,INMC,IINT1,IINT2,IMSCL,INBEG,INEGX,        6          
     1ILOGE,ILQC,INREGP,ILGTX,IJBEG,INDECX,ILITR,IXM,INEGY,ILNY,ILHY,          7          
     2IKBEG,INDECY,IIC,IEFF1,IEFF2,INOS,IX,INMAC,IIED                          8          
      CALL REMARK(15HSTARTING MAINPG)                                                     
      WRITE(6,6)                                                               9          
    6 FORMAT(1H1)                                                             10          
C-----                                                                                    
C-----CHECK PROGRAM FIELD LENGTH AS LOADED                                                
C-----                                                                                    
      LPROG=20510                                                                         
      WRITE(6,7) LPROG,LPROG                                                              
    7 FORMAT(10H0LPROG  = ,I8,13H (DECIMAL) = ,O8,8H (OCTAL))                             
C-----                                                                        11          
C----- READ IN MAX SIZES OF ARRAYS                                            12          
C----                                                                         13          
      READ (5,2)  MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,MXNMJC,      14          
     1MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,MXK9,MXNOS,      15          
     2 MXNED ,MXI2                                                            16          
    2 FORMAT(21I3)                                                            17          
      WRITE(6,3)  MXNLHM,MXNRHM,MXNCAS,MXNCF,MXMN2P,MXNS2,MXNSME,MXNMJC,      18          
     1MXNMIC,MXME,MXMECL,MXNE,MXNECL,MX2F,MX2FMS,MXNPR,MXNCD,MXK9,MXNOS,      19          
     2 MXNED ,MXI2                                                            20          
    3 FORMAT(10H0MXNLHM = ,I4/10H MXNRHM = ,I4/10H MXNCAS = ,I4/              21          
     1 10H MXNCF  = ,I4/10H MXMN2P = ,I4/10H MXNS2  = ,I4/10H MXNSME = ,      22          
     2 I4/10H MXNMJC = ,I4/10H MXNMIC = ,I4/10H MXME   = ,I4/                 23          
     3 10H MXMECL = ,I4/10H MXNE   = ,I4/10H MXNECL = ,I4/10H MX2F   = ,      24          
     4 I4/10H MX2FMS = ,I4/10H MXNPR  = ,I4/10H MXNCD  = ,I4/                 25          
     5 10H MXK9   = ,I4/10H MXNOS  = ,I4/10H MXNED  = ,I4/                    26          
     6 10H MXI2   = ,I4)                                                      27          
C-----                                                                        28          
C----- CALCULATE STARTING ADDRESS OF EACH ARRAY IN BLANK COMMON               29          
C----                                                                         30          
      IARRAY=1                                                                31          
      ISSCPR=IARRAY+MXNLHM*(MXNLHM+1)/2 + MXNLHM*MXNRHM + MXI2                32          
      ISSS=ISSCPR+MAX0(MXNRHM*(MXNRHM+1)/2,5)                                 33          
      IRHM=ISSS+MAX0(MXNRHM*(MXNRHM+1)/2,MXI2+MXNSME*MXNRHM*5,MXNOS)          34          
      ITOT=IRHM+MAX0(MXNLHM*MXNRHM,MXI2*(MXI2+1)+MXI2*MXNRHM)                 35          
      ITOT2=ITOT+MXNLHM+MXNRHM                                                36          
      ITOT3=ITOT2+MAX0(MXNLHM+MXNRHM,MXNCF)                                   37          
      ILAB1=ITOT3+MXNLHM+MXNRHM                                               38          
      ILAB2=ILAB1+MXNLHM                                                      39          
      ILAB3=ILAB2+MXNLHM                                                      40          
      ILAB4=ILAB3+MXNLHM                                                      41          
      ILITY=ILAB4+MXNLHM                                                      42          
      ITRED=ILITY+MXNRHM                                                      43          
      IYM=ITRED+MXNRHM                                                        44          
      IIM=IYM+MXNRHM                                                          45          
      IMS=IIM+MXMN2P                                                          46          
      IIPL=IMS+MXNS2                                                          47          
      INSP=IIPL+MXNSME                                                        48          
      INND=INSP+MXNSME                                                        49          
      IXP=INND+MXNSME                                                         50          
      IYP=IXP+MXNCAS                                                          51          
      INDC=IYP+MXNRHM                                                         52          
      INMI=INDC+MXNMJC                                                        53          
      IMEN=INMI+MXNMIC                                                        54          
      INCL=IMEN+MXME                                                          55          
      ILME=INCL+MXME                                                          56          
      IIBEG=ILME+MXME                                                         57          
      IIDEN=IIBEG+MXME                                                        58          
      ILIT=IIDEN+MXMECL                                                       59          
      INEN=ILIT+MXME                                                          60          
      INCLN=INEN+MXNE                                                         61          
      ILNE=INCLN+MXNE                                                         62          
      INDEN=ILNE+MXNE                                                         63          
      INLIT=INDEN+MXNECL                                                      64          
      INMA=INLIT+MXNE                                                         65          
      INMC=INMA+MXNE                                                          66          
      IINT1=INMC+MX2F                                                         67          
      IINT2=IINT1+MX2F                                                        68          
      IMSCL=IINT2+MX2F                                                        69          
      INBEG=IMSCL+MX2FMS                                                      70          
      INEGX=INBEG+MXNE                                                        71          
      ILOGE=INEGX+MXNPR                                                       72          
      ILQC=ILOGE+MXNPR                                                        73          
      INREGP=ILQC+MXNPR                                                       74          
      ILGTX=INREGP+MXNPR                                                      75          
      IJBEG=ILGTX+MXNPR                                                       76          
      INDECX=IJBEG+MAX0(MXNPR,MXNCF)                                          77          
      ILITR=INDECX+MXNPR                                                      78          
      IXM=ILITR+MXNPR                                                         79          
      INEGY=IXM+MXNPR                                                         80          
      ILNY=INEGY+MXNRHM                                                       81          
      ILHY=ILNY+MXNRHM                                                        82          
      IKBEG=ILHY+MXNRHM                                                       83          
      INDECY=IKBEG+MXNRHM                                                     84          
      IIC=INDECY+MXNRHM                                                       85          
      IEFF1=IIC+MXNCD*80                                                      86          
      IEFF2=IEFF1+MXK9                                                        87          
      INOS=IEFF2+MXK9                                                         88          
      IX=INOS+MXNOS                                                           89          
      INMAC=IX+MXNLHM+MXNRHM                                                  90          
      IIED=INMAC+MXNE                                                         91          
C-----                                                                        92          
C----- CALCULATE LENGTH OF COMMON                                             93          
C-----                                                                        94          
      LCOMN=IIED+MXNED-1                                                      95          
      WRITE(6,4) LCOMN,LCOMN                                                  96          
    4 FORMAT(10H0LCOMN  = ,I8,13H (DECIMAL) = ,O8,8H (OCTAL))                 97          
C-----                                                                        98          
C----- TEST CORE SIZE NOT EXCEEDED                                            99          
C-----                                                                       100          
      IF(LCOMN-100000) 10,10,11                                                           
   11 WRITE(6,12)                                                            102          
   12 FORMAT(27H0CORE REQUIREMENT TOO GREAT)                                 103          
      STOP                                                                   104          
   10 CONTINUE                                                               105          
C-----                                                                                    
C-----CALCULATE PROGRAM FIELD LENGTH AS EXECUTED AND RFL                                  
C-----                                                                                    
      LPROG=LPROG+LCOMN                                                                   
      WRITE(6,7) LPROG,LPROG                                                              
C-----                                                                       106          
      CALL LSMLGQ(                                                           107          
     1 A(IARRAY),A(ISSCPR),A(ISSS),A(IRHM),A(ITOT),A(ITOT2),A(ITOT3),        108          
     2 A(ILAB1),A(ILAB2),A(ILAB3),A(ILAB4),A(ILITY),A(ITRED),A(IYM),         109          
     3 A(IIM),A(IMS),A(IIPL),A(INSP),A(INND),A(IXP),A(IYP),A(INDC),          110          
     4 A(INMI),A(IMEN),A(INCL),A(ILME),A(IIBEG),A(IIDEN),A(ILIT),A(INEN)     111          
     5,A(INCLN),A(ILNE),A)                                                   112          
      CALL REMARK(25HLSMLGP NORMAL TERMINATION)                                           
      STOP                                                                   113          
      END                                                                    114          
