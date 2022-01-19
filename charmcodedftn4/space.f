      SUBROUTINE SPACE(NLINS,LINTAB,MXR,MAXCOL,JBLK,PGEKNT)                               
C---- WRITTEN R.E. 1977                                                                   
C---- USED BY PROGRAM SHEET AND RETURNS .....                                             
C----         NLINS = NO. OF BLANK ROWS AVAILABLE ON PAGE PGEKNT                          
C----         LINTAB = LAST ROW USED                                                      
      DIMENSION JBLK(1)                                                                   
      LEVEL 2, IPAGE                                                                      
      COMMON/ONE/IPAGE(14,70,80)                                                          
      INTEGER PGEKNT                                                                      
      J=MXR+1                                                                             
      IEND=LWORDS(MAXCOL)                                                                 
      KNT=0                                                                               
 1089 J=J-1                                                                               
      KNT=KNT+1                                                                           
      IF(J)1095,1095,1090                                                                 
 1090 I=0                                                                                 
 1091 I=I+1                                                                               
      IF(I-IEND)1092,1092,1089                                                            
 1092 IF(JBLK(I)-IPAGE(I,J,PGEKNT))1095,1091,1095                                         
 1095 LINTAB=J                                                                            
      NLINS=KNT-1                                                                         
      RETURN                                                                              
      END                                                                                 
