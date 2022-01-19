*TEXT                                                                                     
      SUBROUTINE NEGNO(T,BEG,LFD,LIT,I)                                 DEC08000          
C     SUBROUTINE USED TO CHECK FOR 1620 TYPE NEGATIVE NUMBERS           DEC08000          
C     ----------------------------------------------------              DEC08001          
      WRITE (6,100)                                                     M0108002          
  100 FORMAT(//////69H **********  1620 TYPE NEGATIVE NUMBERS ARE NOT PEM0108003          
     1RMITTED  **********     )                                         M0108004          
      CALL Q8QERROR(0,6H STOP.)                                         M0108005          
      END                                                               DEC08044          
*ENDTEXT                                                                                  
