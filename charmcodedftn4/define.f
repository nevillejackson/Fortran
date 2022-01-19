      SUBROUTINE DEFINE                                                                   
C-----MODIFIED N.J. 1977                                                                  
C-----MACHINE DEPENDENT ROUTINE -- CYBER 76 VERSION                                       
C                                                                                         
      COMMON /MACH/ MACHB,MACHC,MACHCD,MACHCC,LC,LP,LCP,LTR,LTP,LPL,LSP                   
      COMMON /CONST/ KONST(64)                                                            
C     MACH   MACHC   NUMBER OF CHARACTERS IN A WORD                                       
C     ====   MACHCD  NUMBER OF WORDS IN AN 80 COL CARD                                    
C            LP      UNIT FOR PRINTER OUTPUT                                              
C            MACHB   NUMBER OF BITS PER CHARACTER                                         
C            LC      UNIT FOR CARD INPUT                                                  
C-----LP     UNIT NUMBER FOR LINE PRINTER                                                 
C     DEFINE                                                                              
C     ------                                                                              
C                                                                                         
C     CALL DEFINE(LC,L)                                                                   
C                                                                                         
C     SETS UP MACHINE CONSTANTS IN COMMON / MACH / AND MUST BE CALLED                     
C     BY ALL PROGRAMS                                                                     
C                                                                                         
C     SETS UP CHARACTER CONSTANTS RIGHT JUSTIFIED AND ZERO FULLED IN /CONST/              
C               DISPLAY  GRAPHIC  026 PUNCH                                               
C     KONST(01)     00B     :     2-8                                                     
C     KONST(02)     01B     A                                                             
C     KONST(03)     02B     B                                                             
C     KONST(04)     03B     C                                                             
C     KONST(05)     04B     D                                                             
C     KONST(06)     05B     E                                                             
C     KONST(07)     06B     F                                                             
C     KONST(08)     07B     G                                                             
C     KONST(09)     10B     H                                                             
C     KONST(10)     11B     I                                                             
C     KONST(11)     12B     J                                                             
C     KONST(12)     13B     K                                                             
C     KONST(13)     14B     L                                                             
C     KONST(14)     15B     M                                                             
C     KONST(15)     16B     N                                                             
C     KONST(16)     17B     O                                                             
C     KONST(17)     20B     P                                                             
C     KONST(18)     21B     Q                                                             
C     KONST(19)     22B     R                                                             
C     KONST(20)     23B     S                                                             
C     KONST(21)     24B     T                                                             
C     KONST(22)     25B     U                                                             
C     KONST(23)     26B     V                                                             
C     KONST(24)     27B     W                                                             
C     KONST(25)     30B     X                                                             
C     KONST(26)     31B     Y                                                             
C     KONST(27)     32B     Z                                                             
C     KONST(28)     33B     0                                                             
C     KONST(28)     34B     1                                                             
C     KONST(30)     35B     2                                                             
C     KONST(31)     36B     3                                                             
C     KONST(32)     37B     4                                                             
C     KONST(33)     40B     5                                                             
C     KONST(34)     41B     6                                                             
C     KONST(35)     42B     7                                                             
C     KONST(36)     43B     8                                                             
C     KONST(37)     44B     9                                                             
C     KONST(38)     45B     +                                                             
C     KONST(39)     46B     -                                                             
C     KONST(40)     47B     *                                                             
C     KONST(41)     50B     /                                                             
C     KONST(42)     51B     (                                                             
C     KONST(43)     52B     )                                                             
C     KONST(44)     53B     $                                                             
C     KONST(45)     54B     =                                                             
C     KONST(46)     55B           BLANK                                                   
C     KONST(47)     56B     ,     COMMA                                                   
C     KONST(48)     57B     .     PERIOD                                                  
C     KONST(49)     60B     #     0-8-6                                                   
C     KONST(50)     61B     [     8-7                                                     
C     KONST(51)     62B     ]     0-8-2                                                   
C     KONST(52)     63B     %     8-6                                                     
C     KONST(53)     64B     "     8-4                                                     
C     KONST(54)     65B     _     0-8-5                                                   
C     KONST(55)     66B     !     11-0                                                    
C     KONST(56)     67B     &     0-8-7                                                   
C     KONST(57)     70B     '     11-8-5                                                  
C     KONST(58)     71B     ?     11-8-6                                                  
C     KONST(59)     72B     <     12-0                                                    
C     KONST(60)     73B     >     11-8-7                                                  
C     KONST(61)     74B     @     8-5                                                     
C     KONST(62)     75B     \     12-8-5                                                  
C     KONST(63)     76B     ^     12-8-6                                                  
C     KONST(64)     77B     ;     12-8-7                                                  
C                                                                                         
C     PARAMETER                                                                           
C                                                                                         
C     LP     UNIT NUMBER FOR FATAL ERROR MESSAGES                                         
C-----LC     UNIT NUMBER FOR CARD INPUT                                                   
C                                                                                         
      MACHB=6                                                                             
      MACHC = 10                                                                          
      MACHCD = 8                                                                          
      MACHCC=MACHC*MACHCD                                                                 
      LC=5                                                                                
      LP=6                                                                                
C                                                                                         
      DO 1 I=1,64                                                                         
    1 KONST(I)=I-1                                                                        
      RETURN                                                                              
C                                                                                         
      END                                                                                 
