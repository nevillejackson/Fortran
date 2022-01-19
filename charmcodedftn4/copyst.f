      SUBROUTINE COPYST( STRING, LENGTH, CARD, INDEX )                                    
C                                                                                         
      DIMENSION STRING( 1 ),CARD( 1 )                                                     
      INTEGER STRING,CARD                                                                 
C                                                                                         
C     COPYST                            WRITTEN 1973 AUTHOR J.N.                          
C     ------                                                                              
C                                                                                         
C     CALL COPYST( STRING, LENGTH, CARD, INDEX )                                          
C                                                                                         
C     ROUTINE FOR COPYING STRINGS WHERE INDEX OF CARD IS TO BE UPDATED                    
C                                                                                         
C     TYPICAL USE                                                                         
C                                                                                         
C     CALL COPYST(   2HPF, 2, CARD, I )                                                   
C     CALL COPYST( STRING, L, CARD, I )                                                   
C     CALL COPYST( 4H(DE), 4, CARD, I )                                                   
C                                                                                         
C     PARAMETERS                                                                          
C                                                                                         
C     STRING  CHARACTERS TO BE COPIED                                                     
C     LENGTH  NUMBER OF CHARACTERS TO BE COPIED                                           
C     CARD    DESTINATION                                                                 
C     INDEX   STRING TO START AT CARD (INDEX) UPDATED ON EXIT                             
C                                                                                         
      CALL COPYC(STRING,1,CARD,INDEX,LENGTH)                                              
C                                                                                         
      INDEX = INDEX + LENGTH                                                              
C                                                                                         
      RETURN                                                                              
C                                                                                         
      END                                                                                 
