      FUNCTION FRACT(LFRAC,LEN)                                                           
C-----WRITTEN N.J. 1977                                                                   
C-----CONVERTS FRACTIONAL PART LFRAC OF A FLOATING POINT NUMBER , TO                      
C----- A DECIMAL FRACTION FRACT AND COUNTS NO OF DIGITS AFTER DECIMAL                     
C-----FOR USE WHEN FRACTIONAL PART RETRIEVED AS AN INTEGER BY NEXITM                      
      F=LFRAC                                                                             
      N=0                                                                                 
    1 N=N+1                                                                               
      F=F/10.                                                                             
      IF(F-1.) 2,1,1                                                                      
    2 FRACT=F                                                                             
      LEN=N                                                                               
      RETURN                                                                              
      END                                                                                 
