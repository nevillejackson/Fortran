      subroutine itoa (int,num)                                         
c     ------------------------------------------------------            
c     subroutine which converts any 1 to 4 digit integer to character   
c     value. integer is received as 'int' and character is returned as  
c     'num'.                                                            
c     ------------------------------------------------------            
      include 'com8'
      character*6 num
      character*6 axi
      num(1:2)=blank
      num(3:6)=axi(int,idum,4,idum)
      return
      end
