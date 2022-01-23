      subroutine tpage(ntitl)                                           
c     -------------------------------------------                       
c     subroutine to print titles                                        
c     -------------------------------------------                       
      include 'decl1'
      include 'com5'
      write(6,1000)                                                     
      if(ntitl.eq.0) return                                             
      go to (10,20,30,40,50),ntitl                                      
   10 write(6,1001) titl1                                               
      go to 60                                                          
   20 write(6,1001) titl1                                               
      write(6,1001) titl2                                               
      go to 60                                                          
   30 write(6,1001) titl1                                               
      write(6,1001) titl2                                               
      write(6,1001) titl3                                               
      go to 60                                                          
   40 write(6,1001) titl1                                               
      write(6,1001) titl2                                               
      write(6,1001) titl3                                               
      write(6,1001) titl4                                               
      go to 60                                                          
   50 write(6,1001) titl1                                               
      write(6,1001) titl2                                               
      write(6,1001) titl3                                               
      write(6,1001) titl4                                               
      write(6,1001) titl5                                               
   60 write(6,1002)                                                     
 1000 format(1h1//)                                                     
 1001 format(1h ,a132)                                               
 1002 format(//)                                                        
      return                                                            
      end                                                               
