      subroutine comprs (nn,l,a,b,c,d)                                  
c     -------------------------------------------------                 
c     subroutine to compress a matrix                                   
c     -------------------------------------------------                 
      include 'decl1'
      dimension a(*),b(*),c(*),d(*),l(*)                    
      kount=0                                                           
      do 20 i=1,nn                                                      
      if (l(i).eq.0) go to 20                                           
      do 10 j=i,nn                                                      
      if (l(j).eq.0) go to 10                                           
      ll=nn*(i-1)-i*(i-3)/2+j-i                                         
      kount=kount+1                                                     
      b(kount)=a(ll)                                                    
   10 continue                                                          
   20 continue                                                          
      kount=0                                                           
      do 30 i=1,nn                                                      
      if (l(i).eq.0) go to 30                                           
      kount=kount+1                                                     
      d(kount)=c(i)                                                     
   30 continue                                                          
      return                                                            
      end                                                               
