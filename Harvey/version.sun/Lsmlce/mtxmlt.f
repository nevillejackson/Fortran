      subroutine mtxmlt (n,a,b,c,value)                                 
c     --------------------------------------------------                
c     subroutine for quadratic multiplication                           
c     --------------------------------------------------                
      include 'decl1'
      dimension a(*),c(*),b(*)                                   
      value=0.0                                                         
      do 40 ij=1,n                                                      
      temp=0.0                                                          
      do 30 ji=1,n                                                      
      if (ij.lt.ji) go to 10                                            
      l=n*(ji-1)-ji*(ji-3)/2+ij-ji                                      
      go to 20                                                          
   10 l=n*(ij-1)-ij*(ij-3)/2+ji-ij                                      
   20 temp=temp+a(ji)*b(l)                                              
   30 continue                                                          
   40 value=value+temp*c(ij)                                            
      return                                                            
      end                                                               
