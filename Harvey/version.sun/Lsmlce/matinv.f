      subroutine matinv (array,nbrc,nerc,nlhm,x,d)                      
c     -------------------------------------------                       
c     subroutine for inversion of symmetrical half-stored matrix        
c     ----------------------------------------------------              
      include 'decl1'
      dimension array(*),x(*)                                      
      ib=1                                                              
      if (d.eq.0.0) ib=0                                                
      id=0                                                              
      d=1.0                                                             
    5 do 20 l=nbrc,nerc                                                 
      k3=nlhm*(l-1)-l*(l-3)/2                                           
      if (ib.eq.0) go to 4                                              
      d=d*(array(k3)/x(l))                                              
      if (id.gt.0) go to 4                                              
      if (d.gt.0.10d-20) go to 4                                        
      id=1                                                              
      write (6,1000) l                                                  
 1000 format (41h0the determinant went to zero at diagonal,i3,2h .,/64h0
     1note-the results of this analysis are probably of little value.)  
    4 recip=1./array(k3)                                                
      array(k3)=-recip                                                  
      do 20 i=nbrc,nerc                                                 
      k11=nlhm*(i-1)-i*(i-3)/2                                          
      if (i-l) 6,20,8                                                   
    6 k1=k11+l-i                                                        
      go to 10                                                          
    8 k1=k3+i-l                                                         
   10 r=recip*array(k1)                                                 
      do 18 j=i,nerc                                                    
      k4=k11+j-i                                                        
      if (j-l) 12,18,14                                                 
   12 k5=nlhm*(j-1)-j*(j-3)/2+l-j                                       
      go to 16                                                          
   14 k5=k3+j-l                                                         
   16 array(k4)=array(k4)-r*array(k5)                                   
   18 continue                                                          
      array(k1)=r                                                       
   20 continue                                                          
      do 32 i=nbrc,nerc                                                 
      do 32 j=i,nerc                                                    
      k1=nlhm*(i-1)-i*(i-3)/2+j-i                                       
      array(k1)=-array(k1)                                              
   32 continue                                                          
      return                                                            
      end                                                               
