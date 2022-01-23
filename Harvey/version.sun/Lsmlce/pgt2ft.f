      real*8 function pgt2ft(tsq,df)                                    
      include 'decl1'
      integer df                                                        
c     equivalence (xm,mx)                                               
c                                                                       
c   function used by prof function                                      
c   ------------------------------                                      
c                                                                       
c   df<=30 to compute exact prob>/t/                                    
c   ref: journal of quality technology, vol 4, no.4, oct.1972, p 196    
c                                                                       
      p=1.d0                                                            
      if(tsq.lt.1.d-10) go to 80                                        
      v=df                                                              
      if (df.gt.30) go to 50                                            
      if (tsq.gt.1.d8) go to 60                                         
      theta=datan(dsqrt(tsq/v))                                         
      m=modf(df,2)                                                       
      t=dsin(theta)                                                     
      c=dcos(theta)                                                     
      if (m.eq.0) go to 10                                              
      p=1.d0-2.d0*theta/3.141592653589793                               
      if (df.eq.1) go to 70                                             
      t=2.d0*t*c/3.141592653589793                                      
 10   p=p-t                                                             
      nt=(df-m-2)/2                                                     
      if (nt.lt.1) go to 70                                             
      c2=c*c                                                            
      d=m                                                               
      do 15 i=1,nt                                                      
      d=d+2.d0                                                          
      t=t*c2*(d-1.d0)/d                                                 
 15   p=p-t                                                             
      go to 70                                                          
c                                                                       
c   for df>30 use fisher's expansion (first 3 terms)                    
c   absolute error < .00002                                             
c   ref: johnson & kotz p. 102                                          
c        'continuous univariate dist-2', houghton mifflin co. 1970      
c                                                                       
 50   t=dsqrt(tsq)                                                      
      if(tsq.gt.36.d0) go to 60                                         
      x=t/(2.d0*v)*(tsq+1.d0-(3.d0+tsq*(5.d0+tsq*(7.d0-3.d0*tsq)))/     
     *  (24.d0*v)-(15.d0+tsq*(3.d0-tsq*(6.d0+tsq*(14.d0-tsq*(11.d0-tsq))
     *  )))/(96.d0*v*v))                                                
      p=derfc(t/1.41421356237)+dexp(-.9189385332-tsq/2.d0)*x            
      go to 70                                                          
c                                                                       
 60   p=0.d0                                                            
      go to 80                                                          
 70   if (p.lt.0.d0) p=0.d0                                             
      if (p.gt.1.d0) p=1.d0                                             
c                                                                       
 80   pgt2ft=p                                                          
      return                                                            
      end                                                               
