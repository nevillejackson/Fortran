      real*8 function prof(ndf,pddf,fval)                               
      include 'decl1'
      double precision jlog10
      integer*4 ddf                                                  
c                                                                       
c   function for computation of prob as used in sas                     
c   -----------------------------------------------                     
c reference: handbook of mathematical functions,                        
c            u.s. dept of commerce, national bureau of standards,       
c            applied math series 55, seventh printing, pp 946-947.      
c                                                                       
c   pndf = numerator degrees of freedom                                 
c   pddf = denominator degrees of freedom                               
c   fval = f-ratio                                                      
c                                                                       
c                                                                       
c---------- check arguments ------------------------------------------  
c                                                                       
      ddf=pddf                                                          
      pndf=dfloat(ndf)                                                  
c                                                                       
c---------- look at arguments for quick return -----------------------  
c                                                                       
      pi=3.1415926535d0                                                 
      if ((ndf.le.0) .or. (ddf.le.0)) go to 500                         
      if (fval.ge.1.d-8) go to 5                                        
         prof=.9999d0                                                   
         return                                                         
 5    if (fval.le.1.d8) go to 7                                         
         prof=0.d0                                                      
         return                                                         
 7    if (ndf.ne.1) go to 8                                             
         prof=pgt2ft(fval,ddf)                                          
         return                                                         
 8    if (ddf.ne.1) go to 10                                            
         q=pgt2ft(1.d0/fval,ndf)                                        
         go to 200                                                      
 10   r1=pndf-2.d0                                                      
      r2=pddf-2.d0                                                      
c                                                                       
c---------- for ndf even use equation 26.6.4 ------------------------   
c                                                                       
      if (modf(ndf,2).ne.0) go to 40                                     
      if (ndf.gt.50) go to 180                                          
      x=pddf/(pddf+pndf*fval)                                           
      if (jlog10(x)*pddf/2.d0.gt.-76.d0) go to 20                       
         prof = 0.d0                                                    
         return                                                         
 20   z=1.d0-x                                                             
      q=x**(.5*pddf)                                                    
      q1=q                                                              
      ir1=r1                                                            
      if (ir1.lt.2) go to 200                                           
      do 30 ir=2,ir1,2                                                  
         r=ir                                                           
         q1=q1*((r2+r)/r*z)                                             
         q=q+q1                                                         
 30      continue                                                       
         go to 200                                                      
c                                                                       
c---------- for ddf even use equation 26.6.5 ------------------------   
c                                                                       
 40   if (modf(ddf,2).ne.0) go to 50                                     
      if (ddf.gt.50) go to 180                                          
      x=pddf/(pddf+pndf*fval)                                           
      if (jlog10(x)*pddf/2.d0.gt.-76.d0) go to 42                       
         prof = 0.d0                                                    
         return                                                         
 42   z=1.d0-x                                                          
      q=z**(.5d0*pndf)                                                  
      q1=q                                                              
      ir2=r2                                                            
      if (ir2.lt.2) go to 48                                            
      do 45 ir=2,ir2,2                                                  
         r=ir                                                           
         q1=q1*((r1+r)/r*x)                                             
         q=q+q1                                                         
 45      continue                                                       
 48   q=1.d0-q                                                          
      go to 200                                                         
c                                                                       
c---------- for ndf and ddf odd, use equation 26.6.8 ----------------   
c                                                                       
 50   if ((ndf.gt.50) .or. (ddf.gt.50)) go to 180                       
      t=dsqrt(pndf*fval/pddf)                                           
      t=datan(t)                                                        
      a=2.d0*t/pi                                                       
      s=dsin(t)                                                         
      c=dcos(t)                                                         
      c2=c*c                                                            
      a1=2.d0*s*c/pi                                                    
      a=a+a1                                                            
      b1=a1*2.d0*c2                                                     
      ir2=r2                                                            
      if (ir2.lt.3) go to 65                                            
      do 60 ir=3,ir2,2                                                  
         r=ir                                                           
         a1=a1*((r-1.d0)/r*c2)                                          
         a=a+a1                                                         
         b1=b1*(r+1.d0)/r*c2                                            
 60   continue                                                          
 65   b=b1                                                              
      s2=s*s                                                            
      ir1=r1                                                            
      if (ir1.lt.3) go to 75                                            
      do 70 ir=3,ir1,2                                                  
         r=ir                                                           
         b1=b1*((r2+r)/r*s2)                                            
         b=b+b1                                                         
 70      continue                                                       
 75   q=1.d0-a+b                                                        
      go to 200                                                         
c                                                                       
c---------- approximation --------------------------------------------  
c                                                                       
 180  if (fval.le.1.d0) go to 190                                       
         a1=pndf                                                        
         b1=pddf                                                        
         x=fval                                                         
         go to 195                                                      
 190  a1=pddf                                                           
      b1=pndf                                                           
      x=1/fval                                                          
 195  r1=2.d0/(9.d0*a1)                                                 
      r2=2.d0/(9.d0*b1)                                                 
      x=x**.3333333333333333                                            
      z=(x*(1.d0-r2)-(1.d0-r1))/dsqrt(r1+x*x*r2)                        
      q=.5d0-.5d0*derf(dabs(z)/1.41421356)                              
      if (z.lt.0.d0) q=1.d0-q                                           
      if (fval.le.1.d0) q=1.d0-q                                        
c                                                                       
c---------- check ---------------------------------------------------   
c                                                                       
 200  if (q.lt.0.d0) q=0.d0                                             
      if (q.gt.1.d0) q=1.d0                                             
      prof=q                                                            
      return                                                            
c                                                                       
c---------- ndf or ddf is equal to 0.  print error message ----------   
c                                                                       
 500  write(6,600)                                                      
 600  format(1x,'numerator df or denominator df is found to be 0.')     
      write(6,610)                                                      
 610  format(1x,'p is set at 0.0 and is meaningless.')                  
      prof=0.d0                                                         
      return                                                            
      end                                                               
