      subroutine mixed (nab,matx,ka,kb,kc,not,tot,nmig,nlhm,ncpr,nrhm,to
     1t3,array,ncdg,fab,tot4,sof,nx,kd,ibet,swt2,swt3,wtt,sofn)         
c     ------------------------------------------------------            
c     subroutine to compute the reduced f matrix and k values           
c     -------------------------------------------------------           
      include 'decl1'
      include 'com9'
      dimension fab(*),tot4(*),array(*),tot(*),tot3(*)      
      if (kc.ne.0) go to 13                                             
      nx=0                                                              
      if (nab.eq.5) nab=2                                               
      if (nab.eq.6) nab=0                                               
      do 11 i=1,matx                                                    
   11 fab(i)=0.0                                                        
      sofn=0.0                                                          
      sof=0.0                                                           
      if (nlhm.eq.0) go to 12                                           
      do 10 i=1,nlhm                                                    
   10 tot4(i)=0.0                                                       
   12 if (nab.eq.2) kb=1                                                
      if (nab.eq.0) kd=1                                                
      if (kb.eq.1) tot4(m99+6)=0.0                                        
      return                                                            
   13 if (kc.eq.2) go to 18                                             
      sn2=nmig                                                          
      if (ibet.eq.1) sn2=swt3                                           
      dm=sn2*sn2                                                        
      sofn=sofn+dm                                                      
      if (kb.eq.1) tot4(m99+6)=tot4(m99+6)+dm                               
      if (nlhm.eq.0) go to 9                                            
      do 14 i=1,nlhm                                                    
   14 tot4(i)=tot4(i)+sn2*tot3(i)                                       
      do 16 i=1,nlhm                                                    
      if (tot3(i).eq.0.0) go to 16                                      
      k1=nlhm*(i-1)-i*(i-3)/2-i                                         
      do 15 j=i,nlhm                                                    
      k=k1+j                                                            
   15 fab(k)=fab(k)+tot3(i)*tot3(j)                                     
   16 continue                                                          
    9 do 17 i=1,not                                                     
   17 tot3(i)=0.0                                                       
      nmig=1                                                            
      swt3=wtt                                                          
      nx=nx+1                                                           
      if (kc.eq.4) go to 18                                             
      if (kd.ne.0) ncdg=1                                               
      if (kd.ne.0) swt2=wtt                                             
      return                                                            
   18 cdg=ncdg                                                          
      if (ibet.eq.1) cdg=swt2                                           
      dm=1./cdg                                                         
      rown=dm*2.                                                        
      sn2=sofn*dm                                                       
      sof=sof+sn2                                                       
      dm2=sofn/(cdg*cdg)                                                
      if (nlhm.eq.0) go to 8                                            
      do 20 i=1,nlhm                                                    
      if (tot(i).eq.0.0) go to 20                                       
      do 19 j=1,nlhm                                                    
      k1=i-j                                                            
      if (k1.gt.0) go to 37                                             
      k=nlhm*(i-1)-i*(i-3)/2+j-i                                        
      if (k1.eq.0) go to 38                                             
      go to 39                                                          
   37 k=nlhm*(j-1)-j*(j-3)/2+i-j                                        
      fab(k)=fab(k)-tot(i)*tot4(j)*dm                                   
      go to 19                                                          
   38 fab(k)=fab(k)+tot(i)*(tot(j)*dm2-tot4(j)*rown)                    
      go to 19                                                          
   39 fab(k)=fab(k)+tot(i)*(tot(j)*dm2-tot4(j)*dm)                      
   19 continue                                                          
   20 continue                                                          
      do 21 i=1,nlhm                                                    
   21 tot4(i)=0.0                                                       
    8 sofn=0.0                                                          
      return                                                            
      end                                                               
