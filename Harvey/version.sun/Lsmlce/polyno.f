      subroutine polyno                                                 
c     -------------------------------------------                       
c     subroutine for computing polynomials for cross classified or      
c     nested main effects                                               
c     ----------------------------------------------------              
      include 'decl1'
      character*6 lp(5)
      include 'com1'
      include 'com2'
      include 'com3'
      include 'com4'
      dimension pm(5)                                         
      data lp(1),lp(2),lp(3),lp(4),lp(5)/6hlinear,6hquad  ,6hcubic ,6hqu
     1ard ,6hquin  /                                                    
      ncas=0                                                            
      write (6,1000)                                                    
 1000 format (1h0,20x,53hlisting of polynomial regressions and standard 
     1errors)                                                           
      write (6,1001)                                                    
 1001 format (1h0,29h rhm    high order   standard,21x,38hnon-orthogonal
     1 regression coefficients)                                         
      write (6,1002)                                                    
 1002 format (1h ,27h name   regression    error,9x,2hb0,9x,2hb1,10x,2hb
     12,10x,2hb3,10x,2hb4,10x,2hb5)                                     
      do 73 k1=1,nsme                                                   
      i2=nnd(k1)-nsp(k1)+1                                              
      df=float(i2)                                                      
c     - - - - - - - - - - - - - - - - - - - - - - -                     
c     defining constants for transformation matrix                      
c     - - - - - - - - - - - - - - - - - - - - - - -                     
      sd=df/(df+1.)*df/(df+1.)                                          
      f=df/(df+1.)*1.0/(df+1.)*(-1.0)                                   
      r=1.0/(df+1.)*1.0/(df+1.)                                         
c     - - - - - - - - - - - - - - - - - - - - - - -                     
c     transformation of z-1 matrix to obtain matrix of weights          
c     which is stored in rhm array                                      
c     - - - - - - - - - - - - - - - - - - - - - - -                     
      do 75 k3=1,i2                                                     
      do 75 k4=k3,i2                                                    
      n2=nsp(k1)+k3-1                                                   
      n3=nsp(k1)+k4-1                                                   
      sum=0.                                                            
      k7=nsp(k1)                                                        
      k6=nnd(k1)                                                        
      do 76 i=k7,k6                                                     
      do 76 j=k7,k6                                                     
      if (i-j.le.0) go to 79                                            
      l=nlhm*(j-1)-j*(j-3)/2+i-j                                        
      go to 82                                                          
   79 l=nlhm*(i-1)-i*(i-3)/2+j-i                                        
   82 if (n2.eq.j.and.n3.eq.i) go to 77                                 
      if (n3.eq.i.and.n2.ne.j.or.n2.eq.j.and.n3.ne.i) go to 78          
      sum=sum+array(l)*r                                                
      go to 76                                                          
   78 sum=sum+array(l)*f                                                
      go to 76                                                          
   77 sum=sum+array(l)*sd                                               
   76 continue                                                          
      l=i2*(k3-1)-k3*(k3-3)/2+k4-k3                                     
   75 rhm(l)=sum                                                        
c     - - - - - - - - - - - - - - - - - - - - - - -                     
c     computation of mean of p and deviations from this mean            
c     - - - - - - - - - - - - - - - - - - - - - - -                     
      k3=ncas+i2+1                                                      
      sum=0.                                                            
      do 83 i=1,i2                                                      
      k2=ncas+i                                                         
   83 sum=sum+xp(k2)                                                    
      sum=(sum+xp(k3))/(df+1.0)                                         
      amnx=sum                                                          
      f=float(i2+1)                                                     
      xp(k3)=xp(k3)-sum                                                 
      do 80 i=2,5                                                       
   80 pm(i)=xp(k3)**i/f                                                 
      do 130 i=1,i2                                                     
      k2=ncas+i                                                         
      xp(k2)=xp(k2)-sum                                                 
      do 130 ii=2,5                                                     
  130 pm(ii)=pm(ii)+(xp(k2)**ii)/f                                      
c     - - - - - - - - - - - - - - - - - - - - - - -                     
c     calculation of constant for the last level for each rhm           
c     - - - - - - - - - - - - - - - - - - - - - - -                     
      do 87 i=1,nrhm                                                    
      sum=0.                                                            
      do 86 j=1,i2                                                      
      k2=matx+(i-1)*nlhm+nsp(k1)+j-1                                    
   86 sum=sum+array(k2)*(-1.0)                                          
   87 yp(i)=sum                                                         
      i3=i2*(i2+1)/2                                                    
c     - - - - - - - - - - - - - - - - - - - - - - -                     
c     beginning of loop for stepwise polynomial fitting                 
c     - - - - - - - - - - - - - - - - - - - - - - -                     
      do 88 i=1,i2                                                      
      if (i.gt.5) go to 88                                              
      write (6,1004) lab1(k7),lp(i),amnx                                
 1004 format (1h0,29x,a6,2x,a6,4x,11hmean of x =,f10.4)                 
      write (6,1003)                                                    
 1003 format (1h )                                                      
      n2=i*(i+1)/2                                                      
      n3=n2+i*nrhm                                                      
      j=i3+1                                                            
      k2=n3+i3                                                          
      do 90 k=j,k2                                                      
   90 rhm(k)=0.                                                         
c     - - - - - - - - - - - - - - - - - - - - - - -                     
c     calculation of weighted ss and cp for polynomials and rhm         
c     - - - - - - - - - - - - - - - - - - - - - - -                     
      do 91 l=1,i                                                       
      do 91 k=l,i                                                       
      do 89 k6=1,i2                                                     
      jj=ncas+k6                                                        
      sms=xp(k3)                                                        
      sum=xp(jj)                                                        
      if (l.eq.1) go to 140                                             
      do 93 l2=2,l                                                      
      sum=sum*xp(jj)                                                    
   93 sms=sms*xp(k3)                                                    
  140 sum=sum-sms                                                       
      do 89 k2=1,i2                                                     
      jj=ncas+k2                                                        
      sms=xp(k3)                                                        
      temp=xp(jj)                                                       
      if (k.eq.1) go to 141                                             
      do 94 l2=2,k                                                      
      temp=temp*xp(jj)                                                  
   94 sms=sms*xp(k3)                                                    
  141 temp=temp-sms                                                     
      if (k6-k2.le.0) go to 95                                          
      k11=i2*(k2-1)-k2*(k2-3)/2+k6-k2                                   
      go to 96                                                          
   95 k11=i2*(k6-1)-k6*(k6-3)/2+k2-k6                                   
   96 rhm(j)=rhm(j)+sum*rhm(k11)*temp                                   
      if (l-k.ne.0) go to 89                                            
      do 97 k4=1,nrhm                                                   
      k5=matx+(k4-1)*nlhm+nsp(k1)+k2-1                                  
      jj=n2+i*(k4-1)+l+i3                                               
   97 rhm(jj)=rhm(jj)+sum*rhm(k11)*(array(k5)-yp(k4))                   
   89 continue                                                          
   91 j=j+1                                                             
c     - - - - - - - - - - - - - - - - - - - - - - -                     
c     inversion of ss and cp matrix                                     
c     - - - - - - - - - - - - - - - - - - - - - - -                     
      do 110 k=1,i                                                      
      k2=i*(k-1)-k*(k-3)/2+i3                                           
      recip=1./rhm(k2)                                                  
      rhm(k2)=-recip                                                    
      do 110 l=1,i                                                      
      k11=i*(l-1)-l*(l-3)/2+i3                                          
      if (l-k) 111,110,112                                              
  111 k4=k11+k-l                                                        
      go to 113                                                         
  112 k4=k2+l-k                                                         
  113 r=rhm(k4)*recip                                                   
      do 114 j=l,i                                                      
      k5=k11+j-l                                                        
      if (j-k) 115,114,116                                              
  115 k6=i*(j-1)-j*(j-3)/2+k-j+i3                                       
      go to 117                                                         
  116 k6=k2+j-k                                                         
  117 rhm(k5)=rhm(k5)-r*rhm(k6)                                         
  114 continue                                                          
      rhm(k4)=r                                                         
  110 continue                                                          
      do 118 j=1,i                                                      
      do 118 k=j,i                                                      
      k2=i*(j-1)-j*(j-3)/2+k-j+i3                                       
  118 rhm(k2)=-rhm(k2)                                                  
c     - - - - - - - - - - - - - - - - - - - - - - -                     
c     computation of b values, standard errors and printing of results  
c     - - - - - - - - - - - - - - - - - - - - - - -                     
      do 120 j=1,nrhm                                                   
      do 122 k=1,i                                                      
      temp=0.                                                           
      do 121 l=1,i                                                      
      if (k-l.lt.0) go to 123                                           
      k2=i*(l-1)-l*(l-3)/2+k-l+i3                                       
      go to 124                                                         
  123 k2=i*(k-1)-k*(k-3)/2+l-k+i3                                       
  124 k4=i3+n2+i*(j-1)+l                                                
  121 temp=temp+rhm(k4)*rhm(k2)                                         
  122 sss(k)=temp                                                       
      k=j*5+i+(k1-1)*nrhm*5                                             
      k5=i3+n2                                                          
      sss(k)=(sss(i)*sss(i))/rhm(k5)                                    
      if (ncpr.eq.0) go to 125                                          
      k2=nrhm*(j-1)-j*(j-3)/2                                           
      temp=sscpr(k2)                                                    
      go to 126                                                         
  125 if (i30.eq.1) go to 127                                           
      temp=(sscpr(j)-tred(j))/edf                                       
      go to 126                                                         
  127 temp=sscpr(j)                                                     
  126 sd=0.0                                                            
      if (temp*rhm(k5).le.0.0) go to 128                                
      sd=dsqrt(div(temp*rhm(k5)))                                       
  128 if(nab.eq.1.or.nab.eq.2.or.nab.eq.4) go to 132                    
      k=matx+(j-1)*nlhm+1                                               
      recip=array(k)+ym(j)                                              
      go to 133                                                         
  132 k=nlhm+j                                                          
      recip=tot(k)/float(ncds)+ym(j)                                    
      if (ibet.eq.1) recip=tot(k)/swt1+ym(j)                            
  133 if (i.eq.1) go to 120                                             
      do 134 ii=2,i                                                     
  134 recip=recip-sss(ii)*pm(ii)                                        
  120 write (6,1005) lity(j),sss(i),sd,recip,(sss(k),k=1,i)             
 1005 format (1h ,a6,8(1x,e11.4))                                       
   88 continue                                                          
   73 ncas=ncas+i2+1                                                    
      df=edf+float(nlhm)                                                
      return                                                            
      end                                                               
