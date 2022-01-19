      program pamfun                                      
c-----
c----- comput genetic & phenotypic parameters of a trait which is
c----- a linear &/or multiplicative function of several traits
c----- the parameters of which are known
c-----
      implicit double precision (a-h,o-z)
      dimension ave(20),ws(20),wave(20),s(20),hsq(20),h(20),rp(20,20),  
     1 rg(20,20),wvp(20,20),wvg(20,20),cv(20),                          
     1 cc(20),wcovp(20),wcovg(20),rpf(20),rgf(20),                      
     1 c(20,10)                                                         
      dimension covp(20),covg(20)                                       
      dimension dd(20),wspfa(20),wsgfa(20),avefa(20)                    
      dimension wcovpf(10,10),wcovgf(10,10),rpff(10,10),rgff(10,10)     
      dimension covpf(10,10),covgf(10,10)                               
      character*80 label                                                
      read 110,label                                                    
  110 format(a)                                                      
      print 111,label                                                   
  111 format(1h0,a)                                              
      print 100                                                         
  100 format(11h0data input)                                            
c-----                                                                  
      read (5,*)not,nof,lsw                                                
      print 2,not,nof,lsw                                               
    2 format(7h0not = ,i3,8h  nof = ,i3,8h  lsw = ,i3)                  
c-----means                                                             
      read (5,*)(ave(i),i=1,not)                                           
      print 20                                                          
   20 format(5h0mean)                                                   
      print 5,(ave(i),i=1,not)                                          
c-----phen s.d.                                                         
      read (5,*)(s(i),i=1,not)                                             
      print 4                                                           
    4 format(25h0phen. standard deviation)                              
      print 5,(s(i),i=1,not)                                            
    5 format(1h0,10f13.5)                                               
c-----hsq                                                               
      read (5,*)(hsq(i),i=1,not)                                           
      print 6                                                           
    6 format(13h0heritability)                                          
      print 5,(hsq(i),i=1,not)                                          
c-----rp upper triang rowwise                                           
      nut=not-1                                                         
      do 7 i=1,nut                                                      
      jf=i+1                                                            
      read (5,*)(rp(i,j),j=jf,not)                                         
      do 8 j=jf,not                                                     
    8 rp(j,i)=rp(i,j)                                                   
    7 rp(i,i)=1.0                                                       
      rp(not,not)=1.0                                                   
      print 9                                                           
    9 format(23h0phenotypic correlation)                                
      do 10 i=1,not                                                     
   10 print 5,(rp(i,j),j=1,not)                                         
c-----rg upper triang rowwise                                           
      do 11 i=1,nut                                                     
      jf=i+1                                                            
      read (5,*)(rg(i,j),j=jf,not)                                         
      do 12 j=jf,not                                                    
   12 rg(j,i)=rg(i,j)                                                   
   11 rg(i,i)=1.0                                                       
      rg(not,not)=1.0                                                   
      print 13                                                          
   13 format(20h0genetic correlation)                                   
      do 14 i=1,not                                                     
   14 print 5,(rg(i,j),j=1,not)                                         
c-----coefficients or powers                                            
      print 15                                                          
   15 format(23h0coefficients or powers)                                
      do 71 j=1,nof                                                     
      read (5,*)(c(i,j),i=1,not)                                           
      print 5,(c(i,j),i=1,not)                                          
   71 continue                                                          
      print 101                                                         
  101 format(11h0work areas)                                            
c-----cv and h                                                          
      do 16 i=1,not                                                     
      h(i)=dsqrt(hsq(i))                                                 
   16 cv(i)=s(i)/ave(i)                                                 
      print 17                                                          
   17 format(11h0coeff.var.)                                            
      print 5,(cv(i),i=1,not)                                           
c-----working sd"s and working averages                                 
      do 18 i=1,not                                                     
      if(lsw) 21,19,22                                                  
   21 stop                                                              
   19 ws(i)=s(i)                                                        
      wave(i)=ave(i)                                                    
      go to 18                                                          
   22 ws(i)=dsqrt(dlog(1.+cv(i)*cv(i)))                                  
      wave(i)=dlog(ave(i))-0.5*dlog(1.+cv(i)*cv(i))                     
   18 continue                                                          
      print 50                                                          
   50 format(13h0working mean)                                          
      print 5,(wave(i),i=1,not)                                         
      print 51                                                          
   51 format(13h0working s.d.)                                          
      print 5,(ws(i),i=1,not)                                           
c-----construct g and p cov matrices                                    
      call diagmy(ws,rp,not,wvp)                                        
      call diagmy(h,rg,not,wvg)                                         
      call diagmy(ws,wvg,not,wvg)                                       
      print 23                                                          
   23 format(30h0working phenotypic cov matrix)                         
      do 24 i=1,not                                                     
   24 print 5,(wvp(i,j),j=1,not)                                        
c-----                                                                  
      print 25                                                          
   25 format(27h0working genetic cov matrix)                            
      do 26 i=1,not                                                     
   26 print 5,(wvg(i,j),j=1,not)                                        
c-----loop over functions                                               
      do 200 iof=1,nof                                                  
      print 102,iof                                                     
  102 format('0parameters for function no. ',i3)                         
c-----extract coeffs                                                    
      do 201 i=1,not                                                    
  201 cc(i)=c(i,iof)                                                    
c-----working mean of function                                          
      wavef=0.0                                                         
      do 40 i=1,not                                                     
   40 wavef=wavef+cc(i)*wave(i)                                         
      print 72,wavef                                                    
   72 format('0working mean of function = ',f15.6)                                 
c-----working variance of function                                      
      wvpf=quadf(cc,wvp,not)                                            
      wvgf=quadf(cc,wvg,not)                                            
      wspf=dsqrt(wvpf)                                                   
      wsgf=dsqrt(wvgf)                                                   
      wspfa(iof)=wspf                                                   
      wsgfa(iof)=wsgf                                                   
      print 27,wvpf,wspf                                                
   27 format(22h0working phen. var. = ,f15.6,9h  s.d. = ,f15.6)         
      print 28,wvgf,wsgf                                                
   28 format(22h0working gene. var. = ,f15.6,9h  s.d. = ,f15.6)         
c-----true mean of function                                             
      if(lsw) 41,42,43                                                  
   41 stop                                                              
   43 avef=dexp(wavef+0.5*dlog(1.+wvpf))                                 
      go to 44                                                          
   42 avef=wavef                                                        
   44 print 70,avef                                                     
      avefa(iof)=avef                                                   
   70 format('0mean of function = ',f15.6)                                          
c-----true varience of function                                         
      if(lsw) 29,30,31                                                  
   29 stop                                                              
   30 vpf=wvpf                                                          
      vgf=wvgf                                                          
      go to 32                                                          
   31 vpf=wvpf*avef*avef                                                
      vgf=wvgf*avef*avef                                                
   32 continue                                                          
      spf=dsqrt(vpf)                                                     
      sgf=dsqrt(vgf)                                                     
      print 33,vpf,spf                                                  
   33 format(14h0phen. var. = ,f15.6,9h  s.d. = ,f15.6)                 
      print 34,vgf,sgf                                                  
   34 format(14h0gene. var. = ,f15.6,9h  s.d. = ,f15.6)                 
c-----hsq of function                                                   
      hsqf=vgf/vpf                                                      
      print 6                                                           
      print 5,hsqf                                                      
c-----working cov of function with all traits                           
      do 60 i=1,not                                                     
      wcovp(i)=0.0                                                      
      wcovg(i)=0.0                                                      
      do 60 j=1,not                                                     
      wcovp(i)=wcovp(i)+cc(j)*wvp(i,j)                                  
   60 wcovg(i)=wcovg(i)+cc(j)*wvg(i,j)                                  
      print 23                                                          
      print 5,(wcovp(i),i=1,not)                                        
      print 25                                                          
      print 5,(wcovg(i),i=1,not)                                        
c-----correlations with function                                        
      do 61 i=1,not                                                     
      rpf(i)=wcovp(i)/(wspf*ws(i))                                      
   61 rgf(i)=wcovg(i)/(wsgf*ws(i)*h(i))                                 
      print 9                                                           
      print 5,(rpf(i),i=1,not)                                          
      print 13                                                          
      print 5,(rgf(i),i=1,not)                                          
c-----true cov of function with all traits                              
      do 62 i=1,not                                                     
      if(lsw) 63,64,65                                                  
   63 stop                                                              
   64 covp(i)=wcovp(i)                                                  
      covg(i)=wcovg(i)                                                  
      go to 62                                                          
   65 covp(i)=wcovp(i)*avef*ave(i)                                      
      covg(i)=wcovg(i)*avef*ave(i)                                      
   62 continue                                                          
      print 66                                                          
   66 format(10h0phen. cov)                                             
      print 5,(covp(i),i=1,not)                                         
      print 67                                                          
   67 format(10h0gene. cov)                                             
      print 5,(covg(i),i=1,not)                                         
  200 continue                                                          
c-----working covs among functions                                      
      if(nof-1) 300,300,301                                             
  301 print 220                                                         
  220 format(49h0covariances and correlations among the functions)      
      do 210 iof=1,nof                                                  
      do 211 i=1,not                                                    
  211 cc(i)=c(i,iof)                                                    
      do 210 jof=1,nof                                                  
      do 212 j=1,not                                                    
  212 dd(j)=c(j,jof)                                                    
      wcovpf(iof,jof)=hermf(cc,dd,wvp,not)                              
  210 wcovgf(iof,jof)=hermf(cc,dd,wvg,not)                              
      print 23                                                          
      do 221 iof=1,nof                                                  
  221 print 5,(wcovpf(iof,jof),jof=1,nof)                               
      print 25                                                          
      do 222 iof=1,nof                                                  
  222 print 5,(wcovgf(iof,jof),jof=1,nof)                               
c-----correlations among functions                                      
      do 230 iof=1,nof                                                  
      do 230 jof=1,nof                                                  
      rpff(iof,jof)=wcovpf(iof,jof)/(wspfa(iof)*wspfa(jof))             
  230 rgff(iof,jof)=wcovgf(iof,jof)/(wsgfa(iof)*wsgfa(jof))             
      print 9                                                           
      do 231 iof=1,nof                                                  
  231 print 5,(rpff(iof,jof),jof=1,nof)                                 
      print 13                                                          
      do 232 iof=1,nof                                                  
  232 print 5,(rgff(iof,jof),jof=1,nof)                                 
c-----true covs among functions                                         
      do 240 iof=1,nof                                                  
      do 240 jof=1,nof                                                  
      if(lsw) 241,242,243                                               
  241 stop                                                              
  242 covpf(iof,jof)=wcovpf(iof,jof)                                    
      covgf(iof,jof)=wcovgf(iof,jof)                                    
      go to 240                                                         
  243 covpf(iof,jof)=wcovpf(iof,jof)*avefa(iof)*avefa(jof)              
      covgf(iof,jof)=wcovgf(iof,jof)*avefa(iof)*avefa(jof)              
  240 continue                                                          
      print 66                                                          
      do 250 iof=1,nof                                                  
  250 print 5,(covpf(iof,jof),jof=1,nof)                                
      print 67                                                          
      do 251 iof=1,nof                                                  
  251 print 5,(covgf(iof,jof),jof=1,nof)                                
  300 stop                                                              
      end                                                               
