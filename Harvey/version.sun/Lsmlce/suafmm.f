      subroutine suafmm                                                 
c     ------------------------------------------------------            
c     subroutine which corrects arrays for computation of ls means      
c     and standard errors for mixed models                              
c     ------------------------------------------------------            
      include 'decl1'
      include 'com1'
      include 'com2'
      include 'com3'
      include 'com4'
      include 'com9'
c----- original dimension
c     integer ic(300)
c----- current dimension
      integer ic(300)
c-----
      character*6 rgrsn
      data rgrsn/6hrgrsn /                                              
      nab=0                                                             
      kb=0                                                              
      go to (1,2,3,2,3,3,7),mty                                         
c     ------------------------------------------------------            
c     sets up b and c-1 for all fixed effects when mty=2 or 4           
c     ------------------------------------------------------            
    2 l=nlhm+1                                                          
      k=matx+nlhm*nrhm                                                  
      k1=l*(l+1)/2+l*nrhm                                               
      do 10 i=1,nrhm                                                    
      k2=l+nrhm+1-i                                                     
      sum=tot5(k2)                                                      
      do 30 j=1,l                                                       
      if (j.eq.l) go to 11                                              
      sum=sum-tot5(l+1-j)*array(k)                                      
      array(k1)=array(k)                                                
      k=k-1                                                             
      go to 30                                                          
   11 array(k1)=sum/tot5(1)                                             
   30 k1=k1-1                                                           
   10 continue                                                          
      l=matx+nlhm+1                                                     
      j=m5000-1
      do 12 i=1,l                                                       
      if (i.gt.matx) go to 13                                           
      array(k1)=fab(k)                                                  
      k=k-1                                                             
      go to 12                                                          
   13 array(k1)=fab(j)                                                  
      j=j-1                                                             
   12 k1=k1-1                                                           
c     ------------------------------------------------------            
c     corrects im array, nlhm, ns, matx and ie when mty=2 or 4          
c     ------------------------------------------------------            
      k=ns+1                                                            
      l=ns                                                              
      k1=ns+1                                                           
      do 14 i=1,k1                                                      
      if (i.eq.k1) go to 15                                             
      im(k)=im(l)+1                                                     
      l=l-1                                                             
      go to 14                                                          
   15 im(k)=1                                                           
   14 k=k-1                                                             
      if (ie.gt.nlhm) go to 31                                          
      k=nlhm+1                                                          
      do 32 i=ie,nlhm                                                   
      lab1(k)=rgrsn                                                     
      lab2(k)=lab2(k-1)                                                 
      lab3(k)=lab3(k-1)                                                 
      lab4(k)=lab4(k-1)                                                 
   32 k=k-1                                                             
   31 nlhm=nlhm+1                                                       
      ns=ns+1                                                           
      kb=1                                                              
      matx=matx+nlhm                                                    
      ie=ie+1                                                           
      go to 1                                                           
c     ------------------------------------------------------            
c     sets up b and c-1 arrays for all fixed effects when mty=3, 5 or 6 
c     ------------------------------------------------------            
    3 k2=ndfa+1                                                         
      l=nlhm+k2                                                         
      k4=m5000-ndfa*nrhm                                                 
      j=1                                                               
      k=ndfa*nrhm+k4-1                                                  
      do 20 i=k4,k                                                      
      rhm(j)=array(i)                                                   
   20 j=j+1                                                             
      k=matx+nlhm*nrhm                                                  
      k1=l*(l+1)/2+l*nrhm                                               
      k4=ndfa*nrhm                                                      
      do 35 i=1,nrhm                                                    
      k3=l+nrhm+1-i                                                     
      sum=tot5(k3)                                                      
      do 16 j=1,l                                                       
      i2=l+1-j                                                          
      if (j.gt.nlhm) go to 17                                           
      sum=sum-tot5(i2)*array(k)                                         
      array(k1)=array(k)                                                
      k=k-1                                                             
      go to 16                                                          
   17 if (j.eq.l) go to 18                                              
      sum=sum-tot5(i2)*rhm(k4)                                          
      array(k1)=rhm(k4)                                                 
      k4=k4-1                                                           
      go to 16                                                          
   18 array(k1)=sum/tot5(1)                                             
   16 k1=k1-1                                                           
   35 continue                                                          
      l=l*(l+1)/2                                                       
      j=m5000-1                                                            
      do 19 i=1,l                                                       
      if (i.gt.matx) go to 21                                           
      array(k1)=fab(k)                                                  
      k=k-1                                                             
      go to 19                                                          
   21 array(k1)=fab(j)                                                  
      j=j-1                                                             
   19 k1=k1-1                                                           
c     ------------------------------------------------------            
c     corrects im and lab arrays matx, nlhm, ns and ie when mty=3,5 or 6
c     ------------------------------------------------------            
      matx=l                                                            
      k=ns+2                                                            
      l=ns                                                              
      k1=ns+2                                                           
      do 22 i=1,k1                                                      
      if (i.gt.ns) go to 99                                             
      im(k)=im(l)+ndfa+1                                                
      l=l-1                                                             
      go to 22                                                          
   99 if (k.eq.2) im(k)=ndfa+1                                          
      if (k.eq.1) im(k)=1                                               
   22 k=k-1                                                             
      if (ie.gt.nlhm) go to 25                                          
      k=nlhm+ndfa+1                                                     
      i=nlhm                                                            
      do 26 j=ie,nlhm                                                   
      lab1(k)=rgrsn                                                     
      lab2(k)=lab2(i)                                                   
      lab3(k)=lab3(i)                                                   
      lab4(k)=lab4(i)                                                   
      i=i-1                                                             
   26 k=k-1                                                             
   25 ie=ie+ndfa+1                                                      
      nlhm=nlhm+ndfa+1                                                  
      kb=ndfa+1                                                         
      ns=ns+2                                                           
c     ------------------------------------------------------            
c     corrects lit, ncl, iden and nos arrays and nom and ml             
c     when mty=3, 5 or 6                                                
c     ------------------------------------------------------            
      j=m300+1-k2                                                          
      k4=m100+1-k2                                                         
      do 37 i=1,k2                                                      
      ic(i)=nos(j)                                                      
      nregp(i)=iden(k4)                                                 
      j=j+1                                                             
   37 k4=k4+1                                                           
      if (nmea.eq.1) go to 23                                           
      k=nom                                                             
      k4=0                                                              
      if (nom.eq.0) go to 36                                            
      do 24 i=1,nom                                                     
      k4=k4+ncl(i)                                                      
      lit(k+1)=lit(k)                                                   
      ncl(k+1)=ncl(k)                                                   
   24 k=k-1                                                             
   36 k3=k4                                                             
      if (non.eq.0) go to 70                                            
      do 71 i=1,non                                                     
   71 nma(i)=nma(i)+1                                                   
   70 k=ndfa+1                                                          
      j=k4+k                                                            
      l=j                                                               
      do 27 i=1,l                                                       
      if (i.gt.k3) go to 28                                             
      iden(j)=iden(k4)                                                  
      k4=k4-1                                                           
      go to 29                                                          
   28 iden(j)=nregp(k)                                                  
      k=k-1                                                             
   29 j=j-1                                                             
   27 continue                                                          
      if (n2f.eq.0) go to 23                                            
      do 98 i=1,n2f                                                     
      int1(i)=int1(i)+1                                                 
   98 int2(i)=int2(i)+1                                                 
   23 lit(1)=lab1(m99+1)                                                  
      ncl(1)=ndfa+1                                                     
      nom=nom+1-nmea                                                    
      ml=0                                                              
      j=ncc+ncl(1)                                                      
      l=j                                                               
      k4=ncc                                                            
      k1=ncl(1)                                                         
      do 33 i=1,l                                                       
      if (i.gt.ncc) go to 34                                            
      nos(j)=nos(k4)                                                    
      if(ibet.eq.1) ww(j)=ww(k4)                                        
      k4=k4-1                                                           
      go to 33                                                          
   34 nos(j)=ic(k1)                                                     
      if(ibet.eq.1) ww(j)=ic(k1)                                        
      k1=k1-1                                                           
   33 j=j-1                                                             
      ncc=l                                                             
      if (ipl(12).eq.0.or.nsme.eq.0) go to 80                           
      do 76 i=1,nsme                                                    
      k=nsme-i+1                                                        
   76 ipl(k+1)=ipl(k)                                                   
      ipl(1)=1                                                          
      nsme=nsme+1                                                       
   80 nme=nme+1                                                         
      go to 1                                                           
c     ------------------------------------------------------            
c     sets up b and c-1 arrays for fixed effects when mty=7             
c     ------------------------------------------------------            
    7 k2=ndfa+ndfc+ndfac+1                                              
      l=nlhm+k2                                                         
      i1=ndfa+ndfc                                                      
      k=k2-1                                                            
      i2=1                                                              
      do 38 i=1,nrhm                                                    
      i3=m5000-ndfa*nrhm-(ndfc+ndfac)*(nrhm-i+1)                         
      do 38 j=1,k                                                       
      k4=i3+j-1-ndfa                                                    
      if (j.le.ndfa) k4=m5000-ndfa*(nrhm-i+1)+j-1                        
      rhm(i2)=array(k4)                                                 
   38 i2=i2+1                                                           
      k=matx+nlhm*nrhm                                                  
      k1=l*(l+1)/2+l*nrhm                                               
      i1=nlhm-iei+1                                                     
      i2=i1+ndfac                                                       
      i3=nlhm+ndfac                                                     
      do 39 i=1,nrhm                                                    
      k3=l+nrhm+1-i                                                     
      i4=(k2-1)*(1+nrhm-i)                                              
      sum=tot5(k3)                                                      
      do 40 j=1,l                                                       
      k4=l+1-j                                                          
      if (j.gt.i1) go to 41                                             
      sum=sum-tot5(k4)*array(k)                                         
      array(k1)=array(k)                                                
      k=k-1                                                             
      go to 40                                                          
   41 if (j.gt.i2) go to 42                                             
      sum=sum-tot5(k4)*rhm(i4)                                          
      array(k1)=rhm(i4)                                                 
      i4=i4-1                                                           
      go to 40                                                          
   42 if (j.gt.i3) go to 43                                             
      sum=sum-tot5(k4)*array(k)                                         
      array(k1)=array(k)                                                
      k=k-1                                                             
      go to 40                                                          
   43 if (j.eq.l) go to 44                                              
      sum=sum-tot5(k4)*rhm(i4)                                          
      array(k1)=rhm(i4)                                                 
      i4=i4-1                                                           
      go to 40                                                          
   44 array(k1)=sum/tot5(1)                                             
   40 k1=k1-1                                                           
   39 continue                                                          
      l=l*(l+1)/2                                                       
      i1=i1*(i1+1)/2                                                    
      i2=i2*(i2+1)/2                                                    
      i3=i3*(i3+1)/2                                                    
      k3=nlhm-iei+1                                                     
      k4=0                                                              
      i4=0                                                              
      j=m5000-1                                                            
      do 45 i=1,l                                                       
      if (i.gt.i1) go to 46                                             
      array(k1)=fab(k)                                                  
      k=k-1                                                             
      go to 45                                                          
   46 if (i.gt.i2) go to 47                                             
      array(k1)=fab(j)                                                  
      j=j-1                                                             
      go to 45                                                          
   47 if (i.gt.i3) go to 48                                             
      if (k4.eq.k3) go to 4                                             
      k4=k4+1                                                           
      array(k1)=fab(k)                                                  
      k=k-1                                                             
      go to 45                                                          
    4 i4=i4+1                                                           
      if (i4.eq.ndfac) go to 5                                          
      go to 45                                                          
    5 i4=0                                                              
      k4=0                                                              
      k3=k3+1                                                           
      go to 45                                                          
   48 array(k1)=fab(j)                                                  
      j=j-1                                                             
   45 k1=k1-1                                                           
c     ------------------------------------------------------            
c     corrects im and lab arrays and matx, nlhm, ns and ie when mty=7   
c     ------------------------------------------------------            
      matx=l                                                            
      k=ns+4                                                            
      i1=ns+1                                                           
      l=ns                                                              
      k1=ns-nme-nne                                                     
      do 49 i=1,i1                                                      
      if (i.gt.k1) go to 50                                             
      im(k)=im(l)+k2                                                    
      l=l-1                                                             
      go to 49                                                          
   50 if (i.eq.k1+1) go to 51                                           
      im(k)=im(l)+k2-ndfac                                              
      l=l-1                                                             
      go to 49                                                          
   51 im(k)=im(l)+k2                                                    
      if (l.eq.0) im(k)=k2                                              
   49 k=k-1                                                             
      im(3)=ndfa+ndfc+1                                                 
      im(2)=ndfa+1                                                      
      im(1)=1                                                           
      if (ie.gt.nlhm) go to 52                                          
      k=nlhm+k2                                                         
      i=nlhm                                                            
      do 53 j=ie,nlhm                                                   
      lab1(k)=rgrsn                                                     
      lab2(k)=lab2(i)                                                   
      lab3(k)=lab3(i)                                                   
      lab4(k)=lab4(i)                                                   
      i=i-1                                                             
   53 k=k-1                                                             
   52 ie=ie+k2                                                          
      nlhm=nlhm+k2                                                      
      kb=k2                                                             
      ns=ns+4                                                           
c     ------------------------------------------------------            
c     corrects lit,ncl,iden and nos arrays and nom and ml when mty=7    
c     ------------------------------------------------------            
      i3=ndfa+ndfc+2                                                    
      k3=i3+(ndfa+1)*(ndfc+1)                                           
      j=m300+1-k3                                                          
      k4=m100+1-i3                                                         
      do 54 i=1,k3                                                      
      ic(i)=nos(j)                                                      
      if (i.gt.i3) go to 54                                             
      nregp(i)=iden(k4)                                                 
      k4=k4+1                                                           
   54 j=j+1                                                             
      i3=ncc                                                            
      if (nom.eq.0) go to 64                                            
      do 65 i=1,nom                                                     
   65 i3=i3-ncl(i)                                                      
   64 if (non.eq.0) go to 66                                            
      do 67 i=1,non                                                     
      if (nmea.eq.0)nma(i)=nma(i)+2                                     
      if(nmea.eq.1)nma(i)=nma(i)+1                                      
   67 i3=i3-ncln(i)                                                     
   66 i3=i3+ml+mlb                                                      
      if (nmea.eq.2) go to 55                                           
      j=2                                                               
      if (nmea.eq.1) j=1                                                
      k=nom                                                             
      k4=0                                                              
      if (nom.eq.0) go to 57                                            
      do 58 i=1,nom                                                     
      k4=k4+ncl(i)                                                      
      lit(k+j)=lit(k)                                                   
      ncl(k+j)=ncl(k)                                                   
   58 k=k-1                                                             
      j=k4+ndfa+ndfc+2                                                  
      if (nmea.eq.1.and.lit(1).eq.lab1(m99-1)) j=k4+ndfa+1                 
      if (nmea.eq.1.and.lit(1).eq.lab1(m99+1)) j=k4+ndfc+1                
      l=k4                                                              
      do 59 i=1,l                                                       
      iden(j)=iden(k4)                                                  
      k4=k4-1                                                           
   59 j=j-1                                                             
   57 k=ndfa+ndfc+2                                                     
      do 56 i=1,k                                                       
   56 iden(i)=nregp(i)                                                  
   55 lit(1)=lab1(m99+1)                                                  
      lit(2)=lab1(m99-1)                                                   
      ncl(1)=ndfa+1                                                     
      ncl(2)=ndfc+1                                                     
      nom=nom+2-nmea                                                    
      if (n2f.eq.0) go to 69                                            
      j=n2f                                                             
      k=2                                                               
      if (nmea.eq.1) k=1                                                
      if (nmea.eq.2) k=0                                                
      do 68 i=1,n2f                                                     
      int1(j+1)=int1(j)+k                                               
      int2(j+1)=int2(j)+k                                               
      nmc(j+1)=nmc(j)                                                   
   68 j=j-1                                                             
   69 int1(1)=1                                                         
      int2(1)=2                                                         
      nmc(1)=0                                                          
      ml=0                                                              
      nme=nme+2                                                         
      n2f=n2f+1                                                         
      j=ncc+k3                                                          
      l=j                                                               
      k4=ncc                                                            
      i1=ncc+(ndfa+1)*(ndfc+1)                                          
      i2=i3+(ndfa+1)*(ndfc+1)                                           
      do 60 i=1,l                                                       
      if (i.gt.i3) go to 61                                             
      nos(j)=nos(k4)                                                    
      if(ibet.eq.1) ww(j)=ww(k4)                                        
      k4=k4-1                                                           
      go to 60                                                          
   61 if (i.gt.i2) go to 62                                             
      nos(j)=ic(k3)                                                     
      if(ibet.eq.1) ww(j)=ic(k3)                                        
      k3=k3-1                                                           
      go to 60                                                          
   62 if (i.gt.i1) go to 63                                             
      nos(j)=nos(k4)                                                    
      if(ibet.eq.1) ww(j)=ww(k4)                                        
      k4=k4-1                                                           
      go to 60                                                          
   63 nos(j)=ic(k3)                                                     
      if(ibet.eq.1) ww(j)=ic(k3)                                        
      k3=k3-1                                                           
   60 j=j-1                                                             
      ncc=l                                                             
      if (ipl(12).eq.0.and.ipl(13).eq.0) go to 1                        
      if (ipl(12).eq.0.and.ipl(13).ne.0) go to 79                       
      if (ipl(13).eq.0.and.ipl(12).ne.0) go to 79                       
      do 78 i=1,nsme                                                    
      k=nsme-i+1                                                        
   78 ipl(k+2)=ipl(k)                                                   
      ipl(1)=1                                                          
      ipl(2)=2                                                          
      nsme=nsme+2                                                       
      go to 1                                                           
   79 do 77 i=1,nsme                                                    
      k=nsme-i+1                                                        
   77 ipl(k+1)=ipl(k)                                                   
      if (ipl(12).eq.0) ipl(1)=2                                        
      if (ipl(13).eq.0) ipl(1)=1                                        
      nsme=nsme+1                                                       
    1 mn2=1                                                             
      if (mty.gt.1) nmea=0                                              
      if (mty.gt.1) nnea=0                                              
      call lsmns                                                        
      if (liop.eq.21) return                                            
      write (6,1001) ijob                                               
 1001 format (1h0,29x,43hlisting of inverse elements for problem no.,i3)
      write (6,1004)                                                    
 1004 format (1h )                                                      
      write (6,1025) (array(i),i=1,matx)                                
 1025 format (7(2x,e15.8))                                              
      write (6,1022)                                                    
 1022 format (1h0,29x,36hlisting of diagonal inverse elements)          
      write (6,1004)                                                    
      k=1                                                               
      do 75 i=1,nlhm                                                    
      x(i)=array(k)                                                     
   75 k=k+nlhm-i+1                                                      
      write (6,1025) (x(k),k=1,nlhm)                                    
      return                                                            
  300 write (6,1021)                                                    
 1021 format (1h0,10x,29hnegative error sum of squares)                 
      return                                                            
      end                                                               
