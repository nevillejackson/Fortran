      subroutine lsanov(ntitl)                                          
c     ------------------------------------------------------            
c     subroutine which computes and lists the anova and variance and    
c     covariance components from indirect analyses                      
c     ------------------------------------------------------            
      include 'decl1'
      include 'com1'
      include 'com2'
      include 'com3'
      include 'com4'
      include 'com5'
      include 'com9'
      character*6 mumym,iblk,ixlab,iblab
      character*6 rgnl,rgnq,rgnc,rl,rq,rc
      character*6 lp(5)
      character*8 ires,irmd
      character*12 regs
      character*6 a2 
      data mumym,iblk,ixlab,iblab/6hmu-ym ,6h      ,6hx     ,6hb     /  
      data lp(1),lp(2),lp(3),lp(4),lp(5)/6hlinear,6hquad  ,6hcubic ,6hqu
     1ard ,6hquin  /,ires,irmd/8hresidual,8hremaindr/                   
      data rgnl,rgnq,rgnc,rl,rq,rc,regs/6h rgn l,6h rgn q,6h rgn c,
     16hrl    ,6hrq    ,6hrc    ,'regressions'/                     
      if (mty.gt.1.and.nrn.eq.nrun) go to 1                             
      if (liop.ge.20) go to 2                                           
      lab1(m99)=iblk                                                     
      call tpage(ntitl)                                                 
      write (6,1011)                                                    
 1011 format (1h0,28x,34hleast-squares analysis of variance)            
      go to 2                                                           
    1 kb=2                                                              
      lab1(m99)=irmd                                                     
      call tpage(ntitl)                                                 
      write (6,1000)                                                    
 1000 format (1h0,24x,'combined least-squares analysis of variance')    
    2 if (nab.eq.0.or.nab.eq.3) lab1(1)=mumym                           
      do 108 k=1,nrhm                                                   
      k5=0                                                              
      if (kb.eq.2.or.liop.lt.20) write (6,1012) lity(k)                 
 1012 format (1h / 42x,a6)                                              
      if (kb.eq.2) go to 3                                              
      if (liop.lt.20) write (6,1013)                                    
 1013 format (1h ,/ 5x,6hsource,8x,4hd.f.,8x,14hsum of squares,6x,12hmea
     1n squares,10x,1hf,6x,'prob')                                      
      go to 4                                                           
    3 write (6,1001)                                                    
 1001 format (1h ,/ 5x,6hsource,8x,4hd.f.,8x,14hsum of squares,6x,12hmea
     1n squares,10x,1hf,6x,'prob',5x,'error line')                      
      if (nlhm.eq.0) go to 16                                           
    4 do 80 i=1,nlhm                                                    
      k1=matx+(k-1)*nlhm+i                                              
   80 rhm(i)=array(k1)                                                  
   16 k6=df                                                             
   82 if (kb.eq.2.or.liop.lt.20) write (6,1004)                         
 1004 format (1h )                                                      
      if (nlhm.eq.0) tred(k)=0.0                                        
      k2=1                                                              
      if (ncpr.eq.0) go to 84                                           
      k1=nrhm*(k-1)-k*(k-3)/2                                           
      ss=sscpr(k1)*edf+tred(k)                                          
      go to 86                                                          
   84 ss=sscpr(k)                                                       
   86 if (nab.gt.1) go to 22                                            
      if (liop.lt.20) write (6,1014) k6,ss                              
 1014 format (1h ,5htotal,13x,i6,f20.6)                     
      go to 21                                                          
   22 if (kb.eq.2) go to 21                                             
      if (liop.lt.20) write (6,1026) k6,ss                              
 1026 format (1h ,9hwithin ss,9x,i6,f20.6)            
   21 k6=k6-nlhm                                                        
      if(k6.eq.0) k6=1                                                  
      edf=k6                                                            
      r=ss-tred(k)                                                      
      sd=r                                                              
      sm=r/edf                                                          
      k3=2                                                              
      recip=nlhm                                                        
      if (nlhm.eq.0) recip=1.0                                          
      recip=tred(k)/recip                                               
      f=recip/sm                                                        
      if (kb.eq.2) go to 5                                              
      fpro=prof(nlhm,edf,f)                                             
      if (liop.lt.20) write (6,1015) nlhm,tred(k),recip,f,fpro          
 1015 format (1h ,15htotal reduction,i9,f20.6,f18.6,f13.3,f10.4)        
      go to 6                                                           
    5 if (mty.eq.3.or.mty.eq.5.or.mty.eq.7) go to 7                     
      if (mty.ne.2) go to 8                                             
      treds=fy(k1)-sd                                                   
      r=edff-edf                                                        
      lab1(m99+1)=name                                                    
      go to 9                                                           
    8 treds=fy(k1)-sab(k1)                                              
      r=edff-edfs                                                       
      lab1(m99+1)=nam5                                                    
    9 ss=treds/r                                                        
      k6=r                                                              
      r=ss/sm                                                           
      if (mty.eq.4) go to 15                                            
      fpro=prof(k6,edf,r)                                               
      write (6,1003) lab1(m99+1),k6,treds,ss,r,fpro,irmd                  
 1003 format (1h ,a6,9x,i7,f22.6,f18.6,f13.3,f10.4,4x,a8)               
      if (mty.eq.6) go to 7                                             
      go to 6                                                           
   15 sod=sab(k1)-sd                                                    
      f=edfs-edf                                                        
      sdf=sod/f                                                         
      l2=f                                                              
      el2=f                                                             
      f=sdf/sm                                                          
      r=ss/sdf                                                          
      fpro=prof(k6,el2,r)                                               
      write (6,1003) lab1(m99+1),k6,treds,ss,r,fpro,name                  
      fpro=prof(l2,edf,f)                                               
      write (6,1003) name,l2,sod,sdf,f,fpro,irmd                        
      go to 6                                                           
    7 l2=ifp*(k-1)+1                                                    
      treds=fsq(l2)                                                     
      r=ndfa                                                            
      ss=treds/r                                                        
      k6=r                                                              
      if (mty.ne.3) go to 17                                            
      lab1(m99-2)=name                                                     
      sod=fy(k1)-sd                                                     
      f=edff-edf                                                        
      go to 18                                                          
   17 if (mty.ne.6) go to 19                                            
      lab1(m99-2)=name                                                     
      lab1(m99+1)=lab1(m99-1)                                                
      sod=sab(k1)-sd                                                    
      f=edfs-edf                                                        
      go to 18                                                          
   19 sod=fy(k1)-sab(k1)                                                
      f=edff-edfs                                                       
      lab1(m99-2)=nam5                                                     
   18 sdf=sod/f                                                         
      l2=f                                                              
      el2=f                                                             
      f=sdf/sm                                                          
      r=ss/sdf                                                          
      fpro=prof(k6,el2,r)                                               
      write (6,1003) lab1(m99+1),k6,treds,ss,r,fpro,lab1(m99-2)              
      if (ifp.eq.1) go to 23                                            
      ipl(12)=1                                                         
      if(ifp.eq.2) go to 23                                             
      do 24 i=2,ifp                                                     
      k3=i-1                                                            
      k4=ifp*(k-1)+i                                                    
      ss=fsq(k4)                                                        
      k6=1                                                              
      if (i.ne.7) go to 37                                              
      k6=ndfa-5                                                         
      ss=ss/dble(k6)                                                   
      r=ss/sdf                                                          
      fpro=prof(k6,el2,r)                                               
      write (6,1006) ires,k6,fsq(k4),ss,r,fpro,lab1(m99-2)                 
 1006 format (1h ,2x,a8,7x,i3,2x,f22.6,f18.6,f13.3,f10.4,4x,a8)         
      go to 24                                                          
   37 r=ss/sdf                                                          
      fpro=prof(k6,el2,r)                                               
      write (6,1006) lp(k3),k6,fsq(k4),ss,r,fpro,lab1(m99-2)               
   24 continue                                                          
   23 if (mty.eq.5) go to 25                                            
      if (mty.eq.7) go to 26                                            
      fpro=prof(l2,edf,f)                                               
      write (6,1003) name,l2,sod,sdf,f,fpro,irmd                        
      go to 6                                                           
   25 treds=sab(k1)-sd                                                  
      r=edfs-edf                                                        
      ss=treds/r                                                        
      k6=r                                                              
      el2=r                                                             
      r=ss/sm                                                           
      f=sdf/ss                                                          
      fpro=prof(l2,el2,f)                                               
      write (6,1003) nam5,l2,sod,sdf,f,fpro,name                        
      fpro=prof(k6,edf,r)                                               
      write (6,1003) name,k6,treds,ss,r,fpro,irmd                       
      go to 6                                                           
   26 wk=(sab(k1)-sd)/(edfs-edf)                                        
      el2=edfs-edf                                                      
      f=sdf/sm                                                          
      fpro=prof(l2,edf,f)                                               
      write (6,1003) nam5,l2,sod,sdf,f,fpro,irmd                        
      l2=ifpc*(k-1)+m35*7+1                                                 
      treds=fsq(l2)                                                     
      r=ndfc                                                            
      ss=treds/r                                                        
      k6=r                                                              
      r=ss/wk                                                           
      fpro=prof(k6,el2,r)                                               
      write (6,1003) lab1(m99-1),k6,treds,ss,r,fpro,name                   
      if (ifpc.eq.1) go to 48                                           
      ipl(13)=2                                                         
      if(ifpc.eq.2) go to 48                                            
      do 46 i=2,ifpc                                                    
      k3=i-1                                                            
      k4=ifpc*(k-1)+i+m35*7                                               
      ss=fsq(k4)                                                        
      k6=1                                                              
      if (i.ne.7) go to 47                                              
      k6=ndfc-5                                                         
      ss=ss/dble(k6)                                                   
      r=ss/wk                                                           
      fpro=prof(k6,el2,r)                                               
      write (6,1006)  ires,k6,fsq(k4),ss,r,fpro,name                    
      go to 46                                                          
   47 r=ss/wk                                                           
      fpro=prof(k6,el2,r)                                               
      write (6,1006) lp(k3),k6,fsq(k4),ss,r,fpro,name                   
   46 continue                                                          
   48 a2=ixlab                                                          
      ss=fsq(m35*14+k)/dble(ndfac)                                        
      r=ss/wk                                                           
      fpro=prof(ndfac,el2,r)                                            
      write (6,1016)lab1(m99+1),a2,lab1(m99-1),ndfac,fsq(m35*14+k)
     1 ,ss,r,fpro,name                                                                
      treds=sab(k1)-sd                                                  
      r=edfs-edf                                                        
      ss=treds/r                                                        
      k6=r                                                              
      r=ss/sm                                                           
      fpro=prof(k6,edf,r)                                               
      write (6,1003) name,k6,treds,ss,r,fpro,irmd                       
    6 k3=3                                                              
      nerc=0                                                            
      l2=1                                                              
      if (nlhm.eq.0) go to 27                                           
      do 104 i3=1,ns                                                    
      nbrc=nerc+1                                                       
      nerc=im(i3)                                                       
      treds=0.0                                                         
      do 212 i=nbrc,nerc                                                
      temp=0.0                                                          
      do 209 j=nbrc,nerc                                                
      if (i-j.lt.0) go to 204                                           
  202 k1=nlhm*(j-1)-j*(j-3)/2+i-j                                       
      go to 208                                                         
  204 k1=nlhm*(i-1)-i*(i-3)/2+j-i                                       
  208 k4=j                                                              
  209 temp=temp+rhm(k4)*array(k1)                                       
      k4=i                                                              
  210 treds=treds+rhm(k4)*temp                                          
  212 continue                                                          
      if (i3.eq.1) f=treds                                              
   94 k6=nerc-nbrc+1                                                    
      k5=k5+k6                                                          
      if (k5.ge.iei.and.k5.lt.ie) go to 98                              
      a2=iblk                                                           
      go to 100                                                         
   98 a2=ixlab                                                          
      lab3(k5)=lab2(k5)                                                 
  100 if (k5.lt.ie) go to 102                                           
      a2=iblab                                                          
      if (lab4(k5).eq.rgnl) a2=rl                                       
      if (lab4(k5).eq.rgnq) a2=rq                                       
      if (lab4(k5).eq.rgnc) a2=rc                                       
      lab1(k5)=lab2(k5)                                                 
  102 r=k6                                                              
      ss=treds/r                                                        
      r=ss/sm                                                           
      k4=k5-k6+1                                                        
      if (k4.eq.iei.and.kb.eq.1.and.mty.eq.7) go to 52                  
      go to 53                                                          
   52 fsq(m35*14+k)=treds                                                  
      ndfac=k6                                                          
   53 fpro=prof(k6,edf,r)                                               
      if (k5.eq.ie.and.nlhm.ge.ie.and.nrn.eq.nrun) write (6,1030) regs
 1030 format (1h ,2x,a12)                                               
      if (kb.eq.2.or.liop.lt.20) write (6,1016) lab1(k5),a2,lab3(k5),k6,
     1treds,ss,r,fpro,lab1(m99)                                          
 1016 format (1h ,a6,1x,a2,1x,a6,i6,f22.6,f18.6,f13.3,f10.4,4x,a8)      
      j=0                                                               
      if (kd.gt.0.and.i3.eq.2) go to 31                                 
      if (mty.eq.6.and.kb.eq.1.and.i3.eq.1) go to 31                    
      go to 30                                                          
   31 if (mty.eq.3.or.mty.gt.4) go to 32                                
      go to 30                                                          
   32 ifp=1                                                             
      if (ipl(1).ne.1) go to 33                                         
      j=1                                                               
      if (k6-5) 34,34,35                                                
   34 ifp=k6+1                                                          
      go to 33                                                          
   35 ifp=7                                                             
   33 i=ifp*(k-1)+1                                                     
      fsq(i)=treds                                                      
      ndfa=k6                                                           
      lab1(m99-1)=lab1(k5)                                                 
      lab1(m99+1)=lab1(k5)                                                
   30 if (kb.eq.1.and.mty.eq.7.and.i3.eq.1) go to 40                    
      go to 41                                                          
   40 ifpc=1                                                            
      if (ipl(1).ne.2) go to 42                                         
      j=2                                                               
      if (k6-5) 43,43,44                                                
   43 ifpc=k6+1                                                         
      go to 42                                                          
   44 ifpc=7                                                            
   42 i=ifpc*(k-1)+1                                                    
      fsq(m35*7+i)=treds                                                  
      ndfc=k6                                                           
      lab1(m99-1)=lab1(k5)                                                 
   41 if (l2.gt.nsme.or.im(i3).ne.nnd(l2)) go to 104                    
      l2=l2+1                                                           
      ss=0.                                                             
      if(k6.eq.1) go to 104                                             
      do 20 l=1,k6                                                      
      if (l.gt.5) go to 20                                              
      i=k*5+l+(l2-2)*nrhm*5                                             
      if (j.ne.1) go to 36                                              
      k4=ifp*(k-1)+l+1                                                  
      fsq(k4)=sss(i)                                                    
   36 r=sss(i)/sm                                                       
      fpro=prof(1,edf,r)                                                
      if (kb.eq.2.or.liop.lt.20) write (6,1027) lp(l),sss(i),sss(i),r,fp
     1ro,lab1(m99)                                                       
 1027 format (1h ,2x,a6,11x,1h1,4x,f20.6,f18.6,f13.3,f10.4,4x,a8)       
      ss=ss+sss(i)                                                      
      if (j.ne.2) go to 20                                              
      k4=ifpc*(k-1)+l+1                                                 
      fsq(m35*7+k4)=sss(i)                                                
   20 continue                                                          
      if (k6.lt.6) go to 104                                            
      treds=treds-ss                                                    
      if (j.ne.2) go to 45                                              
      k4=ifpc*(k-1)+7                                                   
      fsq(m35*7+k4)=treds                                                 
   45 i=k6-5                                                            
      ss=treds/dble(i)                                                 
      r=ss/sm                                                           
      fpro=prof(i,edf,r)                                                
      if (kb.eq.2.or.liop.lt.20)write(6,1028)i,treds,ss,r,fpro,lab1(m99)
 1028 format (1h ,2x,8hresidual,4x,i6,4x,f20.6,f18.6,f13.3,f10.4,4x,a8) 
      if (j.ne.1) go to 104                                             
      k4=ifp*(k-1)+7                                                    
      fsq(k4)=treds                                                     
  104 continue                                                          
   27 k6=edf                                                            
      if (nab.gt.2) sm=sd/edf                                           
      if (kb.eq.2) go to 54                                             
      if (liop.lt.20) write (6,1017) k6,sd,sm                           
 1017 format (1h ,9hremainder,9x,i6,f20.6,f18.6)                        
      go to 310                                                         
   54 write (6,1005) k6,sd,sm                                           
 1005 format (1h ,9hremainder,7x,i6,f22.6,f18.6)                        
  310 if (mty.gt.1.or.nlhm.eq.0) go to 311                              
      if (ncpr.eq.0) go to 312                                          
      k1=nrhm*(k-1)-k*(k-3)/2                                           
      ss=sscpr(k1)*edf+tred(k)                                          
      go to 313                                                         
  312 ss=sscpr(k)                                                       
  313 sd=dsqrt(sm)                                                      
      cv=(sd/tot4(k))*100.                                              
      if (nab.eq.0.or.nab.eq.3) go to 314                               
      ss=tred(k)/ss                                                     
      r=dsqrt(ss)                                                       
      go to 315                                                         
  314 if (nab.eq.3) go to 311                                           
      k6=nlhm+k                                                         
      if (ibet.eq.0) swt1=ncds                                          
      f=(tot(k6)*tot(k6))/swt1                                          
      ss=(tred(k)-f)/(ss-f)                                             
      r=dsqrt(ss)                                                       
  315 continue                                                          
      if (liop.lt.20) write(6,1029) tot4(k),sd,cv,ss,r                  
 1029 format (1h0,5x,'mean =',f12.5,2x,'error standard deviation =',f12.
     15,2x,'cv =',f7.2,2x,'r squared =',f6.3,2x,'r =',f6.3)             
  311 if (ncpr.eq.1) go to 108                                          
      k6=nrhm*(k-1)-k*(k-3)/2                                           
      sscpr(k6)=sm                                                      
  108 continue                                                          
      if(mty.gt.2.and.nrun.eq.nrn) write(6,1031)                        
 1031 format(1h0,'*** tests of significance for all sets of',           
     c' effects where remainder is not used as the error term',         
     c' are approximate',/,4x,' with unbalanced data or in',            
     c' covariance analyses ***')                                       
      if (nrn.lt.nrun.or.mty.eq.1.or.man.eq.1) go to 152                
      call tpage(ntitl)                                                 
      write (6,10)                                                      
   10 format (1h0,15x,'variance and covariance component estimates from 
     1indirect analyses')                                               
      if (mty.gt.3) go to 154                                           
      sdf=edff-edf                                                      
      wk=(dble(ncds)-tot4(m99+1))/sdf                                    
      if (ibet.eq.1) wk=(swt1-tot4(m99+1))/sdf                            
      go to 155                                                         
  154 sdf=edfs-edf                                                      
      wk=(dble(ncds)-tot4(m99+2))/sdf                                    
      if (ibet.eq.1) wk=(swt1-tot4(m99+2))/sdf                            
  155 rr=sdf                                                            
      write (6,1018) name,wk,sdf                                        
 1018 format(1h0,10x,'k for random effects component (',a6,')=',f10.4,3x
     1,'degrees of freedom =',f6.0)                                     
      write (6,1019)                                                    
 1019 format (1h0/25x,51hss, cp, ms, mcp, variance and covariance compon
     1ents//1h ,25hjob row col   rhm     rhm,20x,8hss or cp,19x,9hms or 
     2cov,18x,10hcomponents)                                            
      do 153 i=1,nrhm                                                   
      write (6,1004)                                                    
      k6=nrhm*(i-1)-i*(i-3)/2-i                                         
      do 153 j=i,nrhm                                                   
      k=k6+j                                                            
      if (mty.gt.3) go to 156                                           
      ss= fy(k)-sscpr(k)*edf                                            
      go to 157                                                         
  156 ss=sab(k)-sscpr(k)*edf                                            
  157 sms=ss/sdf                                                        
      if (i.eq.j) xp(i)=sms                                             
      sod=(sms-sscpr(k))/wk                                             
      write (6,1020) ijob,i,j,lity(i),lity(j),ss,sms,sod                
 1020 format (1h ,i3,2i4,2(2x,a6),3f27.8)                               
      if (i.eq.j.and.sod.lt.0.0) sod=0.0                                
      if (mty.lt.4) sab(k)=sod                                          
  153 sss(k)=sod                                                        
      sof=wk                                                            
      r=sdf                                                             
      if (iad.eq.0.or.mty.gt.3) go to 158                               
      l=14                                                              
      call svcvc                                                        
  158 if (mty.lt.4) go to 152                                           
      sdf=edff-edfs                                                     
      f=(dble(ncds)-tot4(m99+1))/sdf                                     
      if (ibet.eq.1) f=(swt1-tot4(m99+1))/sdf                             
      wk=(tot4(m99+2)-tot4(m99+4))/sdf                                      
      write (6,1004)                                                    
      wtt=sdf                                                           
      write (6,11)  nam5,wk,f,sdf                                       
   11 format(1h0,10x,'k values (',a6,') are: k2=',f10.4,3x,'k3=',f10.4, 
     13x,'degrees of freedom =',f6.0)                                   
      write (6,1002) name,nam5                                          
 1002 format (1h0,10x,'negative variance component estimates for ',a6,' 
     1set to zero to compute ',a6,' variance components')               
      write (6,1019)                                                    
      do 159 i=1,nrhm                                                   
      write (6,1004)                                                    
      k6=nrhm*(i-1)-i*(i-3)/2-i                                         
      do 159 j=i,nrhm                                                   
      k=k6+j                                                            
      ss=fy(k)-sab(k)                                                   
      sms=ss/sdf                                                        
      if (i.eq.j) yp(i)=sms                                             
      sod=(sms-sscpr(k)-wk*sss(k))/f                                    
      write (6,1020) ijob,i,j,lity(i),lity(j),ss,sms,sod                
      sab(k)=sss(k)                                                     
      if (i.eq.j.and.sod.lt.0.0) sod=0.0                                
      fy(k)=sod                                                         
  159 sss(k)=sod                                                        
      if (mty.gt.5) go to 160                                           
      if (iad.eq.0) go to 152                                           
      nw(13)=nw(15)                                                     
      nr1(13)=nr1(14)+nr1(15)                                           
      l=13                                                              
      do 162 i=1,ka                                                     
  162 sss(i)=sss(i)+sab(i)                                              
      wk=(f*sdf+sof*r)/(sdf+r)                                          
      sdf=(sdf+r)/2.0                                                   
      write (6,12)                                                      
   12 format (1h0,40x,'from full sibs')                                 
      call svcvc                                                        
      nw(13)=nw(14)+nr1(14)                                             
      nr1(13)=nr1(15)                                                   
      do 163 i=1,ka                                                     
      sss(i)=sss(i)-sab(i)                                              
  163 sscpr(i)=sscpr(i)+sab(i)                                          
      wk=f                                                              
      sdf=sdf*2.0-r                                                     
      write (6,13)                                                      
   13 format (1h0,40x,'from paternal half sibs')                        
      call svcvc                                                        
      nw(13)=nw(14)+nr1(15)                                             
      nr1(13)=nr1(14)                                                   
      do 164 i=1,ka                                                     
      sscpr(i)=sscpr(i)-sab(i)+sss(i)                                   
  164 sss(i)=sab(i)                                                     
      wk=sof                                                            
      sdf=r                                                             
      write (6,14)                                                      
   14 format (1h0,40x,'from maternal half sibs')                        
      call svcvc                                                        
      do 165 i=1,ka                                                     
  165 sscpr(i)=sscpr(i)-fy(i)                                           
      go to 152                                                         
  160 if (tot4(m99+3).eq.0.0) go to 152                                   
      l=15                                                              
      do 161 i=1,ka                                                     
  161 sscpr(i)=sscpr(i)+sab(i)                                          
      wk=f                                                              
      call svcvc                                                        
      do 166 i=1,ka                                                     
  166 sscpr(i)=sscpr(i)-sab(i)                                          
  152 return                                                            
      end                                                               
