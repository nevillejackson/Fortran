      subroutine xmat                                                   
c     -------------------------------------------                       
c     coding subroutine - - sets up x array for each observation        
c     ----------------------------------------------------              
      include 'decl1'
      double precision jlog10
      include 'com1'
      include 'com2'
      include 'com3'
      include 'com4'
      include 'com6'
      do 1 i=1,not                                                      
    1 x(i)=0.                                                           
      n=1                                                               
      k=0                                                               
      if (nab.eq.1.or.nab.eq.2.or.nab.eq.4) go to 2                     
      x(1)=1                                                            
      n=n+1                                                             
c     ----------------------------------------------------              
c     places codes in x for main effects                                
c     ----------------------------------------------------              
    2 if (nme.eq.0) go to 11                                            
      k1=nmea+1                                                         
      do 10 i=k1,nom                                                    
      k=ncl(i)+k                                                        
      l7=ibeg(i)                                                        
      l=lme(i)+l7-1                                                     
      j=ncds                                                            
      call field (icod,l,ic,l7,j)                                       
      if (j.eq.1) go to 902                                             
      ml1=ml+k                                                          
      ml2=ml1-ncl(i)                                                    
      ml3=ml2                                                           
      nend=n+ncl(i)-1                                                   
    4 ml3=ml3+1                                                         
      lscjm=i                                                           
      if (ml3.gt.ml1) go to 901                                         
      if (icod.ne.iden(ml3)) go to 4                                    
      mloc=ml3-ml                                                       
      nos(mloc)=nos(mloc)+1                                             
      ww(mloc)=ww(mloc)+wtt                                             
      if (i.eq.1.and.kb.eq.1.and.mty.eq.6) r1i(mloc)=r1i(mloc)+1.0      
      if (i.eq.2.and.kb.eq.1.and.mty.eq.7) r1i(mloc)=r1i(mloc)+1.0      
      ml3=ml3-ml2                                                       
      mloc=n+ml3-1                                                      
      if (ml3.eq.ncl(i)) go to 6                                        
      x(mloc)=1.                                                        
      go to 10                                                          
    6 k2=nend-1                                                         
      do 8 j=n,k2                                                       
    8 x(j)=-1.                                                          
   10 n=nend                                                            
c     ----------------------------------------------------              
c     places codes in x for nested main effects                         
c     ----------------------------------------------------              
   11 j1=k                                                              
      k=0                                                               
      if (nne.eq.0) go to 19                                            
      k1=nnea+1                                                         
      do 18 i=k1,non                                                    
      k=ncln(i)+k                                                       
      l7=nbeg(i)                                                        
      l=lne(i)+l7-1                                                     
      j=ncds                                                            
      call field (icod,l,ic,l7,j)                                       
      if (j.eq.1) go to 902                                             
      ml1=mlb+k                                                         
      ml2=ml1-ncln(i)                                                   
      ml3=ml2                                                           
      nend=n+ncln(i)-1                                                  
   12 ml3=ml3+1                                                         
      if (ml3.gt.ml1) go to 18                                          
      if (icod.ne.nden(ml3)) go to 12                                   
      mloc=j1+ml3-mlb                                                   
      nos(mloc)=nos(mloc)+1                                             
      ww(mloc)=ww(mloc)+wtt                                             
      ml3=ml3-ml2                                                       
      mloc=n+ml3-1                                                      
      if (ml3.eq.ncln(i)) go to 14                                      
      x(mloc)=1.                                                        
      go to 18                                                          
   14 k2=nend-1                                                         
      do 16 j=n,k2                                                      
   16 x(j)=-1.                                                          
   18 n=nend                                                            
c     ----------------------------------------------------              
c     places codes in x for two-factor interactions                     
c     ----------------------------------------------------              
   19 j1=j1+k                                                           
      k5=0                                                              
      nsub=0                                                            
      if (n2f.eq.0) go to 51                                            
      do 50 i=1,n2f                                                     
      intv=int1(i)                                                      
      isw4=0                                                            
      call code                                                         
      if (l.eq.1) go to 902                                             
      if (isw2.ne.1) go to 20                                           
      isw4=1                                                            
   20 if (isw3.eq.1) go to 904                                          
      icon=ml3*100                                                      
      me3=ml3                                                           
      k3=k1                                                             
      intv=int2(i)                                                      
      call code                                                         
      if (l.eq.1) go to 902                                             
      if (isw3.eq.1) go to 904                                          
      icon=icon+ml3                                                     
      me2=ml3                                                           
      k4=k1                                                             
      k6=k5+nmc(i)                                                      
      if (isw2.eq.1.or.isw4.eq.1) go to 48                              
      mloc=j1+nsub+(me3-1)*k1+me2                                       
      nos(mloc)=nos(mloc)+1                                             
      ww(mloc)=ww(mloc)+wtt                                             
      k7=k6-nmc(i)                                                      
   22 k7=k7+1                                                           
      if (k7.gt.k6) go to 24                                            
      if (icon-mscl(k7)) 22,48,22                                       
   24 k9=k3-1                                                           
      do 26 j=1,k9                                                      
   26 eff1(j)=0.0                                                       
      k9=k4-1                                                           
      do 28 j=1,k9                                                      
   28 eff2(j)=0.0                                                       
      if (me3.eq.k3) go to 30                                           
      eff1(me3)=1.                                                      
      go to 34                                                          
   30 k9=k3-1                                                           
      do 32 j=1,k9                                                      
   32 eff1(j)=-1.                                                       
   34 if (me2.eq.k4) go to 36                                           
      eff2(me2)=1.                                                      
      go to 40                                                          
   36 k9=k4-1                                                           
      do 38 j=1,k9                                                      
   38 eff2(j)=-1.                                                       
   40 l1=0                                                              
      k9=k3-1                                                           
      k2=k4-1                                                           
      do 46 j=1,k9                                                      
      do 46 j2=1,k2                                                     
      ico=j*100+j2                                                      
      if (nmc(i).eq.0) go to 44                                         
      k7=k6-nmc(i)                                                      
   42 k7=k7+1                                                           
      if (k7.gt.k6) go to 44                                            
      if (ico-mscl(k7)) 42,46,42                                        
   44 l1=l1+1                                                           
      mloc=n+l1-1                                                       
      x(mloc)=eff1(j)*eff2(j2)                                          
   46 continue                                                          
   48 k5=k6                                                             
      nsub=nsub+k3*k4                                                   
   50 n=n+(k3-1)*(k4-1)-nmc(i)                                          
c     ----------------------------------------------------              
c     places codes (x-xm), etc, in x array for regressions              
c     ----------------------------------------------------              
   51 if (npr.eq.0) go to 81                                            
      k2=1                                                              
   52 do 80 i=1,npr                                                     
      if (negx(i)-1) 58,54,56                                           
   54 call negf (tx,jbeg,lgtx,ic,i)                                     
      go to 58                                                          
   56 call negz  (tx,jbeg,lgtx,ic,i)                                    
   58 l7=jbeg(i)                                                        
      l=lgtx(i)+l7-1                                                    
      j=ncds                                                            
      call field (icod,l,ic,l7,j)                                       
      if (j.eq.1) go to 903                                             
      xr=icod                                                           
      if (negx(i).eq.0) go to 60                                        
      xr=xr*tx                                                          
   60 xr=xr/(10.**ndecx(i))                                             
      if (xr.eq.0.0) go to 62                                           
      if (loge(i).eq.1) xr=jlog10(xr)                                   
      if (loge(i).eq.2) xr=dsqrt(xr)                                    
   62 xr=xr-xm(i)                                                       
   68 yr=1.                                                             
      k5=k2                                                             
      k3=lqc(i)                                                         
      do 107 j=1,k3                                                     
      x(n)=yr*xr                                                        
      yr=x(n)                                                           
      n=n+1                                                             
      if (iclr(i).eq.0) go to 107                                       
      k6=iclr(i)                                                        
      do 101 k=1,k6                                                     
      intv=irm(k2)                                                      
      call code                                                         
      k9=k1-1                                                           
      do 102 k7=1,k9                                                    
  102 eff1(k7)=0.0                                                      
      if (ml3.eq.k1) go to 103                                          
      eff1(ml3)=1.                                                      
      go to 104                                                         
  103 do 105 k7=1,k9                                                    
  105 eff1(k7)=-1.                                                      
  104 do 106 k7=1,k9                                                    
      x(n)=eff1(k7)*yr                                                  
  106 n=n+1                                                             
  101 k2=k2+1                                                           
      k2=k5                                                             
  107 continue                                                          
      k2=k5+iclr(i)                                                     
   80 continue                                                          
c     ----------------------------------------------------              
c     places (y-ym) in x array for rhm                                  
c     ----------------------------------------------------              
   81 if (nrhm.eq.0)  go to 96                                          
      do 95 i=1,nrhm                                                    
      if (negy(i)-1) 86,82,84                                           
   82 call negf (tx,kbeg,lhy,ic,i)                                      
      go to 86                                                          
   84 call negz  (tx,kbeg,lhy,ic,i)                                     
   86 l7=kbeg(i)                                                        
      l=lhy(i)+l7-1                                                     
      j=ncds                                                            
      call field (icod,l,ic,l7,j)                                       
      if (j.eq.1) go to 903                                             
      yr=icod                                                           
      if (negy(i).eq.0) go to 88                                        
      yr=yr*tx                                                          
   88 yr=yr/(10.**ndecy(i))                                             
      if (yr.eq.0.0) go to 90                                           
      if (lny(i).eq.1) yr=jlog10(yr)                                    
      if (lny(i).eq.2) yr=dsqrt(yr)                                     
   90 yr=yr-ym(i)                                                       
   92 x(n)=yr                                                           
      n=n+1                                                             
   95 continue                                                          
   96 if (n-1.ne.not) go to 900                                         
      if (ibet.eq.0) go to 100                                          
   97 l=0                                                               
      i=n-1                                                             
      do 98 j=1,i                                                       
      if (j.gt.nlhm) l=l+1                                              
      if (j.le.nlhm.or.lny(l).eq.3) go to 99                            
      x(j)=x(j)-(wtt-1.)*ym(l)                                          
      go to 98                                                          
   99 x(j)=x(j)*wtt                                                     
   98 continue                                                          
      go to 100                                                         
  900 j=n-1                                                             
      write (6,1000) j                                                  
 1000 format (1h0,'no. lhm plus no. rhm (',i3,') does not agree with par
     1ameter card no. 1')                                               
      if (n-1.gt.not.or.mty.gt.1) go to 904                             
      not=n-1                                                           
      nlhm=n-1-nrhm                                                     
      matx=nlhm*(nlhm+1)/2                                              
      if (ibet.eq.1) go to 97                                           
      go to 100                                                         
  901 write (6,1001) ncds,lscjm,icod,ijob                               
 1001 format ('0class code missing on card no.',i8,5x,'effect',i3,' cann
     8ot find code ',i5,'. check parameter cards for problem no.',i3)   
      go to 904                                                         
  902 write (6,1002) ncds                                               
 1002 format (1h0,69hunits position of an id field or a control field is
     4 blank on card no.,i6)                                            
      go to 904                                                         
  903 write (6,1003) ncds                                               
 1003 format (1h0,55hunits position of an x or y field is blank on card 
     1 no.,i6)                                                          
  904 mull=1                                                            
      call wcard(in,k,l,ic,idum,iflag)
  100 continue                                                          
      return                                                            
      end                                                               
