      subroutine lsmns                                                  
c     -------------------------------------------                       
c     subroutine for computing and listing least-squares means and      
c     standard errors when nab=0 or nab=3 and both nmea and nnea        
c     equal zero. also used with mixed models.                          
c     -------------------------------------------------------           
      include 'decl1'
      character*6 mu,id1,id2
      include 'com1'
      include 'com2'
      include 'com3'
      include 'com4'
      include 'com9'
      data mu/6hmu    /                                                 
      if (nab.eq.1.or.nab.eq.2.or.nab.eq.4) go to 999                   
      write (6,1001) ijob                                               
 1001 format (1h0,7x,77hlisting of constants, least-squares means and st
     1andard errors for problem no.,i3)                                 
      write (6,2001)                                                    
 2001 format (1h0,76x,8hstandard,26x,8hstandard)                        
      write (6,1002)                                                    
 1002 format (1h ,10h rhm   row,30x,3hno.,3x,9heffective,4x,8hconstant, 
     19x,8herror of,6x,13hleast-squares,7x,8herror of)                  
      write (6,1003)                                                    
 1003 format (1h ,33h name  code  independent variable,7x,4hobs.,5x,3hno
     1.,7x,8hestimate,9x,8hconstant,10x,4hmean,12x,7hls mean)           
      i=0                                                               
      if (ibet.eq.0) swt1=ncds                                          
      if (mty.gt.3.and.mn2.eq.1) tot4(m99+6)=tot4(m99+6)/swt1               
      if(ibet.eq.0) ncrs=ncds                                           
      if(ibet.eq.0) go to 1                                             
      do 84 j=1,ncc                                                     
   84 nos(j)=ww(j)                                                      
      ncrs=swt1                                                         
    1 i=i+1                                                             
      nct=1                                                             
      write (6,1004)                                                    
 1004 format (1h )                                                      
      if (ncpr.eq.1) go to 2                                            
      j=i                                                               
      go to 3                                                           
    2 j=nrhm*(i-1)-i*(i-3)/2                                            
      ll=j                                                              
    3 wk=(sscpr(j)-tred(i))/edf                                         
      if (mn2.eq.1) wk=sscpr(j)                                         
      if (mn2.eq.0) go to 91                                            
      if (wk.lt.0.0) wk=+0.0                                            
      go to (91,92,92,94,94,94,94),mty                                  
   92 if (sab(j).lt.0.0) sab(j)=+0.0                                    
      ad=tot4(m99+5)*sab(j)*array(1)                                      
      rep=dsqrt(wk*array(1)+ad)                                         
      go to 112                                                         
   94 if (fy(j).lt.0.0) fy(j)=+0.0                                      
      if (sab(j).lt.0.0) sab(j)=+0.0                                    
      if (mty.gt.5) go to 96                                            
      ad=array(1)*(tot4(m99+5)*fy(j)+tot4(m99+7)*sab(j))                    
      rep=dsqrt(wk*array(1)+ad)                                         
      go to 112                                                         
   96 ad=array(1)*(tot4(m99+5)*fy(j)+tot4(m99+6)*sab(j))                    
      rep=dsqrt(wk*array(1)+ad)                                         
      go to 112                                                         
   91 rep=dsqrt(wk*array(1))                                            
  112 j=matx+(i-1)*nlhm+1                                               
      sdf=array(j)+ym(i)                                                
      efnum=1.0d0/array(1)                                              
      write (6,1005) lity(i),mu,ncrs,efnum,sdf,rep,sdf,rep              
 1005 format (1h ,a6,3x,1h1,2x,a6,18x,i7,f9.1,4f17.8)                   
      l=1                                                               
      l2=1                                                              
      k=ml+1                                                            
      if (nom.eq.nmea) go to 4                                          
      k8=nom-nmea                                                       
      do 5 j=1,k8                                                       
      k2=ncl(j+nmea)                                                    
      do 5 k1=1,k2                                                      
      if (k1.eq.k2) go to 7                                             
      k3=im(j)+k1                                                       
      k4=nlhm*(k3-1)-k3*(k3-3)/2                                        
      ac=wk*(array(1)+array(k4)+2.0*array(k3))                          
      if (ac.lt.0.0) ac=0.0                                             
      rep=dsqrt(ac)                                                     
      repac=wk*array(k4)                                                
      if (repac.lt.0.0) repac=0.0                                       
      repac=dsqrt(repac)                                                
      k4=matx+(i-1)*nlhm+k3                                             
      ac=array(k4)                                                      
      als=ac+sdf                                                        
      l2=l2+1                                                           
      l1=l2                                                             
      go to 8                                                           
    7 k3=im(j)+1                                                        
      k4=matx+(i-1)*nlhm+k3                                             
      ac=0.                                                             
      us=0.                                                             
      i5=k2-1                                                           
      do 9 m=1,i5                                                       
      ac=ac-array(k4)                                                   
      us=us-array(k3)                                                   
      k4=k4+1                                                           
    9 k3=k3+1                                                           
      k5=im(j)+k2-1                                                     
      ss=0.                                                             
      k3=im(j)+1                                                        
      do 81 m=k3,k5                                                     
      k4=nlhm*(m-1)-m*(m-3)/2                                           
      do 10 n=m,k5                                                      
      if (m.eq.n) go to 80                                              
      k6=k4+n-m                                                         
      ss=ss+2.0*array(k6)                                               
      go to 10                                                          
   80 ss=ss+array(k4)                                                   
   10 continue                                                          
   81 continue                                                          
      als=wk*(array(1)+ss+2.0*us)                                       
      if (als.lt.0.0) als=0.0                                           
      rep=dsqrt(als)                                                    
      ssac=ss*wk                                                        
      if (ssac.lt.0.0) ssac=0.0                                         
      repac=dsqrt(ssac)                                                 
      als=ac+sdf                                                        
      l1=0                                                              
    8 efnum=dfloat(nos(l))                                              
      if (rep.ne.0.0) efnum=wk/(rep*rep)                                
      if (mty.eq.1.or.mn2.eq.0) go to 93                                
      ss=rep*rep                                                        
      ssac=repac*repac                                                  
      yt=ss                                                             
      if (mty.eq.2.or.mty.eq.4) ss=ss+ad                                
      if (j.gt.1.and.(mty.eq.3.or.mty.eq.5.or.mty.eq.6)) ss=ss+ad       
      if (mty.eq.7.and.j.gt.2) ss=ss+ad                                 
      if (mty.eq.3.and.j.eq.1) ss=ss+(ss*r1i(k1)*sab(ll))/wk            
      if (mty.eq.3.and.j.eq.1) ssac=ssac+(ssac*r1i(k1)*sab(ll))/wk      
      if (j.eq.1.and.(mty.eq.5.or.mty.eq.7)) ss=ss+(ss*(r1i(k1)*fy(ll)+ 
     1r2i(k1)*sab(ll)))/wk                                              
      if (j.eq.1.and.(mty.eq.5.or.mty.eq.7)) ssac=ssac+(ssac*(r1i(k1)*  
     *fy(ll)+r2i(k1)*sab(ll)))/wk                                       
      if (mty.eq.6.and.j.eq.1) go to 201                                
      if (mty.eq.7.and.j.eq.2) go to 201                                
      if (mty.eq.7.and.j.eq.1) r2i(k1)=yt/wk                            
      go to 202                                                         
  201 ss=ss+(ss*tot4(k1)*sab(ll))/wk+array(1)*tot4(m99+5)*fy(ll)          
      ssac=ssac+(ssac*tot4(k1)*sab(ll))/wk                              
  202 if (ss.lt.0.0) ss=+0.0                                            
      rep=dsqrt(ss)                                                     
      if (ssac.lt.0.0) ssac=+0.0                                        
      repac=dsqrt(ssac)                                                 
   93 write (6,1006) lity(i),l1,lit(j+nmea),iden(k),nos(l),efnum,ac,    
     1repac,als,rep                                                     
 1006 format (1h ,a6,i4,2x,a6,i5,13x,i7,f9.1,4f17.8)                    
      k=k+1                                                             
    5 l=l+1                                                             
c     -------------------------------------------------------           
c     construction of x and tot2 vectors for nested main effects        
c     -------------------------------------------------------           
    4 k=mlb+1                                                           
      if (non.eq.nnea) go to 6                                          
      k8=nnea+1                                                         
      do 11 j=k8,non                                                    
      k9=0                                                              
      k4=nma(j)-1                                                       
      if (k4.lt.nmea) k9=1                                              
      nsum=1                                                            
      if (k4.le.nmea) go to 13                                          
      do 12 k3=1,k4                                                     
   12 nsum=nsum+ncl(k3+nmea)-1                                          
   13 k2=ncln(j)                                                        
      do 11 k1=1,k2                                                     
      do 14 k3=1,nlhm                                                   
      tot2(k3)=0.                                                       
   14 x(k3)=0.                                                          
      x(1)=1.                                                           
      i2=nom+j-nnea-nmea                                                
      k3=im(i2)+k1                                                      
      if (k9.eq.1) go to 15                                             
      if (nmac(j).ne.ncl(k4+1)) go to 15                                
      i3=ncl(k4+1)-1                                                    
      do 16 k5=1,i3                                                     
      i4=nsum+k5                                                        
   16 x(i4)=-1.0                                                        
      if (k1.ne.k2) go to 17                                            
   20 i5=k2-1                                                           
      do 18 k5=1,i5                                                     
      i4=im(i2)+k5                                                      
      x(i4)=-1.0                                                        
   18 tot2(i4)=-1.0                                                     
      l1=0                                                              
      go to 19                                                          
   15 i4=nsum+nmac(j)                                                   
      x(i4)=1.                                                          
      if (k1.ne.k2) go to 17                                            
      go to 20                                                          
   17 tot2(k3)=1.                                                       
      l2=l2+1                                                           
      l1=l2                                                             
      x(k3)=1.                                                          
   19 yt=ym(i)                                                          
      nr=matx+(i-1)*nlhm+1                                              
c     ------------------------------------------------------            
c     call subroutine which computes constant, ls mean and se, and lists
c     ------------------------------------------------------            
      call candse (ac,repac,als,rep,tot2,x,array,nr,nlhm,wk,yt,i,sss,   
     *nct)                                                              
      efnum=dfloat(nos(l))                                              
      if (rep.ne.0.0) efnum=wk/(rep*rep)                                
      if (mn2.eq.0) go to 206                                           
      if (mty.gt.1) rep=dsqrt(rep*rep+ad)                               
      if (nma(j).eq.1.and.(mty.eq.3.or.mty.eq.5.or.mty.eq.7)) go to 205 
      go to 206                                                         
  205 yt=1.0/float(k2)                                                  
      k6=nmac(j)                                                        
      ss=rep*rep-ad                                                     
      if (mty.eq.3) rep=dsqrt(ss+(ss*r1i(k6)*yt*sab(ll))/wk)            
      if (mty.eq.5.or.mty.eq.7) rep=dsqrt(ss+(ss*yt*(r1i(k6)*fy(ll)+r2i(
     1k6)*sab(ll)))/wk)                                                 
  206 if (k9.eq.1) write (6,2006) lity(i),l1,nlit(j),nden(k),nos(l),    
     1ac,repac                                                          
 2006 format (1h ,a6,i4,2x,a6,i5,13x,i7,9x,2f17.8)                      
      if (k9.eq.1) go to 250                                            
      write (6,1006) lity(i),l1,nlit(j),nden(k),nos(l),efnum,ac,repac,  
     *als,rep                                                           
  250 k=k+1                                                             
   11 l=l+1                                                             
c     ------------------------------------------------------            
c     construction of x and tot2 vectors for two-way subclasses         
c     ------------------------------------------------------            
    6 not=nom+non+1-nmea-nnea                                           
      k8=im(not)                                                        
      k6=0                                                              
      if (n2f.eq.0) go to 21                                            
      k10=nom+nnea                                                      
      do 22 j=1,n2f                                                     
      mj1=0                                                             
      mj2=0                                                             
      mj3=0                                                             
      mj4=0                                                             
      k9=0                                                              
c     ------------------------------------------------------            
c     sets x and tot2 arrays to zero and sets x(1) to 1 for mu          
c     ------------------------------------------------------            
      do 23 k1=1,nlhm                                                   
      tot2(k1)=0.                                                       
   23 x(k1)=0.                                                          
      x(1)=1.                                                           
      k6=k6+nmc(j)                                                      
      nsum=0                                                            
      k4=int2(j)-1                                                      
      msum=0                                                            
      k5=int1(j)-1                                                      
      if (int1(j).gt.nom) go to 24                                      
      if (int1(j).le.nmea) k9=1                                         
      mj1=int1(j)                                                       
      if (k5.eq.0) go to 25                                             
      do 26 k3=1,k5                                                     
   26 nsum=nsum+ncl(k3)                                                 
   25 k2=int1(j)-nmea                                                   
      i1=im(k2)+1                                                       
      if (k2.le.0) i1=1                                                 
      i2=im(k2+1)+1                                                     
      if (k2.le.0) i2=ncl(mj1)                                          
      id1=lit(mj1)                                                      
      go to 27                                                          
c     ------------------------------------------------------            
c     sets up +1 and -1 for major class when first main effect is       
c     nested                                                            
c     ------------------------------------------------------            
   24 k2=nom+1                                                          
      if (int1(j).le.k10) k9=1                                          
      if (k2.gt.k5) go to 28                                            
      do 29 k3=k2,k5                                                    
      k=k3-nom                                                          
   29 nsum=nsum+ncln(k)                                                 
   28 k2=int1(j)-nom                                                    
      k3=int1(j)-nnea-nmea                                              
      i1=im(k3)+1                                                       
      if (k3.le.nom+nnea) i1=1                                          
      i2=im(k3+1)+1                                                     
      if (k3.le.nom+nnea) i2=ncln(k2)                                   
      id1=nlit(k2)                                                      
      k=nma(k2)-nmea                                                    
      mj2=nma(k2)                                                       
      ni=nmac(k2)                                                       
      if (nmac(k2).eq.ncl(k)) go to 60                                  
      k7=im(k)+nmac(k2)                                                 
      x(k7)=1.                                                          
      go to 27                                                          
   60 k1=ncl(k)-1                                                       
      do 61 k3=1,k1                                                     
      k2=im(k)+k3                                                       
   61 x(k2)=-1.                                                         
   27 if (int2(j).gt.nom) go to 30                                      
      if (int2(j).le.nmea) k9=1                                         
      mj3=int2(j)                                                       
      if (k4.eq.0) go to 31                                             
      do 32 k3=1,k4                                                     
   32 msum=msum+ncl(k3)                                                 
   31 k2=int2(j)-nmea                                                   
      i3=im(k2)+1                                                       
      if (k2.le.0) i3=1                                                 
      i4=im(k2+1)+1                                                     
      if (k2.le.0) i4=ncl(mj3)                                          
      id2=lit(mj3)                                                      
      go to 33                                                          
c     ------------------------------------------------------            
c     sets up +1 and -1 for major class when second main effect is      
c     nested                                                            
c     ------------------------------------------------------            
   30 k2=nom+1                                                          
      if (int2(j).le.k10) k9=1                                          
      if (k2.gt.k4) go to 34                                            
      do 35 k3=k2,k4                                                    
      k=k3-nom                                                          
   35 msum=msum+ncln(k)                                                 
   34 k2=int2(j)-nom                                                    
      k3=int2(j)-nnea-nmea                                              
      i3=im(k3)+1                                                       
      if (k3.le.nom+nnea) i3=1                                          
      i4=im(k3+1)+1                                                     
      if (k3.le.nom+nnea) i4=ncln(k2)                                   
      id2=nlit(k2)                                                      
      k=nma(k2)-nmea                                                    
      mj4=nma(k2)                                                       
      nj=nmac(k2)                                                       
      if (nmac(k2).eq.ncl(k)) go to 62                                  
      k7=im(k)+nmac(k2)                                                 
      x(k7)=1.                                                          
      go to 111                                                         
   62 k1=ncl(k)-1                                                       
      do 63 k3=1,k1                                                     
      k2=im(k)+k3                                                       
   63 x(k2)=-1.                                                         
c     ------------------------------------------------------            
c     sets up x matrix for main effects and both x and tot2 arrays for  
c     interaction constants                                             
c     ------------------------------------------------------            
  111 if (mj2.ne.mj4.and.mj1.eq.mj3) go to 100                          
      go to 33                                                          
  100 me1=mj2                                                           
      me2=mj4                                                           
      call intact (me1,me2,ni,nj,n2f,not,int1,int2,x,nmc,mscl,ncl,eff1,e
     1ff2,im,ncln,nom)                                                  
   33 do 36 k=i1,i2                                                     
      j2=k-i1+1                                                         
c     ------------------------------------------------------            
c     sets up x array for first main effect                             
c     ------------------------------------------------------            
      l3=i2-1                                                           
      if (k.eq.i2) go to 42                                             
      do 64 k1=i1,l3                                                    
   64 x(k1)=0.                                                          
      x(k)=1.                                                           
      go to 41                                                          
   42 do 46 j4=i1,l3                                                    
   46 x(j4)=-1.                                                         
c     ------------------------------------------------------            
c     gets identification of class for first main effect                
c     ------------------------------------------------------            
   41 k2=nsum+j2                                                        
      if (int1(j).gt.nom) go to 37                                      
      id3=iden(k2)                                                      
      go to 38                                                          
   37 id3=nden(k2)                                                      
   38 do 36 j1=i3,i4                                                    
      j3=j1-i3+1                                                        
c     ------------------------------------------------------            
c     checks for missing subclass                                       
c     ------------------------------------------------------            
      if (nmc(j).eq.0) go to 45                                         
      k7=j2*100+j3                                                      
      k1=k6-nmc(j)                                                      
   43 k1=k1+1                                                           
      if (k1.gt.k6) go to 45                                            
      if (k7-mscl(k1)) 43,44,43                                         
c     ------------------------------------------------------            
c     sets up x array for second main effect                            
c     ------------------------------------------------------            
   45 i5=i4-1                                                           
      if (j1.eq.i4) go to 47                                            
      do 40 k1=i3,i5                                                    
   40 x(k1)=0.                                                          
      x(j1)=1.                                                          
      go to 48                                                          
   47 do 49 j4=i3,i5                                                    
   49 x(j4)=-1.                                                         
c     ------------------------------------------------------            
c     gets identification of class for second main effect               
c     ------------------------------------------------------            
   48 k2=msum+j3                                                        
      if (int2(j).gt.nom) go to 39                                      
      id4=iden(k2)                                                      
      go to 110                                                         
   39 id4=nden(k2)                                                      
c     ------------------------------------------------------            
c     sets up x and tot2 arrays for interaction effect                  
c     ------------------------------------------------------            
  110 if (mj1.ne.0.and.mj3.eq.0) go to 101                              
      go to 102                                                         
  101 me1=mj1                                                           
      me2=mj4                                                           
      ni=j2                                                             
      call intact (me1,me2,ni,nj,n2f,not,int1,int2,x,nmc,mscl,ncl,eff1,e
     1ff2,im,ncln,nom)                                                  
  102 if (mj3.ne.0.and.mj1.eq.0) go to 103                              
      go to 104                                                         
  103 me1=mj2                                                           
      me2=mj3                                                           
      nj=j3                                                             
      call intact (me1,me2,ni,nj,n2f,not,int1,int2,x,nmc,mscl,ncl,eff1,e
     1ff2,im,ncln,nom)                                                  
  104 if (mj2.ne.mj4.and.mj1.eq.mj3) go to 105                          
      go to 74                                                          
  105 me1=int1(j)                                                       
      me2=mj4                                                           
      ni=j2                                                             
      k2=int2(j)-nom                                                    
      nj=nmac(k2)                                                       
      call intact (me1,me2,ni,nj,n2f,not,int1,int2,x,nmc,mscl,ncl,eff1,e
     1ff2,im,ncln,nom)                                                  
      me1=mj2                                                           
      me2=int2(j)                                                       
      k2=int1(j)-nom                                                    
      ni=nmac(k2)                                                       
      nj=j3                                                             
      call intact (me1,me2,ni,nj,n2f,not,int1,int2,x,nmc,mscl,ncl,eff1,e
     1ff2,im,ncln,nom)                                                  
   74 l3=i2-i1                                                          
      i5=i4-i3                                                          
      n=not+j-1                                                         
      k2=im(n)+1                                                        
      n=l3*i5-nmc(j)                                                    
      m=k2+n-1                                                          
      do 59 j4=k2,m                                                     
      tot2(j4)=0.                                                       
   59 x(j4)=0.                                                          
      do 50 j4=1,l3                                                     
   50 eff1(j4)=0.                                                       
      do 51 j4=1,i5                                                     
   51 eff2(j4)=0.                                                       
      if  (k.eq.i2) go to 52                                            
      eff1(j2)=1.                                                       
      go to 53                                                          
   52 do 54 j4=1,l3                                                     
   54 eff1(j4)=-1.                                                      
   53 if (j1.eq.i4) go to 55                                            
      eff2(j3)=1.                                                       
      go to 56                                                          
   55 do 76 j4=1,i5                                                     
   76 eff2(j4)=-1.                                                      
   56 k5=k8+1                                                           
      do 82 k1=1,l3                                                     
      do 58 k2=1,i5                                                     
      k7=k1*100+k2                                                      
      if (nmc(j).eq.0) go to 71                                         
      j4=k6-nmc(j)                                                      
   70 j4=j4+1                                                           
      if (j4.gt.k6) go to 71                                            
      if (k7-mscl(j4)) 70,58,70                                         
   71 x(k5)=eff1(k1)*eff2(k2)                                           
      tot2(k5)=x(k5)                                                    
      k5=k5+1                                                           
   58 continue                                                          
   82 continue                                                          
      if  (k.eq.i2.or.j1.eq.i4) go to 72                                
      l2=l2+1                                                           
      l1=l2                                                             
      go to 73                                                          
   72 l1=0                                                              
   73 nr=matx+(i-1)*nlhm+1                                              
      yt=ym(i)                                                          
      call candse (ac,repac,als,rep,tot2,x,array,nr,nlhm,wk,yt,i,sss,   
     *nct)                                                              
      efnum=dfloat(nos(l))                                              
      if (rep.ne.0.0) efnum=wk/(rep*rep)                                
      if (mn2.eq.0) go to 204                                           
      if (mty.gt.1) rep=dsqrt(rep*rep+ad)                               
      if ((mty.eq.3.or.mty.gt.4).and.int1(j).eq.1) go to 203            
      go to 204                                                         
  203 ss=rep*rep-ad                                                     
      k1=k-i1+1                                                         
      k2=int2(j)                                                        
      if (k2.gt.nom) go to 204                                          
      yt=1.0/float(ncl(k2))                                             
      if (mty.eq.3) rep=dsqrt(ss+(ss*yt*r1i(k1)*sab(ll))/wk)            
      if (mty.eq.5) rep=dsqrt(ss+(ss*yt*(r1i(k1)*fy(ll)+r2i(k1)*sab(ll))
     1)/wk)                                                             
      if (mty.eq.6) rep=dsqrt(ss+(ss*yt*tot4(k1)*(sab(ll)+fy(ll)))/wk)  
      if (mty.eq.7) rep=dsqrt(ss+(ss*yt*r1i(k1)*sab(ll))/wk+r2i(k1)*yt*r
     11i(k1)*fy(ll))                                                    
  204 if (k9.eq.1) write (6,2007) lity(i),l1,id1,id2,id3,id4,nos(l),    
     1ac,repac                                                          
 2007 format (1h ,a6,i4,2x,a6,3h x ,a6,2i5,i6,9x,2f17.8)                
      if (k9.eq.1) go to 44                                             
      write (6,1007) lity(i),l1,id1,id2,id3,id4,nos(l),efnum,ac,repac,  
     *als,rep                                                           
 1007 format (1h ,a6,i4,2x,a6,3h x ,a6,2i5,i6,f9.1,4f17.8)              
   44 l=l+1                                                             
   36 continue                                                          
      k8=k8+l3*i5-nmc(j)                                                
   22 continue                                                          
c     ------------------------------------------------------            
c     listing of partial regressions and standard errors for            
c     continuous independent variables                                  
c     ------------------------------------------------------            
   21 if (npr.eq.0) go to 83                                            
      k4=1                                                              
      k=ie                                                              
      do 57 j=1,npr                                                     
      if (iclr(j).eq.0) go to 150                                       
      k3=lqc(j)                                                         
      do 151 k5=1,k3                                                    
      k6=iclr(j)                                                        
      if (k5.gt.1) k4=k4-k6                                             
      do 159 k7=1,k6                                                    
      l2=0                                                              
      k2=irm(k4)                                                        
      if (irm(k4).gt.nom) go to 153                                     
      j1=ncl(k2)+1                                                      
      l3=k2-1                                                           
      if (l3.eq.0) go to 154                                            
      do 163 l4=1,l3                                                    
  163 l2=l2+ncl(l4)                                                     
      go to 154                                                         
  153 l5=irm(k4)-nom                                                    
      j1=ncln(l5)+1                                                     
      l3=l5-1                                                           
      if (l3.eq.0) go to 154                                            
      do 164 l4=1,l3                                                    
  164 l2=l2+ncln(l4)                                                    
  154 k8=1                                                              
      if (k7.gt.1) k8=2                                                 
      do 152 j2=k8,j1                                                   
      do 161 l3=1,nlhm                                                  
      tot2(l3)=0.                                                       
  161 x(l3)=0.                                                          
      if (j2.eq.1) l6=k                                                 
      x(l6)=1.                                                          
      if (j2.gt.1) go to 156                                            
      yt=0.                                                             
      tot2(k)=1.                                                        
      go to 155                                                         
  156 if (j2.eq.j1) go to 158                                           
      l2=l2+1                                                           
      tot2(k)=1.                                                        
      x(k)=1.                                                           
      if (lad(m40+10+k4).eq.0) go to 155                                    
      l4=lad(m40+10+k4)-1                                                   
      if (iclr(m40+10+k4).eq.99) go to 162                                  
  168 l5=im(l4)+irm(k4)-nom                                             
      x(l5)=1.                                                          
      if (j2.eq.j1) go to 167                                           
      go to 155                                                         
  162 j3=im(l4)+1                                                       
      l5=im(l4+1)                                                       
      do 166 m=j3,l5                                                    
  166 x(m)=-1.                                                          
      if (j2.eq.j1) go to 167                                           
      go to 155                                                         
  158 i5=k-1                                                            
      i4=k-j1+2                                                         
      do 160 m=i4,i5                                                    
      x(m)=-1.                                                          
  160 tot2(m)=-1.                                                       
      l2=l2+1                                                           
      k=k-1                                                             
      if (lad(m40+10+k4).eq.0) go to 167                                    
      l4=lad(m40+10+k4)-1                                                   
      if (iclr(m40+10+k4).eq.99) go to 162                                  
      go to 168                                                         
  167 k4=k4+1                                                           
  155 nr=matx+(i-1)*nlhm+1                                              
      call candse (ac,repac,als,rep,tot2,x,array,nr,nlhm,wk,yt,i,sss,   
     *nct)                                                              
      l1=k                                                              
      if (j2.ne.1) go to 157                                            
      write (6,1008) lity(i),l1,lab1(k),lab2(k),lab3(k),lab4(k),ac,repac
     1,als,rep                                                          
 1008 format (1h ,a6,i4,4(1x,a6),14x,4f17.8)                            
      go to 152                                                         
  157 idd=iden(l2)                                                      
      if (j2.eq.j1) l1=0                                                
      if (k2.gt.nom) idd=nden(l2)                                       
      write (6,1009) lity(i),l1,idd,lab2(k),lab3(k),lab4(k),ac,repac,   
     *als,rep                                                           
 1009 format (1h ,a6,i4,i5,2x,3(1x,a6),14x,4f17.8)                      
  152 k=k+1                                                             
  159 continue                                                          
  151 continue                                                          
      go to 57                                                          
  150 k3=lqc(j)                                                         
      aa=sdf                                                            
      do 165 k5=1,k3                                                    
      l=nlhm*(k-1)-k*(k-3)/2                                            
      k9=matx+(i-1)*nlhm+k                                              
      ac=array(l)*wk                                                    
      if (ac.lt.0.0) ac=0.0                                             
      rep=dsqrt(ac)                                                     
      als=array(k9)                                                     
      ac=als                                                            
      l1=k                                                              
      write (6,1008) lity(i),l1,lab1(k),lab2(k),lab3(k),lab4(k),ac,rep, 
     1als,rep                                                           
      k=k+1                                                             
  165 continue                                                          
   57 continue                                                          
   83 if (i.ne.nrhm) go to 1                                            
      if(n2f.eq.0) go to 999                                            
      j=0                                                               
      do 990 i=1,n2f                                                    
  990 if(mscl(i).ne.0) j=1                                              
      if(j.eq.0) go to 999                                              
      write(6,2008)                                                     
 2008 format(1h0,'least-squares means for mu and for main or nested',   
     c' main effects associated with interactions where missing',       
     c' subclasses exist are',/,' really not estimable if the missing', 
     c' subclasses are present in the population of inference. ',       
     c'  the means given here are computed by',/,' using the usual',    
     c' linear functions of the solution vector obtained under the',    
     c' summation restriction.  preferably, constants should be',/,     
     c' fitted for the subclass effects and appropriate constrasts',    
     c' made among subclasses when subclasses are missing and',         
     c' interaction exist.')                                            
  999 if (nlc.eq.0) return                                              
      call linfun (nlc,nlhm,x,jbeg,tot2,nrhm,matx,ncpr,sscpr,tred,edf,xp
     *,yp,ac,als,rep,array,sss,nab,lity,mn2,ym,mty,rr,wtt)              
      return                                                            
      end                                                               
