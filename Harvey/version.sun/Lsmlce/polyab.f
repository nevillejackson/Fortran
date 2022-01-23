      subroutine polyab(ntitl)                                          
c     -------------------------------------------------                 
c     main subroutine for partitioning of interaction effects           
c     -------------------------------------------------                 
      include 'decl1'
      character*6 lab(3)
      character*6 id1,id2
c----- original dimensions
c     dimension idmj(1000),idm(4000),idi(4000),        
c    * numm(1000),nran(4000)                                            
c----- current dimensions
      dimension idmj(1000),idm(4000),idi(4000),        
     * numm(1000),nran(4000)                                            
c----- if dimensions altered set idmj(),numm() to m1000
c-----                       and idm(),idi(),nran() to m4000
c-----
      include 'com1p'
      include 'com2'
      include 'com3'
      include 'com4'
      include 'com5'
      include 'com9'
      data lab /6hlinear,6hquad  ,6hcubic /                             
c                                                                       
      if(n2f.eq.0.or.nsme.le.1) go to 997                               
      knab=0                                                            
      if (nab.eq.0.or.nab.eq.3.or.nab.eq.6) knab=1                      
      do 111 i=1,m90                                                     
  111 lqc(i)=im(i)                                                      
      lk=0                                                              
      if (non.eq.0) go to 407                                           
      do 403 i=1,nom                                                    
  403 lk=lk+ncl(i)                                                      
      lkn=0                                                             
      do 404 i=1,non                                                    
  404 lkn=lkn+ncln(i)                                                   
      lknn=lkn+lk                                                       
      not=nom+non                                                       
      k=lk+1                                                            
      do 405 i=k,lknn                                                   
      j=i-k                                                             
  405 iden(i)=nden(j)                                                   
      k=nom+1                                                           
      do 406 i=k,not                                                    
      j=i-nom                                                           
  406 ncl(i)=ncln(j)                                                    
  407 call tpage(ntitl)                                                 
      write(6,1001)                                                     
 1001 format(1h ,27x,78hlisting of polynomial coefficients and partition
     *ing of subclass sum of squares)                                   
      do 900 i2f=1,n2f                                                  
      if(nmc(i2f).ne.0) go to 900                                       
c     --------------------------------------------------                
c     determine main effects which are to be partitioned                
c     --------------------------------------------------                
      do 10 i=1,nsme                                                    
      k=i                                                               
      if (int1(i2f).eq.ipl(i)) go to 20                                 
   10 continue                                                          
      go to 900                                                         
   20 k1=ipl(k)                                                         
      do 30 i=1,nsme                                                    
      k=i                                                               
      if (int2(i2f).eq.ipl(i)) go to 40                                 
   30 continue                                                          
      go to 900                                                         
   40 k2=ipl(k)                                                         
      nsize=ncl(k1)*ncl(k2)-nmc(i2f)-1                                  
      natx=nsize*(nsize+1)/2                                            
      kmc=0                                                             
      kk1=ncl(k1)                                                       
      kk2=ncl(k2)                                                       
      jj1=kk1-1                                                         
      jj2=kk2-1                                                         
      n=0                                                               
      k=im(k1+knab-1)+1                                                 
      kk=k+jj1-1                                                        
      if (mty.gt.1) go to 135                                           
      if (lqc(k1+knab-1).eq.0) go to 135                                
      lqc(k1+knab-1)=0                                                  
      det=0.0                                                           
      call matinv (array,k,kk,nlhm,x,det)                               
  135 do 140 i=1,nlhm                                                   
  140 tot3(i)=0                                                         
      do 150 i=k,kk                                                     
      n=n+1                                                             
  150 tot3(i)=i                                                         
      k=im(k2+knab-1)+1                                                 
      kk=k+jj2-1                                                        
      if (mty.gt.1) go to 136                                           
      if (lqc(k2+knab-1).eq.0) go to 136                                
      lqc(k2+knab-1)=0                                                  
      det=0.0                                                           
      call matinv(array,k,kk,nlhm,x,det)                                
  136 do 151 i=k,kk                                                     
      n=n+1                                                             
  151 tot3(i)=i                                                         
      k=im(nme+nne+i2f+knab-1)+1                                        
      kk=k+jj1*jj2-nmc(i2f)-1                                           
      if (mty.gt.1) go to 137                                           
      if (lqc(nme+nne+i2f+knab-1).eq.0) go to 137                       
      lqc(nme+nne+i2f+knab-1)=0                                         
      det=0.0                                                           
      call matinv (array,k,kk,nlhm,x,det)                               
  137 do 154 i=k,kk                                                     
      n=n+1                                                             
  154 tot3(i)=i                                                         
c     -------------------------------------------------                 
c     find error terms                                                  
c     -------------------------------------------------                 
      do 156 i=1,nrhm                                                   
      k=nrhm*(i-1)-i*(i-3)/2                                            
c-----xzx(m35*m99+k) is a reference to sscpr(k) in 'com1'
c-----xzx() & rhm() have same base address & dimension of rhm()
c----- is (m35*m99) - originally 2450
      r2i(i)=xzx(m35*m99+k)                                                
  156 r1i(i)=r2i(i)*edf                                                 
c     --------------------------------------------------                
c     form vector to find appropriate inverse matrix elements           
c     --------------------------------------------------                
      jount=0                                                           
      lount=0                                                           
      do 301 i1=1,kk1                                                   
      do 300 i2=1,kk2                                                   
      if (i1.eq.kk1.and.i2.eq.kk2) go to 400                            
      indx=i1*50+i2                                                     
      if (nmc(i2f).eq.0) go to 80                                       
      kk=1                                                              
      if (i2f.eq.1) go to 60                                            
      jj=i2f-1                                                          
      do 50 j=1,jj                                                      
   50 kk=kk+nmc(j)                                                      
   60 kkk=kk+nmc(i2f)-1                                                 
      k=i1*100+i2                                                       
      do 70 j=kk,kkk                                                    
      if (mscl(j).eq.k) go to 300                                       
   70 continue                                                          
   80 ii=nmc(i2f)                                                       
      call tgen (nsize,kk1,kk2,jj1,jj2,i1,i2,ii,mscl,tot2)              
      jount=jount+1                                                     
c     --------------------------------------------------                
c     find constants for ab subclasses                                  
c     --------------------------------------------------                
      do 169 jj=1,nrhm                                                  
      n=0                                                               
      k=natx+(jj-1)*nsize+jount                                         
      fab(k)=0.0                                                        
      do 167 ji=1,nlhm                                                  
      if (tot3(ji).eq.0) go to 167                                      
      n=n+1                                                             
      if (tot2(n).eq.0) go to 167                                       
      l=matx+(jj-1)*nlhm+ji                                             
      fab(k)=fab(k)+tot2(n)*array(l)                                    
  167 continue                                                          
  169 continue                                                          
c     --------------------------------------------------                
c     find inverse elements for ab subclasses                           
c     --------------------------------------------------                
      kount=0                                                           
      do 170 ij=1,nlhm                                                  
      if (tot3(ij).eq.0) go to 170                                      
      kount=kount+1                                                     
      tot(kount)=0.0                                                    
      n=0                                                               
      do 165 ji=1,nlhm                                                  
      if (tot3(ji).eq.0) go to 165                                      
      n=n+1                                                             
      if (tot2(n).eq.0) go to 165                                       
      if (ij.lt.ji) go to 161                                           
      l=nlhm*(ji-1)-ji*(ji-3)/2+ij-ji                                   
      go to 162                                                         
  161 l=nlhm*(ij-1)-ij*(ij-3)/2+ji-ij                                   
  162 tot(kount)=tot(kount)+tot2(n)*array(l)                            
  165 continue                                                          
  170 continue                                                          
      do 250 i=1,kk1                                                    
      do 240 j=1,kk2                                                    
      jndx=i*50+j                                                       
      if (jndx.lt.indx) go to 240                                       
      k=i*j                                                             
      if (k.gt.nsize) go to 300                                         
      call tgen (nsize,kk1,kk2,jj1,jj2,i,j,ii,mscl,tot2)                
      lount=lount+1                                                     
      fab(lount)=0.0                                                    
      do 230 kk=1,nsize                                                 
      if (tot2(kk).eq.0) go to 230                                      
      fab(lount)=fab(lount)+tot2(kk)*tot(kk)                            
  230 continue                                                          
  240 continue                                                          
  250 continue                                                          
  300 continue                                                          
  301 continue                                                          
c     --------------------------------------------------                
c     invert ab matrix                                                  
c     --------------------------------------------------                
  400 det=0.0                                                           
      call matinv (fab,1,nsize,nsize,tot2,det)                          
c     --------------------------------------------------                
c     form vectors for a & b main effects                               
c     --------------------------------------------------                
      ll1=0                                                             
      if (k1.eq.1) go to 502                                            
      k=k1-1                                                            
      do 501 i=1,k                                                      
  501 ll1=ll1+ncl(i)                                                    
  502 ll2=0                                                             
      if(k2.eq.1) go to 504                                             
      k=k2-1                                                            
      do 503 i=1,k                                                      
  503 ll2=ll2+ncl(i)                                                    
  504 kkk=kk1*kk2                                                       
      sum1=0                                                            
      sum2=0                                                            
      do 511 i=1,kk1                                                    
      do 510 j=1,kk2                                                    
      k=kk2*(i-1)+j                                                     
      tot2(k)=iden(ll1+i)                                               
  510 sum1=sum1+tot2(k)                                                 
  511 continue                                                          
      do 521 i=1,kk1                                                    
      do 520 j=1,kk2                                                    
      k=kk2*(i-1)+j                                                     
      tot3(k)=iden(ll2+j)                                               
  520 sum2=sum2+tot3(k)                                                 
  521 continue                                                          
      amean1=sum1/dfloat(kkk)                                           
      amean2=sum2/dfloat(kkk)                                           
      do 530 i=1,kkk                                                    
      tot2(i)=tot2(i)-amean1                                            
  530 tot3(i)=tot3(i)-amean2                                            
c     -------------------------------------------------                 
c     calculation of ab sum of squares                                  
c     -------------------------------------------------                 
      do 550 irhm=1,nrhm                                                
      do 540 j=1,nsize                                                  
      k=natx+(irhm-1)*nsize+j                                           
  540 tot(j)=fab(k)                                                     
      call mtxmlt (nsize,tot,fab,tot,ssqx)                              
      eff1(irhm)=ssqx                                                   
  550 eff2(irhm)=ssqx/dfloat(nsize)                                     
c     --------------------------------------------------                
c     form x'zx matrix                                                  
c     --------------------------------------------------                
      j1=jj1                                                            
      if (j1.gt.3) j1=3                                                 
      j2=jj2                                                            
      if (j2.gt.3) j2=3                                                 
      kount=0                                                           
      nnsize=j1+j2+j1*j2                                                
      nnatx=nnsize*(nnsize+1)/2                                         
      do 571 level1=1,nnsize                                            
      call vector (level1,kkk,j1,j2,tot2,tot3,tot4)                     
      do 570 level2=level1,nnsize                                       
      call vector (level2,kkk,j1,j2,tot2,tot3,tot5)                     
      call mtxmlt (nsize,tot5,fab,tot4,ssqx)                            
      kount=kount+1                                                     
  570 xzx(kount)=ssqx                                                   
  571 continue                                                          
c     -------------------------------------------------                 
c     form x'zs matrix                                                  
c     -------------------------------------------------                 
      do 574 irhm=1,nrhm                                                
      do 572 j=1,nsize                                                  
      k=natx+(irhm-1)*nsize+j                                           
  572 tot(j)=fab(k)                                                     
      do 573 level1=1,nnsize                                            
      call vector (level1,kkk,j1,j2,tot2,tot3,tot4)                     
      call mtxmlt (nsize,tot4,fab,tot,ssqx)                             
      k=nnatx+(irhm-1)*nnsize+level1                                    
  573 xzx(k)=ssqx                                                       
      kl=nnatx+1                                                        
      km=nnatx+nnsize                                                   
  574 continue                                                          
c     -------------------------------------------------                 
c     begin stepdown procedure separately for each rhm                  
c     -------------------------------------------------                 
      do 850 irhm=1,nrhm                                                
      s = 0.0                                                           
      ksize=nnsize+1                                                    
      do 603 j=1,nnsize                                                 
      k=nnatx+(irhm-1)*nnsize+j                                         
  603 x(j)=xzx(k)                                                       
c     --------------------------------------------------                
c     form a vector consisting of the degrees of polynomials            
c     --------------------------------------------------                
      do 604 j=1,j1                                                     
  604 lab3(j)=j                                                         
      do 605 j=1,j2                                                     
  605 lab3(j+j1)=j                                                      
      do 606 i=1,j1                                                     
      do 606 j=1,j2                                                     
      k=j1+j2+(i-1)*j2+j                                                
  606 lab3(k)=i+j                                                       
      max=lab3(nnsize)                                                  
      do 610 i=1,6                                                      
  610 lab4(i)=0                                                         
      do 611 i=1,nnsize                                                 
      n=lab3(i)                                                         
  611 lab4(n)=lab4(n)+1                                                 
      do 613 i=1,j1                                                     
      lab2(i)=1                                                         
      go to (613,613,614,613,614,614,615),mty                           
  614 if (int1(i2f).eq.1) lab2(i)=2                                     
      go to 613                                                         
  615 if (int1(i2f).eq.1) lab2(i)=3                                     
      if (int1(i2f).eq.2) lab2(i)=2                                     
  613 continue                                                          
      do 616 i=1,j2                                                     
      lab2(i+j1)=1                                                      
      go to (616,616,617,616,617,617,618),mty                           
  617 if (int2(i2f).eq.1) lab2(i+j1)=2                                  
      go to 616                                                         
  618 if (int2(i2f).eq.1) lab2(i+j1)=3                                  
      if (int2(i2f).eq.2) lab2(i+j1)=2                                  
  616 continue                                                          
      do 619 i=1,j1                                                     
      do 619 j=1,j2                                                     
      k=j1+j2+(i-1)*j2+j                                                
      lab2(k)=1                                                         
      if (mty.ne.7) go to 619                                           
      if (int1(i2f).eq.1.and.int2(i2f).eq.2) lab2(k)=2                  
      if (int1(i2f).eq.2.and.int2(i2f).eq.1) lab2(k)=2                  
  619 continue                                                          
      index=0                                                           
      do 612 i=1,nnsize                                                 
  612 tot5(i)=0.0d-20                                                   
      k3=0                                                              
      do 750 m=1,max                                                    
      level=max-m+1                                                     
  700 continue                                                          
      call comprs (nnsize,lab3,xzx,fab,x,tot4)                          
      ksize=ksize-1                                                     
      lsize=ksize*(ksize+1)/2                                           
c     -------------------------------------------------                 
c     calculation of polynomial coefficients                            
c     -------------------------------------------------                 
      det=0.0                                                           
      call matinv (fab,1,ksize,ksize,tot,det)                           
      do 704 ij=1,ksize                                                 
      tot(ij)=0                                                         
      do 703 ji=1,ksize                                                 
      if (ij.lt.ji) go to 701                                           
      l=ksize*(ji-1)-ji*(ji-3)/2+ij-ji                                  
      go to 702                                                         
  701 l=ksize*(ij-1)-ij*(ij-3)/2+ji-ij                                  
  702 tot(ij)=tot(ij)+fab(l)*tot4(ji)                                   
  703 continue                                                          
  704 continue                                                          
c     --------------------------------------------------                
c     calculation of ssq                                                
c     --------------------------------------------------                
      amin=0.0                                                          
      ii=0                                                              
      k3=k3+1                                                           
      do 710 i=1,nnsize                                                 
      ii=ii+1                                                           
      n=lab3(i)                                                         
      if (n.eq.0) ii=ii-1                                               
      if (n.eq.0.or.n.ne.level) go to 710                               
      l=ksize*(ii-1)-ii*(ii-3)/2                                        
      sss(i)=tot(ii)*tot(ii)/fab(l)                                     
      men(i)=k3                                                         
      k=lab2(i)                                                         
      go to (705,706,707),k                                             
  705 err=r2i(irhm)                                                     
      df=edf                                                            
      go to 708                                                         
  706 err=xp(irhm)                                                      
      df=rr                                                             
      go to 708                                                         
  707 err=yp(irhm)                                                      
      df=wtt                                                            
  708 f=sss(i)/err                                                      
      prob=prof (1,df,f)                                                
      if (prob.lt.amin) go to 710                                       
      indx=i                                                            
      amin=prob                                                         
  710 continue                                                          
      if (amin.lt.prbmin) go to 720                                     
      go to 740                                                         
  720 if (index.ne.0) go to 740                                         
      index=ksize                                                       
      k=0                                                               
      s=0.0                                                             
      do 730 i=1,nnsize                                                 
      if (lab3(i).eq.0) go to 725                                       
      k=k+1                                                             
      l=ksize*(k-1)-k*(k-3)/2                                           
      tot5(i)=tot(k)                                                    
      s=s+tot(k)*tot4(k)                                                
      j=lab2(i)                                                         
      go to (711,712,713),j                                             
  711 err=r2i(irhm)                                                     
      go to 714                                                         
  712 err=xp(irhm)                                                      
      go to 714                                                         
  713 err=yp(irhm)                                                      
  714 sss(nnsize+i)=dsqrt(err*fab(l))                                   
      go to 730                                                         
  725 tot5(i)=0.0d-20                                                   
      sss(nnsize+i)=0.0d-20                                             
  730 continue                                                          
  740 lab3(indx)=0                                                      
      lab4(level)=lab4(level)-1                                         
      if (lab4(level).ne.0) go to 700                                   
  750 continue                                                          
      r2=s/eff1(irhm)                                                   
      r=dsqrt(r2)                                                       
      if (int1(i2f).gt.nom) go to 751                                   
      id1=lit(k1)                                                       
      go to 752                                                         
  751 k=int1(i2f)-nom                                                   
      id1=nlit(k)                                                       
  752 if (int2(i2f).gt.nom) go to 753                                   
      id2=lit(k2)                                                       
      go to 754                                                         
  753 k=int2(i2f)-nom                                                   
      id2=nlit(k)                                                       
  754 if (int1(i2f).lt.int2(i2f)) go to 755                             
      name=id1                                                          
      id1=id2                                                           
      id2=name                                                          
  755 write (6,1002) lity(irhm),id1,id2                                 
 1002 format(/,62x,a6/,56x,a6,5h  x  ,a6//24x,32hcoefficient standard er
     *ror order/4x,6hsource,15x,37hestimate   of coefficient  elim  d.f.
     *,3x,14hsum of squares,7x,12hmean squares,7x,1hf,7x,4hprob,2x,5herr
     *or/)                                                              
      f=eff2(irhm)/r2i(irhm)                                            
      prob=prof (nsize,edf,f)                                           
      k=1                                                               
      write (6,1003) nsize,eff1(irhm),eff2(irhm),f,prob,k               
 1003 format (6h total,49x,i7,f17.6,f18.6,f12.4,f9.4,4x,i1/)            
      ssqx=0                                                            
      do 810 i=1,nnsize                                                 
      ssqx=ssqx+sss(i)                                                  
      l=lab2(i)                                                         
      go to (756,757,758),l                                             
  756 err=r2i(irhm)                                                     
      df=edf                                                            
      nror=1                                                            
      go to 759                                                         
  757 err=xp(irhm)                                                      
      df=rr                                                             
      nror=2                                                            
      go to 759                                                         
  758 err=yp(irhm)                                                      
      df=wtt                                                            
      nror=3                                                            
  759 f=sss(i)/err                                                      
      prob=prof (1,df,f)                                                
      j=j1+j2                                                           
      if (i.eq.1) write (6,1000) id1                                    
 1000 format (1x,a6)                                                    
      if (i.gt.j1) go to 802                                            
      if (tot5(i).eq.0.0d-20) go to 801                                 
      write(6,1004) lab(i),tot5(i),sss(nnsize+i),men(i),sss(i),sss(i),f,
     *prob,nror                                                         
 1004 format (3x,a6,11x,2e15.6,3x,i2,4x,1h1,3x,f16.6,f18.6,f12.4,f9.4,4x
     *,i1)                                                              
      go to 810                                                         
  801 write(6,1005) lab(i),men(i),sss(i),sss(i),f,prob,nror             
 1005 format (3x,a6,19x,2h**,13x,2h**,8x,i2,4x,1h1,3x,f16.6,f18.6,f12.4,
     *f9.4,4x,i1)                                                       
      go to 810                                                         
  802 if (i.gt.j) go to 804                                             
      k=i-j1                                                            
      if (i.eq.j1+1) write (6,1000) id2                                 
      if (tot5(i).eq.0.0d-20) go to 803                                 
      write(6,1004) lab(k),tot5(i),sss(nnsize+i),men(i),sss(i),sss(i),f,
     *prob,nror                                                         
      go to 810                                                         
  803 write (6,1005) lab(k),men(i),sss(i),sss(i),f,prob,k1              
      go to 810                                                         
  804 k=i-j                                                             
      do 806 l=1,j1                                                     
      kl=l                                                              
      do 805 m=1,j2                                                     
      km=m                                                              
      n=m+j2*(l-1)                                                      
      if (k.eq.n) go to 807                                             
  805 continue                                                          
  806 continue                                                          
  807 if (i.eq.j1+j2+1) write (6,999) id1,id2                           
  999 format (1x,a6,5h  x  ,a6)                                         
      if (tot5(i).eq.0.0d-20) go to 808                                 
      write (6,1006) lab(kl),lab(km),tot5(i),sss(nnsize+i),men(i),sss(i)
     *,sss(i),f,prob,nror                                               
 1006 format (3x,a6,5h  x  ,a6,2e15.6,3x,i2,4x,1h1,3x,f16.6,f18.6,      
     *f12.4,f9.4,4x,i1)                                                 
      go to 810                                                         
  808 write (6,1007) lab(kl),lab(km),men(i),sss(i),sss(i),f,prob,nror   
 1007 format (3x,a6,5h  x  ,a6,8x,2h**,13x,2h**,8x,i2,4x,1h1,3x,        
     * f16.6,f18.6,f12.4,f9.4,4x,i1)                                    
  810 continue                                                          
      if (nnsize.eq.nsize) go to 811                                    
      n=nsize-nnsize                                                    
      eff1(irhm)=eff1(irhm)-ssqx                                        
      eff2(irhm)=eff1(irhm)/dfloat(n)                                   
      f=eff2(irhm)/err                                                  
      prob=prof (n,df,f)                                                
      write (6,1008) n,eff1(irhm),eff2(irhm),f,prob,nror                
 1008 format (/9h residual,47x,i4,3x,f16.6,f18.6,f12.4,f9.4,4x,i1)      
  811 go to (760,760,761,760,761,761,762),mty                           
  762 l=wtt                                                             
      k=3                                                               
      wk=wtt*yp(irhm)                                                   
      write (6,1009) k,l,wk,yp(irhm)                                    
 1009 format (/8h error (,i1,1h),45x,i7,f17.6,f18.6)                    
  761 l=rr                                                              
      k=2                                                               
      wk=rr*xp(irhm)                                                    
      write (6,1009) k,l,wk,xp(irhm)                                    
  760 l=edf                                                             
      k=1                                                               
      write (6,1009) k,l,r1i(irhm),r2i(irhm)                            
c     -------------------------------------------------                 
c     calculation of intercept                                          
c     -------------------------------------------------                 
      sum=0                                                             
      sum2=0                                                            
      do 841 i=1,j1                                                     
      if (tot5(i).eq.0.0d-20) go to 841                                 
      sum1=0                                                            
      do 840 k=1,kk1                                                    
      sum1=sum1+(iden(ll1+k)-amean1)**i                                 
  840 continue                                                          
      sum2=sum2+sum1*tot5(i)                                            
  841 continue                                                          
      sum=sum+sum2/dfloat(kk1)                                          
      sum2=0                                                            
      do 843 j=1,j2                                                     
      if (tot5(j1+j).eq.0.0d-20) go to 843                              
      sum1=0                                                            
      do 842 l=1,kk2                                                    
      sum1=sum1+(iden(ll2+l)-amean2)**j                                 
  842 continue                                                          
      sum2=sum2+sum1*tot5(j1+j)                                         
  843 continue                                                          
      sum=sum+sum2/dfloat(kk2)                                          
      sum2=0                                                            
      do 847 i=1,j1                                                     
      do 846 j=1,j2                                                     
      if (tot5(j1+j2+(i-1)*j2+j).eq.0.0d-20) go to 846                  
      sum1=0                                                            
      do 845 k=1,kk1                                                    
      do 844 l=1,kk2                                                    
      sum1=sum1+(iden(ll1+k)-amean1)**i*(iden(ll2+l)-amean2)**j         
  844 continue                                                          
  845 continue                                                          
      sum2=sum2+sum1*tot5(j1+j2+(i-1)*j2+j)                             
  846 continue                                                          
  847 continue                                                          
      sum=sum+sum2/dfloat(kk1*kk2)                                      
      j=matx+(irhm-1)*nlhm+1                                            
      sum=array(j)+ym(irhm)-sum                                         
      write (6,1010) sum,r2,r                                           
 1010 format (/15x,12hintercept = ,e15.6,5x,12hr squared = ,f5.3,5x,3hr 
     *=,f6.3)                                                           
      if (tot5(nnsize).eq.0.0d-20) write(6,1011)                        
 1011 format(/10x,50h**  term was eliminated during stepdown procedures)
c-------------------------------------------------                      
c     listing of prediction equation                                    
c-------------------------------------------------                      
      write(6,1012)                                                     
 1012 format(1h0,44x,'prediction equation',//25x,'coefficients',14x,'b',
     220x,'x')                                                          
      write(6,1013) sum                                                 
 1013 format(1h0,23x,'intercept',8x,f17.9,14x,1h1)                      
      do 901 i=1,nnsize                                                 
      j=j1+j2                                                           
      if(i.gt.j1) go to 902                                             
      if(tot5(i).eq.0.0d-20) go to 903                                  
      write(6,1014) id1,lab(i),tot5(i),id1,amean1,i                     
 1014 format(24x,a6,1x,a6,4x,f17.9,2x,1h(,a6,1x,1h-,1x,f10.4,')**',i1)  
      go to 901                                                         
  903 write(6,1015) id1,lab(i)                                          
 1015 format(24x,a6,1x,a6,13x,2h**,19x,2h**)                            
      go to 901                                                         
  902 if(i.gt.j) go to 904                                              
      k=i-j1                                                            
      if(tot5(i).eq.0.0d-20) go to 905                                  
      write(6,1014) id2,lab(k),tot5(i),id2,amean2,k                     
      go to 901                                                         
  905 write(6,1015) id2,lab(k)                                          
      go to 901                                                         
  904 k=i-j                                                             
      do 906 l=1,j1                                                     
      kl=l                                                              
      do 907 m=1,j2                                                     
      km=m                                                              
      n=m+j2*(l-1)                                                      
      if(k.eq.n) go to 908                                              
  907 continue                                                          
  906 continue                                                          
  908 if(tot5(i).eq.0.0d-20) go to 909                                  
      write(6,1016) lab(kl),lab(km),tot5(i),id1,amean1,kl,id2,amean2,km 
 1016 format(24x,a6,3h x ,a6,2x,f17.9,2x,1h(,a6,1x,1h-,1x,f10.4,        
     2')**',i1,1x,1hx,1x,1h(,a6,1x,1h-,1x,f10.4,')**',i1)               
      go to 901                                                         
  909 write(6,1017) lab(kl),lab(km)                                     
 1017 format(24x,a6,3h x ,a6,11x,2h**,19x,2h**)                         
  901 continue                                                          
c-------------------------------------------------                      
c     listing of estimated subclass means                               
c-------------------------------------------------                      
      write(6,1018) id1,id2,id1,id2                                     
 1018 format(1h0,15x,'estimated',1x,a6,3h x ,a6,1x,'subclass means',    
     2//25x,a6,1x,a6,5x,'estimate',/)                                   
      do 620 i=1,kk1                                                    
      xa=iden(ll1+i)                                                    
      do 621 j=1,kk2                                                    
      xb=iden(ll2+j)                                                    
      sum1=sum                                                          
      do 622 k=1,nnsize                                                 
      j3=j1+j2                                                          
      if(k.gt.j1) go to 623                                             
      if(tot5(k).eq.0.0d-20) go to 622                                  
      sum1=sum1 + tot5(k)*(xa-amean1)**k                                
      go to 622                                                         
  623 if(k.gt.j3) go to 624                                             
      kk=k-j1                                                           
      if(tot5(k).eq.0.0d-20) go to 622                                  
      sum1=sum1+tot5(k)*(xb-amean2)**kk                                 
      go to 622                                                         
  624 kk=k-j3                                                           
      do 625 l=1,j1                                                     
      kl=l                                                              
      do 626 m=1,j2                                                     
      km=m                                                              
      n=m+j2*(l-1)                                                      
      if(kk.eq.n) go to 627                                             
  626 continue                                                          
  625 continue                                                          
  627 if(tot5(k).eq.0.0d-20) go to 622                                  
      sum1=sum1+tot5(k)*((xa-amean1)**kl)*((xb-amean2)**km)             
  622 continue                                                          
      write(6,1019) iden(ll1+i),iden(ll2+j),sum1                        
 1019 format(25x,i4,3x,i4,3x,f12.4)                                     
  621 continue                                                          
  620 continue                                                          
  850 continue                                                          
  900 continue                                                          
  997 continue                                                          
c                                                                       
c    calculation of blup values                                         
c                                                                       
      if(nab.eq.3.or.nab.eq.4) go to 996                                
      go to 930                                                         
  996 if(iad.ne.1) go to 930                                            
      not=nlhm+nrhm                                                     
      rewind 12                                                         
      rewind 13                                                         
      do 911 i=1,nrhm                                                   
  911 tot(i)=0.0                                                        
      nc=nlhm*nrhm                                                      
      do 912 i=1,nc                                                     
      j=matx+i                                                          
  912 sss(i)=array(j)                                                   
      if(nab.eq.3) go to 913                                            
      n=0                                                               
      i=0                                                               
  914 i=i+1                                                             
      read(13,1035) imj,ncdg,gni,(x(j),j=1,not)                         
 1035 format(i9,i4,111e15.8)                                            
      idmj(i)=imj                                                       
      numm(i)=ncdg/2                                                    
      do 915 j=1,nrhm                                                   
      k=nrhm*(i-1)+j                                                    
      l=nlhm+j                                                          
      fab(k)=x(l)                                                       
      n3=nlhm*(j-1)                                                     
      do 916 m=1,nlhm                                                   
      l=n3+m                                                            
  916 fab(k)=fab(k)-x(m)*sss(l)                                         
      fab(k)=fab(k)/gni                                                 
  915 tot(j)=tot(j)+fab(k)                                              
      if(n.eq.0) go to 917                                              
      go to 918                                                         
  917 n=n+1                                                             
      read(12,1020) idf,idr,ni,rown,(tot2(j),j=1,not)                   
 1020 format(2i9,i4,110e15.8)                                           
      if(n.eq.1) go to 918                                              
      if(idf.lt.id11) go to 998
      if(idf.gt.id11) go to 914
  918 idm(n)=idr                                                        
      idi(n)=idf                                                        
      a=ni                                                              
      nran(n)=ni                                                        
      do 919 j=1,nrhm                                                   
      k=nrhm*(i-1)+j                                                    
      k1=nrhm*(n-1)+j                                                   
      l=nlhm+j                                                          
      array(k1)=tot2(l)                                                 
      n3=nlhm*(j-1)                                                     
      do 920 m=1,nlhm                                                   
      l=n3+m                                                            
  920 array(k1)=array(k1)-tot2(m)*sss(l)                                
  919 array(k1)=(array(k1)-fab(k)*a)*rown                               
      if(n.eq.min) go to 921                                            
      id11=idf                                                           
      go to 917                                                         
  921 if(i.ne.mjn) go to 998                                            
      write(6,1021)                                                     
 1021 format(1h1,10x,'listing of estimates of mu')                      
      write(6,1022)                                                     
 1022 format(1h )                                                       
      write(6,1023) (lity(j),j=1,nrhm)                                  
 1023 format(1h ,16x,10(5x,a6))                                         
      do 922 i=1,nrhm                                                   
  922 tot(i)=tot(i)/float(mjn)+ym(i)                                    
      write(6,1022)                                                     
      write(6,1024) (tot(i),i=1,nrhm)                                   
 1024 format(1h ,16x,10f11.4)                                           
      write(6,1025)                                                     
 1025 format(1h0,10x,'listing of major class constants')                
      write(6,1027) (lity(j),j=1,nrhm)                                  
 1027 format(1h0,4x,'major class  no.',10(4x,a6))                       
      write(6,1022)                                                     
      do 923 i=1,mjn                                                    
      k1=nrhm*(i-1)+1                                                   
      do 924 j=1,nrhm                                                   
      k=k1+j-1                                                          
  924 fab(k)=fab(k)-tot(j)+ym(j)                                        
      k=k1+nrhm-1                                                       
      write(6,1026) idmj(i),numm(i),(fab(j),j=k1,k)                     
 1026 format(1h ,4x,i9,3x,i4,10(1x,f9.4))                               
  923 continue                                                          
      write(6,1028)                                                     
 1028 format(//10x,'listing of blup of random effects')                 
      write(6,1029) (lity(j),j=1,nrhm)                                  
 1029 format(/3x,'major class  random class no.',10(4x,a6))             
      write(6,1022)                                                     
      do 925 i=1,min                                                    
      k1=nrhm*(i-1)+1                                                   
      do 926 j=1,nrhm                                                   
      k=k1+j-1                                                          
  926 tot3(j)=array(k)                                                  
      write(6,1030) idi(i),idm(i),nran(i),(tot3(j),j=1,nrhm)            
 1030 format(1h ,1x,i9,5x,i9,2x,i4,10(1x,f9.4))                         
  925 continue                                                          
      go to 930                                                         
  913 write(6,1028)                                                     
      write(6,1031) (lity(i),i=1,nrhm)                                  
 1031 format(//5x,'random class   no. ',10(4x,a6))                      
  929 read(12,1032,end=930) idr,ni,rown,(tot2(j),j=1,not)               
 1032 format(i9,i4,110e15.8)                                            
      do 927 i=1,nrhm                                                   
      l=nlhm+i                                                          
      tot(i)=tot2(l)                                                    
      n3=nlhm*(i-1)                                                     
      do 928 j=1,nlhm                                                   
      l=n3+j                                                            
  928 tot(i)=tot(i)-tot2(j)*sss(l)                                      
      tot(i)=tot(i)*rown                                                
  927 continue                                                          
      write(6,1033) idr,ni,(tot(i),i=1,nrhm)                            
 1033 format(1h ,4x,i9,4x,i4,1x,10(1x,f9.4))                            
      go to 929                                                         
  998 write(6,1034)                                                     
 1034 format(1h0,10x,'error in sequence of major or random classes')    
  930 continue                                                          
      return                                                            
      end                                                               
