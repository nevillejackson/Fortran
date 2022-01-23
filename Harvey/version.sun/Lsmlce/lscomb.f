      subroutine lscomb(ntitl)                                          
c     -------------------------------------------                       
c     subroutine which calls matinv, computes and lists constants.      
c     also calls polyno and svcvc if needed                             
c     ----------------------------------------------------              
      include 'decl1'
      include 'com1'
      include 'com2'
      include 'com3'
      include 'com4'
      include 'com5'
      include 'com9'
      character*80 fnam
      m4999=m5000-1
c     ----------------------------------------------------              
c     inversion and listing of inverse matrix                           
c     ----------------------------------------------------              
      nbrc=1                                                            
      nerc=nlhm                                                         
      k=1                                                               
      if (nlhm.eq.0) go to 9                                            
      do 18 i=1,nlhm                                                    
      x(i)=array(k)                                                     
   18 k=k+nlhm-i+1                                                      
      det=1.0                                                           
      call matinv (array,nbrc,nerc,nlhm,x,det)                          
      k=1                                                               
      do 224 i=1,nlhm                                                   
      x(i)=array(k)                                                     
  224 k=k+nlhm-i+1                                                      
      do 225 i=1,nlhm                                                   
      if (x(i).le.0.0d+00) go to 226                                    
      if (x(i).gt.1.0d+05) go to 226                                    
  225 continue                                                          
      go to 24                                                          
  226 write (6,1022)                                                    
      write (6,1004)                                                    
      write (6,1025) (x(k),k=1,nlhm)                                    
      write (6,1024) det,det                                            
      write (6,2226)                                                    
 2226 format (//5x,56hdependency found in the inverse matrix.  job termi
     *nated.)                                                           
      mull=3                                                            
      if(nlc.eq.0) mull=2                                               
      return                                                            
   24 if (liop.gt.10) go to 19                                          
      write (6,1001) ijob                                               
      write (6,1004)                                                    
      if (liop.ne.10) go to 26                                          
      write (6,1025) (array(i), i=1,matx)                               
      write (6,1022)                                                    
 1022 format (1h0,29x,36hlisting of diagonal inverse elements)          
      write (6,1004)                                                    
      write (6,1025) (x(k),k=1,nlhm)                                    
      go to 19                                                          
 1025 format (7(2x,e15.8))                                              
 1001 format (1h0,29x,43hlisting of inverse elements for problem no.,i3)
   26 write (6,1002)                                                    
 1002 format (1h0,8hrow  col,15x,21hindependent variables,34x,15hinverse
     1 element)                                                         
      write (6,1003)                                                    
 1003 format (1h ,9hcode code,9x,3hrow,22x,6hcolumn,16x,41hfixed point f
     1ormat  floating point format)                                     
   28 do 32 i=nbrc,nerc                                                 
      write (6,1004)                                                    
 1004 format (1h )                                                      
   30 do 32 j=i,nerc                                                    
      k1=nlhm*(i-1)-i*(i-3)/2+j-i                                       
      write (6,1005) i,j,lab1(i),lab2(i),lab3(i),lab4(i),lab1(j),lab2(j)
     1,lab3(j),lab4(j),array(k1),array(k1)                              
 1005 format (1h ,2(i3,2x),2(a6,1x),2a6,2(a6,1x),2a6,2x,f17.8,8x,e15.8) 
   32 continue                                                          
c     ----------------------------------------------------              
c     computation and listing of constants                              
c     ----------------------------------------------------              
   19 write (6,1024) det,det                                            
 1024 format(1h0,44hthe determinant of the correlation matrix is,f30.16,
     1d28.16)                                                           
    9 edf=df-dble(nlhm)                                                
      if (edf.gt.0) go to 57                                            
      write (6,1060)                                                    
 1060 format (85h0the degrees of freedom for the remainder(rdf) are zero
     1. however, the program has set/84h this value to 1 so that the pro
     2blem can be finished without encountering interrupts/78h caused by
     3 division by zero. all results which are a function of rdf should 
     4be/13h disregarded.)                                              
      edf=1.0                                                           
   57 if (kb.eq.0.and.kd.eq.0) go to 50                                 
      call mixedf(matx,kb,nlhm,array,fab,tot4,sof,nx,kd,liop)           
   50 if (nlhm.eq.0.or.nrhm.eq.0) go to 8                               
      if (liop.ge.20) go to 55                                          
      if (nab.eq.1.or.nab.eq.2.or.nab.eq.4) go to 56                    
      go to 55                                                          
   56 write (6,1006) ijob                                               
 1006 format (1h0,18x,45hlisting of constant estimates for problem no.,i
     13)                                                                
      write (6,1007)                                                    
 1007 format (1h0,10h rhm   row,43x,18hconstant estimates/ 1h ,33h name 
     1 code  independent variable,9x,41hfixed point format  floating poi
     2nt format)                                                        
   55 do 54 k=1,nrhm                                                    
      write (6,1004)                                                    
   34 treds=0.0                                                         
      do 52 i=nbrc,nerc                                                 
      temp=0.0                                                          
      do 44 j=nbrc,nerc                                                 
      if (i-j.lt.0) go to 38                                            
   36 k1=nlhm*(j-1)-j*(j-3)/2+i-j                                       
      go to 40                                                          
   38 k1=nlhm*(i-1)-i*(i-3)/2+j-i                                       
   40 k4=(k-1)*nlhm+j                                                   
   44 temp=temp+rhm(k4)*array(k1)                                       
      k4=nlhm*(k-1)+i                                                   
   48 treds=treds+rhm(k4)*temp                                          
      tred(k)=treds                                                     
      k1=matx+(k-1)*nlhm+i                                              
      array(k1)=temp                                                    
      if (liop.ge.20.or.nab.eq.0.or.nab.eq.3) go to 52                  
      write (6,1008) lity(k),i,lab1(i),lab2(i),lab3(i),lab4(i),temp,temp
 1008 format (1h ,a6,i4,2x,4(a6,1x),f17.8,8x,e15.8)                     
   52 continue                                                          
   54 continue                                                          
      if (mty.eq.1.or.nrn.gt.1) go to 10                                
      do 20 i=1,not                                                     
      tot5(i)=tot(i)                                                    
   20 continue                                                          
      go to (11,12,13,12,13,13,10),mty                                  
c     ------------------------------------------------------            
c     store inverse elements for mu                                     
c     ------------------------------------------------------            
   12 j=m5000-nlhm                                                       
      k1=1                                                              
      do 21 i=j,m4999                                                    
      fab(i)=array(k1)                                                  
   21 k1=k1+1                                                           
      go to 11                                                          
c     ------------------------------------------------------            
c     store iden, nos and inverse elements for mu and a constants       
c     ------------------------------------------------------            
   13 k=ncl(1)-1                                                        
      k2=ncl(1)                                                         
      k1=k2                                                             
      j=m300+1-k2                                                          
      k4=m100                                                            
      do 84 i=1,k2                                                      
      iden(k4)=iden(k1)                                                 
      nos(j)=nos(i)                                                     
      if(ibet.eq.1) nos(j)=ww(i)                                        
      k4=k4-1                                                           
      k1=k1-1                                                           
   84 j=j+1                                                             
      if (mty.eq.6) go to 12                                            
      j=m5000-nlhm*k2+(k2*k)/2                                           
      k1=1                                                              
      do 22 i=j,m4999                                                    
      fab(i)=array(k1)                                                  
   22 k1=k1+1                                                           
      go to 11                                                          
c     ------------------------------------------------------            
c     store inverse elements for mu and a for model 7                   
c     ------------------------------------------------------            
   17 k=ncl(1)-1
      j=ncl(2)-1                                                        
      k2=ncl(1)                                                         
      k3=k2+ncl(2)                                                      
      k1=k3+k2*ncl(2)                                                   
      i2=m300+1-k1                                                         
      k4=m100                                                            
      k1=k3                                                             
      do 87 i=1,k3                                                      
      iden(k4)=iden(k1)                                                 
      nos(i2)=nos(i)                                                    
      if(ibet.eq.1) nos(i2)=ww(i)                                       
      k4=k4-1                                                           
      k1=k1-1                                                           
   87 i2=i2+1                                                           
      k1=0                                                              
      do 88 i=1,nme                                                     
   88 k1=k1+ncl(i)                                                      
      if (nne.eq.0) go to 91                                            
      do 90 i=1,nne                                                     
   90 k1=k1+ncln(i)                                                     
   91 k3=k1+k2*ncl(2)                                                   
      k1=k1+1                                                           
      do 89 i=k1,k3                                                     
      nos(i2)=nos(i)                                                    
   89 i2=i2+1                                                           
      k1=nlhm-k2-j                                                      
      j=m5000-matx+(k1*(k1+1))/2-(nlhm-iei+1)*(k*j)+((k*j)*(k*j-1))/2 
      nden(m100)=j                                                       
      k1=j+nlhm*k2-(k2*(k2-1))/2-1                                      
      nden(m100-1)=k1                                                       
      j=((nlhm-k2)*(nlhm-k2+1))/2+1                                     
      k1=j+nlhm*k2-(k2*(k2-1))/2-1                                      
      nden(m100-2)=k1                                                       
      k4=1                                                              
      do 27 i=j,k1                                                      
      fab(i)=array(k4)                                                  
   27 k4=k4+1                                                           
      go to 11                                                          
   10 if (mty.eq.7.and.nrn.eq.3) go to 14                               
      if(mty.eq.7.and.nrn.eq.2) go to 17                                
      go to 80                                                          
c     ------------------------------------------------------            
c     store inverse elements for c and ac constants                     
c     ------------------------------------------------------            
   14 j=nden(m100)                                                       
      k1=nden(m100-1)                                                       
      k4=nden(m100-2)                                                       
      do 70 i=j,k1                                                      
      k3=j+k1-i                                                         
      fab(k3)=fab(k4)                                                   
   70 k4=k4-1                                                           
      j=ncl(2)-1                                                        
      k3=nlhm*j-(j*(j-1))/2                                             
      k1=nlhm-j-ndfa*j+nmc(1)                                           
      j=m5000-k3-(nlhm-iei+1)*j*ndfa+((j*ndfa)*(j*ndfa-1))/2  
      k3=j+k3-1                                                         
      k4=1                                                              
      do 15 i=j,k3                                                      
      fab(i)=array(k4)                                                  
   15 k4=k4+1                                                           
      k2=iei-1                                                          
      j=k2*nlhm-(k2*(k2-1))/2+1                                         
      k3=k3+1                                                           
      do 75 i=k3,m4999                                                   
      fab(i)=array(j)                                                   
   75 j=j+1                                                             
   80 if (mty.eq.6.and.nrn.eq.3) go to 81                               
      if (mty.eq.5.and.nrn.eq.2) go to 94                               
      go to 11                                                          
   81 k=ncl(1)-1                                                        
      j=m5000-(nlhm+1)*ncl(1)+(ncl(1)*k)/2                               
      k4=m5000-nlhm-1                                                    
      do 78 i=k4,m4999                                                   
      fab(j)=fab(i)                                                     
   78 j=j+1                                                             
      k1=1                                                              
      do 82 i=j,m4999                                                    
      fab(i)=array(k1)                                                  
   82 k1=k1+1                                                           
   94 k2=ncl(1)                                                         
      j=k2                                                              
      k4=m100                                                            
      do 93 i=1,k2                                                      
      iden(k4)=iden(j)                                                  
      j=j-1                                                             
   93 k4=k4-1                                                           
   11 if (liop.ge.20) go to 53                                          
      if (mty.gt.1.and.nrun.eq.nrn) go to 53                            
      mn2=0                                                             
      call lsmns                                                        
c     ----------------------------------------------------              
c     computation and listing, if desired, of inverse elements          
c             of segments                                               
c     ----------------------------------------------------              
   53 if (mty.eq.1.or.nrn.ne.nrun) go to 85                             
      do 86 i=1,matx                                                    
   86 fab(i)=array(i)                                                   
   85 nerc=0                                                            
      if (liop.lt.3.or.liop.gt.9)  go to 221                            
      write (6,1000) ijob                                               
 1000 format (1h0,23x,55hlisting of inverse elements of segments for pro
     1blem no.,i3)                                                      
      write (6,1002)                                                    
      write (6,1003)                                                    
  221 do 58 i2=1,ns                                                     
      nbrc=nerc+1                                                       
      nerc=im(i2)                                                       
      det=0.0                                                           
      call matinv (array,nbrc,nerc,nlhm,x,det)                          
      if (liop.lt.3.or.liop.gt.9)  go to 58                             
      do 220 i=nbrc,nerc                                                
      write (6,1004)                                                    
      do 220 j=i,nerc                                                   
      k1=nlhm*(i-1)-i*(i-3)/2+j-i                                       
      write (6,1005) i,j,lab1(i),lab2(i),lab3(i),lab4(i),lab1(j),lab2(j)
     1,lab3(j),lab4(j),array(k1),array(k1)                              
  220 continue                                                          
   58 continue                                                          
c     ----------------------------------------------------              
c     computation and listing of residual matrix for rhm                
c     ----------------------------------------------------              
    8 if (ncpr.eq.0) go to 77                                           
c-----
c----- save option 4 -- residual covariances -- unix format
c-----
      if (nsave.eq.4) then
        call argopf(14,fnam,4,'new','formatted')
      endif
c-----
      if(nrhm.eq.1) go to 200                                           
      write (6,1009)                                                    
 1009 format (1h0,22x,40hresidual matrices for right hand members// 1h ,
     127h job row col   rhm      rhm,11x,14herror ss or cp,7x,15herror m
     2s or cov,5x,11hcorrelation)                                       
  200 do 74 k=1,nrhm                                                    
      write (6,1004)                                                    
      k3=nrhm*(k-1)-k*(k-3)/2                                           
      k1=(k-1)*nlhm                                                     
      do 74 l=k,nrhm                                                    
      k2=(l-1)*nlhm                                                     
      tr=0.0                                                            
      if (nlhm.eq.0) go to 7                                            
      do 64 i=1,nlhm                                                    
      k4=k1+i                                                           
      k5=matx+k2+i                                                      
   64 tr=tr+array(k5)*rhm(k4)                                           
    7 j=k3+l-k                                                          
      ess=sscpr(j)-tr                                                   
c     ------------------------------------------------------            
c     retains ss and df for residual for indirect analysis when run     
c     no. is less than no. runs                                         
c     ------------------------------------------------------            
      if (kb.eq.0.and.kd.eq.0) go to 66                                 
      if (kd.eq.0) go to 65                                             
      fy(j)=ess                                                         
      edff=edf                                                          
      go to 66                                                          
   65 sab(j)=ess                                                        
      edfs=edf                                                          
   66 ems=ess/edf                                                       
      sscpr(j)=ems                                                      
      if (k.eq.l) go to 72                                              
      ak=0.0                                                            
      if (nlhm.eq.0) go to 6                                            
      do 68 i=1,nlhm                                                    
      k4=k2+i                                                           
      k5=matx+k4                                                        
   68 ak=ak+array(k5)*rhm(k4)                                           
    6 jj=nrhm*(l-1)-l*(l-3)/2                                           
      sm=(sscpr(jj)-ak)/edf                                             
      f=ems/dsqrt(sscpr(k3)*sm)                                         
      go to 1074                                                          
   72 f=1.                                                              
      if(nrhm.eq.1) go to 77                                            
 1074 write (6,1010) ijob,k,l,lity(k),lity(l),ess,ems,f                 
 1010 format (1h ,i4,i3,i4,2x,2(2x,a6),f22.6,f23.8,f14.4)               
c-----
c----- save option 4
c-----
      if(nsave.eq.4) then
        write(14,1075) k,l,ems
 1075   format(2i5,d22.15)
      endif
c-----
   74 continue
c     ----------------------------------------------------              
c     calls polyno subroutine if polynomials are to be fitted           
c     ----------------------------------------------------              
   77 if (nsme.eq.0) go to 76                                           
      call polyno                                                       
c     ----------------------------------------------------              
c     computes and lists ss, ms and f for anova for each rhm            
c     ----------------------------------------------------              
   76 if (nrhm.eq.0) go to 5                                            
      call lsanov(ntitl)                                                
c     ----------------------------------------------------              
c     computation and listing of k, ss, cp, ms, mcp and variance and    
c     covariance components for one or more sets of main or nested      
c     effects, if iran is not equal to zero                             
c     ----------------------------------------------------              
    5 mn2=nrhm*(nrhm+1)/2                                               
      if (npr.eq.0) go to 4                                             
      if (nrn.eq.nrun) go to 4                                          
      do 3 i=1,npr                                                      
    3 x(i)=xm(i)                                                        
    4 if (iran.eq.0) go to 25                                           
      call tpage(ntitl)                                                 
      write (6,1017)                                                    
 1017 format (1h0,15x,'variance and covariance component estimates from 
     1direct analysis')                                                 
      kk=0                                                              
      lk=ns+1                                                           
      do 150 m=1,iran                                                   
      if (ns2(m).eq.0.or.ns2(m).gt.2) go to 147                         
      kk=kk+ns2(m)                                                      
      sdf=0.                                                            
      ak=0.                                                             
      do 114 j=1,ka                                                     
  114 sss(j)=0.                                                         
      if (ns2(m).eq.1) go to 115                                        
      i3=0                                                              
  116 i3=i3+1                                                           
      if (im(i3).eq.ms(kk)) go to 120                                   
      if (i3.eq.lk) go to 147                                           
      go to 116                                                         
  120 l=kk-1                                                            
      j=0                                                               
  122 j=j+1                                                             
      if (ms(l).le.im(j)) go to 123                                     
      if (j.eq.lk) go to 147                                            
      go to 122                                                         
  123 if (j.eq.1) go to 139                                             
      if (im(j-1).ne.ms(l)-1) go to 147                                 
  139 do 126 i2=j,i3                                                    
      if (i2.eq.1) go to 124                                            
      nbrc=im(i2-1)+1                                                   
      go to 125                                                         
  124 nbrc=1                                                            
      if (ms(l).ne.1) go to 147                                         
  125 nerc=im(i2)                                                       
      det=0.                                                            
      call matinv (array,nbrc,nerc,nlhm,x,det)                          
  126 continue                                                          
      nbrc=ms(l)                                                        
      nerc=ms(kk)                                                       
      det=0.                                                            
      call matinv (array,nbrc,nerc,nlhm,x,det)                          
  135 do 127 i2=j,i3                                                    
      if (i2.eq.1) go to 128                                            
      nbrc1=im(i2-1)+1                                                  
      mrank=im(i2)-im(i2-1)                                             
      go to 129                                                         
  128 mrank=im(i2)                                                      
      nbrc1=1                                                           
  129 nerc1=im(i2)                                                      
      rank=mrank                                                        
      sod=0.                                                            
      sd=0.                                                             
      do 130 i=nbrc1,nerc1                                              
      do 130 k=i,nerc1                                                  
      if (i-k.lt.0) go to 131                                           
      l=nlhm*(i-1)-i*(i-3)/2                                            
      sd=sd+array(l)                                                    
      go to 130                                                         
  131 l=nlhm*(i-1)-i*(i-3)/2+k-i                                        
      sod=sod+array(l)*2.                                               
  130 continue                                                          
      ak=ak+((sd-(sod/rank))/(rank+1.))*rank                            
      sdf=sdf+rank                                                      
  127 continue                                                          
      go to 132                                                         
  115 i3=0                                                              
  133 i3=i3+1                                                           
      if (im(i3).eq.ms(kk)) go to 134                                   
      if (i3.eq.lk) go to 147                                           
      go to 133                                                         
  134 j=i3                                                              
      go to 135                                                         
  132 if (ns2(m).eq.2) go to 136                                        
      nbrc=nbrc1                                                        
      nerc=nerc1                                                        
  136 do 142 k=1,nrhm                                                   
      do 142 i2=k,nrhm                                                  
      sum=0.0                                                           
      do 140 i=nbrc,nerc                                                
      n3=matx+(k-1)*nlhm+i                                              
      do 140 j=nbrc,nerc                                                
      if (i-j.le.0) go to 137                                           
      l=nlhm*(j-1)-j*(j-3)/2+i-j                                        
      go to 138                                                         
  137 l=nlhm*(i-1)-i*(i-3)/2+j-i                                        
  138 n2=matx+(i2-1)*nlhm+j                                             
  140 sum=sum+array(n3)*array(l)*array(n2)                              
      l2=nrhm*(k-1)-k*(k-3)/2+i2-k                                      
  142 sss(l2)=sss(l2)+sum                                               
      wk=ak/sdf                                                         
      k=m99+2-m                                                           
      write (6,1004)                                                    
      write (6,1018) lab4(k),wk,sdf                                     
 1018 format (1h0,10x,'k for random effects component (',a6,')=',f8.4,3x
     1,'degrees of freedom =',f5.0)                                     
      write (6,1019)                                                    
 1019 format (1h0/25x,51hss, cp, ms, mcp, variance and covariance compon
     1ents//1h ,25hjob row col   rhm     rhm,20x,8hss or cp,19x,9hms or 
     2cov,18x,10hcomponents)                                            
      do 146 i=1,nrhm                                                   
      write (6,1004)                                                    
      do 146 j=i,nrhm                                                   
      k=nrhm*(i-1)-i*(i-3)/2+j-i                                        
      sms=sss(k)/sdf                                                    
      sod=(sms-sscpr(k))/wk                                             
      write (6,1020) ijob,i,j,lity(i),lity(j),sss(k),sms,sod            
 1020 format (1h ,i3,2i4,2(2x,a6),3f27.8)                               
      if (i.eq.j.and.sod.lt.0.0) sod=0.0                                
  146 sss(k)=sod                                                        
c     ----------------------------------------------------              
c     calls svcvc subroutine to compute heritabilities, genetic         
c     correlations, etc., if i309 is not equal to zero                  
c     ----------------------------------------------------              
      if (i309(m).eq.0) go to 150                                       
      l=m                                                               
      call svcvc                                                        
      go to 150                                                         
  147 k=m99+2-m                                                           
      write (6,1023) m,lab4(k)                                          
 1023 format (1h0,'error in iran parameter card no.',i5,4x,a6)          
  150 continue                                                          
c        ---------------------------------------------------            
c         store constants for a                                         
c         --------------------------------------------------            
   25 if (nrn.gt.1) go to 31                                            
      go to (151,151,29,151,29,29,29),mty                               
   29 k4=m4999                                                           
      k2=ncl(1)                                                         
      do 23 i=1,nrhm                                                    
      k=matx+(nrhm-i+1)*nlhm-(nlhm-k2)                                  
      if (mty.eq.7) k=matx+nlhm*(nrhm-i)+k2                             
      do 23 j=2,k2                                                      
      k1=k-j+2                                                          
      array(k4)=array(k1)                                               
   23 k4=k4-1                                                           
      go to 151                                                         
   31 if (mty.eq.7.and.nrn.eq.3) go to 33                               
      go to 35                                                          
c        ---------------------------------------------------            
c        store c and ac constants                                       
c        ---------------------------------------------------            
   33 k3=ncl(2)-1                                                       
      k2=iei-1                                                          
      k1=k3+ndfa*k3-nmc(1)                                              
      k3=ndfa*k3-nmc(1)                                                 
      k4=m4999-ndfa*nrhm                                                 
      do 16 i=1,nrhm                                                    
      i2=matx+nlhm*(nrhm-i)+k2+k3                                       
      do 16 j=1,k1                                                      
      k=i2-j+1                                                          
      if (j.gt.k3) k=k-k2+ncl(2)-1                                      
      array(k4)=array(k)                                                
   16 k4=k4-1                                                           
      go to 151                                                         
   37 k=ncl(1)-1                                                        
      k4=m4999                                                           
      do 83 i=1,nrhm                                                    
      k3=matx+(nrhm-i+1)*nlhm-(nlhm-k)                                  
      do 83 j=1,k                                                       
      k1=k3-j+1                                                         
      array(k4)=array(k1)                                               
   83 k4=k4-1                                                           
      go to 151                                                         
   35 if (mty.eq.6.and.nrn.eq.3) go to 37                               
  151 return                                                            
      end                                                               
