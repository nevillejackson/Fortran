      subroutine rcbm(ntitl,nnn)                                        
c     -------------------------------------------                       
c     subroutine which reads data cards, calls codex, computes ls or ml 
c     matrices, calls sclnos and lists certain matrices, means, sd and  
c     correlations                                                      
c     ----------------------------------------------------              
      include 'decl1'
      include 'com1'
      include 'com2'
      include 'com3'
      include 'com4'
      include 'com5'
      include 'com6'
      include 'com7'
      include 'com8'
      include 'com9'
      character*6 rgrsn
      data rgrsn/6hrgrsn /                               
      if (in.ne.5) rewind in                                            
      if (npr.eq.0.or.nrn.gt.1) go to 10                                
      k=0                                                               
      k1=0                                                              
      do 12 i=1,npr                                                     
      if (lad(i).ne.3) go to 12                                         
      if (in.ne.5) go to 26                                             
      k1=1                                                              
      go to 12                                                          
   26 k=1                                                               
      xm(i)=0.0                                                         
   12 continue                                                          
      if (k1.eq.0) go to 13                                             
      write (6,1000)                                                    
 1000 format (1h0,'means for x variables can not be computed before anal
     *ysis.  adjustment is made to xm.'/)                               
      go to 10                                                          
   13 if (k.eq.0) go to 10                                              
      ncds=0                                                            
      swt1=0.0                                                          
      call fcard(in,k,l,ic,idum,iflag)
      if(iflag)902,25,902
   25 k=1                                                               
      l=lrec                                                              
      idum=1
      call rcard (in,k,l,ic,idum,iflag)                                   
      if(iflag)14,600,902
  600 continue
      ncds=ncds+1                                                       
      if (ncd.lt.2) go to 16                                            
      do 19 i=2,ncd                                                     
      l=i*lrec                                                            
      k=l-lrec+1                                                            
      call rcard (in,k,l,ic,i,iflag)                                      
      if(iflag) 22,22,902
   22 k1=icn1+k-1                                                       
      k2=k1-lrec                                                          
      if (ic(k1:k1).le.ic(k2:k2)) go to 901                                   
   19 continue                                                          
   16 if (ibet.eq.0) go to 23                                           
      l7=lbeg                                                           
      l=lthn+l7-1                                                       
      j=ncds                                                            
      call field (icod,l,ic,l7,j)                                       
      wtt=icod                                                          
      wtt=wtt/(10.**nndc)                                               
      swt1=swt1+wtt                                                     
   23 do 15 i=1,npr                                                     
      if (lad(i).ne.3) go to 15                                         
      if (negx(i)-1) 27,28,29                                           
   28 call negf(tx,jbeg,lgtx,ic,i)                                      
      go to 27                                                          
   29 call negz(tx,jbeg,lgtx,ic,i)                                      
   27 l7=jbeg(i)                                                        
      l=lgtx(i)+l7-1                                                    
      j=ncds                                                            
      call field (icod,l,ic,l7,j)                                       
      xr=icod                                                           
      if (negx(i).eq.0) go to 31                                        
      xr=xr*tx                                                          
   31 xr=xr/(10.**ndecx(i))                                             
      if (ibet.eq.1) xr=wtt*xr                                          
      xm(i)=xm(i)+xr                                                    
   15 continue                                                          
      go to 25                                                          
   14 do 24 i=1,npr                                                     
      if (lad(i).ne.3) go to 24                                         
      if (ibet.eq.0) swt1=ncds                                          
      xm(i)=xm(i)/swt1                                                  
      if (mty.eq.1) write (6,1003) i,xm(i)                              
 1003 format (1h0,10x,'xm(',i2,')=',f15.5)                              
   24 continue                                                          
      rewind in                                                         
   10 if (liop.eq.1.or.liop.eq.3) go to 1                               
      go to 2                                                           
    1 if(lparm.eq.1) call tpage(ntitl)                                  
      write (6,1001) ijob                                               
 1001 format (1h0,20x,35hlisting of x matrix for problem no.,i3)        
    2 matx=nlhm*(nlhm+1)/2                                              
      do 4 i=1,matx                                                     
    4 array(i)=0.0                                                      
      k1=nlhm*nrhm                                                      
      do 5 i=1,k1                                                       
    5 rhm(i)=0.0                                                        
      min=0                                                             
      if (ncpr.eq.0) go to 6                                            
      ka=nrhm*(nrhm+1)/2                                                
      if (nab.eq.2) min=1                                               
      go to 7                                                           
    6 ka=nrhm                                                           
    7 if (nab.eq.5.or.nab.eq.6) go to 3                                 
      go to 11                                                          
    3 call mixed (nab,matx,ka,kb,kc,not,tot,nmig,nlhm,ncpr,nrhm,tot3,arr
     1ay,ncdg,fab,tot4,sof,nx,kd,ibet,swt2,swt3,wtt,sofn)               
      if (mty.gt.3.and.nrn.eq.1) kd=2                                   
   11 do 8 i=1,ka                                                       
    8 sscpr(i)=0.0                                                      
      not=nlhm+nrhm                                                     
      do 9 i=1,not                                                      
      tot(i)=0.0                                                        
      tot2(i)=0.0                                                       
    9 tot3(i)=0.0                                                       
      k1=0                                                              
      ncds=0                                                            
      ssum=0.                                                           
      swt1=0.                                                           
      swt2=0.                                                           
      swt4=0.                                                           
      snsm=0.                                                           
      ll=1                                                              
      nsm=0                                                             
      nsum=0                                                            
      gni=0.                                                            
      mjn=0                                                             
      ncdg=0                                                            
      nmig=1                                                            
      call fcard(in,k,l,ic,idum,iflag)
      if(iflag) 902,630,902
  630 continue
      if (mty.gt.5.and.nrn.eq.nrun) go to 227                           
      go to 228                                                         
  227 do 229 i=1,m99                                                     
  229 tot4(i)=r2i(i)                                                    
  228 if (kb.eq.1.and.mty.gt.5) go to 225                               
      go to 20                                                          
  225 iend=m99+1
      do 226 i=1,iend                                                    
      r1i(i)=0.0                                                        
  226 r2i(i)=0.0                                                        
   20 k=1                                                               
      nab2=0                                                            
      if (nab.eq.2.and.(mty.eq.3.or.mty.eq.5.or.mty.eq.7)) nab2=1       
      l=lrec                                                              
      call rcard(in,k,l,ic,idum,iflag)                                    
      if(iflag) 17,601,902
  601 continue
      ntra=0                                                            
      do 21 i=1,lrec                                                      
   21 if (ic(i:i).ne.blank) ntra=1                                         
      go to 18                                                          
   17 ntra=0                                                            
   18 if (ntra.eq.0) go to 156                                          
      ncds=ncds+1                                                       
      if (ncd.lt.2) go to 32                                            
      do 30 j=2,ncd                                                     
      l=j*lrec                                                            
      k=l-lrec+1                                                            
      call rcard(in,k,l,ic,j,iflag)                                     
      if(iflag)9999,9999,902
 9999 k1=icn1+k-1                                                       
      k2=k1-lrec                                                          
      if (ic(k1:k1).le.ic(k2:k2)) go to 901                                   
   30 continue                                                          
   32 continue                                                          
      if (ibet.eq.0) go to 35                                           
      l7=lbeg                                                           
      l=lthn+l7-1                                                       
      j=ncds                                                            
      call field(icod,l,ic,l7,j)                                        
      wtt=icod                                                          
      wtt=wtt/(10.**nndc)                                               
   35 if (nab.gt.1.or.kd.gt.0) go to 100                                
   36 continue                                                          
      call xmat                                                         
      if (mull.eq.1) return                                             
      if (ncds.eq.1) swt3=wtt                                           
      swt1=swt1+wtt                                                     
      if (ncds.eq.1) swt4=wtt                                           
      if (liop.eq.1.or.liop.eq.3) go to 41                              
      go to 39                                                          
   41 write (6,1002) (x(i),i=1,not)                                     
 1002 format (1h ,17f7.2)                                               
   39 if (nlhm.eq.0) go to 47                                           
   42 do 46 i=1,nlhm                                                    
      if (x(i).eq.0.0) go to 46                                         
      do 45 j=i,nlhm                                                    
      k=nlhm*(i-1)-i*(i-3)/2+j-i                                        
      if (ibet.eq.0) go to 48                                           
      array(k)=array(k)+(x(i)*x(j))/wtt                                 
      go to 45                                                          
   48 array(k)=array(k)+x(i)*x(j)                                       
   45 continue                                                          
   46 continue                                                          
   47 do 50 i=1,not                                                     
      if (kb.eq.0.and.kd.eq.0) go to 50                                 
      tot3(i)=tot3(i)+x(i)                                              
   50 tot(i)=tot(i)+x(i)                                                
      if (nrhm.eq.0) go to 20                                           
      if (ncpr.eq.0) go to 56                                           
      do 55 i=1,nrhm                                                    
      do 55 j=i,nrhm                                                    
      k=nrhm*(i-1)-i*(i-3)/2+j-i                                        
      k2=nlhm+i                                                         
      k3=nlhm+j                                                         
      if (ibet.eq.0) go to 44                                           
      sscpr(k)=sscpr(k)+(x(k2)*x(k3))/wtt                               
      go to 55                                                          
   44 sscpr(k)=sscpr(k)+x(k2)*x(k3)                                     
   55 continue                                                          
      go to 58                                                          
   56 continue                                                          
      do 57 i=1,nrhm                                                    
      k2=nlhm+i                                                         
      if (ibet.eq.0) go to 43                                           
      sscpr(i)=sscpr(i)+(x(k2)*x(k2))/wtt                               
      go to 57                                                          
   43 sscpr(i)=sscpr(i)+x(k2)*x(k2)                                     
   57 continue                                                          
   58 k=0                                                               
      do 59 i=1,nrhm                                                    
      j2=nlhm+i                                                         
      do 59 j=1,nlhm                                                    
      k=k+1                                                             
      if (ibet.eq.0) go to 49                                           
      rhm(k)=rhm(k)+(x(j)*x(j2))/wtt                                    
      go to 59                                                          
   49 rhm(k)=rhm(k)+x(j)*x(j2)                                          
   59 continue                                                          
      go to 20                                                          
c     ----------------------------------------------------              
c     absorption process                                                
c     ----------------------------------------------------              
  100 continue                                                          
      if (nab.lt.4.and.kb.eq.0) go to 110                               
      imj1=0                                                            
      do 102 i=1,nmjc                                                   
      k=ndc(i)                                                          
      if (ic(k:k).ne.blank) go to 102                                      
      if (i.eq.nmjc) go to 900                                          
      ic(k:k)=zero                                                           
  102 imj1=ixa(ic(k:k))+imj1*10                                                
      imi1=0                                                            
      do 104 i=1,nmic                                                   
      k=nmi(i)                                                          
      if (ic(k:k).ne.blank) go to 104                                      
      if (i.eq.nmic) go to 900                                          
      ic(k:k)=zero                                                           
  104 imi1=ixa(ic(k:k))+imi1*10                                                
      if (ncds.eq.1) go to 108                                          
      if (imj2-imj1) 164,107,901                                        
  107 if (imi2-imi1) 164,106,901                                        
  106 nmig=nmig+1                                                       
      swt3=swt3+wtt                                                     
  108 imi2=imi1                                                         
  109 imj2=imj1                                                         
      go to 36                                                          
  110 imj1=0                                                            
      do 112 i=1,ncc                                                    
      k=ndc(i)                                                          
      if (ic(k:k).ne.blank) go to 112                                      
      if (i.eq.ncc) go to 900                                           
      ic(k:k)=zero                                                           
  112 imj1=ixa(ic(k:k))+imj1*10                                                
      if (nab.eq.2) go to 201                                           
      if (ncds.eq.1) go to 114                                          
      if (imj2-imj1) 116,114,901                                        
  114 ncdg=ncdg+1                                                       
      swt2=swt2+wtt                                                     
      go to 109                                                         
  201 iin1=0                                                            
      k2=0                                                              
      if (nint.eq.0) go to 203                                          
      do 202 i=1,nint                                                   
      k=mz(i)                                                           
      if (ic(k:k).ne.blank) go to 202                                      
      if (i.eq.nint) go to 900                                          
      ic(k:k)=zero                                                           
  202 iin1=iin1*10+ixa(ic(k:k))                                                
      if (mty.eq.3.and.ncds.eq.1) go to 212                             
      imi1=0                                                            
      if (mty.eq.3.or.nmic.eq.0) go to 203                              
      do 204 i=1,nmic                                                   
      k=nmi(i)                                                          
      if (ic(k:k).ne.blank) go to 204                                      
      if (i.eq.nmic) go to 900                                          
      ic(k:k)=zero                                                           
  204 imi1=imi1*10+ixa(ic(k:k))                                                
  203 if (ncds.eq.1) go to 213                                          
      if (imj2-imj1) 206,207,901                                        
  207 nmig=nmig+1                                                       
      swt3=swt3+wtt                                                     
      if (nint.eq.0) go to 212                                          
      if (iin2-iin1) 206,209,901                                        
  209 min=min+1                                                         
      swt4=swt4+wtt                                                     
      if (mty.eq.3.or.nmic.eq.0) go to 212                              
      if (imi2-imi1) 206,213,901                                        
  213 imi2=imi1                                                         
  212 iin2=iin1                                                         
      if (k2.eq.1) go to 116                                            
      go to 114                                                         
  206 if (nab2.eq.0) go to 217                                          
      nsum=nsum+ncdg*ncdg                                               
      ssum=ssum+swt2*swt2                                               
      if (imj2.lt.imj1.or.iin2.lt.iin1) go to 214                       
  217 k2=1                                                              
      go to 213                                                         
  214 nsm=nsm+min*min                                                   
      snsm=snsm+swt4*swt4                                               
      swt4=wtt                                                          
      min=1                                                             
      if (imj2.lt.imj1) go to 215                                       
      k2=1                                                              
      go to 213                                                         
  215 r1i(ll)=float(nsm)/float(nmig)                                    
      r2i(ll)=float(nsum)/float(nmig)                                   
      if (ibet.eq.1) r1i(ll)=snsm/swt3                                  
      if (ibet.eq.1) r2i(ll)=ssum/swt3                                  
      swt3=wtt                                                          
      nmig=1                                                            
      nsm=0                                                             
      nsum=0                                                            
      ssum=0.                                                           
      snsm=0.                                                           
      ll=ll+1                                                           
      k2=1                                                              
      go to 213                                                         
  116 dm=ncdg                                                           
      if (ibet.eq.1) dm=swt2                                            
      if (kd.eq.0) go to 115                                            
      kc=1                                                              
      nmig=ncdg                                                         
      swt3=swt2                                                         
      call mixed (nab,matx,ka,kb,kc,not,tot,nmig,nlhm,ncpr,nrhm,tot3,arr
     1ay,ncdg,fab,tot4,sof,nx,kd,ibet,swt2,swt3,wtt,sofn)               
      go to 109                                                         
  115 if (nab.eq.2) go to 118                                           
      rown=1./(dm+rep)                                                  
      go to 120                                                         
  118 rown=1./dm                                                        
  120 if (nlhm.eq.0) go to 130                                          
      do 124 i=1,nlhm                                                   
      if (tot(i).eq.0.0) go to 124                                      
      do 122 j=i,nlhm                                                   
      k=nlhm*(i-1)-i*(i-3)/2+j-i                                        
  122 array(k)=array(k)-tot(i)*tot(j)*rown                              
  124 continue                                                          
  130 if(iad.ne.1) go to 131                                            
      if(nab.eq.4) go to 450                                            
      write(12,1055) imj2,ncdg,rown,(tot(i),i=1,not)                    
 1055 format(i9,i4,110e15.8)                                            
      go to 131                                                         
  450 write(12,1056) imj2,imi2,nmig,rown,(tot(i),i=1,not)               
 1056 format(2i9,i4,110e15.8)                                           
  131 if (liop.eq.0.or.liop.eq.5.or.liop.gt.9)  go to 135               
      if (nab.ne.2) iin2=0                                              
      if (nab.eq.0.or.nab.eq.1.or.nab.eq.3) imi2=0                      
      if (nab.eq.4) go to 132                                           
      write (6,1050) imj2,iin2,imi2,ncdg                                
 1050 format (1h ,10x,14hminor class is,3i10,5h  ni=,i4/1h ,6htotals)   
      go to 134                                                         
  132 write (6,1050) imj2,imi2,iin2,nmig                                
  134 write (6,1052) (tot(i),i=1,not)                                   
  135 if (nrhm.eq.0) go to 146                                          
      if (ncpr.eq.0) go to 138                                          
      do 136 i=1,nrhm                                                   
      do 136 j=i,nrhm                                                   
      k=nrhm*(i-1)-i*(i-3)/2+j-i                                        
      k2=nlhm+i                                                         
      k3=nlhm+j                                                         
  136 sscpr(k)=sscpr(k)-tot(k2)*tot(k3)*rown                            
      go to 142                                                         
  138 do 140 i=1,nrhm                                                   
      k2=nlhm+i                                                         
  140 sscpr(i)=sscpr(i)-tot(k2)*tot(k2)*rown                            
  142 if (nlhm.eq.0) go to 146                                          
      k=0                                                               
      do 144 i=1,nrhm                                                   
      j2=nlhm+i                                                         
      do 144 j=1,nlhm                                                   
      k=k+1                                                             
  144 rhm(k)=rhm(k)-tot(j)*tot(j2)*rown                                 
  146 if (nab.eq.1) go to 299                                           
      do 148 i=1,not                                                    
      if (kb.eq.1) go to 148                                            
      tot3(i)=tot3(i)+tot(i)                                            
  148 tot2(i)=tot2(i)+tot(i)                                            
      if (nab.ne.4) go to 152                                           
      do 150 i=1,not                                                    
      tot2(i)=tot2(i)-dm*tot(i)*rown                                    
  150 tot(i)=0.0                                                        
      gni=gni-dm*dm*rown                                                
      go to 166                                                         
  152 mjn=mjn+1                                                         
      do 154 i=1,not                                                    
  154 tot(i)=0.0                                                        
      ncdg=1                                                            
      swt2=wtt                                                          
      if (kb.eq.1) swt2=0.0                                             
      if (kb.eq.1) ncdg=0                                               
      if (ntra.eq.0) go to 193                                          
      if (kb.eq.1) go to 108                                            
      go to 109                                                         
  156 if (kb.eq.1) go to 158                                            
      if (nab.eq.2.and.(mty.eq.3.or.mty.eq.5.or.mty.eq.7)) go to 216    
      if (nab.ne.0) go to 158                                           
      df=ncds                                                           
      go to 299                                                         
  216 nsum=nsum+ncdg*ncdg                                               
      ssum=ssum+swt2*swt2                                               
      snsm=snsm+swt4*swt4                                               
      nsm=nsm+min*min                                                   
      r1i(ll)=float(nsm)/float(nmig)                                    
      r2i(ll)=float(nsum)/float(nmig)                                   
      if (ibet.eq.1) r1i(ll)=snsm/swt3                                  
      if (ibet.eq.1) r2i(ll)=ssum/swt3                                  
  158 if (nab.ne.1) go to 160                                           
      df=ncds-1                                                         
      go to 162                                                         
  160 if (kb.ne.1) go to 161                                            
      ncdg=ncdg+nmig                                                    
      swt2=swt2+swt3                                                    
      if (mty.lt.6) go to 230                                           
      k=ncl(1)                                                          
      if (mty.eq.7) k=ncl(2)                                            
      do 220 i=1,k                                                      
      r2i(i)=r2i(i)+r1i(i)*r1i(i)                                       
      if (ibet.eq.0) go to 219                                          
      r2i(i)=r2i(i)/ww(i)                                               
      go to 220                                                         
  219 r2i(i)=r2i(i)/float(nos(i))                                       
  220 r1i(i)=0.0                                                        
  230 kc=4                                                              
      call mixed (nab,matx,ka,kb,kc,not,tot,nmig,nlhm,ncpr,nrhm,tot3,arr
     1ay,ncdg,fab,tot4,sof,nx,kd,ibet,swt2,swt3,wtt,sofn)               
  161 if (nab.lt.4) go to 116                                           
      go to 164                                                         
  162 dm=ncds                                                           
      if (ibet.eq.1) dm=swt1                                            
      rown=1./dm                                                        
      go to 120                                                         
  164 ncdg=ncdg+nmig                                                    
      swt2=swt2+swt3                                                    
      if (kb.ne.1) go to 163                                            
      if (mty.lt.6) go to 231                                           
      k=ncl(1)                                                          
      if (mty.eq.7) k=ncl(2)                                            
      do 221 i=1,k                                                      
      r2i(i)=r2i(i)+r1i(i)*r1i(i)                                       
  221 r1i(i)=0.0                                                        
  231 kc=1                                                              
      call mixed (nab,matx,ka,kb,kc,not,tot,nmig,nlhm,ncpr,nrhm,tot3,arr
     1ay,ncdg,fab,tot4,sof,nx,kd,ibet,swt2,swt3,wtt,sofn)               
      if (imj2.eq.imj1) go to 108                                       
      kc=2                                                              
      call mixed (nab,matx,ka,kb,kc,not,tot,nmig,nlhm,ncpr,nrhm,tot3,arr
     1ay,ncdg,fab,tot4,sof,nx,kd,ibet,swt2,swt3,wtt,sofn)               
      go to 116                                                         
  163 dm=nmig                                                           
      if (ibet.eq.1) dm=swt3                                            
      swt2=swt2+swt3                                                    
      ncdg=ncdg+nmig                                                    
      rown=1./(dm+rep)                                                  
      gni=gni+dm                                                        
      go to 120                                                         
  166 min=min+1                                                         
      nmig=1                                                            
      swt3=wtt                                                          
      if (ntra.eq.0.or.imj1.ne.imj2) go to 168                          
      go to 108                                                         
  168 rown=1./gni                                                       
      if (nlhm.eq.0) go to 172                                          
      do 170 i=1,nlhm                                                   
      do 170 j=i,nlhm                                                   
      k=nlhm*(i-1)-i*(i-3)/2+j-i                                        
  170 array(k)=array(k)-tot2(i)*tot2(j)*rown                            
  172 if (nrhm.eq.0) go to 184                                          
      if (ncpr.eq.0) go to 176                                          
      do 174 i=1,nrhm                                                   
      do 174 j=i,nrhm                                                   
      k=nrhm*(i-1)-i*(i-3)/2+j-i                                        
      k3=nlhm+j                                                         
      k2=nlhm+i                                                         
  174 sscpr(k)=sscpr(k)-tot2(k2)*tot2(k3)*rown                          
      go to 180                                                         
  176 do 178 i=1,nrhm                                                   
      k2=nlhm+i                                                         
  178 sscpr(i)=sscpr(i)-tot2(k2)*tot2(k2)*rown                          
  180 k=0                                                               
      do 182 i=1,nrhm                                                   
      j2=nlhm+i                                                         
      do 182 j=1,nlhm                                                   
      k=k+1                                                             
  182 rhm(k)=rhm(k)-tot2(j)*tot2(j2)*rown                               
      if(iad.eq.0) go to 184                                            
      if(nab.ne.4) go to 184                                            
      write(13,1057) imj2,ncdg,gni,(tot2(i),i=1,not)                    
 1057 format(i9,i4,111e15.8)                                            
  184 if (liop.eq.0.or.liop.eq.5.or.liop.gt.9)  go to 190               
      write (6,1051) imj2,gni                                           
 1051 format (1h0,5x,14hmajor class is,i12,3x,31hreduced diagonal elemen
     1t (gni)=,f18.8)                                                   
      write (6,1052) (tot2(i),i=1,not)                                  
 1052 format (1h ,9f13.3)                                               
  190 do 191 i=1,not                                                    
  191 tot2(i)=0.0                                                       
      mjn=mjn+1                                                         
      gni=0.0                                                           
      ncdg=0                                                            
      swt2=0.0                                                          
      if (ntra.eq.0) go to 196                                          
      go to 108                                                         
  193 do 194 i=1,not                                                    
  194 tot(i)=tot2(i)                                                    
      df=ncds-mjn                                                       
      if (nab.eq.3) df=df+float(mjn)                                    
      go to 299                                                         
  196 do 198 i=1,not                                                    
  198 tot(i)=tot3(i)                                                    
      df=ncds-mjn                                                       
      go to 299                                                         
  900 write (6,1053) ncds                                               
 1053 format (1h0,69hunits position of an id field or a control field is
     1 blank on card no.,i5)                                            
      go to 902                                                         
  901 write (6,1054) ncds,ijob                                          
 1054 format (1h0,38hcards out of sequence---check card no.,i5,5x,15hfor
     1 problem no.,i3)                                                  
  902 mull=1                                                            
      call wcard(in,k,l,ic,idum,iflag)
      return                                                            
  299 continue                                                          
      if(lparm.eq.1) call tpage(ntitl)                                  
      if (nab.eq.0) go to 300                                           
      go to (301,302,303,304), nab                                      
  300 if (kd.eq.0) go to 305                                            
      kc=4                                                              
      nmig=ncdg                                                         
      swt3=swt2                                                         
      swt2=swt1                                                         
      ncdg=ncds                                                         
      call mixed (nab,matx,ka,kb,kc,not,tot,nmig,nlhm,ncpr,nrhm,tot3,arr
     1ay,ncdg,fab,tot4,sof,nx,kd,ibet,swt2,swt3,wtt,sofn)               
  305 write (6,1060) ncds                                               
 1060 format (1h0,5x,53htotal least-squares analysis.  no equations abso
     1rbed.,5x,13hdf=no. cards=,i6)                                     
      go to 310                                                         
  301 write (6,1061) df                                                 
 1061 format (1h0,21x,38hequation for the overall mean absorbed,5x,3hdf=
     1,f7.0)                                                            
      go to 310                                                         
  302 write (6,1062) mjn,ncds,df                                        
 1062 format (1h0,10x,55hno. of classes or subclasses absorbed by least 
     1squares=,i6,5x,10hno. cards=,i6,5x,3hdf=,f7.0)                    
      go to 310                                                         
  303 write (6,1063) mjn,rep,ncds,df                                    
 1063 format (1h0,60hno. of classes or subclasses absorbed by maximum li
     1kelihood=,i6,3x,7h1-r/r =,f7.4,3x,10hno. cards=,i6,3x,3hdf=,f7.0) 
      go to 310                                                         
  304 write (6,1064) mjn,min,rep,ncds,df                                
 1064 format (1h0,37h no. of major classes absorbed by ml=,i4,3x,21hno. 
     1minor subclasses=,i6,3x,7h1-r/r =,f7.4,3x,10hno. cards=,i6,3x,3hdf
     3=,f7.0)                                                           
  310 if (ibet.eq.0) go to 309                                          
      write (6,1116) swt1                                               
 1116 format (1h0,30x,'weighted least squares analysis--sum of weights =
     1',f10.2)                                                          
  309 ncc=0                                                             
      if (nom+non.eq.0) go to 320                                       
      call sclnos                                                       
  320 if (nlhm.eq.0.or.(npr.eq.0.and.liop.gt.9))  go to 330             
      write (6,1100) ijob                                               
 1100 format (1h0,60hoverall means and standard deviations of lhm for pr
     1oblem no.,i3)                                                     
      write (6,1101)                                                    
 1101 format (1h0,35h coded lhm    independent variables,11x,4hmean,9x,4
     1hs.d.)                                                            
      write (6,1102)                                                    
 1102 format (1h )                                                      
      l=1                                                               
      k3=1                                                              
      do 329 i=1,nlhm                                                   
      amn=tot(i)/float(ncds)                                            
      if (ibet.eq.1) amn=tot(i)/swt1                                    
      if (nab.eq.3) tot(i)=array(i)                                     
      if (i.lt.ie.and.liop.gt.9) go to 329                              
      if(liop.le.9) go to 322                                           
      do 321 j=1,nnn                                                    
      if(i.eq.nregp(j)) go to 322                                       
  321 continue                                                          
      go to 329                                                         
  322 k=nlhm*(i-1)-i*(i-3)/2                                            
      if (nab.eq.1.or.nab.eq.2.or.nab.eq.4) go to 324                   
      ac=(array(k)-(tot(i)*tot(i)/array(1)))/(df-1.)                    
       if (ac.lt.0.0) ac=0.0                                            
       sd=dsqrt(ac)                                                     
      go to 326                                                         
  324 sd=dsqrt(array(k)/df)                                             
  326 if (i.lt.ie) go to 328                                            
      if (lab1(i).ne.rgrsn) go to 328                                   
      l=l-1                                                             
      if (l.ne.0) go to 328                                             
      if (lqc(k3).eq.1) go to 393                                       
      ac=amn+xm(k3)                                                     
      write (6,1117) ac                                                 
 1117 format (10x,'xbar=',f13.5)                                        
  393 if (lqc(k3)-2) 390,391,392                                        
  390 amn=amn+xm(k3)                                                    
      k3=k3+1                                                           
      l=1                                                               
      go to 328                                                         
  391 k3=k3+1                                                           
      l=2                                                               
      go to 328                                                         
  392 k3=k3+1                                                           
      l=3                                                               
  328 write (6,1103) i,lab1(i),lab2(i),lab3(i),lab4(i),amn,sd           
  329 continue                                                          
 1103 format (1h ,i6,6x,a6,2(1x,a6),a6,2f13.5)                          
  330 if (nrhm.eq.0) go to 342                                          
      write (6,1104)                                                    
 1104 format (1h0,15x,44hoverall means and standard deviations of rhm)  
      write (6,1102)                                                    
      do 340 i=1,nrhm                                                   
      k2=nlhm+i                                                         
      amn=tot(k2)/float(ncds)                                           
      if (ibet.eq.1) amn=tot(k2)/swt1                                   
      k=nlhm*(i-1)+1                                                    
      if (nab.eq.0.or.nab.eq.3) tot(k2)=rhm(k)                          
      if (ncpr.eq.0) go to 332                                          
      j=nrhm*(i-1)-i*(i-3)/2                                            
      go to 334                                                         
  332 j=i                                                               
  334 if (nab.eq.1.or.nab.eq.2.or.nab.eq.4) go to 336                   
      sd=dsqrt((sscpr(j)-(tot(k2)*tot(k2)/array(1)))/(df-1.))           
      go to 338                                                         
  336 sd=dsqrt(sscpr(j)/df)                                             
  338 amn=amn+ym(i)                                                     
      if (mty.eq.1) tot4(i)=amn                                         
  340 write (6,1105) lity(i),amn,sd                                     
 1105 format (1h ,12x,a6,4x,5hmean=,f12.5,10x,5hs.d.=,f12.5)            
  342 if (liop.eq.7) go to 500                                          
  344 if (nlhm.eq.0) go to 359                                          
      if (nab.ne.3) go to 345                                           
      do 343 i=1,not                                                    
  343 tot5(i)=tot2(i)                                                   
  345 if (ie.gt.nlhm.and.liop.gt.9)  go to 359                          
      k1=ie-1                                                           
      do 399 i=1,nlhm                                                   
      tot2(i)=0.0                                                       
  399 tot3(i)=0.0                                                       
      if (npr.eq.0) go to 421                                           
      k5=1                                                              
      do 400 i=1,npr                                                    
      k2=lqc(i)                                                         
      do 401 j=1,k2                                                     
      k1=k1+1                                                           
      if (j.eq.1) amn=(-1.)*(tot(k1)/float(ncds))                       
      if (ibet.eq.1) amn=(-1.)*(tot(k1)/swt1)                           
      tot3(k1)=amn**j                                                   
      if (nab.eq.0.or.nab.eq.3) tot2(k1)=1.0                            
      if (lad(i).ne.0) tot2(k1)=tot2(k1)+m99+1                           
      if (iclr(i).eq.0) go to 401                                       
      if (j.gt.1) k5=k5-k3                                              
      k3=iclr(i)                                                        
      do 402 k=1,k3                                                     
      j1=irm(k5)                                                        
      if (j1.gt.nom) go to 403                                          
      k4=k1+1                                                           
      k6=k1+ncl(j1)-1                                                   
      if (j1.le.nmea) go to 408                                         
      j1=nmea+1                                                         
      j2=irm(k5)-1                                                      
      if (nab.eq.0.or.nab.eq.3) nsum=2                                  
      if (j2.lt.j1) go to 405                                           
      do 404 n=j1,j2                                                    
  404 nsum=nsum+ncl(n)-1                                                
      go to 405                                                         
  403 j1=irm(k5)-nom                                                    
      k4=k1+1                                                           
      k6=k1+ncln(j1)-1                                                  
      if (j1.le.nnea) go to 408                                         
      j1=nnea+1                                                         
      j2=irm(k5)-nom-1                                                  
      nsum=mn2-nme+1                                                    
      if (nab.eq.0.or.nab.eq.3) nsum=nsum+1                             
      if (j2.lt.j1) go to 405                                           
      do 406 n=j1,j2                                                    
  406 nsum=nsum+ncln(n)-1                                               
  405 do 407 n=k4,k6                                                    
      tot3(n)=amn**j                                                    
      tot2(n)=float(nsum)                                               
      if (lad(i).ne.0) tot2(n)=tot2(n)+m99+1                             
  407 nsum=nsum+1                                                       
  408 k5=k5+1                                                           
  402 k1=k6                                                             
  401 continue                                                          
  400 continue                                                          
      do 415 n=1,nlhm                                                   
      n3=nlhm-n+1                                                       
      m=tot2(n3)                                                        
      if (m.ge.m99+1) m=m-(m99+1)                                             
      if (m.eq.0) go to 415                                             
      ac=tot3(n3)                                                       
      do 416 ll=n,nlhm                                                  
      m=tot2(n3)                                                        
      k4=1                                                              
      k6=1                                                              
      n4=nlhm-ll+1                                                      
      j7=nlhm*(n4-1)-n4*(n4-3)/2                                        
      j2=j7+n3-n4                                                       
      ad=tot3(n4)                                                       
      m1=tot2(n4)                                                       
      if (m.ge.m99+1.and.m1.ge.m99+1) go to 416                             
      if (m.lt.m99+1.and.m1.ge.m99+1) k4=0                                  
      if (m.ge.m99+1.and.m1.lt.m99+1) k6=0                                  
      if (m1.ge.m99+1) m1=m1-(m99+1)                                          
      if (m.ge.m99+1.and.m1.eq.0) go to 416                               
      if (m.ge.m99+1) m=m-(m99+1)                                             
      j1=nlhm*(m-1)-m*(m-3)/2                                           
      if (m1.eq.0.or.k4.eq.0) go to 410                                 
      j3=nlhm*(m1-1)-m1*(m1-3)/2                                        
      j4=j1+n4-m                                                        
      if (n4.lt.m) j4=j7+m-n4                                           
      j5=j3+m-m1                                                        
      if (m1.gt.m) j5=j1+m1-m                                           
      j6=j3+n3-m1                                                       
      if (k6.eq.0) go to 411                                            
      array(j2)=array(j2)+ac*(ad*array(j5)+array(j4))+ad*array(j6)      
      go to 416                                                         
  410 j6=j1+n4-m                                                        
      if (n4.lt.m) j6=j7+m-n4                                           
      array(j2)=array(j2)+ac*array(j6)                                  
      if (n4.eq.1.and.(nab.eq.0.or.nab.eq.3)) tot(n3)=array(j2)         
      go to 416                                                         
  411 array(j2)=array(j2)+ad*array(j6)                                  
  416 continue                                                          
  415 continue                                                          
      do 418 n=1,nrhm                                                   
      j1=nlhm*(n-1)                                                     
      do 419 ll=1,nlhm                                                  
      if (ll.lt.ie) go to 419                                           
      j2=j1+ll                                                          
      m=tot2(ll)                                                        
      if (m.eq.0.or.m.ge.m99+1) go to 419                                 
      j4=j1+m                                                           
      rhm(j2)=rhm(j2)+rhm(j4)*tot3(ll)                                  
  419 continue                                                          
  418 continue                                                          
  421 write (6,1102)                                                    
      write (6,1106) ijob                                               
 1106 format (1h0,15x,64hsums of squares, c.p. and correlations among lh
     1m for problem no.,i3)                                             
      write (6,1107)                                                    
 1107 format (1h0,4h row,5h  col,15x,21hindependent variables)          
      write (6,1108)                                                    
 1108 format (1h ,9hcode code,9x,3hrow,22x,6hcolumn,23x,28hs.sqs. or c.p
     1.   correlation)                                                  
      do 360 i=1,nlhm                                                   
      do 360 j=i,nlhm                                                   
      if (i.lt.ie.and.j.lt.ie.and.liop.gt.9)  go to 360                 
      if(liop.le.9) go to 361                                           
      do 355 ll=1,nnn                                                   
      if(i.eq.nregp(ll)) go to 370                                      
  355 continue                                                          
      go to 360                                                         
  370 do 365 ll=1,nnn                                                   
      if(j.eq.nregp(ll)) go to 361                                      
  365 continue                                                          
      go to 360                                                         
  361 rlhm=0.0                                                          
      k=nlhm*(i-1)-i*(i-3)/2                                            
      k1=nlhm*(j-1)-j*(j-3)/2                                           
      k3=k+j-i                                                          
      if (nab.eq.1.or.nab.eq.2.or.nab.eq.4) go to 354                   
       if (i.eq.1) go to 356                                            
      scp=array(k3)-(tot(i)*tot(j))/array(1)                            
      ssy1=array(k)-(tot(i)*tot(i))/array(1)                            
      ssy2=array(k1)-(tot(j)*tot(j))/array(1)                           
      rlhm=scp/dsqrt(div(ssy1*ssy2))                                    
      go to 356                                                         
  354 rlhm=array(k3)/dsqrt(array(k)*array(k1))                          
  356 if (i.lt.ie.and.liop.gt.9)  go to 360                             
      if (i.ne.j) go to 358                                             
      rlhm=1.                                                           
      write (6,1102)                                                    
  358 write (6,1109) i,j,lab1(i),lab2(i),lab3(i),lab4(i),lab1(j),lab2(j)
     1,lab3(j),lab4(j),array(k3),rlhm                                   
  360 continue                                                          
 1109 format (1h ,i3,i5,2x,a6,2(1x,a6),2a6,2(1x,a6),a6,f23.8,f13.4)     
  359 if (nrhm.eq.0) go to 386                                          
      if (nlhm.eq.0) go to 376                                          
      if (ie.gt.nlhm.and.liop.gt.9)  go to 376                          
      write (6,1102)                                                    
      write (6,1110) ijob                                               
 1110 format (1h0,71h sums of crossproducts and correlations of lhm with
     1 rhm for problem no.,i3)                                          
      write (6,1111)                                                    
 1111 format (1h0,41hrhm  lhm  rhm name   independent variable,16x,4hc.p
     1.,7x,11hcorrelation)                                              
      do 375 i=1,nrhm                                                   
      k4=nlhm+i                                                         
      write (6,1102)                                                    
      do 375 j=1,nlhm                                                   
      if (j.lt.ie.and.liop.gt.9)  go to 375                             
      if(liop.le.9) go to 371                                           
      do 373 ll=1,nnn                                                   
      if(j.eq.nregp(ll)) go to 371                                      
  373 continue                                                          
      go to 375                                                         
  371 rlr=0.0                                                           
      k=nlhm*(j-1)-j*(j-3)/2                                            
      if (ncpr.eq.0) go to 362                                          
      k1=nrhm*(i-1)-i*(i-3)/2                                           
      go to 364                                                         
  362 k1=i                                                              
  364 k2=nlhm*(i-1)+j                                                   
      if (nab.eq.1.or.nab.eq.2.or.nab.eq.4) go to 372                   
       if (j.eq.1) go to 374                                            
      scp=rhm(k2)-(tot(j)*tot(k4))/array(1)                             
      ssy1=array(k)-(tot(j)*tot(j))/array(1)                            
      ssy2=sscpr(k1)-(tot(k4)*tot(k4))/array(1)                         
      rlr=scp/dsqrt(div(ssy1*ssy2))                                     
      go to 374                                                         
  372 rlr=rhm(k2)/dsqrt(array(k)*sscpr(k1))                             
  374 write (6,1112) i,j,lity(i),lab1(j),lab2(j),lab3(j),lab4(j),rhm(k2)
     1,rlr                                                              
  375 continue                                                          
 1112 format (1h ,i3,i5,3x,a6,2x,2(a6,1x),2a6,f23.8,f10.4)              
  376 if (ncpr.eq.0.or.nrhm.eq.1) go to 386                             
      if (nrn.ne.nrun.and.liop.eq.20) go to 386                         
      write (6,1102)                                                    
      write (6,1113) ijob                                               
 1113 format (1h0,15x,64hsums of squares, c.p. and correlations among rh
     1m for problem no.,i3)                                             
      write (6,1114)                                                    
 1114 format (1h0,20x,3hrow,6h   col,4x,3hrhm,4x,3hrhm,15x,14hs.sqs. or 
     1c.p.,4x,11hcorrelation)                                           
      do 384 i=1,nrhm                                                   
      do 384 j=i,nrhm                                                   
      k=nrhm*(i-1)-i*(i-3)/2                                            
      k1=nrhm*(j-1)-j*(j-3)/2                                           
      k3=k+j-i                                                          
      if (nab.eq.1.or.nab.eq.2.or.nab.eq.4) go to 380                   
      k4=nlhm+i                                                         
      k5=nlhm+j                                                         
      scp=sscpr(k3)-(tot(k4)*tot(k5))/array(1)                          
      ssy1=sscpr(k)-(tot(k4)*tot(k4))/array(1)                          
      ssy2=sscpr(k1)-(tot(k5)*tot(k5))/array(1)                         
      rlhm=scp/dsqrt(ssy1*ssy2)                                         
      go to 382                                                         
  380 rlhm=sscpr(k3)/dsqrt(sscpr(k)*sscpr(k1))                          
  382 if (i.ne.j) go to 384                                             
      write (6,1102)                                                    
  384 write (6,1115) i,j,lity(i),lity(j),sscpr(k3),rlhm                 
 1115 format (1h ,20x,i3,i6,3x,a6,2x,a6,f25.8,f14.4)                    
  386 n=nlhm                                                            
  500 continue                                                          
      if (nab.ne.3) return                                              
      do 501 i=1,not                                                    
  501 tot(i)=tot5(i)                                                    
      return                                                            
      end                                                               
