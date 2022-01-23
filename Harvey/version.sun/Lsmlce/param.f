      subroutine param(ntitl,nnn)                                       
c     -------------------------------------------                       
c     subroutine which reads and lists parameter cards, and sets up     
c     arrays for labeling output                                        
c     ----------------------------------------------------              
      include 'decl1'
      character*6 blank,cubic,mu,quad,rgrsn,linear,rgnl,rgnq,rgnc
      double precision jlog10
      include 'com1'
      include 'com2'
      include 'com3'
      include 'com4'
      include 'com5'
      include 'com9'
      data blank,cubic,mu,quad,rgrsn,linear/6h      ,6hcubic ,6hmu    , 
     16hquad  ,6hrgrsn ,6hlinear/,rgnl,rgnq,rgnc/6h rgn l,6h rgn q,6h rg
     2n c/                                                              
c     ----------------------------------------------------              
c     reads and lists parameter card 1.                                 
c     ----------------------------------------------------              
      write(6,2000)                                                     
 2000 format('1'///' mixed model least-squares and maximum likelihood co
     *mputer program'/' walter r. harvey     phone (614)-422-5763'/     
     *' dairy science dept., the ohio state univ., columbus ohio 43210'/
     */)                                                                
    1 read  (5, * ) ijob,nab,ncd,icn1,nlhm,nrhm,nmea,nme,nnea,nne,n2f, 
     1npr,nlc,ncpr,iran,mpop,liop,in,mty,man,nrun,nrn,ibet,lbeg,lthn,nnd
     2c,prbmin,lparm,ntitl,nsave
 1000 format (23i2,i3,2i2,f5.4,2i2)                                     
      if (prbmin.eq.0) prbmin=0.10                                      
      if (nrn.eq.1) mull=0                                              
      if (nrun.ne.nrn) mpop=1                                           
      do 201 i=1,13                                                     
  201 ipl(i)=0                                                          
      if (in.eq.0) in=5                                                 
      if(ntitl.ge.1) read(5,*) titl1                                 
      if(ntitl.ge.2) read(5,*) titl2                                 
      if(ntitl.ge.3) read(5,*) titl3                                 
      if(ntitl.ge.4) read(5,*) titl4                                 
      if(ntitl.ge.5) read(5,*) titl5                                 
 2001 format(a80)                                               
      if(lparm.eq.1)                                                    
     *write (6,1001) ijob                                               
 1001 format (1h0,20x,42hlisting of parameter cards for problem no.,i3) 
      if(lparm.eq.1)                                                    
     *write (6,1002) ijob,nab,ncd,icn1,nlhm,nrhm,nmea,nme,nnea,nne,n2f, 
     1npr,nlc,ncpr,iran,mpop,liop,in,mty,man,nrun,nrn,ibet,lbeg,lthn,nnd
     2c,prbmin,lparm,ntitl,nsave
 1002 format(1h0,'ijob nab ncd icn1 nlhm nrhm nmea nme nnea nne n2f npr 
     1nlc ncpr iran mpop liop in mty man nrun nrn ibet ibeg lthn nndc pr
     2b lpar ntit nsav'/' ',i3,2i4,4i5,i4,i5,4i4,4i5,i4,i3,i4,i5,i4
     3,4i5,f5.2,i4,2i5)                                                            
      ntote=(nlhm*(nlhm+1))/2+(nlhm*nrhm)                               
      if (ntote.gt.m5000) go to 901                                      
      if ((nab.eq.0.or.nab.eq.3.or.nab.eq.4).and.mty.gt.1) go to 902    
      if (nab.gt.6) go to 902                                           
      if (nme.gt.m20) go to 903                                          
      if (nne.gt.m50) go to 903                                          
      ntote=nme+nmea                                                    
      if (ntote.gt.m20) go to 903                                        
      ntote=nne+nnea                                                    
      if (ntote.gt.m50) go to 903                                        
      if (n2f.gt.m30) go to 903                                          
      if (npr.gt.m90) go to 903                                          
      linout=nme+nne+n2f+npr                                            
      if (linout.gt.m91) go to 908                                       
c     ----------------------------------------------------              
c     reads control identification from parameter cards if absorption   
c                   is to occur                                         
c     ----------------------------------------------------              
      if (mty.gt.1.or.iran.gt.0) ncpr=1                                 
      k8=1                                                              
      if (nab.eq.1.or.nab.eq.2.or.nab.eq.4.or.nab.eq.5) go to 4         
      im(1)=1                                                           
      lab1(1)=mu                                                        
      lab2(1)=blank                                                     
      lab3(1)=blank                                                     
      lab4(1)=blank                                                     
      k8=2                                                              
    4 if (nab.eq.0) go to 13                                            
      go to (13,16,5,5,3,2),nab                                         
    2 read  (5,*) name,iad,nr1(14),nw(14),ncc,(ndc(i),i=1,ncc)        
  999 format (a6,i1,i3,34i2)                                            
      if(lparm.eq.1)                                                    
     *write (6,1003) name,iad,nr1(14),nw(14),ncc,(ndc(i),i=1,ncc)       
 1003 format (1h0,54h name   iad  nr1   nw  ncc  ndc(i) where i=1,2,...n
     1cc./1h ,a6,4x,i1,2x,i3,22(3x,i2))                                 
      if (ncc.gt.10) go to 904                                          
      go to 13                                                          
    5 read  (5,*) iad,rep,nmjc,(ndc(i),i=1,nmjc),nmic,(nmi(i),i=1,nmi
     1c)                                                                
      if(lparm.eq.1)                                                    
     *write (6,1005) iad,rep,nmjc,(ndc(i),i=1,nmjc)                     
 1004 format (i2,f8.6,31i2)                                             
 1005 format (1h0,43hiad    rep   ncc  ndc(i) where i=1,2,...ncc/1h ,   
     1i2,2x,f8.6,i4,20i5)                                               
      if (nmjc.gt.10) go to 904                                         
      if (nab.eq.3) ncc=nmjc                                            
      rr=rep                                                            
      rep=(1.-rep)/rep                                                  
      if (nab.eq.4) go to 6                                             
      go to 13                                                          
   16 read (5,*) name,iad,nr1(14),nw(14),ncc,(ndc(i),i=1,ncc),nint,   
     1(mz(i),i=1,nint),nmic,(nmi(i),i=1,nmic)                           
      if(lparm.eq.1)                                                    
     *write (6,1024) name,iad,nr1(14),nw(14),ncc,(ndc(i),i=1,ncc)       
 1024 format (1h0,57h name   iad  nr1   nw  nmjc  ndc(i) where i=1,2,...
     1nmjc. /1h ,a6,4x,i1,2x,i3,22(3x,i2))                              
      if(lparm.eq.1)                                                    
     *write (6,1025) nint,(mz(i),i=1,nint)                              
 1025 format (1h0,23x,33hnint  mz(i)  where i=1,2,...nint./1h ,21x,12(3x
     1,i2))                                                             
      if(lparm.eq.1)                                                    
     *write (6,1026) nmic,(nmi(i),i=1,nmic)                             
 1026 format (1h0,23x,33hnmic  nmi(i) where i=1,2,...nmic./1h ,21x,12(3x
     1,i2))                                                             
      if (ncc.gt.10.or.nint.gt.10.or.nmic.gt.10) go to 904              
      go to 13                                                          
    3 read (5,*)  nam5,iad,nr1(15),nw(15),nmjc,(ndc(i),i=1,nmjc),nmic,
     1(nmi(i),i=1,nmic)                                                 
      if(lparm.eq.1)                                                    
     *write (6,1006) nam5,iad,nr1(15),nw(15),nmjc,(ndc(i),i=1,nmjc)     
 1006 format (1h0,57h nam5   iad  nr1   nw  nmjc  ndc(i) where i=1,2,...
     1nmjc. /1h ,a6,4x,i1,2x,i3,22(3x,i2))                              
    6 if(lparm.eq.1)                                                    
     *write (6,1007) nmic,(nmi(i),i=1,nmic)                             
 1007 format (1h0,23x,33hnmic  nmi(i) where i=1,2,...nmic. /1h ,24x,15(i
     12,3x))                                                            
      if (nmjc.gt.10.or.nmic.gt.10) go to 904                           
      tot4(m99+3)=iad                                                     
      lgt=10**nmic                                                      
c     ----------------------------------------------------              
c     reads and lists parameter cards for main effects                  
c     ----------------------------------------------------              
   13 k1=1                                                              
      k2=0                                                              
      if (nmea.eq.0) go to 10                                           
      do 8 i=1,nmea                                                     
      read  (5,*)   men(i),lit(i),k7,mpol,lme(i),ibeg(i),(nos(j),j=1,
     1k7)                                                               
 1008 format (i2,a6,i2,2i1,i2,16i4,2x/(20i4))                           
      if(lparm.eq.1)                                                    
     *write (6,1009) i,men(i),lit(i),k7,mpol,lme(i),ibeg(i)             
 1009 format (1h0,6hfor i=,i4,47h  men(i)  lit(i)  ncl(i)  mpol  lme(i) 
     1 ibeg(i)/1h ,12x,i2,6x,a6,2x,i2,6x,i2,4x,i2,7x,i2)                
      ncl(i)=k7                                                         
      if(lparm.eq.1)                                                    
     *write (6,1010) (nos(j),j=1,k7)                                    
 1010 format (1h0,72hiden(j) where j=k2 by steps of one as long as j is 
     1less than k2+ncl(i)+1/(1h ,i5,5x,i4,3x,20i5))                     
      do 7 j=1,k7                                                       
      k3=k1+j-1                                                         
      iden(k3)=nos(j)                                                   
    7 nos(j)=0                                                          
      k1=k1+k7                                                          
      if (nrun.ne.nrn.or.mpol.eq.0) go to 8                             
      if (i.eq.1) ipl(12)=men(i)                                        
    8 k2=k2+k7                                                          
   10 ml=k2                                                             
      nom=nme+nmea                                                      
      if (nme.eq.0) go to 12                                            
      k3=nmea+1                                                         
      do 11 i=k3,nom                                                    
      read  (5,*)   men(i),lit(i),k7,mpol,lme(i),ibeg(i),(nos(j),j=1,
     1k7)                                                               
      if(lparm.eq.1)                                                    
     *write (6,1009) i,men(i),lit(i),k7,mpol,lme(i),ibeg(i)             
      if(lparm.eq.1)                                                    
     *write (6,1010) (nos(j),j=1,k7)                                    
      ncl(i)=k7                                                         
      do 9 j=1,k7                                                       
      k5=k1+j-1                                                         
      iden(k5)=nos(j)                                                   
    9 nos(j)=0                                                          
      if (k8.ne.1) go to 17                                             
      im(k8)=ncl(i)-1                                                   
      k5=1                                                              
      go to 18                                                          
   17 im(k8)=ncl(i)-1+im(k8-1)                                          
      k5=im(k8-1)+1                                                     
c     ----------------------------------------------------              
c     sets up beginning and ending row numbers for main effects for     
c     which polynomials are to be fitted                                
c     ----------------------------------------------------              
   18 if (mpol.eq.0) go to 99                                           
      if (nsme.gt.12) go to 905                                         
      nsme=nsme+1                                                       
      ipl(nsme)=men(i)                                                  
      nsp(nsme)=k5                                                      
      nnd(nsme)=im(k8)                                                  
      k6=ncl(i)                                                         
      do 100 j=1,k6                                                     
      k=ncas+j                                                          
      l=k1+j-1                                                          
  100 xp(k)=iden(l)                                                     
      ncas=ncas+ncl(i)                                                  
      if (ncas.gt.80) go to 906                                         
c     ----------------------------------------------------              
c     sets up lab arrays for listing later                              
c     ----------------------------------------------------              
   99 k=k1                                                              
      k6=im(k8)                                                         
      do 15 j=k5,k6                                                     
      lab1(j)=lit(i)                                                    
      call itoa (iden(k),lab2(j))                                       
      lab3(j)=blank                                                     
      lab4(j)=blank                                                     
   15 k=k+1                                                             
      k1=k1+ncl(i)                                                      
      k2=k2+ncl(i)                                                      
   11 k8=k8+1                                                           
c     ----------------------------------------------------              
c     reads and lists parameter cards for nested main effects           
c     ----------------------------------------------------              
   12 nsc=k2-ml                                                         
      mn2=nsc                                                           
      k1=1                                                              
      k2=0                                                              
      if (nnea.eq.0) go to 20                                           
      do 19 i=1,nnea                                                    
      read  (5,*)   nma(i),nmac(i),nen(i),nlit(i),k7,mpol,lne(i),nbeg
     1(i),(nos(j),j=1,k7)                                               
 1011 format (3i2,a6,i2,2i1,i2,15i4,2x/(20i4))                          
      if(lparm.eq.1)                                                    
     *write (6,1012) i,nma(i),nmac(i),nen(i),nlit(i),k7,mpol,lne(i),nbeg
     1(i)                                                               
 1012 format (1h0,6hfor i=,i4,66h  nma(i)  nmac(i)  nen(i)  nlit(i)  ncl
     1n(i)  mpol  lne(i)  nbeg(i)/ 1h ,13x,i2,i9,i9,5x,a6,i5,i9,i6,i8)  
      if(lparm.eq.1)                                                    
     *write (6,1013) k1,k2,k7,(nos(j),j=1,k7)                           
 1013 format (1h0,16hnden(j) where j=,i4,45h  incremented by one until j
     1 is greater than ,i4,1h+,i4/(1h ,19(i4,2x)))                      
      ncln(i)=k7                                                        
      do 14 j=1,k7                                                      
      k3=k1+j-1                                                         
      nden(k3)=nos(j)                                                   
   14 nos(j)=0                                                          
      k1=k1+k7                                                          
   19 k2=k2+k7                                                          
   20 mlb=k2                                                            
      non=nnea+nne                                                      
      if (nne.eq.0) go to 25                                            
      k3=nnea+1                                                         
      do 24 i=k3,non                                                    
      read  (5,*)   nma(i),nmac(i),nen(i),nlit(i),k7,mpol,lne(i),nbeg
     1(i),(nos(j),j=1,k7)                                               
      if(lparm.eq.1)                                                    
     *write (6,1012) i,nma(i),nmac(i),nen(i),nlit(i),k7,mpol,lne(i),nbeg
     1(i)                                                               
      if(lparm.eq.1)                                                    
     *write (6,1013) k1,k2,k7,(nos(j),j=1,k7)                           
      ncln(i)=k7                                                        
      do 22 j=1,k7                                                      
      k5=k1+j-1                                                         
      nden(k5)=nos(j)                                                   
   22 nos(j)=0                                                          
      if (k8.ne.1) go to 31                                             
      im(k8)=ncln(i)-1                                                  
      k5=1                                                              
      go to 32                                                          
   31 im(k8)=ncln(i)-1+im(k8-1)                                         
      k5=im(k8-1)+1                                                     
c     ----------------------------------------------------              
c     sets up arrays needed when polynomials are to be fitted           
c     ----------------------------------------------------              
   32 if (mpol.eq.0) go to 98                                           
      nsme=nsme+1                                                       
      if (nsme.gt.12) go to 905                                         
      ipl(nsme)=nen(i)                                                  
      nsp(nsme)=k5                                                      
      nnd(nsme)=im(k8)                                                  
      k6=ncln(i)                                                        
      do 101 j=1,k6                                                     
      k=ncas+j                                                          
      l=k1+j-1                                                          
  101 xp(k)=nden(l)                                                     
      ncas=ncas+ncln(i)                                                 
      if (ncas.gt.80) go to 906                                         
c     ----------------------------------------------------              
c     sets up lab arrays to identify listings                           
c     ----------------------------------------------------              
   98 k=k1                                                              
      k6=im(k8)                                                         
      do 23 j=k5,k6                                                     
      lab1(j)=nlit(i)                                                   
      call itoa (nden(k),lab2(j))                                       
      lab3(j)=blank                                                     
      lab4(j)=blank                                                     
   23 k=k+1                                                             
      k8=k8+1                                                           
      k1=k1+k7                                                          
   24 k2=k2+k7                                                          
c     ----------------------------------------------------              
c     reads and lists parameter cards for two-factor interaction effects
c     ----------------------------------------------------              
   25 nsc=nsc+k2-mlb                                                    
      k1=1                                                              
      k2=0                                                              
      iei=im(k8-1)+1                                                    
      if (k8.eq.1) iei=1                                                
      if (n2f.eq.0) go to 50                                            
      do 49 i=1,n2f                                                     
      read  (5,*)   int1(i),int2(i),k7,(nos(j),j=1,k7)               
 1014 format (3i2,16i4,2x/(18i4))                                       
      if(lparm.eq.1)                                                    
     *write (6,1015) i,int1(i),int2(i),k7                               
 1015 format (1h0,6hfor i=,i4,25h  int1(i) int2(i)  nmc(i)/ 1h ,12x,3(i2
     1,6x))                                                             
      if(lparm.eq.1                                                     
     *  .and.k7.ne.0) write (6,1016) k1,k2,k7,(nos(j),j=1,k7)           
 1016 format (1h0,16hmscl(j) where j=,i4,45h  incremented by one until j
     1 is greater than ,i4,1h+,i4/(1h ,i5,i9,12i8))                     
      nmc(i)=k7                                                         
      do 26 j=1,k7                                                      
      k3=k1+j-1                                                         
      mscl(k3)=nos(j)                                                   
   26 nos(j)=0                                                          
      nsum=0                                                            
      msum=0                                                            
      if (int1(i).gt.nom) go to 29                                      
      k6=int1(i)-1                                                      
      if (k6.eq.0) go to 28                                             
      do 27 j=1,k6                                                      
   27 nsum=nsum+ncl(j)                                                  
   28 k6=int1(i)                                                        
      ndf1=ncl(k6)-1                                                    
      go to 34                                                          
   29 k6=int1(i)-nom                                                    
      ndf1=ncln(k6)-1                                                   
      k3=nom+1                                                          
      k4=int1(i)-1                                                      
      if (k4.lt.k3) go to 34                                            
      do 30 j=k3,k4                                                     
      k=j-nom                                                           
   30 nsum=nsum+ncln(k)                                                 
   34 if (int2(i).gt.nom) go to 38                                      
      k4=int2(i)-1                                                      
      if (k4.eq.0) go to 36                                             
      do 35 j=1,k4                                                      
   35 msum=msum+ncl(j)                                                  
   36 k6=int2(i)                                                        
      ndf2=ncl(k6)-1                                                    
      go to 41                                                          
   38 k6=int2(i)-nom                                                    
      ndf2=ncln(k6)-1                                                   
      k3=nom+1                                                          
      k4=int2(i)-1                                                      
      if (k4.lt.k3) go to 41                                            
      do 40 j=k3,k4                                                     
      k=j-nom                                                           
   40 msum=msum+ncln(k)                                                 
   41 if (k8.ne.1) go to 52                                             
      im(k8)=ndf1*ndf2-nmc(i)                                           
      j=1                                                               
      go to 53                                                          
   52 im(k8)=im(k8-1)+ndf1*ndf2-nmc(i)                                  
      j=im(k8-1)+1                                                      
c     ----------------------------------------------------              
c     sets up lab arrays to identify listings                           
c     ----------------------------------------------------              
   53 k8=k8+1                                                           
      do 48 k=1,ndf1                                                    
      do 48 l=1,ndf2                                                    
      k6=nsum+k                                                         
      k7=msum+l                                                         
      if (nmc(i).eq.0) go to 43                                         
      k5=k*100+l                                                        
      k3=0                                                              
      k4=k1-1                                                           
   42 k3=k3+1                                                           
      k4=k4+1                                                           
      if (k3.gt.nmc(i)) go to 43                                        
      if (k5.ne.mscl(k4)) go to 42                                      
      go to 48                                                          
   43 if (int1(i).gt.nom) go to 44                                      
      n=int1(i)                                                         
      lab1(j)=lit(n)                                                    
      call itoa (iden(k6),lab3(j))                                      
      go to 45                                                          
   44 n=int1(i)-nom                                                     
      lab1(j)=nlit(n)                                                   
      call itoa (nden(k6),lab3(j))                                      
   45 if (int2(i).gt.nom) go to 46                                      
      n=int2(i)                                                         
      lab2(j)=lit(n)                                                    
      call itoa (iden(k7),lab4(j))                                      
      go to 47                                                          
   46 n=int2(i)-nom                                                     
      lab2(j)=nlit(n)                                                   
      call itoa (nden(k7),lab4(j))                                      
   47 j=j+1                                                             
   48 continue                                                          
      k1=k1+nmc(i)                                                      
      nsc=nsc+(ndf1+1)*(ndf2+1)                                         
   49 k2=k2+nmc(i)                                                      
c     ----------------------------------------------------              
c     reads and lists parameter cards for regressions                   
c     ----------------------------------------------------              
   50 do 51 i=1,nsc                                                     
      ww(i)=0.0                                                         
   51 nos(i)=0                                                          
      ie=im(k8-1)+1                                                     
      if (k8.eq.1) ie=1                                                 
      if (npr.eq.0) go to 65                                            
      ntote=0                                                           
      k9=1                                                              
      nnn=0                                                             
      do 64 k=1,npr                                                     
      read (5,*) negx(k),loge(k),lqc(k),k4,lgtx(k),jbeg(k),ndecx(k),x
     1m(k),litr(k),k2,lad(k),(ms(j),j=1,k2)                             
 1017 format (2i1,3i2,i3,i2,e15.8,a6,23i2/17i2)                         
      if (lqc(k).gt.1.and.lad(k).eq.0) go to 909                        
      if (nab.eq.3.and.lad(k).eq.0) go to 910                           
      go to 54
  909 lad(k)=1                                                          
      write (6,1909)                                                    
 1909 format (1h0,5x,'** warning ** lqc>1 and lad=0.  lad has been set t
     *o 1.  adjustment is made to xm.'/)                                
      go to 54                                                          
  910 lad(k)=1                                                          
      write (6,1910)                                                    
 1910 format (1h0,'nab=3 and lad=0.  lad has been set to 1.  adjustment 
     *is made to xm.')                                                  
      go to 54                                                          
   54 if (nrn.gt.1.and.lad(k).eq.3.and.in.ne.5) xm(k)=x(k)              
      if(lparm.eq.1)                                                    
     *write (6,1018) k,negx(k),loge(k),lqc(k),k4,lgtx(k),jbeg(k),ndecx(k
     1),xm(k),litr(k),k2,lad(k)                                         
 1018 format (1h0,6hfor k=,i2,95h  negx(k) loge(k) lqc(k) nregp(k) lgtx(
     1k) jbeg(k) ndecx(k)     xm(k)     litr(k) iclr(k) lad(k)/1h ,12x, 
     2i2,3i8,i7,2i9,2x,e15.8,2x,a6,2x,i3,5x,i2)                         
      if(lparm.eq.1                                                     
     *  .and.k2.gt.0) write (6,2018) (ms(j),j=1,k2)                     
 2018 format (1h0,6hirm(i)/1h ,40i3)                                    
      iclr(k)=k2                                                        
      if (k2.eq.0) go to 108                                            
      ntote=ntote+k2                                                    
      if (ntote.gt.m40) go to 907                                        
      linout=linout+k2                                                  
      if (linout.gt.90) go to 908                                       
      do 110 i=1,k2                                                     
      irm(k9)=ms(i)                                                     
      lad(m40+10+k9)=0                                                      
      iclr(m40+10+k9)=0                                                     
      if (ms(i).le.nom) go to 110                                       
      j=ms(i)-nom                                                       
      do 119 l=1,k2                                                     
      if (ms(l).ne.nma(j)) go to 119                                    
      lad(m40+10+k9)=nma(j)                                                 
      iclr(m40+10+k9)=nmac(j)                                               
      n=ms(l)                                                           
      if (ncl(n).eq.nmac(j)) iclr(m40+10+k9)=99                             
  119 continue                                                          
  110 k9=k9+1                                                           
c     ----------------------------------------------------              
c     sets up lab arrays to identify listings                           
c     ----------------------------------------------------              
      do 102 j=1,k2                                                     
      nw(j)=1                                                           
      n=ms(j)                                                           
      if (n.gt.nom) go to 103                                           
      nr1(j)=n-1                                                        
      if (nr1(j).eq.0) go to 104                                        
      ns=nr1(j)                                                         
      do 105 l=1,ns                                                     
  105 nw(j)=nw(j)+ncl(l)                                                
  104 nr1(j)=ncl(n)                                                     
      go to 102                                                         
  103 ns=n-nom-1                                                        
      if (ns.eq.0) go to 107                                            
      do 106 l=1,ns                                                     
  106 nw(j)=nw(j)+ncln(l)                                               
  107 nr1(j)=ncln(ns+1)                                                 
  102 continue                                                          
  108 k3=lqc(k)                                                         
      do 63 j=1,k3                                                      
      im(k8)=im(k8-1)+1                                                 
      nnn=nnn+1                                                         
      nregp(nnn)=im(k8)                                                 
      if (k8.eq.1) im(k8)=1                                             
      i=im(k8)                                                          
      k8=k8+1                                                           
      lab1(i)=rgrsn                                                     
      lab2(i)=litr(k)                                                   
      lab4(i)=blank                                                     
      if (j-2) 60,61,62                                                 
   60 lab3(i)=linear                                                    
      go to 109                                                         
   61 lab3(i)=quad                                                      
      go to 109                                                         
   62 lab3(i)=cubic                                                     
  109 if (iclr(k).eq.0) go to 63                                        
      do 112 l=1,k2                                                     
      im(k8)=im(k8-1)+nr1(l)-1                                          
      m=k9-k2                                                           
      do 118 n=1,k2                                                     
      if (lad(m40+10+m).eq.ms(l)) lad(m40+10+m)=k8                              
      m=m+1                                                             
  118 continue                                                          
      k5=im(k8-1)+1                                                     
      k4=im(k8)                                                         
      k8=k8+1                                                           
      k7=nw(l)                                                          
      do 111 i=k5,k4                                                    
      lab3(i)=litr(k)                                                   
      n=ms(l)                                                           
      if (n.gt.nom) go to 113                                           
      lab2(i)=lit(n)                                                    
      call itoa (iden(k7),lab1(i))                                      
      go to 114                                                         
  113 k6=n-nom                                                          
      lab2(i)=nlit(k6)                                                  
      call itoa (nden(k7),lab1(i))                                      
  114 if (j-2) 115,116,117                                              
  115 lab4(i)=rgnl                                                      
      go to 111                                                         
  116 lab4(i)=rgnq                                                      
      go to 111                                                         
  117 lab4(i)=rgnc                                                      
  111 k7=k7+1                                                           
  112 continue                                                          
   63 continue                                                          
      if (xm(k).eq.0.0) go to 64                                        
      if (loge(k).eq.1) xm(k)=jlog10(xm(k))                             
   64 if (loge(k).eq.2) xm(k)=dsqrt(xm(k))                              
c     ----------------------------------------------------              
c     reads and lists parameter cards for rhm                           
c     ----------------------------------------------------              
   65 if (nrhm.eq.0) go to 67                                           
      do 66 i=1,nrhm                                                    
      read  (5,*)   negy(i),lny(i),lhy(i),kbeg(i),ndecy(i),ym(i),lity
     1(i)                                                               
 1019 format (2i1,i2,i3,i2,e15.8,a6)                                    
      if(lparm.eq.1)                                                    
     *write (6,1020) i,negy(i),lny(i),lhy(i),kbeg(i),ndecy(i),ym(i),lity
     1(i)                                                               
 1020 format (1h0,6hfor i=,i3,59h  negy(i) lny(i) lhy(i) kbeg(i) ndecy(i
     1)   ym(i)    lity(i)/ 1h ,12x,i2,2i7,2i8,2x,e15.8,1x,a6)          
      if (ym(i).eq.0.0) go to 66                                        
      if (lny(i).eq.1) ym(i)=jlog10(ym(i))                              
   66 if (lny(i).eq.2) ym(i)=dsqrt(ym(i))                               
   67 if (iran.eq.0) go to 68                                           
      k=1                                                               
      do 76 j=1,iran                                                    
      k3=m99+2-j                                                          
      read (5,*)  lab4(k3),i309(j),nr1(j),nw(j),l,(nos(i),i=1,l)     
 1023 format (a6,i1,i3,4i2)                                             
      if(lparm.eq.1)                                                    
     *write (6,1021) lab4(k3),i309(j),nr1(j),nw(j),l,(nos(i),i=1,l)     
 1021 format (1h0,'classification  i309 nr1   nw  ns2  ms(1) ms(2)'/(1h 
     1,4x,a6,5x,6i5))                                                   
      ns2(j)=l                                                          
      do 77 m=1,l                                                       
      k1=k+m-1                                                          
      ms(k1)=nos(m)                                                     
   77 nos(m)=0                                                          
   76 k=k+l                                                             
   68 if (mull.eq.0) go to 72                                           
      write (6,1027)                                                    
 1027 format (1h0,'job bombed out on a previous run')                   
      return                                                            
   72 if (n2f.eq.0) go to 75                                            
c     ----------------------------------------------------              
c     checks for consecutive numbering of all main effects              
c     ----------------------------------------------------              
      j=1                                                               
      if (nom.eq.0) go to 70                                            
      do 69 i=1,nom                                                     
      if (men(i).ne.j) go to 900                                        
   69 j=j+1                                                             
   70 if (non.eq.0) go to 75                                            
      do 71 i=1,non                                                     
      if (nen(i).ne.j) go to 900                                        
   71 j=j+1                                                             
      go to 75                                                          
  900 write (6,1022) ijob                                               
 1022 format (1h0,73hcodes for effects not consecutive---check parameter
     1 cards for problem no.,i3)                                        
      mull=1                                                            
   75 continue                                                          
      ns=k8-1                                                           
      return                                                            
  901 write (6,1901)                                                    
 1901 format (1h0,42hprogram size exceeded.  check limitations.)        
      go to 950                                                         
  902 write (6,1902)                                                    
 1902 format (1h0,60hincompatible absorption option and model type speci
     *fication.)                                                        
      go to 950                                                         
  903 write (6,1903)                                                    
 1903 format (1h0,54hprogram limitations exceeded on parameter card type
     * 1.)                                                              
      go to 950                                                         
  904 write (6,1904)                                                    
 1904 format (1h0,97hmaximum number of control columns exceeded for ncc,
     * nmjc, nint, or nmic on parameter card type 2.)                   
      go to 950                                                         
  905 write (6,1905)                                                    
 1905 format (1h0,92hmaximum number of sets of main and nested effects f
     *or which polynomials are fitted exceeded.)                        
      go to 950                                                         
  906 write (6,1906)                                                    
 1906 format (1h0,96hmaximum number of classes for main and nested effec
     *ts for which polynomials are fitted exceeded.)                    
      go to 950                                                         
  907 write (6,1907)                                                    
 1907 format (1h0,50hmaximum number of individual regressions exceeded.)
      go to 950                                                         
  908 write (6,1908)                                                    
 1908 format(1h0,51hmaximum number of lines allowed for anova exceeded.)
  950 mull=1                                                            
      return                                                            
      end                                                               
