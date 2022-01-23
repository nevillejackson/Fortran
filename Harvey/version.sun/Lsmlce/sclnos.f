      subroutine sclnos                                                 
c     -------------------------------------------                       
c     subroutine to list distribution of class and subclass numbers     
c     ------------------------------------------------------            
      include 'decl1'
      character*6 iblk,mis,misn,misi,id1,id2,miss
      include 'com1'
      include 'com2'
      include 'com3'
      include 'com4'
      data iblk,mis,misn,misi/6h      ,6hmis ni,6hid mis,6hmis id/      
      if(nrn.gt.1.and.liop.gt.11) go to 99                              
      write (6,1001) ijob                                               
 1001 format (1h0,60h  distribution of class and subclass numbers for pr
     1oblem no.,i4)                                                     
      if (ibet.eq.0) go to 1                                            
      write (6,1005)                                                    
 1005 format (1h0,19x,14hidentification,13x,3hno.,8x,14hsum of weights) 
      write (6,1007) ncds,swt1                                          
 1007 format (1h0,17x,5htotal,21x,i6,10x,f10.2)                         
      go to 2                                                           
    1 write (6,1002)                                                    
 1002 format (1h0,19x,14hidentification,13x,3hno.)                      
      write (6,1007) ncds                                               
    2 l=1                                                               
      k=ml+1                                                            
      miss=iblk                                                         
      k1=nmea+1                                                         
      if (k1.gt.nom) go to 17                                           
      do 16 i=k1,nom                                                    
      write (6,1000)                                                    
 1000 format (1h )                                                      
      k2=ncl(i)                                                         
      do 16 j=1,k2                                                      
      if (nos(l).ne.0) go to 10                                         
      miss=mis                                                          
      kput=2                                                            
   10 if (ibet.eq.0) go to 3                                            
      write (6,1003) lit(i),iden(k),nos(l),miss,ww(l)                   
      go to 4                                                           
    3 write (6,1003) lit(i),iden(k),nos(l),miss                         
 1003 format (1h ,17x,a6,i5,15x,i6,2x,a6,2x,f10.2)                      
    4 k=k+1                                                             
      miss=iblk                                                         
   16 l=l+1                                                             
   17 k=mlb+1                                                           
      k1=nnea+1                                                         
      if (k1.gt.non) go to 23                                           
      do 24 i=k1,non                                                    
      write (6,1000)                                                    
      nsum=0                                                            
      k2=ncln(i)                                                        
      do 22 j=1,k2                                                      
      nsum=nsum+nos(l)                                                  
      if (nos(l).ne.0) go to 20                                         
      miss=mis                                                          
      kput=2                                                            
   20 if (ibet.eq.0) go to 5                                            
      write (6,1003)nlit(i),nden(k),nos(l),miss,ww(l)                   
      go to 6                                                           
    5 write (6,1003) nlit(i),nden(k),nos(l),miss                        
    6 miss=iblk                                                         
      k=k+1                                                             
   22 l=l+1                                                             
      if (nma(i).le.nmea) go to 24                                      
      k6=nma(i)-1                                                       
      k4=0                                                              
      k5=nmea+1                                                         
      if (k6.lt.k5) go to 9                                             
      do 11 k3=k5,k6                                                    
   11 k4=k4+ncl(k3)                                                     
    9 k4=k4+nmac(i)                                                     
      if (nsum.eq.nos(k4)) go to 24                                     
      write (6,1006) nlit(i),nsum,nos(k4)                               
 1006 format (1h ,'sum of subclass numbers for  ',a6,'  is ',i5,'  and i
     1s not equal to ',i5,'  the main effect number')                   
   24 continue                                                          
   23 k6=0                                                              
      if (n2f.eq.0) go to 81                                            
      do 80 i=1,n2f                                                     
      write (6,1000)                                                    
      k6=k6+nmc(i)                                                      
      nsum=0                                                            
      msum=0                                                            
      if (int1(i).gt.nom) go to 34                                      
      k1=int1(i)-1                                                      
      if (k1.eq.0) go to 31                                             
      do 30 j=1,k1                                                      
   30 nsum=nsum+ncl(j)                                                  
   31 k1=int1(i)                                                        
      k3=ncl(k1)                                                        
      id1=lit(k1)                                                       
      go to 40                                                          
   34 k1=nom+1                                                          
      k2=int1(i)-1                                                      
      if (k1.gt.k2) go to 37                                            
      do 36 j=k1,k2                                                     
      k=j-nom                                                           
   36 nsum=nsum+ncln(k)                                                 
   37 k1=int1(i)-nom                                                    
      k3=ncln(k1)                                                       
      id1=nlit(k1)                                                      
   40 if (int2(i).gt.nom) go to 46                                      
      k2=int2(i)-1                                                      
      if (k2.eq.0) go to 43                                             
      do 42 j=1,k2                                                      
   42 msum=msum+ncl(j)                                                  
   43 k2=int2(i)                                                        
      k4=ncl(k2)                                                        
      id2=lit(k2)                                                       
      go to 50                                                          
   46 k1=nom+1                                                          
      k2=int2(i)-1                                                      
      if (k1.gt.k2) go to 49                                            
      do 48 j=k1,k2                                                     
      k=j-nom                                                           
   48 msum=msum+ncln(k)                                                 
   49 k1=int2(i)-nom                                                    
      k4=ncln(k1)                                                       
      id2=nlit(k1)                                                      
   50 do 80 j=1,k3                                                      
      k2=nsum+j                                                         
      if (int1(i).gt.nom) go to 54                                      
      id3=iden(k2)                                                      
      go to 56                                                          
   54 id3=nden(k2)                                                      
   56 do 80 k=1,k4                                                      
      k5=msum+k                                                         
      if (nos(l).ne.0.and.nmc(i).eq.0) go to 70                         
      miss=mis                                                          
      if (nmc(i).eq.0.and.nos(l).eq.0) kput=2                           
      if (nmc(i).eq.0) go to 70                                         
      k7=j*100+k                                                        
      k1=k6-nmc(i)                                                      
   60 k1=k1+1                                                           
      if (k1.gt.k6) go to 68                                            
      if (k7-mscl(k1)) 60,64,60                                         
   64 if (nos(l).eq.0) miss=misi                                        
      if (nos(l).ne.0) miss=misn                                        
      if (j.eq.k3.or.k.eq.k4) kput=2                                    
      go to 70                                                          
   68 if (nos(l).ne.0) miss=iblk                                        
      if (nos(l).eq.0) kput=2                                           
   70 if (int2(i).gt.nom) go to 74                                      
      id4=iden(k5)                                                      
      go to 76                                                          
   74 id4=nden(k5)                                                      
   76 if (ibet.eq.0) go to 7                                            
      write (6,1004) id1,id2,id3,id4,nos(l),miss,ww(l)                  
      go to 8                                                           
    7 write (6,1004) id1,id2,id3,id4,nos(l),miss                        
 1004 format (1h ,17x,a6,3h x ,a6,2i5,i7,2x,a6,2x,f10.2)                
    8 miss=iblk                                                         
   80 l=l+1                                                             
   81 ncc=l-1                                                           
      return                                                            
   99 k1=nmea+1                                                         
      l=1                                                               
      if(k1.gt.nom) go to 102                                           
      do 100 i=k1,nom                                                   
      k2=ncl(i)                                                         
      do 100 j=1,k2                                                     
  100 l=l+1                                                             
  102 k1=nnea+1                                                         
      if(k1.gt.non) go to 103                                           
      do 104 i=k1,non                                                   
      k2=ncln(i)                                                        
      do 104 j=1,k2                                                     
  104 l=l+1                                                             
  103 if(n2f.eq.0) go to 81                                             
      do 107 i=1,n2f                                                    
      if(int1(i).gt.nom) go to 108                                      
      k1=int1(i)                                                        
      k3=ncl(k1)                                                        
      go to 109                                                         
  108 k1=int1(i)-nom                                                    
      k3=ncln(k1)                                                       
  109 if(int2(i).gt.nom) go to 110                                      
      k2=int2(i)                                                        
      k4=ncl(k2)                                                        
      go to 111                                                         
  110 k1=int2(i)-nom                                                    
      k4=ncln(k1)                                                       
  111 do 107 j=1,k3                                                     
      do 107 k=1,k4                                                     
  107 l=l+1                                                             
      go to 81                                                          
      end                                                               
