      subroutine intact (me1,me2,ni,nj,n2f,not,int1,int2,x,nmc,mscl,ncl,
     1eff1,eff2,im,ncln,nom)                                            
c     -------------------------------------------                       
c     subroutine which checks for the presence of constants for an      
c     interaction and sets ones, zeros and minus ones into the x array  
c     ------------------------------------------------------            
      include 'decl1'
      dimension int1(*),int2(*),x(*),nmc(*),mscl(*),ncl(*),eff1(*)
     1 ,eff2(*),im(*),ncln(*)                                      
      n=me1*100+me2                                                     
      int=me2*100+me1                                                   
      j=0                                                               
      m=0                                                               
    1 m=m+1                                                             
      if (m.gt.n2f) go to 9                                             
      i=int1(m)*100+int2(m)                                             
      if (n.eq.i) go to 2                                               
      if (int.eq.i) go to 8                                             
      go to 1                                                           
    8 j=1                                                               
    2 int=m                                                             
      if (me1.gt.nom) go to 30                                          
      n1=ncl(me1)                                                       
      go to 31                                                          
   30 i=me1-nom                                                         
      n1=ncln(i)                                                        
   31 if (me2.gt.nom) go to 32                                          
      n2=ncl(me2)                                                       
      go to 33                                                          
   32 i=me2-nom                                                         
      n2=ncln(i)                                                        
   33 n=not+m-1                                                         
      ib=im(n)+1                                                        
      ms1=0                                                             
      n=m-1                                                             
      if (n.eq.0) go to 14                                              
      do 11 i=1,n                                                       
   11 ms1=ms1+nmc(i)                                                    
   14 if (j.eq.1) go to 3                                               
      ncl1=n1                                                           
      ncl2=n2                                                           
      n1=ni                                                             
      n2=nj                                                             
      go to 12                                                          
    3 ncl1=n2                                                           
      ncl2=n1                                                           
      n1=nj                                                             
      n2=ni                                                             
   12 l=ncl1-1                                                          
      m=ncl2-1                                                          
      ncel=l*m-nmc(int)                                                 
      nend=ib+ncel-1                                                    
      do 15 i=ib,nend                                                   
   15 x(i)=0.                                                           
      do 16 i=1,l                                                       
   16 eff1(i)=0.                                                        
      do 17 i=1,m                                                       
   17 eff2(i)=0.                                                        
      if (n1.eq.ncl1) go to 18                                          
      eff1(n1)=1.                                                       
      go to 19                                                          
   18 do 20 i=1,l                                                       
   20 eff1(i)=-1.                                                       
   19 if (n2.eq.ncl2) go to 21                                          
      eff2(n2)=1.                                                       
      go to 22                                                          
   21 do 23 i=1,m                                                       
   23 eff2(i)=-1.                                                       
   22 do 24 i=1,l                                                       
      do 25 j=1,m                                                       
      k=i*100+j                                                         
      if (nmc(int).eq.0) go to 26                                       
      k1=ms1+nmc(int)                                                   
   27 ms1=ms1+1                                                         
      if (ms1.gt.k1) go to 26                                           
      if (k-mscl(ms1)) 27,25,27                                         
   26 x(ib)=eff1(i)*eff2(j)                                             
      ib =ib+1                                                          
   25 continue                                                          
   24 continue                                                          
    9 return                                                            
      end                                                               
