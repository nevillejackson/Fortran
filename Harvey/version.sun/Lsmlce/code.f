      subroutine code                                                   
c     -------------------------------------------                       
c     subroutine used to get data from card                             
c     code parameters (ml3,intv,nom,ncl,ibeg,lme,ncln,nbeg,lne,ic,iden, 
c     nden,ml,k1,icod,isw2,isw3,l,l7,ijob)                              
c     ----------------------------------------------------              
      include 'decl1'
      include 'com2'
      include 'com3'
      include 'com4'
      include 'com6'
      isw2=0                                                            
      isw3=0                                                            
      ml3=0                                                             
      intvm1=intv-1                                                     
      if (intv.gt.nom) go to 6                                          
      k1=ncl(intv)                                                      
      l7=ibeg(intv)                                                     
      isw=0                                                             
      l=lme(intv)+l7-1                                                  
      if (intvm1.eq.0) go to 12                                         
      do 4 j=1,intvm1                                                   
    4 ml3=ncl(j)+ml3                                                    
      go to 12                                                          
    6 k2=nom+1                                                          
      if (k2.gt.intvm1) go to 9                                         
      do 8 j=k2,intvm1                                                  
      k3=j-nom                                                          
    8 ml3=ncln(k3)+ml3                                                  
    9 kix=intv-nom                                                      
      l7=nbeg(kix)                                                      
      k1=ncln(kix)                                                      
      l=lne(kix)+l7-1                                                   
      isw=1                                                             
   12 k=k1+ml3                                                          
      j=ncds                                                            
      call field (icod,l,ic,l7,j)                                       
      l=0                                                               
      if (j.eq.1) go to 50                                              
      ml1=ml3                                                           
   16 ml3=ml3+1                                                         
      if (isw.eq.1) go to 20                                            
      if (ml3.gt.k) go to 900                                           
      if (icod.ne.iden(ml3)) go to 16                                   
      go to 26                                                          
   20 if (ml3.le.k) go to 24                                            
      isw2=1                                                            
      go to 49                                                          
   24 if (icod.ne.nden(ml3)) go to 16                                   
   26 ml3=ml3-ml1                                                       
      go to 49                                                          
  900 write (6,1000) ncds,intv,icod,ijob                                
 1000 format ('0class code missing on card no.',i8,5x,'effect',i3,' cann
     8ot find code ',i5,'. check parameter cards for problem no.',i3)   
      isw3=1                                                            
   49 return                                                            
   50 l=1                                                               
      return                                                            
      end                                                               
