      subroutine vector (nn,kkk,n1,n2,v1,v2,v)                          
c     --------------------------------------------------                
c     subroutine to form contrast vectors                               
c     --------------------------------------------------                
      include 'decl1'
c----- original dimension
c     dimension v(*),v1(*),v2(*),vt(106)                          
c----- current dimension
      dimension v(*),v1(*),v2(*),vt(207)                          
c-----
c                                                                       
      n12=n1+n2                                                         
c     --------------------------------------------------                
c     form vector for first main effect                                 
c     --------------------------------------------------                
      if (nn.gt.n1) go to 200                                           
      do 110 i=1,kkk                                                    
  110 vt(i)=v1(i)**nn                                                   
c                                                                       
      kk=kkk-1                                                          
      dkk=kk                                                            
      dkkk=kkk                                                          
c                                                                       
      do 140 i=1,kk                                                     
      v(i)=0                                                            
      do 130 j=1,kkk                                                    
      if (i.eq.j) go to 120                                             
      v(i)=v(i)-vt(j)/dkkk                                              
      go to 130                                                         
  120 v(i)=v(i)+vt(j)*dkk/dkkk                                          
  130 continue                                                          
  140 continue                                                          
      return                                                            
c     --------------------------------------------------                
c     form vector for second main effect                                
c     --------------------------------------------------                
  200 if (nn.gt.n12) go to 300                                          
      kk=nn-n1                                                          
      do 210 i=1,kkk                                                    
  210 vt(i)=v2(i)**kk                                                   
c                                                                       
      kk=kkk-1                                                          
      dkk=kk                                                            
      dkkk=kkk                                                          
c                                                                       
      do 240 i=1,kk                                                     
      v(i)=0                                                            
      do 230 j=1,kkk                                                    
      if (i.eq.j) go to 220                                             
      v(i)=v(i)-vt(j)/dkkk                                              
      go to 230                                                         
  220 v(i)=v(i)+vt(j)*dkk/dkkk                                          
  230 continue                                                          
  240 continue                                                          
      return                                                            
c     --------------------------------------------------                
c     form vector for interaction effects                               
c     --------------------------------------------------                
  300 kk=nn-n12                                                         
      do 320 i=1,n1                                                     
      k1=i                                                              
      do 310 j=1,n2                                                     
      k2=j                                                              
      i12=j+n2*(i-1)                                                    
      if (kk.eq.i12) go to 330                                          
  310 continue                                                          
  320 continue                                                          
  330 do 340 i=1,kkk                                                    
  340 vt(i)=v1(i)**k1*v2(i)**k2                                         
c                                                                       
      kk=kkk-1                                                          
      dkk=kk                                                            
      dkkk=kkk                                                          
c                                                                       
      do 370 i=1,kk                                                     
      v(i)=0                                                            
      do 360 j=1,kkk                                                    
      if (i.eq.j) go to 350                                             
      v(i)=v(i)-vt(j)/dkkk                                              
      go to 360                                                         
  350 v(i)=v(i)+vt(j)*dkk/dkkk                                          
  360 continue                                                          
  370 continue                                                          
c                                                                       
      return                                                            
      end                                                               
