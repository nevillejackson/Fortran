      subroutine tgen (nsize,kk1,kk2,jj1,jj2,i1,i2,ii,mscl,t)           
c      -------------------------------------------------                
      include 'decl1'
      dimension t(*),mscl(*)                                        
      do 10 i=1,nsize                                                   
   10 t(i)=0.0                                                          
      if (i1.eq.kk1) go to 20                                           
      t(i1)=1.0                                                         
      if (i1.ne.kk1) go to 40                                           
   20 do 30 i=1,jj1                                                     
   30 t(i)=-1.0                                                         
   40 i=jj1+i2                                                          
      t(i)=1.0                                                          
      if (i2.ne.kk2) go to 60                                           
      j=jj1+1                                                           
      jj=jj1+jj2                                                        
      do 50 i=j,jj                                                      
   50 t(i)=-1.0                                                         
   60 i=jj1+jj2                                                         
      do 90 j=1,jj1                                                     
      do 90 jj=1,jj2                                                    
      if (ii.eq.0) go to 80                                             
      indx=j*100+jj                                                     
      do 70 jjj=1,ii                                                    
      if (indx.eq.mscl(jjj)) go to 90                                   
   70 continue                                                          
   80 i=i+1                                                             
      t(i)=t(j)*t(jj+jj1)                                               
   90 continue                                                          
      return                                                            
      end                                                               
