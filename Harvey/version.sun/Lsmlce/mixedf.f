      subroutine mixedf(matx,kb,nlhm,array,fab,tot4,sof,nx,kd,liop)     
c     ------------------------------------------------------            
c     subroutine to complete computations and listing of sums of        
c     squares, k values, etc for mixed models                           
c     ------------------------------------------------------            
      include 'decl1'
      include 'com9'
      dimension fab(*),tot4(*),array(*)                         
      sum=0.0                                                           
      if (nlhm.eq.0) go to 5                                            
      do 27 i=1,nlhm                                                    
      k=nlhm*(i-1)-i*(i-3)/2                                            
      do 27 j=i,nlhm                                                    
      if (kd.gt.0.and.i.eq.1) go to 27                                  
      if (i-j.lt.0) go to 28                                            
      sum=sum+array(k)*fab(k)                                           
      go to 27                                                          
   28 k1=k+j-i                                                          
      sum=sum+array(k1)*fab(k1)*2.                                      
   27 continue                                                          
    5 ak=sum+sof                                                        
      write (6,1) nx,ak,sof,sum                                         
    1 format (1h0,3hns=,i4,3x,6hk--ab=,f14.6,4x,9htr d-1 u=,f14.6,4x,27h
     1tr c-1 (reduced f matrix) =,f14.6)                                
      if (kd.eq.1) tot4(m99+1)=ak                                         
      if (kd.eq.1) tot4(m99+5)=sof                                        
      if (kd.eq.2) tot4(m99+4)=ak                                         
      if (kd.eq.2) tot4(m99+7)=sof                                        
      if (kb.eq.1) tot4(m99+2)=ak                                         
      if (nlhm.eq.0.or.liop.ge.10) return                               
      write (6,2)                                                       
    2 format (1h0,20x,31hlisting of the reduced f matrix)               
      write (6,3)                                                       
    3 format (1h0)                                                      
      write (6,4) (fab(i),i=1,matx)                                     
    4 format (7(1x,e15.8))                                              
      return                                                            
      end                                                               
