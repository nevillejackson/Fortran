      subroutine candse(ac,repac,als,rep,tot2,x,array,nr,nlhm,wk,yt,i,  
     *sss,nct)                                                          
c     -------------------------------------------                       
c     subroutine which computes constant, ls mean and standard errors   
c     from the x, tot2 and array arrays                                 
c     -----------------------------------------------------             
      include 'decl1'
      include 'com9'
      dimension x(*),array(*),tot2(*),sss(*)                   
      als=0.                                                            
      ac=0.                                                             
      do 100 l=1,nlhm                                                   
      if (x(l).eq.0.) go to 100                                         
      als=als+x(l)*array(nr)                                            
      ac=ac+tot2(l)*array(nr)                                           
  100 nr=nr+1                                                           
      als=als+yt                                                        
      if (i.gt.1) go to 105                                             
      rep=0.                                                            
      repac=0.                                                          
      do 101 j=1,nlhm                                                   
      temp=0.                                                           
      tempac=0.                                                         
      do 102 k=1,nlhm                                                   
      if (x(k).eq.0.) go to 102                                         
      if (j-k.lt.0) go to 103                                           
      k1=nlhm*(k-1)-k*(k-3)/2+j-k                                       
      go to 104                                                         
  103 k1=nlhm*(j-1)-j*(j-3)/2+k-j                                       
  104 temp=temp+x(k)*array(k1)                                          
      tempac=tempac+tot2(k)*array(k1)                                   
  102 continue                                                          
      if (x(j).eq.0.) go to 101                                         
      rep=rep+x(j)*temp                                                 
      repac=repac+tot2(j)*tempac                                        
  101 continue                                                          
      sss(nct)=rep                                                      
      sss(nct+m300)=repac                                                
      nct=nct+1                                                         
      rep=rep*wk                                                        
      repac=repac*wk                                                    
      if (rep.lt.0.0) rep=0.0                                           
      if (repac.lt.0.0) repac=0.0                                       
      rep=dsqrt(rep)                                                    
      repac=dsqrt(repac)                                                
      return                                                            
  105 rep=sss(nct)*wk                                                   
      repac=sss(nct+m300)*wk                                             
      if (rep.lt.0.0) rep=0.0                                           
      if (repac.lt.0.0) repac=0.0                                       
      rep=dsqrt(rep)                                                    
      repac=dsqrt(repac)                                                
      nct=nct+1                                                         
      return                                                            
      end                                                               
