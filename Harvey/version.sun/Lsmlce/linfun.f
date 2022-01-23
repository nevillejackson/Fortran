      subroutine linfun(nlc,nlhm,x,jbeg,tot2,nrhm,matx,ncpr,sscpr,tred,e
     *df,xp,yp,ac,als,rep,array,sss,nab,lity,mn2,ym,mty,rr,wtt)         
c     ------------------------------------------------------            
c     subroutine to compute linear functions                            
c     ------------------------------------------------------            
      include 'decl1'
      character*13 a123
      character*6 lity(*)
      dimension x(*),jbeg(*),tot2(*),sscpr(*),xp(*),yp(*),array(*)
     1 ,sss(*),tred(*),ym(*)                          
      write (6,1051)                                                    
 1051 format (1h0,23x,66hlisting of selected linear functions, standard 
     1errors and f values)                                              
      write (6,1052)                                                    
 1052 format (1h0,2x,11hdescription,2x,12herr t  no.  ,84hr(1) c(1) r(2)
     1 c(2) r(3) c(3) r(4) c(4) r(5) c(5) r(6) c(6) r(7) c(7) r(8) c(8) 
     2etc.)                                                             
      do 301 k1=1,nlc                                                   
      read (5,*) a123,kb,nerc,(jbeg(j),tot2(j),j=1,nerc)         
 1050 format (a,2i2,3x,6(i2,f8.3)/(8(i2,f8.3)))                    
      if (mty.eq.1) kb=1                                                
      if (kb.eq.0.or.kb.gt.3) kb=1                                      
      write (6,1053) a123,kb,nerc,(jbeg(j),tot2(j),j=1,nerc)        
 1053 format (1h0,a,2x,i2,5x,i2,5x,6(i2,2x,f8.3,2x)/(17x,8(i2,2x,f8
     1.3,2x)))                                                          
      write (6,1004)                                                    
 1004 format (1h )                                                      
      do 302 j=1,nlhm                                                   
  302 x(j)=0.                                                           
      do 303 j=1,nerc                                                   
      k=jbeg(j)                                                         
  303 x(k)=tot2(j)                                                      
      do 309 j=1,nlhm                                                   
  309 tot2(j)=0.                                                        
      yt=0.0                                                            
      do 304 i=1,nrhm                                                   
      nct=1                                                             
      nr=matx+(i-1)*nlhm+1                                              
      if (ncpr.eq.1) go to 305                                          
      k4=i                                                              
      go to 306                                                         
  305 k4=nrhm*(i-1)-i*(i-3)/2                                           
  306 if (kb.eq.1) wk=(sscpr(k4)-tred(i))/edf                           
      if (kb.eq.1.and.mn2.eq.1) wk=sscpr(k4)                            
      if (kb.eq.2) wk=xp(i)                                             
      if (kb.eq.3) wk=yp(i)                                             
      if (wk.lt.0.) go to 300                                           
      call candse (ac,repac,als,rep,tot2,x,array,nr,nlhm,wk,yt,i,sss,   
     *nct)                                                              
      if (nab.eq.0.or.nab.eq.3) als=als+x(1)*ym(i)                      
      f=(als/rep)*(als/rep)                                             
      ssq=f*wk                                                          
      if (kb.eq.1) df=edf                                               
      if (kb.eq.2) df=rr                                                
      if (kb.eq.3) df=wtt                                               
      probf=prof(1,df,f)                                                
  304 write (6,1054) lity(i),als,rep,ssq,f,probf                        
  301 continue                                                          
 1054 format (1h ,a6,4x,14hlinear funct =,f14.6,3x,9hstd err =,f14.6,3x,
     * 4hss =,f14.6,3x,3hf =,f14.6,3x,6hprob =,f6.3)                    
      return                                                            
  300 write (6,1021)                                                    
 1021 format (1h0,10x,29hnegative error sum of squares)                 
      return                                                            
      end                                                               
