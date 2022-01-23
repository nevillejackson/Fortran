      subroutine svcvc                                                  
c     -------------------------------------------                       
c     subroutine used to compute variance and covariance components,    
c     heritabilities, genetic, phenotypic and environmental correlations
c     and standard errors                                               
c     ----------------------------------------------------              
      include 'decl1'
      include 'com1'
      include 'com2'
      include 'com3'
      include 'com4'
      character*80 fnam
      logical mess
c----- open units for save option - P,G,E statistics
c--    1=cov's  2=correlations
      if (nsave.eq.1.or.nsave.eq.2) then
c-      P
        call argopf(14,fnam,4,'new','formatted')
c-      G
        call argopf(15,fnam,5,'new','formatted')
c-      E
        call argopf(16,fnam,6,'new','formatted')
      endif
c-----
c----- save option 3 -- between & within cov matrices
c-----
      if (nsave.eq.3) then
c-----  between is sss()
        call argopf (14,fnam,4,'new','unformatted')
        call bput(14,fnam,sss,nrhm,nrhm,-nrhm,mess)
        if(mess) stop
c-----  within is sscpr()
        call argopf(15,fnam,5,'new','unformatted')
        call bput(15,fnam,sscpr,nrhm,nrhm,-nrhm,mess)
        if(mess) stop
      endif
c-----
      w=nw(l)                                                           
      r1=nr1(l)                                                         
      r1=r1/100.                                                        
      w=w/100.                                                          
      c=1./r1                                                           
      d=(1.-w)/r1                                                       
      e=w/r1                                                            
      tn=(sdf+1.)*wk                                                    
      sk3=wk-1.                                                         
      sk4=wk*wk                                                         
      c1=(c*c*2.)*(tn-1.)                                               
      c2=sk4*sdf                                                        
      c3=sk4*edf                                                        
c----- edf is residual df - ie for within MS
c----- sscpr()  is within Cov component
c----- sdf is random effect df - ie for between or random effect MS
c----- sss() is between Cov component
      sk5=2.*sk3                                                        
      c4=c3*sdf                                                         
      ident=0                                                           
      write (6,1003)                                                    
 1003 format (1h0,15x,'negative variance component estimates set to zero
     1 for these computations')                                         
      write (6,1000)                                                    
 1000 format (1h0/1h ,76hestimates of heritabilities, genetic, phenotypi
     1c, environmental correlations//1h ,' job row col   rhm    rhm  '
     2' heritability standard  phenotypic standard  environmental '
     +'standard neg   variance '
     3'or cov components'/ 1h ,28x,'or genetic r error     correlation'
     4' error    correlation  error    ',10x,5hamong,12x,6hwithin)                            
      do 60 i=1,nrhm                                                    
      write (6,1001)                                                    
 1001 format (1h )                                                      
      do 60 j=i,nrhm                                                    
      if (i-j) 16,12,12                                                 
c----- i.eq.j
   12 k=nrhm*(i-1)-i*(i-3)/2                                            
      h=(sss(k)*c)/(d*sss(k)+sscpr(k))                                  
      t=sss(k)/(sss(k)+sscpr(k))                                        
      t1=1.-t                                                           
      seh=dsqrt((c1*t1*t1*(1.+(sk3*t))*(1.+(sk3*t)))/c4)                
      rp=1.                                                             
      re=1.                                                             
      serp=0.d0
      sere=0.d0
      neg=0                                                             
c----- cov matrices - diagonal element
      covp=d*sss(k) + sscpr(k)
      covg=c*sss(k)
      cove=covp-covg
c-----
      go to 46                                                          
c----- i.lt.j
   16 k=nrhm*(i-1)-i*(i-3)/2                                            
      k1=nrhm*(j-1)-j*(j-3)/2                                           
      k2=k+j-i                                                          
c----- k is i,i index
c----- k1 is j,j index
c----- k2 is i,j index
c----- cov matrices - off-diag element
      covp=d*sss(k2) + sscpr(k2)
      covg=c*sss(k2)
      cove=covp-covg
c----- rg
      sum=sss(k)*sss(k1)                                                
      if (sum) 18,18,20                                                 
   18 h=0.d0                                                             
      seh=0.d0                                                            
      go to 22                                                          
   20 sum=dsqrt(sum)                                                    
      h=sss(k2)/sum                                                     
   22 dem1=(sscpr(k)+(d*sss(k)))*(sscpr(k1)+(d*sss(k1)))                
      if (dem1.gt.0.d0) go to 26                                         
   24 rp=0.d0                                                            
      serp=0.d0
      go to 28                                                          
   26 rp=(sscpr(k2)+(d*sss(k2)))/dsqrt(dem1)                            
      serp=dsqrt((1.d0-rp*rp)/(sdf+edf-1.d0))
   28 rg=h*h                                                            
      if (h) 30,40,30                                                   
   30 t2=sss(k1)/(sss(k1)+sscpr(k1))                                    
      t3=t*t2                                                           
      if (d-1.) 32,34,32                                                
   32 rp1=(sscpr(k2)+sss(k2))/dsqrt((sscpr(k)+sss(k))*(sscpr(k1)+sss(k1)
     1))                                                                
      go to 36                                                          
   34 rp1=rp                                                            
   36 a1=1.+rg                                                          
      b1=dsqrt(t3*rg)                                                   
      c5=(t+t2)/t3                                                      
      d1=(rg*((t-t2)**2))/(2.*t3)                                       
      tk=(1.+(sk3*t))*(1.+(sk3*t2))                                     
      rpb=rp1+(sk3*b1)                                                  
      r=rp1-b1                                                          
      serg1=((a1*(tk+(rpb*rpb)))-(2.*b1*rpb*(c5+sk5))+d1)/(c2*t3)       
      serg2=((a1*(t1*(1.-t2)+(r**2)))-(2.*b1*r*(c5-2.))+d1)/(c3*t3)     
      c5=serg1+serg2                                                    
      if (c5.lt.0.d0) c5=0.d0                                             
      seh=dsqrt(c5)                                                     
c----- re
   40 dem2=(sscpr(k)-(e*sss(k)))*(sscpr(k1)-(e*sss(k1)))                
      if (dem2) 42,44,44                                                
c----- VEi * VEj negative
   42 dem2=-dem2                                                        
      re=(sscpr(k2)-(e*sss(k2)))/(dsqrt(dem2)*(-1.))                    
      sere=0.d0
      neg=1                                                             
      go to 48                                                          
c----- VEi * VEj positive
   44 re=(sscpr(k2)-(e*sss(k2)))/dsqrt(dem2)                            
c-----
      hi=(sss(k)*c)/(d*sss(k)+sscpr(k))
      hj=(sss(k1)*c)/(d*sss(k1)+sscpr(k1))
      able=(1.d0-hi)*(1.d0-hj)
      if(able.gt.0.d0) then
        sere=(serp*serp + hi*hj*seh*seh)
        if(sere .gt. 0.d0) then
          sere=dsqrt(sere/able)
        else
          sere=0.d0
        endif
      else
        sere=0.d0
      endif
c-----
      neg=0                                                             
      go to 48                                                          
   46 k2=k                                                              
   48 if (h.eq.0.d0) then
   49 write (6,1004) ijob,i,j,lity(i),lity(j),rp,neg,sss(k2),sscpr(k2)  
 1004 format (1h ,3i4,2x,a6,1x,a6,20x,f10.3,30x,i9,2f18.8)              
        else
      write (6,1002) ijob,i,j,lity(i),lity(j),h,seh,rp,serp,re,sere,neg,
     1 sss(k2),sscpr(k2)                                                           
 1002 format (1h ,3i4,2x,a6,1x,a6,6f10.3,i9,2f18.8)                     
        endif
c----- save option
      if (nsave.eq.1) then
        write(14,2000) i,j,covp
 2000   format(2i5,f30.15)
        write(15,2000) i,j,covg
        write(16,2000) i,j,cove
      else if(nsave.eq.2) then
        if(i.eq.j) then
          write(14,2000) i,j,covp
          cove=1.d0-h
          write(16,2000) i,j,cove
        else
          write(14,2000) i,j,rp
          write(16,2000) i,j,re
        endif
        write(15,2000) i,j,h
      endif
c-----
   60 continue                                                          
      return                                                            
      end                                                               
