      subroutine unresa(liop,ntr,new,ndg,nw,ntr4,p,g,h,sig,
     +a,dg,label,pp,ginv,x,bu,cyi,dgy,rgyi,head,head2,l,
     +ini,inh,inf,ino,nh,nf,no)
c-----solve unrestricted index all traits
c-----unrestricted indices - new+ndg=nw off
      implicit double precision (a-h,o-z)
      character*10 label(mvntr)
      character*100 head,head2
      dimension ini(mvntr),inh(mvntr),inf(mvntr),ino(mvntr)
      dimension p(mvntr4,mvntr4),g(mvntr4,mvntr4),h(mvntr4),sig(mvntr4)
      dimension a(mvnw,mvntr),dg(mvndg,mvntr)
      dimension ginv(mvntr4,mvntr4),pp(mvntr4,mvntr4),x(mvntr4,mvntr4)
      dimension bu(mvnw,mvntr4)
      dimension cyi(mvntr4),dgy(mvntr4),rgyi(mvntr4),l(mvntr)
      logical ok,exsing
      common /limits/ mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      common /stdio/ lc,lp,lerr
c-----store p in pp & g in ginv before inversion
      do 21 i=1,ntr4
         do 21 j=1,ntr4
            ginv(i,j)=g(i,j)
   21 pp(i,j)=p(i,j)
c-----calc p inverse
      do 7 i=1,ntr4
         call pivot(p,ntr4,i,liop,ok)
         if(.not.ok) then
            if(.not.exsing(ini,inh,inf,ino,ntr,i)) then
               write(lp,3125) i
 3125          format(' unexpected singularity at row ',i3,
     +        ' of phenotypic covariance matrix'/
     +        ' ---> check phenotypic correlations for dependencies',
     +        ' or variances for ill-condition')
               stop
            endif
         endif
    7 continue
      if(liop) 301,301,300
  300 continue
      write(lp,44)
   44 format(29h0inverse ph covariance matrix)
      do 45 i=1,ntr4
   45 write(lp,35)(p(i,j),j=1,i)
   35 format(' ',5f20.4)
  301 continue
c-----mult p-inverse by g
      do8i=1,ntr4
      do8j=1,ntr4
      sum=0.0
      do9k=1,ntr4
    9 sum=sum+p(i,k)*g(k,j)
    8 x(i,j)=sum
      if(liop) 303,303,302
  302 continue
      write(lp,46)
   46 format(18h0p-inverse * g = x)
      do 47 i=1,ntr4
   47 write(lp,35)(x(i,j),j=1,ntr4)
  303 continue
c-----invert g cov matrix
      do 1450 i=1,ntr4
         call pivot(ginv,ntr4,i,liop,ok)
         if(.not.ok) then
            if(.not.exsing(ini,inh,inf,ino,ntr,i)) then
               write(lp,3150) i
 3150          format(' unexpected singularity at row ',i3,
     +      ' of genetic covariance matrix'/
     +      ' ---> check genetic correlations for dependencies',
     +      ' or variances for ill condition')
               stop
            endif
         endif
 1450 continue
      if(liop) 1451,1451,1452
 1452 write(lp,1453)
 1453 format('0g-inverse')
      do 1454 i=1,ntr4
 1454 write(lp,35)(ginv(i,j),j=1,ntr4)
 1451 continue
c-----loop over economic wts for unrestricted index
      nw=new+ndg
      idg=0
      do 998 inew=1,nw
         if(liop.eq.1) then
            write(lp,38)
   38       format(/10x,10('----------'))
            write(lp,20) inew
   20       format(/' economic weights set ',i3)
         endif
         if(inew.le.new) go to 1400
c-----convert desired gains to implied weights
         idg=idg+1
         if(liop.eq.1) then
            write(lp,1410)(dg(idg,i),i=1,ntr)
 1410       format('0desired gains'/' ',5f20.4/(' ',5f20.4))
            write(lp,1411)
 1411       format('0weights')
         endif
         call weight(new,ndg,idg,a,ginv,pp,dg,ntr,ntr4)
c-----idg is current desired gain
 1400    continue
         do 13 i=1,ntr4
c-----calc index coefficients
            sum=0.0
            do 14 k=1,ntr
   14       sum=sum+x(i,k)*a(inew,k)
   13    bu(inew,i)=sum
c-----calc cov(hi) and gain
         chi=0.0
         do 15 i=1,ntr
            do 15 j=1,ntr4
   15    chi=chi+a(inew,i)*g(i,j)*bu(inew,j)
         gain=sqrtf(chi)
         if(liop.eq.1) then
   10       format(10f4.0)
            write(lp,35)(a(inew,i),i=1,ntr)
            write(lp,23)(bu(inew,i),i=1,ntr4)
            write(lp,22) gain,chi
   23       format(/'  coefficients for unrestricted index',/1h0,5f20.4/
     +      (1h ,5f20.4))
   22       format(/' genetic gain for unrestricted index in economic '
     +      /'weight units per unit selection differential per '
     +      ,'generation =',  f15.4/      ,' cov(hi) =',f20.4)
         endif
c-----calculate v(i) , v(h) , r(ih)
         vi=0.0
         vh=0.0
         do 120 i=1,ntr
            do 120 j=1,ntr
  120    vh=vh+a(inew,i)*g(i,j)*a(inew,j)
         do 121 i=1,ntr4
            do 121 j=1,ntr4
  121    vi=vi+bu(inew,i)*pp(i,j)*bu(inew,j)
         sum=vi*vh
         if(sum) 122,123,122
  123    rih=0.0
         go to 124
  122    rih=chi/sqrtf(sum)
  124    if(liop.eq.1) then
            write(lp,125)vi,vh,rih
  125       format(/' v(i) =',f20.4,10h   v(h) = ,f20.4,11h   r(ih) = ,f
     +20.4)
         endif
c-----genetic covariance between y(j) and i
         do 130 j=1,ntr4
            cyi(j)=0.0
            do 130 i=1,ntr4
  130    cyi(j)=cyi(j)+bu(inew,i)*g(i,j)
c-----correlated changes in actual units
         do 210 j=1,ntr4
  210    dgy(j)=cyi(j)/sqrtf(vi)
c-----genetic correlation b/n y(j) and i
         do 131 j=1,ntr4
            sum=h(j)*h(j)*vh
            if(sum) 132,133,132
  133       rgyi(j)=0.0
            go to 131
  132       rgyi(j)=cyi(j)/sqrtf(sum)
  131    continue
c-----
         isr=0
         call indout(head,head2,a,chi,vi,vh,rih,gain,bu,cyi,dgy,rgyi,ntr
     +   ,ntr4,l,inew,isr,label,nh,nf,no)
  998 continue
      return
      end
