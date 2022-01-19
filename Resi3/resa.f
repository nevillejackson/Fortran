      subroutine resa(isr,liop,nr,ns,nsr,nw,ntr,ntr4,label
     +,q,u,p,w,v,pv,s,ps,t,bu,b,g,cyi,dgy,pp,a,h,rgyi,head,head2,l
     +,nh,nf,no)
c-----solve restricted index given restrictions setup in q(,) & u()
      implicit double precision (a-h,o-z)
      dimension q(mvntr4,mvntr4),u(mvntr4)
      dimension p(mvntr4,mvntr4),g(mvntr4,mvntr4)
      dimension w(mvntr4,mvntr4),v(mvntr4,mvntr4),pv(mvntr4,mvntr4)
      dimension s(mvntr4),ps(mvntr4),t(mvntr4)
      dimension bu(mvnw,mvntr4),b(mvnw,mvntr4)
      dimension cyi(mvntr4),dgy(mvntr4),rgyi(mvntr4)
      dimension pp(mvntr4,mvntr4),a(mvnw,mvntr),h(mvntr4),l(mvntr4)
      character*10 label(mvntr)
      character*100 head,head2
      logical ok
c-----
      common /limits/ mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      common /stdio/ lc,lp,lerr
c-----
c-----printout constraint equations
  155 nrs=nr+ns
      if(nrs) 997,997,156
  156 if(liop) 404,404,405
  405 write(lp,73)
   73 format(/' q matrix - constraint equations lhs')
      do 74 i=1,nrs
   74 write(lp,35)(q(i,j),j=1,ntr4)
   35 format(' ',5f20.4)
      write(lp,75)
   75 format(/' u vector - constraint equations rhs')
      write(lp,35)(u(i),i=1,nrs)
c-----calc reg for each set of ec wts
c-----calc b for each set of ce wts for this set of resi
c-----steps 1,2,3,4,5,7,8 are independant of ec wts and so outside wts loop
c-----step 1 calc q * p-inverse * q-transpose = w
  404 if(liop) 306,306,305
  305 continue
      write(lp,80)
   80 format(/' q * p-inverse * q = w')
  306 continue
      do 81 i=1,nrs
         do 82 j=1,nrs
            sum=0.0
            do 83 k=1,ntr4
               do 83 ll=1,ntr4
                  sum=sum+q(i,k)*p(k,ll)*q(j,ll)
   83       continue
            w(i,j)=sum
   82    continue
         if(liop) 81,81,304
  304    write(lp,35)(w(i,j),j=1,nrs)
   81 continue
c-----step 2 invert w
      if(liop) 308,308,307
  307 continue
      write(lp,84)
   84 format(/' w-inverse')
  308 continue
      do 85 i=1,nrs
         call pivot(w,nrs,i,liop,ok)
         if(.not.ok) then
            if(i.gt.nr) then
c-----dont check binet singularities
               write(lp,4085) i
 4085          format(' unexpected singularity at row',i3,' of W_matrix'
     +/          '   --> check restrictions for dependencies')
               stop
            endif
         endif
   85 continue
      if(liop) 310,310,309
  309 continue
      do 86 i=1,nrs
   86 write(lp,35)(w(i,j),j=1,nrs)
c-----step 3 calc q-transpose * w-inverse * q = v
      write(lp,87)
   87 format(/' q * w-inverse * q = v')
  310 continue
      do 88 i=1,ntr4
         do 89 j=1,ntr4
            sum=0.0
            do 90 k=1,nrs
               do 90 ll=1,nrs
                  sum=sum+q(k,i)*w(k,ll)*q(ll,j)
   90       continue
            v(i,j)=sum
   89    continue
         if(liop)88,88,311
  311    write(lp,35)(v(i,j),j=1,ntr4)
   88 continue
c-----step 4 calc p-inverse * v = pv
      if(liop) 313,313,312
  312 continue
      write(lp,91)
   91 format(/' p-inverse * v = pv')
  313 continue
      do 92 i=1,ntr4
         do 93 j=1,ntr4
            sum=0.0
            do 94 k=1,ntr4
   94       sum=sum+p(i,k)*v(k,j)
            pv(i,j)=sum
   93    continue
         if(liop) 92,92,314
  314    write(lp,35)(pv(i,j),j=1,ntr4)
   92 continue
c-----step 5 calc i - pv = pv
      if(liop) 316,316,315
  315 continue
      write(lp,95)
   95 format(/' i - pv = pv')
  316 continue
      do 117 i=1,ntr4
         do 96 j=1,ntr4
            if(i-j) 97,98,97
   97       pv(i,j)=-pv(i,j)
            go to 96
   98       pv(i,j)=1.-pv(i,j)
   96    continue
         if(liop) 117,117,317
  317    write(lp,35)(pv(i,j),j=1,ntr4)
  117 continue
c-----step 7 calc q-transpose * w-inverse * u = s
      if(liop) 319,319,318
  318 continue
      write(lp,102)
  102 format(/' q * w-inverse * u = s')
  319 continue
      do 103 i=1,ntr4
         sum=0.0
         do 104 k=1,nrs
            do 104 ll=1,nrs
  104    sum=sum+q(k,i)*w(k,ll)*u(ll)
  103 s(i)=sum
      if(liop) 321,321,320
  320 continue
      write(lp,35)(s(i),i=1,ntr4)
c-----step 8 calc p-inverse * s = ps
      write(lp,105)
  105 format(/' p-inverse * s = ps')
  321 continue
      do 106 i=1,ntr4
         sum=0.0
         do 107 j=1,ntr4
  107    sum=sum+p(i,j)*s(j)
  106 ps(i)=sum
      if(liop) 323,323,322
  322 continue
      write(lp,35)(ps(i),i=1,ntr4)
c-----loop over economic wts
  323 continue
      do 997 inew=1,nw
c-----step 6 calc pv * b-unrestricted = t
         if(liop.eq.1)then
            write(lp,38)
   38       format(/10x,10('----------'))
            write(lp,20) inew
   20       format(/' economic weights set no ',i3)
         endif
         if(liop) 325,325,324
  324    continue
         write(lp,99)
   99    format(/' pv * unrestricted-b = t')
  325    continue
         do 100 i=1,ntr4
            sum=0.0
            do 101 j=1,ntr4
  101       sum=sum+pv(i,j)*bu(inew,j)
  100    t(i)=sum
         if(liop) 327,327,326
  326    continue
         write(lp,35)(t(i),i=1,ntr4)
c-----step 9 restricted index coefficients
  327    continue
         do 109 i=1,ntr4
  109    b(inew,i)=t(i)+ps(i)
c-----phen variance of index vi
c-----gen variance of index vh
c-----covariance of h,i chi
         vi=0.0
         vh=0.0
         chi=0.0
         do 110 i=1,ntr4
            do 110 j=1,ntr4
  110    vi=vi+b(inew,i)*pp(i,j)*b(inew,j)
         do 140 i=1,ntr
            do 140 j=1,ntr
  140    vh=vh+a(inew,i)*g(i,j)*a(inew,j)
         do 141 i=1,ntr
            do 141 j=1,ntr4
  141    chi=chi+a(inew,i)*g(i,j)*b(inew,j)
         gain=sqrtf(chi)
c-----correlation between i and h  rih
         sum=vi*vh
         if(sum) 112,113,112
  113    rih=0.0
         go to 114
  112    rih=chi/sqrtf(sum)
c-----genetic covariance between y(j) and i = bt*g(j) = cyi(j)
  114    do 115 j=1,ntr4
            cyi(j)=0.0
            do 115 i=1,ntr4
  115    cyi(j)=cyi(j)+b(inew,i)*g(i,j)
c-----correlated change in actual units
         do 230 j=1,ntr4
  230    dgy(j)=cyi(j)/sqrtf(vi)
c-----genetic correlation between y(j) and i
         do 118 j=1,ntr4
            sum=h(j)*h(j)*vh
            if(sum) 119,126,119
  126       rgyi(j)=0.0
            go to 118
  119       rgyi(j)=cyi(j)/sqrtf(sum)
  118    continue
         call indout(head,head2,a,chi,vi,vh,rih,gain,b,cyi,dgy,rgyi,ntr
     +   ,ntr4,l,inew,isr,label,nh,nf,no)
  997 continue
      return
      end
