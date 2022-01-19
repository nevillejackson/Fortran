      subroutine kemset(isr,nr,ns,q,u,c,
     + ntr,ntr4,nsr,gres,g,liop)
c-----setup one set of delta_G restrictions
c-----for each trait for which gres(,) .ne. 0
c-----ns	counts restriction equations for this set
      implicit double precision (a-h,o-z)
      dimension q(mvntr4,mvntr4),u(mvntr4)
      dimension g(mvntr4,mvntr4)
      dimension c(mvntr,mvntr)
      dimension gres(mvns,mvntr)
c-----
      common /limits/mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      common /stdio/ lc,lp,lerr
c-----
c-----note gres(,) =1 -> trait j allowed to change
c-----             =0 -> delta_G=0 for trait j
c-----             constraints to non-zero delta_G not allowed
c-----             constraints to functions of >1 trait not allowed
c-----
c-----count constraints
      ns=0
      do 1 j=1,ntr
      if(gres(isr,j).eq.0) then
        ns=ns+1
      endif
    1 continue
c-----initialize ns constraints to zero in c() & u()
      do 2 i=1,ns
      u(i)=0.d0
      do 2 j=1,ntr
    2 c(i,j)=0.d0
c-----
      kns=0
      do 3 j=1,ntr
      if(gres(isr,j).eq.0) then
        kns=kns+1
        c(kns,j)=1.0
      endif
    3 continue
c-----
      if(liop) 8,8,9
    9 write(lp,10)
   10 format('0 C matrix')
      do 7 i=1,ns
    7 write(lp,35)(c(i,j),j=1,ntr)
    8 continue
c----- copy constraints into q with genetic scaling
      if(liop) 399,399,400
  400 write(lp,64)
   64 format(/'Q matrix - ns rows')
  399 continue
      do 65 i=1,ns
      do 69 j=1,ntr4
      sum=0.d0
      do 70 k=1,ntr
   70 sum=sum+c(i,k)*g(k,j)
   69 q(i+nr,j)=sum
      if(liop) 65,65,401
  401 write(lp,35)(q(i+nr,j),j=1,ntr4)
   65 continue
   35 format(' ',5f20.4)
      return
      end
