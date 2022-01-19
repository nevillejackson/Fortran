      subroutine parold(ij,ntr4,liop,sig,h,p,g)
c-----read population parameters from STDIN 
c----- requires manual calculation of I,H,F,O partition correlations,sig,h
c-----
      implicit double precision (a-h,o-z)
      character*80 iform
      dimension sig(mvntr4),h(mvntr4),p(mvntr4,mvntr4)
      dimension g(mvntr4,mvntr4)
      common /limits/ mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      common /stdio/ lc,lp,lerr
c-----
        read1,ntr4
        write(lp,201) ij,ntr4
  201   format(8h1job no ,i3/8h0ntr4 = ,i3)
        if(ntr4-50) 980,980,981
  981   write(lp,982)
  982   format('0ntr4 max exceeded')
         stop
  980   continue
        read(lc,250) iform
  250   format(a)
        write(lp,251) iform
  251   format(22h0variable input format/1h ,a)
c-----  read phenotypic stand. deviations
        read(lc,iform)(sig(i),i=1,ntr4)
c-----  matrices must be lower triangle
  380   read(lc,iform) (h(i),i=1,ntr4)
        do3i=1,ntr4
    3   read(lc,iform)(p(i,j),j=1,i)
        do4i=1,ntr4
    4   read(lc,iform)(g(i,j),j=1,i)
  382   continue
c-----
c-----  list inputs
        if(liop) 391,390,390
  390   continue
        write(lp,31)(h(i),i=1,ntr4)
        write(lp,32)(sig(i),i=1,ntr4)
        write(lp,33)
        do34i=1,ntr4
   34   write(lp,35)(p(i,j),j=1,i)
        write(lp,36)
        do37i=1,ntr4
   37   write(lp,35)(g(i,j),j=1,i)
        write(lp,38)
   31   format(16h0 heritabilities,/1h ,5f20.4/(1h ,5f20.4))
   32   format('0 ph stand. deviations',/1h ,5f20.4/(1h ,5f20.4))
   33   format(17h0 ph correlations)
   35   format(1h ,5f20.4)
   36   format(18h0 gen correlations)
   38   format(1h0,10x,10(10h----------))
  391   continue
c-----  convert , hsq to gen vars to gen sds
        do5i=1,ntr4
    5   h(i)=sqrtf(h(i))*sig(i)
        write(lp,51)
   51   format(16h0gen stand devns)
        write(lp,35)(h(i),i=1,ntr4)
    1   format(20i3)
c-----  convert p and g correln matrices to covariances
        do6i=1,ntr4
        do6j=1,i
        p(i,j)=p(i,j)*sig(i)*sig(j)
    6   g(i,j)=g(i,j)*h(i)*h(j)
c-----  square up p and g covariances
        call squp(p,ntr4)
        call squp(g,ntr4)
        if(liop) 392,393,393
  393   continue
        write(lp,40)
   40   format(17h0ph (co)variances)
        do 41 i=1,ntr4
   41   write(lp,35)(p(i,j),j=1,i)
        write(lp,42)
   42   format(18h0gen (co)variances)
        do 43 i=1,ntr4
   43   write(lp,35)(g(i,j),j=1,i)
  392   continue
      return
      end
