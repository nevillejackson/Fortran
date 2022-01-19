      program resi2
c
c  purpose of program
c  ------- -- -------
c
c     restricted selection index calculations
c      accepts and lists "njobs" sets of heritabilities,phenotypic variance
c      phenotypic and genetic correlations for "ntr" traits. accepts and li
c      "new" sets of economic weights for each set of heritabilities etc.
c     accepts and lists "ndg" sets of desired gains and converts
c     these to implied economic weights,given heritabilities,etc.
c     calculates and lists , for each set of economic weights(new or ndg) ,
c     unrestricted selection index coefficients and genetic gains in
c      units of relative economic value (ie as the cov of i and h).
c     accepts and lists "nsr" sets of linear restrictions to the index
c     coefficients ("binet" type restriction) or to the rates of change
c     under index selection in certain traits ("tallis" type restriction)
c     or functions of traits ("kempthorne" type restriction).
c     for each set of heritabilities etc. calculates and
c     lists , for each set of economic weights , the restricted
c     selection index coefficients and genetic gains , in units of
c     relative economic value. also lists the genetic correlation
c     between the index and each trait.
c
c     warning : the algorithm used by this program is incorrect if
c     a kempthorne or tallis type restriction constrains rate of change to
c     a non-zero quantity
c
c
c      author
c      ------
c         n.jackson
c         c.s.i.r.o.,division of animal production
c         p.o. box 239 blacktown,nsw,2140
c         australia
c
c     references
c     ----------
c        james,j.w.(1968) biometrics 24:1015-1018
c        binet,f.e.(1965) biometrics 21:291-299
c        kempthorne,o. & nordskog,a.w.(1959) biometrics 15:10-19
c        tallis,g.m.(1962) biometrics 18:120-122
c
c  version  CP/M  FORTRAN-80  64K
c  -------  ----  ----------  ---
c        resi2 is invoked by  
c        
c        submit submitfile datafile         
c        
c        where datafile is a CP/M filename for a file containing
c                       heritability,Rg, and Rp when miop=1
c   
c              submitfile is a .SUB file containing a submit script
c
c        limits 10 traits, 4 sets of economic weights, 10 sets of restrictions
c
c  structure of data file(s)
c  --------- -- ---- ----
c
c      data records type 1
c      data records type 2 for job 1
c      data records type 3 for job 1
c      data records type 4 for job 1
c      data records type 2 for job 2
c      data records type 3 for job 2
c      data records type 4 for job 2
c       ---
c       ---
c      data records type 2 for job njobs
c      data records type 3 for job njobs
c      data records type 4 for job njobs
c
c  punching schedule for data records type 1
c  -------- -------- --- ---- ----- ---- -
c
c      columns          symbol          information
c      -------          ------          -----------
c
c          1-3           njobs    number of sets of gen and phen parameters
c          4-6            liop    list option  0=part,1=full,-1=minimum
c          7-9            miop    0 = input on STDIN
c                                 1 = input of heritabilities
c                                              genetic correlations
c                                              phenotypic correlations
c                                     on file MI whose UNIX name must be the
c                                     first command_line_argument.
c                                     File MI has a special format with one
c                                     record per trait pair, and each record
c                                     of the form -- i,j,g(i,j),p(i,j)
c                                     where i,j are trait no's
c                                           g(i,j) heritability or rg
c                                           p(i,j) rp
c   
c
c  punching schedule for data records type 2
c  -------- -------- --- ---- ----- ---- -
c
c      columns          symbol          information
c      -------          ------          -----------
c
c     note alternative input via tape10 of j,i,h or g,p
c     one element per record , variable format
c
c          1-3             ntr    number of traits (max=10)
c
c         1-80           iform    variable format for records type 2
c
c         vary          sig(1)    phen variance of trait 1
c           -           sig(2)    phen variance of trait 2
c           -               -      -
c           -         sig(ntr)    phen variance of trait ntr
c
c         vary            h(1)    heritability of trait 1
c           -             h(2)    heritability of trait 2
c           -               -      -
c           -           h(ntr)    heritability of trait ntr
c
c         vary          p(1,1)    phen correlation between traits 1 and 1
c
c         vary          p(1,2)    phen correlation between traits 1 and 2
c           -           p(2,2)    phen correlation between traits 2 and 2
c           -               -      -
c
c         vary        p(1,ntr)    phen correlation between traits 1 and ntr
c           -               -      -
c           -       p(ntr,ntr)    phen correlation between traits ntr and n
c
c         vary          g(1,1)    gen correlation between traits 1 and 1
c
c         vary          g(1,2)    gen correlation between traits 1 and 2
c           -           g(2,2)    gen correlatio  between traits 2 and 2
c           -               -      -
c
c         vary        g(1,ntr)    gen correlation between traits 1 and ntr
c           -               -      -
c           -       g(ntr,ntr)    gen correlation between traits ntr and nt
c
c  punching schedule for data records type 3
c  -------- -------- --- ---- ----- ---- -
c
c      columns          symbol          information
c      -------          ------          -----------
c          1-3             new    number of sets of economic weights(max=4)
c
c          4-6             ndg    number of sets of desired gains(max=4)
c                                 number of indices=new+ndg(max=4)
c
c         1-80           iform    variable format for records type 3
c
c         vary          a(1,1)    economic weight of trait 1
c           -           a(1,2)    economic weight of trait 2
c           -               -      -
c           -         a(1,ntr)    economic weight of trait ntr
c
c         repeat a for each set of economic weights
c
c        vary           dg(1,1)  desired gain of trait 1
c         vary           dg(1,2)  desired gain of trait 2
c           -                -     -
c           -            dg(1,ntr) desired gain of trait ntr
c
c         repeat for each set of desired gains
c
c  punching schedule for data records type 4
c  -------- -------- --- ---- ----- ---- -
c
c      columns          symbol          information
c      -------          ------          -----------
c          1-3             nsr    number of sets of restrictions(max=10)
c
c         1-80           iform    variable format for records type 4
c
c          1-3              nr    number of restrictions of binet type
c          4-6              ns    no of restrictions of kempthorne or
c                                    tallis type
c
c         vary         ek(1,1)    lhm of binet type restrictions no 1
c           -          ek(1,2)     -
c           -               -      -
c           -        ek(1,ntr)     -
c
c         vary         ek(2,1)    lhm of binet type restriction no 2
c           -          ek(2,2)     -
c           -               -      -
c           -        ek(2,ntr)     -
c
c           -                      -
c           -                      -
c           -       ek(nr,ntr)     -
c
c         vary            d(1)    rhms of binet type restrictions
c           -             d(2)     -
c           -               -      -
c           -            d(nr)     -
c
c         vary          c(1,1)    lhm of tallis type restriction no 1
c           -           c(1,2)     -
c           -               -      -
c           -         c(1,ntr)     -
c
c         vary          c(2,1)    lhm of tallis type restriction no 2
c           -           c(2,2)     -
c           -               -      -
c           -         c(2,ntr)     -
c
c           -               -      -
c           -               -      -
c           -        c(ns,ntr)     -
c
c         vary           el(1)    rhms of tallis type restrictions
c           -            el(2)     -
c           -               -      -
c           -            el(ns)    -
c
c         repeat nr,ns,ek,d,c,el, for each set of restrictions
c
c
      implicit double precision (a-h,o-z)
      dimension p(10,10),g(10,10),x(10,10),v(10,10),pv(10,10),pp(10,10)
      dimension ek(10,10),c(10,10),q(10,10),w(10,10)
      dimension a(4,10),b(4,10,10)
      dimension dg(4,10)
      dimension h(10),sig(10),reg(10),t(10),s(10),ps(10),cyi(10)
      dimension d(10),u(10),el(10),dgy(10)
      logical iform(80),fin(11)
      dimension ginv(10,10)
      logical lf
      data lf /z'0a'/
c-----
      lc=5
      lp=6
      mi=10
      mo=11
      call open(lp,'RESI2   LST',0)
c-----
c-----
      read (lc,1) njobs,liop,miop,moop
      write(lp,200)lf,njobs,liop,miop,moop
  200 format(a1,9h resi run/9h0njobs = ,i3,10h   liop = ,i3,
     +      10h   miop = ,i3,10h   moop = ,i3)
c-----loop over jobs
      do 999 ij=1,njobs
c-----read job parameters
c-----p and g matrices read upper triangular columnwise symmetric + diag
      read(lc,1)ntr
      write(lp,201) lf,ij,ntr
  201 format(a1,8h job no ,i3/7h0ntr = ,i3)
      if(ntr-50) 980,980,981
  981 write(lp,982)lf
  982 format(a1,17h ntr max exceeded)
      stop
  980 continue
      read(lc,250) iform
  250 format(80a1)
      write(lp,251) lf,iform
  251 format(a1,22h variable input format/1h ,80a1)
c-----read phenotypic variances
      read(lc,iform)(sig(i),i=1,ntr)
      if(miop) 381,380,381
c-----file input of Hsq,Rg,Rp
c-----matrices must be lower triangle
c-----variable format for unit 10
c-----
c-----open CP/M data file
  381 read (lc,252)idrv,fin
  252 format(i1,1x,8a1,1x,3a1)
      call open(mi,fin,idrv)
c----- idrv 0 - current loged drive
c-----      1 - drive A
c----       2 - drive B
c----  fin  padded to 11 characters with no dot
c----
      read(lc,250) iform
      write(lp,251) lf,iform
      last=ntr*(ntr+1)/2
      do 383 k=1,last
  383 read(mi,iform) i,j,g(i,j),p(i,j)
      do 384 i=1,ntr
      h(i)=g(i,i)
  384 g(i,i)=1.0
      go to 382
c-----data card type 2 input
c-----matrices must be lower triangle
  380 read(lc,iform) (h(i),i=1,ntr)
      do3i=1,ntr
    3 read(lc,iform)(p(i,j),j=1,i)
      do4i=1,ntr
    4 read(lc,iform)(g(i,j),j=1,i)
  382 continue
c-----
      if(liop) 391,390,390
  390 continue
      write(lp,31)lf,(h(i),i=1,ntr)
      write(lp,32)lf,(sig(i),i=1,ntr)
      write(lp,33)lf
      do34i=1,ntr
   34 write(lp,35)lf,(p(i,j),j=1,i)
      write(lp,36)lf
      do37i=1,ntr
   37 write(lp,35)lf,(g(i,j),j=1,i)
      write(lp,38)lf
   31 format(a1,16h  heritabilities,/1h ,5f20.4/(1h ,5f20.4))
   32 format(a1,14h  ph variances,/1h ,5f20.4/(1h ,5f20.4))
   33 format(a1,17h  ph correlations)
   35 format(a1,1h ,5f20.4)
   36 format(a1,18h  gen correlations)
   38 format(a1,1h ,10x,10(10h----------))
  391 continue
c     a=ec wts  h=heritys  sig=ph vars  p=ph corr  g=gen corr
c-----convert phen vars to sds , hsq to gen vars to gen sds
      do5i=1,ntr
      sig(i)=sqrtf(sig(i))
    5 h(i)=sqrtf(h(i))*sig(i)
      write(lp,50)lf
   50 format(a1,15h ph stand devns)
      write(lp,35)lf,(sig(i),i=1,ntr)
      write(lp,51)lf
   51 format(a1,16h gen stand devns)
      write(lp,35)lf,(h(i),i=1,ntr)
    1 format(20i3)
c-----convert p and g correln matrices to covariances
      do6i=1,ntr
      do6j=1,i
      p(i,j)=p(i,j)*sig(i)*sig(j)
    6 g(i,j)=g(i,j)*h(i)*h(j)
c-----square up p and g covariances
      call squp(p,ntr)
      callsqup(g,ntr)
      if(liop) 392,393,393
  393 continue
      write(lp,40)lf
   40 format(a1,17h ph (co)variances)
      do 41 i=1,ntr
   41 write(lp,35)lf,(p(i,j),j=1,i)
      write(lp,42)lf
   42 format(a1,' gen (co)variances')
      do 43 i=1,ntr
   43 write(lp,35)lf,(g(i,j),j=1,i)
  392 continue
c-----store p in pp & g in ginv before inversion
      do 121 i=1,ntr
      do 121 j=1,ntr
      ginv(i,j)=g(i,j)
  121 pp(i,j)=p(i,j)
c-----calc p inverse
      do7i=1,ntr
    7 callpivot(p,ntr,i)
      if(liop) 301,301,300
  300 continue
      write(lp,44)lf
   44 format(a1,29h inverse ph covariance matrix)
      do 45 i=1,ntr
   45 write(lp,35)lf,(p(i,j),j=1,i)
  301 continue
c-----mult p-inverse by g
      do8i=1,ntr
      do8j=1,ntr
      sum=0.0
      do9k=1,ntr
    9 sum=sum+p(i,k)*g(k,j)
    8 x(i,j)=sum
      if(liop) 303,303,302
  302 continue
      write(lp,46)lf
   46 format(a1,18h p-inverse * g = x)
      do 47 i=1,ntr
   47 write(lp,35)lf,(x(i,j),j=1,ntr)
  303 continue
c-----invert g cov matrix
      do 1450 i=1,ntr
 1450 call pivot(ginv,ntr,i)
      if(liop) 1451,1451,1452
 1452 write(lp,1453)lf
 1453 format(a1,' g-inverse')
      do 1454 i=1,ntr
 1454 write(lp,35)lf,(ginv(i,j),j=1,ntr)
 1451 continue
c-----economic weights & or desired gains
      read(lc,1)new,ndg
      write(lp,205) lf,new,ndg
  205 format(a1,' no of sets of economic weights = ',i3/
     +      ' no of sets of desired gains = ',i3)
      if(new-20) 984,984,985
  985 write(lp,986)lf
  986 format(a1,17h new max exceeded)
      stop
  984 continue
c-----loop over economic wts for unrestricted index
      nw=new+ndg
      read(lc,250) iform
      write(lp,251) lf,iform
      idg=0
      do 998 inew=1,nw
      write(lp,38)lf
      write(lp,20)lf,inew
      if(inew.le.new) go to 1400
c-----read desired gains
      idg=idg+1
      read(lc,iform)(dg(idg,i),i=1,ntr)
      write(lp,1410)lf,(dg(idg,i),i=1,ntr)
 1410 format(a1,' desired gains'/' ',5f20.4/(' ',5f20.4))
      write(lp,1411)lf
 1411 format(a1,' weights')
      call weight(new,ndg,idg,a,ginv,pp,dg,ntr)
      go to 1401
 1400 read(lc,iform)(a(inew,i),i=1,ntr)
 1401 do 13 i=1,ntr
c-----calc index coefficients
      sum=0.0
      do 14 k=1,ntr
   14 sum=sum+x(i,k)*a(inew,k)
   13 b(inew,i,1)=sum
c-----calc cov(hi) and gain
      chi=0.0
      do 15 i=1,ntr
      do 15 j=1,ntr
   15 chi=chi+a(inew,i)*g(i,j)*b(inew,j,1)
   10 format(10f4.0)
      write(lp,35)lf,(a(inew,i),i=1,ntr)
      write(lp,23)lf,(b(inew,i,1),i=1,ntr)
      gain=sqrtf(chi)
      write(lp,22) lf,gain,chi
      reg(inew)=gain
   23 format(a1,' coefficients for unrestricted index',/1h0,5f20.4/(
     +      1h ,5f20.4))
   20 format(a1,19h   economic wts set,i3)
   22 format(a1,'genetic gain for unrestricted index in economic weight'
     +      ,' units per unit selection differential per generation ='
     +,f15.4/
     +      ,' cov(hi) =',f20.4)
c-----calculate v(i) , v(h) , r(ih)
      vi=0.0
      vh=0.0
      do 120 i=1,ntr
      do 120 j=1,ntr
      vi=vi+b(inew,i,1)*pp(i,j)*b(inew,j,1)
  120 vh=vh+a(inew,i)*g(i,j)*a(inew,j)
      sum=vi*vh
      if(sum) 122,123,122
  123 rih=0.0
      go to 124
  122 rih=chi/sqrtf(sum)
  124 write(lp,125)lf,vi,vh,rih
  125 format(a1,8h v(i) = ,f20.4,10h   v(h) = ,f20.4,11h   r(ih) = ,
     + f20.4)
c-----genetic covariance between y(j) and i
      do 130 j=1,ntr
      cyi(j)=0.0
      do 130 i=1,ntr
  130 cyi(j)=cyi(j)+b(inew,i,1)*g(i,j)
      write(lp,116)lf
      write(lp,35)lf,(cyi(j),j=1,ntr)
c-----correlated changes in actual units
      do 210 j=1,ntr
  210 dgy(j)=cyi(j)/sqrtf(vi)
      write(lp,211)lf
  211 format(a1,' correlated change in y(j) in actual units per unit '
     +     ,'selection differential per generation')
      write(lp,35)lf,(dgy(j),j=1,ntr)
c-----genetic correlation b/n y(j) and i
      do 131 j=1,ntr
      sum=h(j)*h(j)*vh
      if(sum) 132,133,132
  133 cyi(j)=0.0
      go to 131
  132 cyi(j)=cyi(j)/sqrtf(sum)
  131 continue
      write(lp,127)lf
      write(lp,35)lf,(cyi(j),j=1,ntr)
  998 continue
c-----test whether this job has sets of restrictions
      write(lp,38)lf
      write(lp,220)lf
  220 format(a1,19h restricted indices)
      read (lc,1)nsr
      write(lp,52)lf,nsr
   52 format(a1,30h no of sets of restrictions = ,i4)
      if(nsr-49) 987,987,988
  988 write(lp,989)lf
  989 format(a1,17h nsr max exceeded)
      stop
  987 continue
      if(nsr) 999,999,996
c-----loop over sets of restrictions
  996 read(lc,250) iform
      write(lp,251) lf,iform
      do 60 isr=1,nsr
      write(lp,38)lf
      write(lp,58)lf,isr
   58 format(a1,20h restriction set no ,i4)
      read (lc,1)nr,ns
      write(lp,53)lf,nr,ns
   53 format(a1,25h no of resi binet type = ,i4/
     +      40h0no of resi kempthorne or tallis type = ,i4)
c-----binet type
      if(nr+ns-ntr) 57,1600,1600
 1600 write(lp,1601)lf
 1601 format(a1,' nr+ns must not exceed ntr')
      stop
   57 if(nr) 54,54,55
   55 if(liop) 394,395,395
  395 write(lp,62)lf
  394 continue
   62 format(a1,9h k matrix)
      do 61 i=1,nr
      read(lc,iform)(ek(i,j),j=1,ntr)
      do 67 j=1,ntr
   67 q(i,j)=ek(i,j)
      if(liop) 61,397,397
  397 write(lp,35)lf,(ek(i,j),j=1,ntr)
   61 continue
      read(lc,iform)(d(i),i=1,nr)
      do 68 i=1,nr
   68 u(i)=d(i)
      write(lp,63)lf
      if(liop) 396,398,398
  398 continue
   63 format(a1,9h d vector)
      write(lp,35)lf,(d(i),i=1,nr)
  396 continue
c-----kempthorne or tallis type
   54 if(ns) 155,155,56
   56 if(liop) 399,400,400
  400 write(lp,64)lf
  399 continue
   64 format(a1,9h c matrix)
      do 65 i=1,ns
      read(lc,iform)(c(i,j),j=1,ntr)
      do 69 j=1,ntr
      sum=0.0
      do 70 k=1,ntr
   70 sum=sum+c(i,k)*g(k,j)
      inr=i+nr
   69 q(inr,j)=sum
      if(liop) 65,401,401
  401 write(lp,35)lf,(c(i,j),j=1,ntr)
   65 continue
      read(lc,iform)(el(i),i=1,ns)
      do 71 i=1,ns
      inr=i+nr
   71 u(inr)=el(ns)
      write(lp,66)lf
      if(liop) 402,403,403
  403 continue
   66 format(a1,9h l vector)
      write(lp,35)lf,(el(i),i=1,ns)
  402 continue
c-----combine both types in printout
  155 nrs=nr+ns
      if(nrs) 60,60,156
  156 if(liop) 404,405,405
  405 write(lp,73)lf
   73 format(a1,9h q matrix)
      do 74 i=1,nrs
   74 write(lp,35)lf,(q(i,j),j=1,ntr)
      write(lp,75)lf
   75 format(a1,9h u vector)
      write(lp,35)lf,(u(i),i=1,nrs)
c-----calc reg for each set of ec wts
c-----calc b for each set of ce wts for this set of resi
c-----steps 1,2,3,4,5,7,8 are independant of ec wts and so outside wts loop
c-----step 1 calc q * p-inverse * q-transpose = w
  404 if(liop) 306,306,305
  305 continue
      write(lp,80)lf
   80 format(a1,22h q * p-inverse * q = w)
  306 continue
      do 81 i=1,nrs
      do 82 j=1,nrs
      sum=0.0
      do 83 k=1,ntr
      do 83 l=1,ntr
      sum=sum+q(i,k)*p(k,l)*q(j,l)
   83 continue
      w(i,j)=sum
   82 continue
      if(liop) 81,81,304
  304 write(lp,35)lf,(w(i,j),j=1,nrs)
   81 continue
c-----step 2 invert w
      if(liop) 308,307,307
  307 continue
      write(lp,84)lf
   84 format(a1,10h w-inverse)
  308 continue
      do 85 i=1,nrs
   85 call pivot(w,nrs,i)
      if(liop) 310,310,309
  309 continue
      do 86 i=1,nrs
   86 write(lp,35)lf,(w(i,j),j=1,nrs)
c-----step 3 calc q-transpose * w-inverse * q = v
      write(lp,87)lf
   87 format(a1,22h q * w-inverse * q = v)
  310 continue
      do 88 i=1,ntr
      do 89 j=1,ntr
      sum=0.0
      do 90 k=1,nrs
      do 90 l=1,nrs
      sum=sum+q(k,i)*w(k,l)*q(l,j)
   90 continue
      v(i,j)=sum
   89 continue
      if(liop) 88,88,311
  311 write(lp,35)lf,(v(i,j),j=1,ntr)
   88 continue
c-----step 4 calc p-inverse * v = pv
      if(liop) 313,313,312
  312 continue
      write(lp,91)lf
   91 format(a1,19h p-inverse * v = pv)
  313 continue
      do 92 i=1,ntr
      do 93 j=1,ntr
      sum=0.0
      do 94 k=1,ntr
   94 sum=sum+p(i,k)*v(k,j)
      pv(i,j)=sum
   93 continue
      if(liop) 92,92,314
  314 write(lp,35)lf,(pv(i,j),j=1,ntr)
   92 continue
c-----step 5 calc i - pv = pv
      if(liop) 316,316,315
  315 continue
      write(lp,95)lf
   95 format(a1,12h i - pv = pv)
  316 continue
      do 117 i=1,ntr
      do 96 j=1,ntr
      if(i-j) 97,98,97
   97 pv(i,j)=-pv(i,j)
      go to 96
   98 pv(i,j)=1.-pv(i,j)
   96 continue
      if(liop) 117,117,317
  317 write(lp,35)lf,(pv(i,j),j=1,ntr)
  117 continue
c-----step 7 calc q-transpose * w-inverse * u = s
      if(liop) 319,319,318
  318 continue
      write(lp,102)lf
  102 format(a1,22h q * w-inverse * u = s)
  319 continue
      do 103 i=1,ntr
      sum=0.0
      do 104 k=1,nrs
      do 104 l=1,nrs
  104 sum=sum+q(k,i)*w(k,l)*u(l)
  103 s(i)=sum
      if(liop) 321,321,320
  320 continue
      write(lp,35)lf,(s(i),i=1,ntr)
c-----step 8 calc p-inverse * s = ps
      write(lp,105)lf
  105 format(a1,19h p-inverse * s = ps)
  321 continue
      do 106 i=1,ntr
      sum=0.0
      do 107 j=1,ntr
  107 sum=sum+p(i,j)*s(j)
  106 ps(i)=sum
      if(liop) 323,323,322
  322 continue
      write(lp,35)lf,(ps(i),i=1,ntr)
c-----loop over economic wts
  323 continue
      do 997 inew=1,nw
c-----step 6 calc pv * b-unrestricted = t
      write(lp,38)lf
      write(lp,20) lf,inew
      if(liop) 325,325,324
  324 continue
      write(lp,99)lf
   99 format(a1,24h pv * unrestricted-b = t)
  325 continue
      do 100 i=1,ntr
      sum=0.0
      do 101 j=1,ntr
  101 sum=sum+pv(i,j)*b(inew,j,1)
  100 t(i)=sum
      if(liop) 327,327,326
  326 continue
      write(lp,35)lf,(t(i),i=1,ntr)
c-----step 9 restricted index coefficients
  327 continue
      write(lp,108)lf
  108 format(a1,30h restricted index coefficients)
      do 109 i=1,ntr
  109 b(inew,i,isr+1)=t(i)+ps(i)
      write(lp,35)lf,(b(inew,i,isr+1),i=1,ntr)
c-----phen variance of index vi
c-----gen variance of index vh
c-----covariance of h,i chi
      vi=0.0
      vh=0.0
      chi=0.0
      do 110 i=1,ntr
      do 110 j=1,ntr
      vi=vi+b(inew,i,isr+1)*pp(i,j)*b(inew,j,isr+1)
      vh=vh+a(inew,i)*g(i,j)*a(inew,j)
  110 chi=chi+a(inew,i)*g(i,j)*b(inew,j,isr+1)
      gain=sqrtf(chi)
      write(lp,111) lf,gain,chi
  111 format(a1,' genetic gain for restricted index in economic weight '
     +     ,'units per unit selection differential per generation ='
     + ,f20.4/
     +      ,'0cov(hi) =',f20.4)
c-----correlation between i and h  rih
      sum=vi*vh
      if(sum) 112,113,112
  113 rih=0.0
      go to 114
  112 rih=chi/sqrtf(sum)
  114 write(lp,125)lf,vi,vh,rih
c-----genetic covariance between y(j) and i = bt*g(j) = cyi(j)
      do 115 j=1,ntr
      cyi(j)=0.0
      do 115 i=1,ntr
  115 cyi(j)=cyi(j)+b(inew,i,isr+1)*g(i,j)
      write(lp,116)lf
  116 format(a1,38h genetic covariance between y(j) and i)
      write(lp,35)lf,(cyi(j),j=1,ntr)
c-----correlated change in actual units
      do 230 j=1,ntr
  230 dgy(j)=cyi(j)/sqrtf(vi)
      write(lp,211)lf
      write(lp,35)lf,(dgy(j),j=1,ntr)
c-----genetic correlation between y(j) and i
      do 118 j=1,ntr
      sum=h(j)*h(j)*vh
      if(sum) 119,126,119
  126 cyi(j)=0.0
      go to 118
  119 cyi(j)=cyi(j)/sqrtf(sum)
  118 continue
      write(lp,127)lf
  127 format(a1,39h genetic correlation between y(j) and i)
      write(lp,35)lf,(cyi(j),j=1,ntr)
  997 continue
   60 continue
  999 continue
      stop
      end
      double precision function sqrtf(x)
      implicit double precision (a-h,o-z)
      sqrtf=dsqrt(x)
      return
      end
      subroutine squp(x,n)
      implicit double precision (a-h,o-z)
      dimension x(10,10)
c     squares up a lower triangular matrix
      n1=n-1
      do 1 i=1,n1
      i1=i+1
      do 1 j=i1,n
    1 x(i,j)=x(j,i)
      return
      end
      double precision function absf(x)
      implicit double precision (a-h,o-z)
      logical lf
      data lf /z'0a'/
      absf=dabs(x)
      return
      end
      subroutine pivot (x,n,m)
      implicit double precision (a-h,o-z)
      dimension x(10,10)
c     does a single pivot step on the diagonal
      if(absf(x(m,m)).le.0.000001)go to 5
   11 do 1 i=1,n
      do 1 j=1,n
      if(i.eq.m.or.j.eq.m)go to 1
    8 x(i,j)=x(i,j)-x(i,m)*x(m,j)/x(m,m)
    1 continue
      do 2 i=1,n
      if(i.eq.m)go to 2
    9 x(i,m)=-x(i,m)/x(m,m)
    2 continue
      do 3 j=1,n
      if(j.eq.m)go to 3
   10 x(m,j)=x(m,j)/x(m,m)
    3 continue
      x(m,m)=1.0/x(m,m)
      return
    5 write(lp,6)lf,m
    6 format(a1,' pivot step zero',i3)
      return
      end
      subroutine weight(new,ndg,idg,a,g,p,d,ntr)
      implicit double precision (a-h,o-z)
      dimension a(4,10),g(10,10),p(10,10),d(4,10)
c-----computes w=ginv*p*ginv*d
      do 3 i=1,ntr
      sumj=0.0
      do 4 j=1,ntr
      suml=0.0
      do 5 l=1,ntr
      sumk=0.0
      do 6 k=1,ntr
    6 sumk=sumk+g(i,k)*p(k,l)
    5 suml=suml+sumk*g(l,j)
    4 sumj=sumj+suml*d(idg,j)
      idgn=idg+new
    3 a(idgn,i)=sumj
      return
      end
