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
c  invoking resi2 under UNIX
c  -------- ----- ----- ----
c        resi2 filename <<eoi
c        )
c        ) data records redirected to STDIN
c        )
c        eoi
c
c        The 'filename' option is required when parameter MIOP=1
c        The above shellscript is invohed by 
c          sh -x scriptname >outfile 2>&1 &
c
c
c  structure of data file(s)
c  --------- -- ---- ----
c
c      data records type 1
c      data records type 2 for job 1
c      data records type 3 for job 1
c     data records type 4 for job 1
c      data records type 2 for job 2
c      data records type 3 for job 2
c     data records type 4 for job 2
c       ---
c       ---
c      data records type 2 for job njobs
c      data records type 3 for job njobs
c     data records type 4 for job njobs
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
c          1-3             ntr    number of traits (max=50)
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
c          1-3             new    number of sets of economic weights(max=20
c
c          4-6             ndg    number of sets of desired gains
c                                 number of indices=new+ndg
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
c          1-3             nsr    number of sets of restrictions
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
      dimension p(50,50),g(50,50),x(50,50),v(50,50),pv(50,50),pp(50,50)
      dimension ek(50,50),c(50,50),q(50,50),w(50,50)
      dimension a(20,50),b(20,50,50)
      dimension dg(20,50)
      dimension h(50),sig(50),reg(50),t(50),s(50),ps(50),cyi(50)
      dimension d(50),u(50),el(50),dgy(50)
      character*80 iform
      dimension ginv(50,50)
c-----
      character*30 fin
c-----
      if(iargc() .ge.1) then
        call getarg(1,fin)
        open(mi,file=fin,status='old')
      endif
      lc=5
      lp=6
      mi=10
      mo=11
c-----
c-----
      read (lc,1) njobs,liop,miop,moop
      write(lp,200)njobs,liop,miop,moop
  200 format(9h0resi run/9h0njobs = ,i3,10h   liop = ,i3,
     +      10h   miop = ,i3,10h   moop = ,i3)
c-----loop over jobs
      do 999 ij=1,njobs
c-----read job parameters
c-----p and g matrices read upper triangular columnwise symmetric + diag
      read1,ntr
      write(lp,201) ij,ntr
  201 format(8h1job no ,i3/7h0ntr = ,i3)
      if(ntr-50) 980,980,981
  981 write(lp,982)
  982 format(17h0ntr max exceeded)
      stop
  980 continue
      read(lc,250) iform
  250 format(a)
      write(lp,251) iform
  251 format(22h0variable input format/1h ,a)
c-----read phenotypic variances
      read(lc,iform)(sig(i),i=1,ntr)
      if(miop) 381,380,381
c-----file input
c-----matrices must be lower triangle
c-----variable format for unit 10
  381 read(lc,250) iform
      write(lp,251) iform
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
      write(lp,31)(h(i),i=1,ntr)
      write(lp,32)(sig(i),i=1,ntr)
      write(lp,33)
      do34i=1,ntr
   34 write(lp,35)(p(i,j),j=1,i)
      write(lp,36)
      do37i=1,ntr
   37 write(lp,35)(g(i,j),j=1,i)
      write(lp,38)
   31 format(16h0 heritabilities,/1h ,5f20.4/(1h ,5f20.4))
   32 format(14h0 ph variances,/1h ,5f20.4/(1h ,5f20.4))
   33 format(17h0 ph correlations)
   35 format(1h ,5f20.4)
   36 format(18h0 gen correlations)
   38 format(1h0,10x,10(10h----------))
  391 continue
c     a=ec wts  h=heritys  sig=ph vars  p=ph corr  g=gen corr
c-----convert phen vars to sds , hsq to gen vars to gen sds
      do5i=1,ntr
      sig(i)=sqrtf(sig(i))
    5 h(i)=sqrtf(h(i))*sig(i)
      write(lp,50)
   50 format(15h0ph stand devns)
      write(lp,35)(sig(i),i=1,ntr)
      write(lp,51)
   51 format(16h0gen stand devns)
      write(lp,35)(h(i),i=1,ntr)
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
      write(lp,40)
   40 format(17h0ph (co)variances)
      do 41 i=1,ntr
   41 write(lp,35)(p(i,j),j=1,i)
      write(lp,42)
   42 format(18h0gen (co)variances)
      do 43 i=1,ntr
   43 write(lp,35)(g(i,j),j=1,i)
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
      write(lp,44)
   44 format(29h0inverse ph covariance matrix)
      do 45 i=1,ntr
   45 write(lp,35)(p(i,j),j=1,i)
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
      write(lp,46)
   46 format(18h0p-inverse * g = x)
      do 47 i=1,ntr
   47 write(lp,35)(x(i,j),j=1,ntr)
  303 continue
c-----invert g cov matrix
      do 1450 i=1,ntr
 1450 call pivot(ginv,ntr,i)
      if(liop) 1451,1451,1452
 1452 write(lp,1453)
 1453 format('0g-inverse')
      do 1454 i=1,ntr
 1454 write(lp,35)(ginv(i,j),j=1,ntr)
 1451 continue
c-----economic weights & or desired gains
      read1,new,ndg
      write(lp,205) new,ndg
  205 format(34h0no of sets of economic weights = ,i3/
     +      '0no of sets of desired gains = ',i3)
      if(new-20) 984,984,985
  985 write(lp,986)
  986 format(17h0new max exceeded)
      stop
  984 continue
c-----loop over economic wts for unrestricted index
      nw=new+ndg
      read(lc,250) iform
      write(lp,251) iform
      idg=0
      do 998 inew=1,nw
      write(lp,38)
      write(lp,20)inew
      if(inew.le.new) go to 1400
c-----read desired gains
      idg=idg+1
      read(lc,iform)(dg(idg,i),i=1,ntr)
      write(lp,1410)(dg(idg,i),i=1,ntr)
 1410 format('0desired gains'/' ',5f20.4/(' ',5f20.4))
      write(lp,1411)
 1411 format('0weights')
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
      write(lp,35)(a(inew,i),i=1,ntr)
      write(lp,23)(b(inew,i,1),i=1,ntr)
      gain=sqrtf(chi)
      write(lp,22) gain,chi
      reg(inew)=gain
   23 format(37h0 coefficients for unrestricted index,/1h0,5f20.4/(
     +      1h ,5f20.4))
   20 format(19h0  economic wts set,i3)
   22 format('0genetic gain for unrestricted index in economic weight '
     +      ,'units per unit selection differential per generation ='
     +,f15.4/
     +      ,'0cov(hi) =',f20.4)
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
  124 write(lp,125)vi,vh,rih
  125 format(8h0v(i) = ,f20.4,10h   v(h) = ,f20.4,11h   r(ih) = ,f20.4)
c-----genetic covariance between y(j) and i
      do 130 j=1,ntr
      cyi(j)=0.0
      do 130 i=1,ntr
  130 cyi(j)=cyi(j)+b(inew,i,1)*g(i,j)
      write(lp,116)
      write(lp,35)(cyi(j),j=1,ntr)
c-----correlated changes in actual units
      do 210 j=1,ntr
  210 dgy(j)=cyi(j)/sqrtf(vi)
      write(lp,211)
  211 format('0correlated change in y(j) in actual units per unit '
     +     ,'selection differential per generation')
      write(lp,35)(dgy(j),j=1,ntr)
c-----genetic correlation b/n y(j) and i
      do 131 j=1,ntr
      sum=h(j)*h(j)*vh
      if(sum) 132,133,132
  133 cyi(j)=0.0
      go to 131
  132 cyi(j)=cyi(j)/sqrtf(sum)
  131 continue
      write(lp,127)
      write(lp,35)(cyi(j),j=1,ntr)
  998 continue
c-----test whether this job has sets of restrictions
      write(lp,38)
      write(lp,220)
  220 format(19h0restricted indices)
      read 1,nsr
      write(lp,52)nsr
   52 format(30h0no of sets of restrictions = ,i4)
      if(nsr-49) 987,987,988
  988 write(lp,989)
  989 format(17h0nsr max exceeded)
      stop
  987 continue
      if(nsr) 999,999,996
c-----loop over sets of restrictions
  996 read(lc,250) iform
      write(lp,251) iform
      do 60 isr=1,nsr
      write(lp,38)
      write(lp,58)isr
   58 format(20h0restriction set no ,i4)
      read 1,nr,ns
      write(lp,53)nr,ns
   53 format(25h0no of resi binet type = ,i4/
     +      40h0no of resi kempthorne or tallis type = ,i4)
c-----binet type
      if(nr+ns-ntr) 57,1600,1600
 1600 write(lp,1601)
 1601 format('0nr+ns must not exceed ntr')
      stop
   57 if(nr) 54,54,55
   55 if(liop) 394,395,395
  395 write(lp,62)
  394 continue
   62 format(9h0k matrix)
      do 61 i=1,nr
      read(lc,iform)(ek(i,j),j=1,ntr)
      do 67 j=1,ntr
   67 q(i,j)=ek(i,j)
      if(liop) 61,397,397
  397 write(lp,35)(ek(i,j),j=1,ntr)
   61 continue
      read(lc,iform)(d(i),i=1,nr)
      do 68 i=1,nr
   68 u(i)=d(i)
      write(lp,63)
      if(liop) 396,398,398
  398 continue
   63 format(9h0d vector)
      write(lp,35)(d(i),i=1,nr)
  396 continue
c-----kempthorne or tallis type
   54 if(ns) 155,155,56
   56 if(liop) 399,400,400
  400 write(lp,64)
  399 continue
   64 format(9h0c matrix)
      do 65 i=1,ns
      read(lc,iform)(c(i,j),j=1,ntr)
      do 69 j=1,ntr
      sum=0.0
      do 70 k=1,ntr
   70 sum=sum+c(i,k)*g(k,j)
   69 q(i+nr,j)=sum
      if(liop) 65,401,401
  401 write(lp,35)(c(i,j),j=1,ntr)
   65 continue
      read(lc,iform)(el(i),i=1,ns)
      do 71 i=1,ns
   71 u(i+nr)=el(ns)
      write(lp,66)
      if(liop) 402,403,403
  403 continue
   66 format(9h0l vector)
      write(lp,35)(el(i),i=1,ns)
  402 continue
c-----combine both types in printout
  155 nrs=nr+ns
      if(nrs) 60,60,156
  156 if(liop) 404,405,405
  405 write(lp,73)
   73 format(9h0q matrix)
      do 74 i=1,nrs
   74 write(lp,35)(q(i,j),j=1,ntr)
      write(lp,75)
   75 format(9h0u vector)
      write(lp,35)(u(i),i=1,nrs)
c-----calc reg for each set of ec wts
c-----calc b for each set of ce wts for this set of resi
c-----steps 1,2,3,4,5,7,8 are independant of ec wts and so outside wts loop
c-----step 1 calc q * p-inverse * q-transpose = w
  404 if(liop) 306,306,305
  305 continue
      write(lp,80)
   80 format(22h0q * p-inverse * q = w)
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
  304 write(lp,35)(w(i,j),j=1,nrs)
   81 continue
c-----step 2 invert w
      if(liop) 308,307,307
  307 continue
      write(lp,84)
   84 format(10h0w-inverse)
  308 continue
      do 85 i=1,nrs
   85 call pivot(w,nrs,i)
      if(liop) 310,310,309
  309 continue
      do 86 i=1,nrs
   86 write(lp,35)(w(i,j),j=1,nrs)
c-----step 3 calc q-transpose * w-inverse * q = v
      write(lp,87)
   87 format(22h0q * w-inverse * q = v)
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
  311 write(lp,35)(v(i,j),j=1,ntr)
   88 continue
c-----step 4 calc p-inverse * v = pv
      if(liop) 313,313,312
  312 continue
      write(lp,91)
   91 format(19h0p-inverse * v = pv)
  313 continue
      do 92 i=1,ntr
      do 93 j=1,ntr
      sum=0.0
      do 94 k=1,ntr
   94 sum=sum+p(i,k)*v(k,j)
      pv(i,j)=sum
   93 continue
      if(liop) 92,92,314
  314 write(lp,35)(pv(i,j),j=1,ntr)
   92 continue
c-----step 5 calc i - pv = pv
      if(liop) 316,316,315
  315 continue
      write(lp,95)
   95 format(12h0i - pv = pv)
  316 continue
      do 117 i=1,ntr
      do 96 j=1,ntr
      if(i-j) 97,98,97
   97 pv(i,j)=-pv(i,j)
      go to 96
   98 pv(i,j)=1.-pv(i,j)
   96 continue
      if(liop) 117,117,317
  317 write(lp,35)(pv(i,j),j=1,ntr)
  117 continue
c-----step 7 calc q-transpose * w-inverse * u = s
      if(liop) 319,319,318
  318 continue
      write(lp,102)
  102 format(22h0q * w-inverse * u = s)
  319 continue
      do 103 i=1,ntr
      sum=0.0
      do 104 k=1,nrs
      do 104 l=1,nrs
  104 sum=sum+q(k,i)*w(k,l)*u(l)
  103 s(i)=sum
      if(liop) 321,321,320
  320 continue
      write(lp,35)(s(i),i=1,ntr)
c-----step 8 calc p-inverse * s = ps
      write(lp,105)
  105 format(19h0p-inverse * s = ps)
  321 continue
      do 106 i=1,ntr
      sum=0.0
      do 107 j=1,ntr
  107 sum=sum+p(i,j)*s(j)
  106 ps(i)=sum
      if(liop) 323,323,322
  322 continue
      write(lp,35)(ps(i),i=1,ntr)
c-----loop over economic wts
  323 continue
      do 997 inew=1,nw
c-----step 6 calc pv * b-unrestricted = t
      write(lp,38)
      write(lp,20) inew
      if(liop) 325,325,324
  324 continue
      write(lp,99)
   99 format(24h0pv * unrestricted-b = t)
  325 continue
      do 100 i=1,ntr
      sum=0.0
      do 101 j=1,ntr
  101 sum=sum+pv(i,j)*b(inew,j,1)
  100 t(i)=sum
      if(liop) 327,327,326
  326 continue
      write(lp,35)(t(i),i=1,ntr)
c-----step 9 restricted index coefficients
  327 continue
      write(lp,108)
  108 format(30h0restricted index coefficients)
      do 109 i=1,ntr
  109 b(inew,i,isr+1)=t(i)+ps(i)
      write(lp,35)(b(inew,i,isr+1),i=1,ntr)
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
      write(lp,111) gain,chi
  111 format('0genetic gain for restricted index in economic weight '
     +     ,'units per unit selection differential per generation ='
     + ,f20.4/
     +      ,'0cov(hi) =',f20.4)
c-----correlation between i and h  rih
      sum=vi*vh
      if(sum) 112,113,112
  113 rih=0.0
      go to 114
  112 rih=chi/sqrtf(sum)
  114 write(lp,125)vi,vh,rih
c-----genetic covariance between y(j) and i = bt*g(j) = cyi(j)
      do 115 j=1,ntr
      cyi(j)=0.0
      do 115 i=1,ntr
  115 cyi(j)=cyi(j)+b(inew,i,isr+1)*g(i,j)
      write(lp,116)
  116 format(38h0genetic covariance between y(j) and i)
      write(lp,35)(cyi(j),j=1,ntr)
c-----correlated change in actual units
      do 230 j=1,ntr
  230 dgy(j)=cyi(j)/sqrtf(vi)
      write(lp,211)
      write(lp,35)(dgy(j),j=1,ntr)
c-----genetic correlation between y(j) and i
      do 118 j=1,ntr
      sum=h(j)*h(j)*vh
      if(sum) 119,126,119
  126 cyi(j)=0.0
      go to 118
  119 cyi(j)=cyi(j)/sqrtf(sum)
  118 continue
      write(lp,127)
  127 format(39h0genetic correlation between y(j) and i)
      write(lp,35)(cyi(j),j=1,ntr)
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
      dimension x(50,50)
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
      absf=dabs(x)
      return
      end
      subroutine pivot (x,n,m)
      implicit double precision (a-h,o-z)
      dimension x(50,50)
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
    5 write(lp,6)m
    6 format('0pivot step zero',i3)
      return
      end
      subroutine weight(new,ndg,idg,a,g,p,d,ntr)
      implicit double precision (a-h,o-z)
      dimension a(20,50),g(50,50),p(50,50),d(20,50)
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
    3 a(idg+new,i)=sumj
      return
      end
