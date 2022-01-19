      program resi3
c
c  purpose of program
c  ------- -- -------
c
c      selection index calculations
c           - any combination of - (i) individual values
c                                - (ii) half sib family means
c                                - (iii) full sib family means
c                                - (iv) progeny test means
c           - for multiple traits
c           - with gains restricted to zero for any subset of traits
c           - gains estimated for all traits and for index
c           - any subset of traits (including empty subset) for each
c             of (i),(ii),(iii),(iv) above.
c           - for multiple sets of economic weights &/or desired gains
c           - three types of index computed
c                     - all_traits unrestricted combined index
c                     - selected traits unrestricted combined index
c                     - selected traits restricted combined index
c               each for all sets of economic weights or desired gains
c
c
c
c      authors
c      ------
c         N.Jackson
c         C.S.I.R.O.,Division of Animal Production
c         p.o. box 239 Blacktown,N.S.W.,2140
c         Australia
c
c         C.A.Dean & P.P.Cotterill
c         C.S.I.R.O.,Division of Forest Research
c         Cunningham Laboratory
c         306 Carmody Road,St.Lucia,Qld.,4067
c         Australia
c
c         Copyright 1987 by C.S.I.R.O.
c
c     references
c     ----------
c        james,j.w.(1968) biometrics 24:1015-1018
c        binet,f.e.(1965) biometrics 21:291-299
c        kempthorne,o. & nordskog,a.w.(1959) biometrics 15:10-19
c        tallis,g.m.(1962) biometrics 18:120-122
c        cotterill,p.p. & jackson,n.(1981) silvae genetica 30:106-108
c        cotterill,p.p. & jackson,n.(1985) silvae genetica 34:56-63
c        cotterill,p.p. & jackson,n.(1987) in press
c
c     invoking resi3 under UNIX
c     -------- ----- ----- ----
c        Set up the following shellscript on a file
c
c        resi3 filename <<eoi
c        )
c        )  index parameter records redirected to STDIN
c        )
c        eoi
c
c        The 'filename' option is required when parameter MIOP=1
c        The above shellscript is invoked by 
c          sh -x scriptname >outfile 2>&1 &
c        in the Bourne shell or
c          sh -x scriptname >&outfile &
c        in the C_shell,
c        where 'scriptname' is the file containing the shellscript.
c
c
c
      implicit double precision (a-h,o-z)
c-----array size limits as follows:-
c     mxltr  - no traits in library
c     mxltr2 - size of correlation matrices in library
c      mxntr - no traits in 'this run'
c       mxnw - no sets of econ.wts. + des.gain implied wts.
c      mxndg - no sets desired gains (= implied wts.)
c       mxns - no sets of delta_G restrictions
c     mxntr4 - no of trait*block combinations (block is I,H,F or O)
c
      parameter ( mxltr = 50,
     +           mxltr2 = mxltr*(mxltr+1)/2,
     +            mxntr = 20,
     +             mxnw = 12,
     +            mxndg = 4,
     +             mxns = 5,
     +           mxntr4 = 4*mxntr )
c----- array names and uses
c     hl(mxltr)		heritability,lib
c     sigl(mxltr)	phen. s.d.,lib
c     chl(mxltr)	common env. % var half_sib,lib
c     cfl(mxltr)	common env. % var full_sib,lib
c     rgl(mxltr2)	gen. correln.,lib
c     rpl(mxltr2)	phen. correln.,lib
c     rchl(mxltr2)	common env. correln. half_sib,lib
c     rcfl(mxltr2)	common env. correln. full_sib,lib
c
c     sigp(mxntr4)	phen. s.d., I,H,F,O partitions
c     sigg(mxntr4)	gen. s.d., I,H,F,O partitions
c     p(mxntr4,mxntr4)	P cov matrix I,H,F,O partitions
c     g(mxntr4,mxntr4)	G cov matrix I,H,F,O partitions
c     ch(mxntr,mxntr)	Chs cov matrix I partition 
c     cf(mxntr,mxntr)	Cfs cov matric I partition
c     
c     l(mxntr)		library trait no
c     label(mxntr)	trait name
c     ini(mxntr)	flag for in index I partition (1=in,0=not-in)
c     inh(mxntr)	flag for in index H partition
c     inf(mxntr)	flag for in index F partition
c     ino(mxntr)	flag for in index O partition
c
c     a(mxnw,mxntr)	economic weights
c     dg(mxndg,mxntr)	desired gains
c     gres(mxns,mxntr)	restrictions on genetic gain (1=not constrained,
c                                                   2=constrained to delta_G=0)
c
c     listi(mxntr)	trait no within index_set I partition
c     listh(mxntr)	trait no within index_set H partition
c     listf(mxntr)	trait no within index_set F partition
c     listo(mxntr)	trait no within index_set O partition
c                       (n.b. for library trait no use l() )
c
c     pp(mxntr4,mxntr4)	preserves P when P is inverted
c     ginv(mxntr4,mxntr4) G_inverse
c     x(mxntr4,mxntr4)	P_inverse*G
c     bu(mxnw,mxntr4)	coeff's of all_traits unrestricted combined index
c     b(mxnw,mxntr4)	coeff's of selected_traits combined index
c			(unrestricted or restricted as current)
c     cyi(mxntr4)	cov(Y(i),Index)
c     dgy(mxntr4)	delta_G(Y(i))
c     rgyi(mxntr4)      Rg(Y(i),Index)
c
c     q(mxntr4,mxntr4)	Binet + delta_G restrictions LHS
c     u(mxntr4)         Binet + delte_G restrictions RHS
c
c     c(mxntr,mxntr)	delta_G restriction LHS - temp workspace
c
c     w(mxntr4,mxntr4)	q*P_inverse*q
c     v(mxntr4,mxntr4)	q*w_inverse*q
c     pv(mxntr4,mxntr4)	I-P_inverse*V
c     s(mxntr4)		q*w_inverse*u
c     ps(mxntr4)	P_inverse*s
c     t(mxntr4)		pv*b
c
c----- single variable names & uses
c	ntr	no traits per partition in unrestricted combined index
c	ntr4	ntr*4
c	nsr	no of sets of delta_G restrictions
c	nr	no of Binet restrictions (only one set)
c	ns	no of delta_G restrictions within a set
c	ltr	no of traits in library file
c-----
      character head*100,label*10,head2*100
      dimension hl(mxltr),sigl(mxltr),chl(mxltr),cfl(mxltr)
      dimension rgl(mxltr2),rpl(mxltr2),rchl(mxltr2),rcfl(mxltr2)
      dimension sigp(mxntr4),sigg(mxntr4)
      dimension p(mxntr4,mxntr4),g(mxntr4,mxntr4)
      dimension ch(mxntr4,mxntr4),cf(mxntr4,mxntr4)
      dimension l(mxntr),label(mxntr),ini(mxntr),inh(mxntr),
     + inf(mxntr),ino(mxntr)
      dimension a(mxnw,mxntr),dg(mxndg,mxntr),gres(mxns,mxntr)
      dimension listi(mxntr),listh(mxntr),listf(mxntr),listo(mxntr)
      dimension pp(mxntr4,mxntr4),ginv(mxntr4,mxntr4),x(mxntr4,mxntr4)
      dimension b(mxnw,mxntr4),bu(mxnw,mxntr4)
      dimension cyi(mxntr4),dgy(mxntr4),rgyi(mxntr4)
      dimension q(mxntr4,mxntr4),u(mxntr4)
      dimension c(mxntr,mxntr)
      dimension w(mxntr4,mxntr4),v(mxntr4,mxntr4)
      dimension pv(mxntr4,mxntr4),s(mxntr4),t(mxntr4),ps(mxntr4)
c-----equivalence old names (h & sig) to new (sigg & sigp)
      dimension h(mxntr4),sig(mxntr4)
      equivalence (h,sigg),(sig,sigp)
c-----
      common /stdio/ lc,lp,lerr
      common /limits/mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      common /files/ mi
c-----limit variables
      data mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
     +    /mxltr,mxltr2,mxntr,mxnw,mxndg,mxns,mxntr4/
c-----I/O units
      data lc,lp,lerr /5,6,6/
      data mi /10/
c-----
c-----write logo to front of output
      call logo
c-----
      write(lp,208)
  208 format(///' JOB PARAMETERS AS READ FROM STANDARD INPUT FILE')
      read (lc,*) njobs,liop,miop,ltr
      write(lp,200)njobs,liop,miop,ltr
  200 format(/' njobs = ',i3/' liop = ',i3/
     +      ' miop = ',i3/' ltr = no. library traits = ',i3)
c-----
c-----read population parameters library file
      if(miop.gt.0) then
        call parlib(miop,ltr,hl,sigl,rgl,rpl,chl,cfl,rchl,rcfl)
      endif
c-----
c-----loop over jobs
      do 999 ij=1,njobs
c-----read combined index parameters if miop.gt.0
      if(miop.gt.0) then
        call parind(ij,head,ltr,ntr,new,ndg,nsr,nr,ntr4,
     +    l,label,ini,inh,inf,ino,a,dg,gres,
     +    listi,listh,listf,listo,nh,nf,no)
        call covset(ij,liop,ltr,ntr,nh,nf,no,hl,sigl,rgl,rpl,
     +    chl,rchl,cfl,rcfl,ntr4,p,g,sigp,sigg,
     +    l,label,ch,cf)
      endif
c-----
c-----
c-----read job parameters from stdin if miop.eq.0
c-----p and g matrices read upper triangular columnwise symmetric + diag
      if (miop.eq.0) then
        call parold(ij,ntr4,liop,sig,h,p,g)
      endif
c-----end of stdin input
c-----
c-----following code common to combined or individual indices (ie any miop)
c-----
c-----
c-----all_traits unrestricted combined index
c-----(ie all traits in P & G matrices = ntr4 traits = ntr*4)
c-----(ie all ntr traits in each of I,H,F,O partitions)
      if(liop.eq.1) then
        write(lp,38)
        write(lp,38)
      endif
c-----
      head2='ALL TRAITS UNRESTRICTED COMBINED INDEX'
c-----
      call unresa(liop,ntr,new,ndg,nw,ntr4,p,g,h,sig,
     + a,dg,label,pp,ginv,x,bu,cyi,dgy,rgyi,head,head2,l,
     + ini,inh,inf,ino,nh,nf,no)
c-----end of unrestricted index
c-----
c-----
c-----
c-----selected_traits unrestricted combined index
c-----(ie Binet restrictions for traits not in index
c-----  but no delta_G restrictions)
      if(liop.eq.1) then
        write(lp,38)
        write(lp,38)
      endif
c-----
      head2='SELECTED TRAITS UNRESTRICTED COMBINED INDEX'
c-----
c-----setup Binet restriction matrices
      call binset(nr,q,u,ntr,ntr4,label,ini,inh,inf,ino)
c-----solve this index
c----- ns is zero for resa because no delta_G restrictions
       call resa(isr,liop,nr,0,nsr,nw,ntr,ntr4,label
     + ,q,u,p,w,v,pv,s,ps,t,bu,b,g,cyi,dgy,pp,a,h,rgyi,head,head2,l
     + ,nh,nf,no)
c-----
c-----
c-----selected traits restricted combined indices
c-----(ie Binet restrictions + delta_G constraints)
c-----
      if(liop.eq.1) then
        write(lp,38)
        write(lp,38)
   38   format(/10x,10('----------'))
      endif
c-----
      head2='SELECTED TRAITS RESTRICTED COMBINED INDEX'
c-----
      if(liop.eq.1) then
        write(lp,52)nsr
   52   format(/' no of sets of restrictions = ',i4)
      endif
      if(nsr-mvns) 987,987,988
  988 write(lp,989)
  989 format(/' nsr max exceeded')
      stop
  987 continue
      if(nsr) 999,999,996
c-----loop over sets of restrictions
  996 do 60 isr=1,nsr
      if(liop.eq.1) then
        write(lp,38)
        write(lp,58)isr
   58   format(/' restriction set no ',i4)
      endif
c-----setup delta_G restriction matrices one set at a time
c----- ns set in kemset
      call kemset(isr,nr,ns,q,u,c,ntr,ntr4,nsr,gres,g,liop)
c-----
      if(liop.eq.1) then
        write(lp,53)nr,ns
   53   format(25h0no of resi binet type = ,i4/
     +      40h0no of resi kempthorne or tallis type = ,i4)
      endif
      if(nr+ns-ntr4) 57,1600,1600
 1600 write(lp,1601)
 1601 format(' nr+ns must not exceed ntr4')
      go to 60
c-----
   57 call resa(isr,liop,nr,ns,nsr,nw,ntr,ntr4,label
     + ,q,u,p,w,v,pv,s,ps,t,bu,b,g,cyi,dgy,pp,a,h,rgyi,head,head2,l
     + ,nh,nf,no)
c-----
c-----end of isr loop is 60
   60 continue
c-----
c-----end of ij loop is 999
  999 continue
      stop
      end
