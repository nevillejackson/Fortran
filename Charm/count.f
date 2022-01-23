      program count
c-----Re-written 20/3/86 to do counts,sums,means or just counts (UNIX)
c-----latest update 11/4/86  Anne Swinton
c-----Limits for *c and *l directives reduced to 10.
c-----Count summary file optional, requires user to supply a format.
c-----numbers treated as type REAL (NOS) type DOUBLE PRECISION (UNIX)
c-----kcc count of control change directives  (limit=10)
c-----ksd   "   "  summing            "       (limit=50)
c-----kld   "   "  list (traits only) "       (limit=10)
c-----ksf   "   "  records written to count summary file (lto2)
      dimension cnam(10),lenc(10),icfb(10),icfl(10),os(10),
     +tnam(50),lent(50),itb(50),itl(50),itd(50),
     +pnam(10),lenp(10),ipb(10),ipl(10),ipd(10),
     +fc(10),in(20,50),nrec(20) 
      dimension fmt(50,3),dline(2)
      dimension head(2),ihl(2),chead(2),cfhd(2),rchd(2),thd(2,50,4)
      dimension trays(20,50),traits(20,50)
      dimension jnt(50),tsumsq(50),trayt(50)
      character lit*1000
      character chead*120,cfhd*120,rchd*120,thd*120,ehd*120,ohd*120 
      character bline*120,dline*120,head*120,lhead*120,par*1 
      character fc*10,trac*10,fmt*7
      character cnam*10,tnam*10,pnam*10,sform*80,os*1 
      double precision trays,trait,traits,tave,tsd
      double precision trayt,tsumsq 
      logical list,listf,csf,newhd,rdend,change,partc,multi 
      common /stdio/ lin,lout,lmess
      common /tio/ lti,lto,lto2
      common /limits/ lcard,lline,lpar,lrec,nval
      common /ca/ chead
      common /cb/ cfhd
      common /cc/ rchd
      common /cd/ thd
      common /ce/ ehd
      common /cf/ ohd
      common /cg/ lhead 
      common /ci/ trac
      common /cj/ fmt
      common /cl/ tnam
      common /cm/ pnam
      common /cn/ sform
      common /co/ os
      common /cp/ lit
      common /cq/ head
      common /cr/ dline
      common /cr/ bline
      data head,chead,cfhd,rchd,ehd,ohd,lhead/11*' '/ 
      data thd,fc/400*' ',10*' '/ 
      data cnam,tnam,pnam/10*' ',50*' ',10*' '/
      data trac,par/1*' ',1*' '/
      data fmt/150*' '/
      data lti,lto,lto2/10,11,12/
c---------
      write(lmess,20) 
   20 format ((/),' count run') 
      call argopn (lti,1,'old')
      call argopn (lto,2,'new')
      if (iargc().eq.3) then
      call argopn (lto2,3,'new')
      end if
c-----read count directives
      call cntrd (cnam,lenc,icfb,icfl,os,tnam,lent,itb,itl,itd,
     +pnam,lenp,ipb,ipl,ipd,kcc,ksd,kld,list,listf,sform,irl2,csf) 
      if (iargc().eq.3.and.(.not.csf)) then
      write (lmess,30) 
   30 format(' ***warning*** no count summary file format .. no file')
      end if
c-----page change before counts begin and set standard headings
c-----and set overall counts to zero
      if (ksd.eq.0) then
      call cnthd (head,bline,cnam,lenc,kcc,ihl,pnam,lenp,kld,listf,
     +lhead,lhl)
      dline(1) = bline
      dline(2) = bline
      else
      call hdset (kcc,ksd,cnam,lenc,chead,cfhd,tnam,lent,rchd,thd,
     +ehd,ohd,jnt,trayt,tsumsq,lhead,pnam,lenp,kld,listf,multi) 
c-----set formats for char/double precision conversions according to
c-----the *s directives
      do 2 i = 1,ksd
      id = itd(i)
      call rlform (id,fmt(i,1))
      id = id+1
      call rlform (id,fmt(i,2))
      id = id+2
      call rlform (id,fmt(i,3))
    2 continue
      end if
      knt = 0
      ksf = 0
      change = .true.
      partc = .false.
      newhd = .false.
      k = 0
c-----read a record from input (tape10)
    1 call readch (irl,iseq,lti,lit,rdend,knt)
      if (rdend) goto 8
c-----         ***          ***          ***          ***
    6 if ((change).or.(partc)) then
c-----control field content has changed
      if (listf) then
      if (newhd) write (lto,40) lhead
      newhd = .false.
      call flist (ipb,ipl,ipd,kld,lit,iseq)
      else if (list) then
      write (lto,27) iseq,lit(1:irl)
   27 format (1x,i10,1x,a)
      end if
      if (ksd.eq.0) then
      ie = 11
      n = 1
      do 41 i = 1,kcc
      ib = ie-icfl(i)+1
      ire = icfb(i)+icfl(i)-1
      fc(i) = lit(icfb(i):ire)
      dline(n)(ib:ie) = fc(i)
      if (i.ge.k) nrec(i) = 1
      ie = ie+22
      if (ie.gt.99) then
      ie = 11
      n = 2
      end if
   41 continue
      else
c-----control field sub-heading set to blank from col.17 or from 
c-----changed field's col. to end
      if (partc) then
      ie = 7 + k*10 
      if (ie.ge.117) then
      ie = ie-100
      ii = 2
      else
      ii = 1
      end if
      else
      ie = 17 
      k = 1
      end if
      call blanks (kcc,ii,ie,cfhd)
      ie = ie-1
c-----control change loop
      ii = 1
      do 3 i = k,kcc
      if (i.eq.11.and.ii.eq.1) then
      ie = 16
      ii = 2
      end if
      ie = ie+10
      ib = ie+1-icfl(i)
      ire = icfb(i)+icfl(i)-1
c-----set field content to be compared and write it to sub-head
      fc(i) = lit(icfb(i):ire)
      cfhd(ii)(ib:ie) = fc(i) 
      nrec(i) = 1
c-----summing loop ... initiate sums (trays) and sums of squares (traits)
c-----for each trait field 
      do 4 j = 1,ksd
      in(i,j) = 0
      jj = itl(j)
      trac(:jj) = lit(itb(j):(itb(j)+jj-1))
c-----check for eleven punching or blank data field
      if (trac(jj:jj).eq.'-'.or.trac(jj:jj).eq.' ') then
      trays(i,j) = 0.0
      traits(i,j) = 0.0
      else
c-----convert character string to type double precision
      in(i,j) = in(i,j)+1
      call chdp (itl(j),trac,itd(j),trait)
      trays(i,j) = trait
      traits(i,j) = trait*trait
      end if
    4 continue
    3 continue
      end if
      change = .false.
      partc = .false.
      else
c-----          ***          ***          ***          ***
c-----check for change in control field(s) 
      do 5 k = 1,kcc
      ire = icfb(k)+icfl(k)-1
      if (os(k).eq.'a') then
      if (fc(k).gt.lit(icfb(k):ire)) call seqerr (k,knt)
      else
      if (fc(k).lt.lit(icfb(k):ire)) call seqerr (k,knt)
      end if
      if (fc(k).ne.lit(icfb(k):ire)) goto 9 
c-----unchanged, trait values added to sums and counts incremented 
      nrec(k) = nrec(k)+1
      do 7 m = 1,ksd
      ie = itb(m)+itl(m)-1
c-----check for valid trait value
      par = lit(ie:ie)
      if (par.ne.'-'.and.par.ne.'+'.and.par.ne.' ') then
      in(k,m) = in(k,m)+1
      trac(:itl(m)) = lit(itb(m):ie)
      call chdp (itl(m),trac,itd(m),trait)
      trays(k,m) = trays(k,m)+trait 
      traits(k,m) = traits(k,m) + (trait*trait) 
      end if
    7 continue
    5 continue
c-----          ***          ***          ***          ***
c-----check for list directive
      if (listf) then
      call flist (ipb,ipl,ipd,kld,lit,iseq)
      else if (list) then
      write (lto,27) iseq,lit(1:irl)
      end if
      end if
      k = 1
      goto 1
c-----control field changed, counts,sums,means and sd's done 
c-----for changed field and lesser ones. if change at field one
c-----total counts incremented (... overall results at end)
    8 k = 1
    9 if (k.eq.1) then
      change = .true.
      do 88 i = k,ksd
      jnt(i) = jnt(i)+in(1,i) 
      trayt(i) = trayt(i)+trays(1,i)
   88 tsumsq(i) = tsumsq(i)+traits(1,i) 
      else
      partc = .true.
      end if
c-----check for *c and no *s
      if (ksd.eq.0) then
      i = 1
      m = 22*k
      do 50 l = k,kcc
      if (l.eq.6) i=2
      mm = m-9
      write (dline(i)(mm:m),'(i10)') nrec(l)
   50 m = m+22
      do 55 n = 1,i
      write (lto,40) dline(n)
   40 format (a)
      dline(n) = bline
   55 continue
      if (.not.rdend) goto 6
      dline(1)(5:11) = 'overall'
      write (dline(1)(12:21),'(i10)') knt
      write (lto,40) dline(1)
      goto 70
      end if
c-----set count fields to blank before writing results 
      call blankc (thd,rchd,ksd,multi)
      kk = 17+((k-1)*10)
      kl = kk+9
      if (k.lt.11) then
      ii = 1
      else
      ii = 2
      kk = kk-100
      kl = kl-100
      end if
c-----incremental count of records for each control field
      do 10 l = k,kcc
      if (l.eq.11) ii=2
      write (rchd(ii)(kk:kl),'(i10)')nrec(l)
c-----do counts,sums,means,standard deviations for all traits
c-----in a control field one at a time 
      do 11 n = 1,ksd
      write (thd(ii,n,1)(kk:kl),'(i10)')in(l,n) 
      write (thd(ii,n,2)(kk:kl),fmt(n,1))trays(l,n)
      if (in(l,n).eq.0) then
      tave = 0.0
      tsd = 0.0
      else
      x = in(l,n)
      tave = trays(l,n)/x
c-----call subroutine to estimate sd for this trait(n) of field
      call sdest (x,trays(l,n),traits(l,n),tsd) 
      end if
      write (thd(ii,n,3)(kk:kl),fmt(n,2))tave
      write (thd(ii,n,4)(kk:kl),fmt(n,3))tsd 
      if (l.eq.kcc.and.(csf).and.in(l,n).gt.0) then
      ksf = ksf+1
      write(lto2,sform)irl2,ksf,(fc(i),i=1,kcc),n,in(l,n),trays(l,n),
     +tave,tsd
      end if
   11 continue
      kk = kk+10
      if (kk.gt.107) then
      kk = kk-100
      kl = kl-100
      end if
   10 kl = kl+10
c-----write head (chead,cfhd,rchd) and counts (thd) to output
      ij = 1
      if (multi) ij=2
      do 19 ii = 1,ij
      write (lto,40) chead(ii)
      write (lto,40) cfhd(ii) 
      write (lto,40) rchd(ii) 
      do 12 n = 1,ksd
      do 13 nn = 1,4
   13 write (lto,40) thd(ii,n,nn) 
   12 continue
   19 continue
      write (lto,40) ehd
      newhd = .true.
      if (.not.rdend) goto 6
c-----end of data on input ... overall totals to output
      write (lto,40) ohd
      call blankc (thd,rchd,ksd,multi)
      write (rchd(1)(17:26),'(i10)')knt 
      do 14 n = 1,ksd
      write (thd(1,n,1)(17:26),'(i10)')jnt(n) 
      write (thd(1,n,2)(17:26),fmt(n,1))trayt(n) 
      x = jnt(n)
      tave = trayt(n)/x
      write (thd(1,n,3)(17:26),fmt(n,2))tave 
      call sdest (x,trayt(n),tsumsq(n),tsd)
      write(thd(1,n,4)(17:26),fmt(n,3))tsd
   14 continue
      write (lto,40) rchd
      do 15 n = 1,ksd
      do 16 nn = 1,4
   16 write (lto,40) thd(1,n,nn)
   15 continue
   70 call eofr (lti,knt)
      end file lto
      rewind lto
      close (lto,status='keep')
      if (csf) then
      write (lmess,60) lto2,irl2 
   60 format(' file unit',i4,' record length written =',i10,
     +' characters')
      call eofw (lto2,ksf)
      end if
      stop
      end
