      subroutine hdset (kcc,ksd,cnam,lenc,chead,cfhd,tnam,lent,
     +rchd,thd,ehd,ohd,int,trayt,tsumsq,lhead,pnam,lenp,kld,listf 
     +,multi)
c-----sets standard headings for output tape11 and sets overall
c-----counts to zero
      dimension cnam(10),lenc(10),tnam(50),lent(50),pnam(10), 
     +lenp(10),int(50),trayt(50),tsumsq(50) 
      dimension chead(2),cfhd(2),rchd(2),thd(2,50,4)
      double precision trayt,tsumsq
      character chead*120,cfhd*120,rchd*120,thd*120,ehd*120 
      character ohd*120,lhead*120 
      character cnam*10,tnam*10,pnam*10 
      logical listf,multi 
      common /tio/ lti,lto,lto2
      multi = .false.
c-----set page change required before counts begin 
      write (lto,10) 1
   10 format (i1)
c-----set count headings as given by control change directives 
      ie = 16 
      j = 1
      do 1 i = 1,kcc
      if (i.eq.11) then
      j = 2
      ie = 26
      multi = .true.
      end if
      ie = ie+10
      ib = ie+1-lenc(i)
    1 chead(j)(ib:ie) = cnam(i)(1:lenc(i))
c-----field content and record count headings
      cfhd(1)(1:14) = ' control field'
      rchd(1)(1:13) = ' record count' 
      if (multi) then
      cfhd(2) = cfhd(1)
      rchd(2) = rchd(1)
      end if
      do 2 i = 1,ksd
      int(i) = 0.0
      trayt(i) = 0.0
      tsumsq(i) = 0.0
      ie = 1+lent(i)
      thd(1,i,1)(2:ie) = tnam(i)(:lent(i))
      thd(1,i,1)(13:15) = 'cnt' 
      thd(1,i,2)(13:15) = 'tot' 
      thd(1,i,3)(13:15) = 'ave' 
      thd(1,i,4)(14:15) = 'sd'
      if (multi) then
      thd(2,i,1) = thd(1,i,1)
      thd(2,i,2) = thd(1,i,2)
      thd(2,i,3) = thd(1,i,3)
      thd(2,i,4) = thd(1,i,4)
      end if
    2 continue
      ehd(1:1) = '0'
      ohd(20:26) = 'overall'
c-----heading if field(s) to be listed
      if (listf) then
      ie = 9 
      lhead(2:7) = 'seq no'
      do 3 i = 1,kld
      ie = ie+11
      ib = ie+1-lenp(i)
    3 lhead(ib:ie) = pnam(i)(1:lenp(i)) 
      write (lto,20) lhead
   20 format (a)
      end if
      return
      end
