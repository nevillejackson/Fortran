      subroutine cnthd (head,bline,cnam,lenc,kcc,ihl,pnam,lenp,kld,
     +listf,lhead,lhl)
c-----sets heading for count output, to be printed at top of page
      dimension ihl(2),head(2),cnam(10),pnam(10),lenp(10),lenc(10)
      character head*120,lhead*120,bline*120,cnam*10,pnam*10
      logical listf
      common /tio/ lti,lto,lto2
      common /stdio/ lin,lout,lmess
      do 1 i = 1,120
    1 bline(i:i) = ' '
      head(1) = bline
      head(2) = bline
      head(1)(1:1) = '1'
      k = 11
      n = 1
      do 2 j = 1,kcc
      if (j.eq.6) then
      k = 11
      n = n+1
      end if
      kk = k-lenc(j)+1
      head(n)(kk:k) = cnam(j)(1:lenc(j))
      k = k+11
      kk = k-4
      head(n)(kk:k) = 'count'
      k = k+11
    2 continue
      if (n.eq.2) then
      ihl(1) = 120
      end if
      ihl(n) = k-11
      do 4 l=1,n
      write (lto,20) head(l)(1:ihl(l))
   20 format (a)
    4 continue
      if (listf) then
      lhead(2:7) = 'seq no'
      ie = 9
      do 3 i = 1,kld
      ie = ie+11
      ib = ie+1-lenp(i)
    3 lhead(ib:ie) = pnam(i)(1:lenp(i))
      write (lto,20) lhead(1:ie)
      lhl = ie
      end if
      return
      end
