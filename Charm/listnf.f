      program listnf
c-----ftn5 "charm" program written july 1985 by anne swinton.
c-----UNIX version 1/10/85  latest update 13/3/87
c-----lists whole file 120 characters per line, therefore a file
c-----of record length 110 char. plus rl and seq.no. takes one line.
      dimension recln(8),chcnt(2)
      character rec*1000,recln*120,chcnt*120
      equivalence (rec,recln(1))
      logical rdend
      common /cd/ rec
      common /ce/ chcnt
      common /stdio/ lin,lout,lmess
      data rec,chcnt/1*' ',2*' '/
      rdend = .false.
      write (lmess,10)
   10 format ((/),' listnf run')
      call argopn (10,1,'old')
      l = 0
    1 call rdrec (isn,10,rec,rdend,l,irl)
      if (rdend) goto 3
      if (l.eq.1) then
c-----set first 2 lines of output as column counters.
      call cntset (chcnt)
      call lincnt (isn,lines)
      end if
      do 2 j = 1,lines
      write (lmess,40) recln(j)
   40 format (' ',a)
    2 continue
      goto 1
    3 call eofr (10,l)
      stop
      end
