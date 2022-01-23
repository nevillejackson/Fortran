      program listns
c-----ftn5 "charm" program written july 1985 by anne swinton.
c-----UNIX version 30/9/85  latest update 13/3/87
c-----lists whole file 120 characters per line
c-----record length and sequence number not printed.
      dimension recln(8),chcnt(2)
      character lit*1000,recln*120,chcnt*120
      equivalence (lit,recln(1))
      logical rdend
      common /cd/ lit
      common /ce/ chcnt
      common /stdio/ lin,lout,lmess
      data lit,chcnt/1*' ',2*' '/
      rdend = .false.
      write (lmess,10)
   10 format ((/),' listns run')
      call argopn (10,1,'old')
      l = 0
    1 call readch (irl,iseq,10,lit,rdend,l)
      if (rdend) goto 3
      if (l.eq.1) then
      write (lmess,15) 10,irl
   15 format(' file unit ',i3,' record length read =',i9,' characters')
      write (lmess,20)
   20 format (' ')
c-----set first 2 lines of output as column counters
      call cntse1 (chcnt)
      call lincn1 (irl,lines)
      end if
      do 2 j = 1,lines
      write (lmess,40) recln(j)
   40 format (' ',a)
    2 continue
      goto 1
    3 call eofr (10,l)
      stop
      end
