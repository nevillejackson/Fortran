      program undo
c-----ftn5 program to remove record length and sequence number from
c-----the beginning of each record in a created file.
c-----3/10/86 Anne Swinton ... name changed from strip to undo 6/7/87
      character lit*1000
      logical rdend
      common /cd/ lit
      common /stdio/ lin,lout,lmess
      common /tio/ lti,lto
      data lti,lto/10,11/
      data lit/1*' '/
      rdend = .false.
      write (lmess,10)
   10 format ((/),' undo run')
      call argopn (lti,1,'old')
      call argopn (lto,2,'new')
      l = 0
    1 call readch (irl,iseq,lti,lit,rdend,l)
      if (rdend) goto 2
      write (lto,20) lit(:irl)
   20 format (a)
      goto 1
    2 call eofr (lti,l)
      call eofw (lto,l)
      stop
      end
