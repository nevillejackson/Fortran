      subroutine readch (irl,iseq,lti,lit,rdend,l)
c------reads a "charm" record with record length(irl i3) and sequence
c------number (iseq i6) in first 10 cols followed by record (lit(1:irl)
c------of type character).
      character lit*1000
      logical rdend
      common /stdio/ lin,lout,lmess
c------returns ios = -1 if eof
c------        ios = 0 if ok
c------        ios = +n if error condition n, aborts without return
      rdend = .false.
      read(lti,10,err=98,end=99,iostat=ios)irl,iseq,lit(:irl)
   10 format (i3,i6,1x,a)
      l = l+1
      return
   98 call errr (lti,ios,iseq)
      call jobend
      return
c------end of input file ... number of records read/written = l
   99 rdend = .true.
      return
      end
