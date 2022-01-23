      subroutine rdrec (isn,lti,rec,rdend,l,irl)
c-----reads a "charm" record plus it's length and sequence number into
c-----"rec" length "isn" (itp+10) from unit lti.
      character rec*1000,lit*1000
      logical rdend
      common /stdio/ lin,lout,lmess
      data lit/1*' '/
c-----returns ios = -1 if eof
c-----        ios = 0 if ok
c-----        ios = +n if error condition n, aborts without return
      rdend = .false.
      if (l.eq.0) then
      read (lti,10,err=98,end=99,iostat=ios) irl,iseq,lit(:irl)
   10 format (i3,i6,1x,a)
      isn = 10+irl
      write (rec(1:3),'(i3)') irl
      write (rec(4:9),'(i6)') iseq
      write (rec(11:isn),'(a)') lit(:irl)
      l = l+1
      write (lmess,15) lti,irl
   15 format (' file unit ',i3,' record length read =',i9,' characters')
      write (lmess,20)
   20 format (' ')
      return
      else
      read(lti,30,err=98,end=99,iostat=ios)rec(1:isn)
   30 format (a)
      l = l+1
      return
      end if
   98 call errr (lti,l,ios)
      call jobend
      return
c-----end of input file ... number of records read/written = l
   99 rdend = .true.
      return
      end
