      program cardim
c-----ftn5 "charm" program written july 1985 by anne swinton.
c-----latest update 30/7/85
c-----UNIX version 11/9/85
c-----card image file written to tape 11.
      character rec*1000,rl*10
      logical rdend
      common /cd/ rec
      common /stdio/ lin,lout,lmess
      common /tio/ lti,lto
      data lti,lto/10,11/
      data rec/1*' '/
      rdend = .false.
      rewind lti
      rewind lto
      write (lmess,10)
   10 format ((/),' cardim run')
      call argopn (lti,1,'old')
      call argopn (lto,2,'new')
      call lnread (rl,irl,itp,lti,iw)
      l = 0
    1 call readcf (itp,lti,rec,rdend,l)
      if (rdend) goto 2
      write (lto,20) rec(:itp)
   20 format (a)
      goto 1
    2 call eofr (lti,l)
      call eofw (lto,l)
      stop
      end
      subroutine lnread(rl,irl,itp,lti,iw)
c-----reads record length record (a10) from unit lti.
c-----checks "itp" length to be read before sequence number
      character rl*10,bf*1
      common /limits/ lc,ll,lp,lr,nv
      common /stdio/ lin,lout,lmess
      read (lti,10,err=98,end=99,iostat=ios)rl
   10 format (a10)
c-----convert rl to integer irl and check within limits
      read (rl,'(i10)')irl
      if (irl.gt.lr) goto 40
c-----check if blank fill required from rl to sequence no. word
      bf = rl(10:10)
      read (bf,'(i1)')ibf
      if (ibf.ne.0) then
c-----blank fill for "itp"
      itp = irl + (10-ibf)
      else
      itp = irl
      end if
      iw = itp/10
      write (lmess,30) lti,itp
   30 format (' file tape',i3,'   record length read =',i5)
      return
   40 call exceed (4,irl)
      return
   98 call errr(lti,1,ios)
      call jobend
      return
   99 call eofr (lti,1)
      return
      end
      subroutine readcf (itp,lti,rec,rdend,l)
c-----reads a  record of length itp characters from tape
c-----10 into string rec.
      character rec*1000
      logical rdend
      common /stdio/ lin,lout,lmess
c-----returns ios = -1 if eof
c-----        ios = 0 if ok
c-----        ios = +n if error condition n, aborts without return
      rdend = .false.
      read(lti,10,err=98,end=99,iostat=ios)rec(1:itp)
   10 format (a)
      l = l+1
      return
   98 call errr (lti,l,ios)
      call jobend
      return
c-----end of input file ... number of records read/written = l
   99 rdend = .true.
      return
      end
