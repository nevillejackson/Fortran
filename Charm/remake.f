      program remake
c-----written for NOS 28/1/87 adapted for UNIX 16/3/87
c-----strips record length record from beginning of 'old' format CHARM file
c-----and drops sequence number from the end of each record.
c-----re-writes file in 'new' CHARM format with rl and seq.no. at beginning
c-----of each record in the file.
      character rec*1000
      logical rdend
      common /cd/ rec
      common /stdio/ lin,lout,lmess
      common /limits/ lc,ll,lp,lr,nv
      common /tio/ lti,lto
      data lti,lto/10,11/
      data rec/1*' '/
      rdend = .false.
      write (lmess,10)
   10 format ((/),' remake run')
      call argopn (lti,1,'old')
      call argopn (lto,2,'new')
c-----read record length record from beginning of input file (old CHARM format)
      read (lti,'(i10)',end=99,err=98,iostat=ios) irl
      if (irl.gt.lr) call exceed (4,irl)
      write (lmess,20) irl
   20 format (' record length read =',i5,' characters')
      write (lmess,30) irl
   30 format (' record length written =',i5,' characters')
c-----record count = k
      k = 0
c-----read records from unit10, add rl and sequence number to beginning
c-----of each record then write it to unit11
    1 read (lti,'(a)',end=99,err=98,iostat=ios) rec(1:irl)
      k = k+1
      call recwr (irl,k,rec,lto)
      goto 1
   98 call errr (lti,k,ios)
   99 call eofr (lti,k)
      call eofw (lto,k)
      stop
      end
