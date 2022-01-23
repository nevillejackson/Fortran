      program reclen
c-----reads record length of CHARM files, writes filename + rl to output
c-----along with length of record ( ilen = irl+10 ).
c-----written Sept.'86 (Anne Swinton) for making tapes for TMUS backup 
      character fname*14
      data lti,lto,lti2/10,11,12/
      data fname/1*' '/
c-----get file of groups to have record lengths read
      call argopn (lti,1,'old')
c-----open output file and set counter
      call argopn (lto,2,'new')
      k = 0
    1 read (lti,10,err=97,end=99,iostat=ios) fname
   10 format (a)
      k = k+1
      open (lti2,file=fname,status='old',err=98,iostat=ios)
      read (lti2,20) irl
   20 format (i3)
      ilen = irl+10
      write (lto,30) fname,irl,ilen
   30 format (a,1x,i3.3,1x,i3.3)
      rewind lti2
      close (lti2,status='keep')
      goto 1
   97 call errr (lti,ios,k)
   98 call errr (lti2,ios,k)
   99 call eofr (lti,k)
      call eofw (lto,k)
      stop
      end
