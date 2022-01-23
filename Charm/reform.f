      program reform
c------ftn5 program written by anne swinton feb.'85
c------UNIX version 4/9/85  latest update 12/3/87
c------"charm" program to manipulate data fields within a record.
c------each record in the file treated identically.
c------output file may have a record length greater, equal to
c------or smaller than that on the input file.
c------files rewound at beginning and end of program.
c------limits ..... 100 directives/run  (rflim lrf)
c------       ..... 10 characters/"emit" field  (limits lp and emf*10)
      dimension rfdt(100),idest(100),iorb(100)
     +,ifl(100),iore(100),emf(100),lemf(100)
      character emf*10,rfdt*1,rec*1000,lit*1000
      common /ca/ emf
      common /cb/ rfdt
      common /cd/ lit
      common /stdio/ lin,lout,lmess
      common /zrec/ rec
      common /limits/ lc,ll,lp,lr,nv
      common /rflim/ lrf
      logical rdend
      data lrf,rfdt,emf/100,100*' ',100*' '/
      lti = 10
      lto = 11
      rdend = .false.
c-----
      write (lmess,20)
   20 format ((/),' reform run')
      call argopn(lti,1,'old')
      call argopn(lto,2,'new')
c------read reform directives with "refrd", calls "nextrf","chint","intck"
      call refrd (idest,iorb,iore,ifl,emf,lemf,rfdt,knt)
c------estimate record length of reformed records to be
c------written to tape11
      call rlmax (idest,ifl,lemf,knt,rfdt,irl2)
      l = 0
c------"readch" reads records (tape10) one at a time, reforms
c------them according to job directives.
  100 call readch (irl,iseq,lti,lit,rdend,l)
      if (rdend) goto 300
      if (l.eq.1) then
      write (lmess,10) lti,irl
   10 format (' file tape ',i3,' record length read =',i7,' characters')
c------check record length within limits
      if (irl2.gt.lr) then
      call exceed (4,irl2)
                      end if
c-----check fields being copied across do not exceed record length
      call fldckf (irl,iorb,iore,rfdt,knt)
      write (lmess,40) irl2
   40 format (' file tape11  record length written = ',i10,
     +' characters')
      end if
      do 110 n=1,irl2
  110 rec(n:n) = ' '
      do 200 i=1,knt
      j = idest(i)
      if (rfdt(i).eq.'c') then
      k = iorb(i)
      kk = iore(i)
      jj = j+ifl(i)-1
      rec(j:jj) = lit(k:kk)
      else
      jj = j+lemf(i)-1
      n = lemf(i)
      rec(j:jj) = emf(i)(1:n)
      end if
  200 continue
c------records written to output (tape 11)
      call recwr (irl2,l,rec,lto)
      goto 100
c------reform ... tape10 to tape11 ... complete
  300 call eofr (lti,l)
      call eofw (lto,l)
      stop
      end
