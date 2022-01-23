      program editr
c-----ftn5 program written april '85 by anne swinton
c-----corrections and changes 4/7/85   (aa00118)
c-----UNIX version 16/9/85  latest update 11/4/86
c-----reads "charm" file (tape10) writes output to tape11
c-----file must be in order of sequence number and directives
c-----must handle records in sequence. the different param-
c-----eter types for *s must be sequential by column no.
c-----tapes rewound at beginning and end of program.
c-----limits ..... 100 directives/run .. (limed in edlim common)
c-----       ..... 10 characters/parameter string .. (lp in limits)
c-----       ..... 80 characters/line .. *i records >80 chars
c-----             are multi-line.
c-----       ..... 10 insert records/*i directive .. (limrec in
c-----             edlim common)
      dimension icn(10),ptype(10),par(10),lenp(10),ifield(10),recin(10)
      character type*1,ptype*1,par*10,recin*1000,rec*1000,lit*1000,
     +string*80
      common /ca/ lit
      common /cb/ string
      common /cc/ type
      common /cd/ ptype
      common /ce/ par
      common /cf/ recin
      common /zrec/ rec
      common /limits/ lc,ll,lp,lr,nv
      common /edlim/ limed,limrec
      common /stdio/ lin,lout,lmess
      logical keep,fin,rdend,windup,first,match
      data ptype,par,recin/10*' ',10*' ',10*' '/
      data limed,limrec/100,10/
      write (lout,10)
   10 format ((/),' editr run')
      call argopn (10,1,'old')
      call argopn (11,2,'new')
      lti = 10
      lto = 11
      keep = .false.
      rdend = .false.
      windup = .false.
      fin = .false.
      first = .true.
      iknt = 0
      call readch (irl,iseq,lti,lit,rdend,iknt)
      write (lmess,20) lto,irl
   20 format (' file tape',i3,'   record length written ='
     +,i6,' characters')
c-----determine lines/record (10 chars/word  8 words/line)
      call linest (irl,lines)
      irk = 0
      kntd = 0
c-----read a directive, check for required record on tape10
    1 call edrd (windup,keep,type,lno,icn,ptype,par,lenp,ifield,
     +nsf,lines,itp,recin,irec,kntd,fin,irl)
c-----when *i follows immediately after *d and record sequence
c-----number is the same for both directives
      if (type.eq.'i'.and.lno.eq.iseq) then
      do 33 n = 1,irec
      irk = irk+1
   33 call recwr (irl,irk,recin(n),lto)
      write (lmess,21) irl,irk,recin(n)(:irl)
   21 format (i3,i6,1x,a)
      if (fin) goto 2
      goto 1
      end if
    2 if (first) goto 4
      if (.not.windup) match = .false.
      call readch (irl,iseq,lti,lit,rdend,iknt)
    4 first = .false.
      if (rdend) goto 96
      if ((windup).or.lno.ne.iseq) then
      irk = irk+1
      call recwr (irl,irk,lit,lto)
      goto 2
      else if (type.eq.'i') then
      match = .true.
      irk = irk+1
      call recwr (irl,irk,lit,lto)
      do 3 n = 1,irec
      irk = irk+1
    3 call recwr (irl,irk,recin(n),lto)
      write (lmess,21) irl,irk,recin(n)(:irl)
      if (fin) then
      windup = .true.
      goto 2
      end if
      else if (type.eq.'s') then
      match = .true.
      call inted(lit,rec,irl,itp,icn,ptype,par,lenp,ifield,nsf)
      irk = irk+1
      call recwr (irl,irk,rec,lto)
      write (lmess,22) rec(:irl)
   22 format (' ',a)
      else if (type.eq.'l') then
      match = .true.
      write (lmess,25) irl,iseq,lit(:irl)
   25 format (' ',i3,i6,1x,a)
      irk = irk+1
      call recwr (irl,irk,lit,lto)
      write (lmess,30) irk
   30 format (' new sequence number =',i10)
      else if (type.ne.'d') then
      write (lmess,35) type
   35 format (' ***fatal*** invalid directive type  *',a)
      call jobend
      else
      match = .true.
      write (lmess,23) iseq
   23 format (' record number ',i3,' listed then deleted')
      write (lmess,25) irl,iseq,lit(:irl)
      end if
      goto 1
   96 if (.not.match) then
      write (lmess,40) lno
   40 format (' *** fatal *** line number',i6,' not found')
      end if
      call eofr (lti,iknt)
      call eofw (lto,irk)
      stop
      end
