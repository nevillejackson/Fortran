      program recode
c-----Ftn5 program written by Anne Swinton .. 7/3/85
c-----UNIX version 14/9/85  latest update 12/3/87
c-----Reads "charm" file (unit10) writes to unit11 all
c-----records selected and recoded according to blocks of
c-----directives ... records not selected by any blocks of
c-----directives written to U12 or U13 depending on there
c-----being an odd or even number of blocks respectively.
c-----Records and data within records only retained as indicated
c-----by directive blocks, ie. if not explicitly recoded then
c-----record/data lost. Scratch files must be in call to program.
c-----eg.  recode a b dum1 dum2 <<eoi
c-----All rewound at beginning and end of program.
c-----Limits ..... 50 *all/block
c-----       ..... 50 *code/block
c-----       ..... 50 select (*e +/or *i)/block
c-----       ..... 50 blocks/run
c-----       ..... other limits as in select and reform programs.
c-----No facility for *el (available in select program)
c-----
      dimension idesta(50),idest(50),iorba(50),iorb(50),icla(50),
     +icl(50),emfa(50),emf(50),lemfa(50),lemf(50),
     +ibeg(50,5),ifl(50,5),ifend(50,5),rel(50,2),val(50,10)
     +,nfield(50),nval(50),radt(50),rcdt(50),sdt(50)
     +,iore(50),iorea(50)
      character string*80,lit*1000,litf*80,rec*1000,emfa*10,emf*10
     +,brec*1000,rel*2,val*10,radt*1,rcdt*1,sdt*1
      common /ca/ string
      common /cb/ lit
      common /cc/ litf
      common /cd/ emfa
      common /ce/ emf
      common /cf/ rel
      common /cg/ val
      common /ch/ brec
      common /cj/ radt
      common /ck/ rcdt
      common /cl/ sdt
      common /zrec/ rec
      common /limits/ lc,ll,lp,lr,nv
      common /rclim/ limr
      common /stdio/ lin,lout,lmess
      logical rdend,endrd,even,keep
      data emfa,emf,rel,val/50*' ',50*' ',100*' ',500*' '/
      data radt,rcdt,sdt,limr/50*' ',50*' ',50*' ',50/
      data litf/1*' '/
c-----
      write (lmess,20)
   20 format ((/),' recode run')
      call argopn (10,1,'old')
      call argopn (11,2,'new')
      call argopn (12,3,'new')
      call argopn (13,4,'new')
      lti = 10
      lto = 11
      lto2 = 12
      even = .true.
      knt = 0
      nb = 0
      nad = 0
c-----read recode directives one block at a time
    1 call recrd (idest,idesta,iorb,iorba,iore,iorea,icl,icla,
     +emf,emfa,lemf,lemfa,ibeg,ifl,ifend,nfield,rel,val,nval,
     +radt,rcdt,sdt,nad,ncd,nsd,nb,endrd)
      if(nb.gt.limr)then
      write(lmess,30)limr
   30 format (' block limit exceeded   limit = ',i3,' blocks')
      call jobend
      else if(nb.eq.1)then
c-----calculate new record length and check it's acceptable
      call maxrl (idest,idesta,icl,icla,lemf,lemfa,radt,rcdt,nad,
     +ncd,irl2)
      do 2 l=1,irl2
    2 brec(l:l) = ' '
      end if
    3 iknt = 0
      irk = 0
      isk = 0
    4 call readch (irl,iseq,lti,lit,rdend,iknt)
      if (rdend) goto 9
      if (iknt.eq.1) then
      if (nb.eq.1) then
      call fldckc (irl,iorba,iorea,radt,nad)
      call fldckc (irl,iorb,iore,rcdt,ncd)
      write (lmess,40) lti,irl
   40 format(' file tape',i3,'  record length read =',i6,' characters')
      write (lmess,50) lto,irl2
   50 format(' file tape',i3,'   record length written = ',i6,' ch'
     +,'aracters')
      else
      call fldckc (irl,iorb,iore,rcdt,ncd)
      write (lmess,40) lti,irl
      end if
      write (lmess,50) lto2,irl
      end if
c-----check record against directives ... loop 5
      keep = .false.
      do 5 i = 1,nsd
c-----write lit field value(s) to litf ... loop 6
      call setlitf (litf)
      k = 1
      l = 0
      jj = nfield(i)
      do 6 j = 1,jj
      l = l+ifl(i,j)
      litf(k:l) = lit(ibeg(i,j):ifend(i,j))
      k = k+ifl(i,j)
    6 continue
c-----check field litf against directive value(s)
      call selcomr (keep,litf,l,sdt,i,rel,val,nval)
      if(.not.keep)goto 8
    5 continue
c-----record selected/not excluded by directive block
      isk = isk+1
      rec(1:irl2) = brec
      call recif (nad,idesta,iorba,iorea,icla,emfa,lemfa,radt,
     +ncd,idest,iorb,iore,icl,emf,lemf,rcdt,rec,lit,irl2)
c-----total number of records recoded = knt = sequence no. (tape11)
      knt = knt+1
      call recwr (irl2,knt,rec,lto)
      goto 4
c-----record rejected by current directive block
    8 irk = irk+1
      write (lto2,60) irl,irk,lit(:irl)
   60 format (i3,i6,1x,a)
      goto 4
c-----eof (input file) for current block.
    9 call reof (lti,iknt)
      call weof (lto2,irk)
      write (lmess,70) lto,isk
   70 format (' end of block on unit ',i3,'  -- record count =',
     +i6)
c-----if all records recoded but more directives or no more directives.
      if(irk.eq.0.or.(endrd))goto 99
      if (even) then
      even = .false.
      lti = 12
      lto2 = 13
                else
      even = .true.
      lti = 13
      lto2 = 12
                end if
      rewind lti
      rewind lto2
      goto 1
c-----no more recode blocks
   93 write (lmess,75) ios 
   75 format (' could not open temp file ,iostat = ',i4)
      call jobend
   99 call eofw (lto,knt)
      close (10,status='keep')
      close (12,status='delete')
      close (13,status='delete')
      stop
      end
