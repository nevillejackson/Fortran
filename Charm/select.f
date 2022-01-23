      program select
c------ftn5 program written by anne swinton .. jan/85.
c------latest update 10/7/85    (aa00125)
c------UNIX version 3/9/85
c------reads "charm" file (tape10) and makes "charm" file (tape11)
c------containing records selected according to directive(s)
c------testing one or more fields per record ... records not
c------selected written to tape12.
c------tapes rewound at beginning and end of program.
c------limits ..... 50 directives/run
c------       ..... 5 combined fields/directive
c------       ..... 10 chars max. field length  (limits common lp)
c------       ..... 10 field values/directive  (limits common nv)
c------equivalences available ... eq,lt,gt ..and " > val1 < val2 "
c------a *el directive will list to output all the records
c------excluded by that particular directive.
c------
      dimension sdt(50),ibeg(50,5),ifend(50,5),rel(50,2)
      dimension val(50,10),ifl(50,5),nfield(50),nval(50)
      character sdt*1,rel*2,val*10,string*80,lit*1000,rec*1000
      character litf*80
      logical rdend,keep,elist
      common /ca/ sdt
      common /cb/ rel
      common /cc/ val
      common /cd/ lit
      common /ce/ litf
      common /ch/ string
      common /zrec/ rec
      common /stdio/ lin,lout,lmess
      common /limits/ lc,ll,lp,lr,nv
      common /tio/ lti,lto,lto2
      data sdt,rel,val/50*' ',100*' ',500*' '/
      data lti,lto,lto2/10,11,12/
c-----
      write (lmess,10)
   10 format ((/),' select run')
c------get filename arguments and open
      call argopn (lti,1,'old')
      call argopn (lto,2,'new')
      if (iargc().eq.3) then
      call argopn (lto2,3,'new')
      end if
c------read select directives
      call selrd (ndir,sdt,j,ibeg,ifl,ifend,nfield,rel,k,val,nval)
c------read a record (tape10) ... loop 1
      isk = 0
      irk = 0
      rdend = .false.
      knt = 0
    1 call readch (irl,iseq,lti,lit,rdend,knt)
      if (rdend) goto 999
      if (knt.eq.1) then
      write (lmess,30) lto,irl
   30 format (' file tape ',i2,'   record length written = ',i10
     +,' characters')
      write (lmess,30) lto2,irl
      end if
c------check record against directives ... loop 2
      keep = .false.
      do 2 i = 1,ndir
c------write lit field value(s) to litf ... loop 3
      call setlitf (litf)
      k = 1
      l = 0
      jj = nfield(i)
      do 3 j = 1,jj
      l = l+ifl(i,j)
      litf(k:l) = lit(ibeg(i,j):ifend(i,j))
      k = k+ifl(i,j)
    3 continue
c------check field litf against directive value(s)
      call selcomp (keep,litf,l,sdt,i,rel,val,nval,elist)
      if(.not.keep) goto 4
    2 continue
c------record selected/not excluded by all directive(s)
      isk = isk+1
      call recwr (irl,isk,lit,lto)
      go to 1
c------record does not satisfy select or is excluded by directives
    4 irk = irk+1
      if (iargc().eq.3) then
      call recwr (irl,irk,lit,lto2)
      end if
      if (elist) then
      write (lmess,40) lit(1:irl)
   40 format (' ',a)
      end if
      go to 1
c------all records completed
  999 call eofr (lti,knt)
      call eofw (lto,isk)
      if (iargc().eq.3) then
      call eofw (lto2,irk)
      end if
      stop
      end
