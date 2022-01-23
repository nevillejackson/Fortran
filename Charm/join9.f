

      program join9
c-----concatenate up to 9 files into one file
      character*1000 rec
      logical endrd
      common /limits/ lc,ll,lp,lr,nv
      common /stdio/ lin,lout,lmess
      data lti /10/
      data endrd /.false./
c-----
      write(lmess,20)
   20 format(/,'join9 run')
c-----command line 
      nlti=iargc()-1
      lto=lti+nlti
      ltl=lto-1
      do 1 i=lti,ltl
    1 call argopn(i,i-lti+1,'old')
      call argopn(lto,nlti+1,'new')
c-----determine output record length
      lmax=0
      lmin=lr
      do 2 i=lti,ltl
      call readch(irl,iseq,i,rec,endrd,k)
      if(endrd) then
        write(lmess,21) i
   21   format(' empty file on unit ',i3)
        irl=0
      endif
      if(irl.gt.lmax) then
        lmax=irl
      endif
      if(irl.lt.lmin) then
        lmin=irl
      endif
    2 continue
      if(lmax.ne.lmin) then
        write(lmess,22)
   22   format(' *** warning *** record lengths differ',
     +   ' - short records blank filled')
      endif
      if(lmax.gt.lr) then
        call exceed(4,lmax)
      endif
      do 3 i=lti,ltl
    3 rewind i
c-----concatenate
      k=0
      do 4 i=lti,ltl
      kount=0
      endrd=.false.
    5 call setblk(rec,lmax)
      call readch(irl,iseq,i,rec,endrd,kount)
      if(endrd) then
        call eofr(i,kount)
        if(i.eq.ltl) then
          go to 6
        else
          go to 4
        endif
      else
        k=k+1
        call recwr(lmax,k,rec,lto)
      endif
      go to 5
    4 continue
c----- end
    6 call eofw(lto,k)
      write(lmess,2581)lmax
 2581 format(' record length of output file = ',i5)
      stop
      end
