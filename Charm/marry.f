
      program marry
c-----concatenates records from previously matched files 
      character*1000 recp,recs
      character flg*2,space*10
      logical gap,endrdp,endrds
      common /limits/ lc,ll,lp,lr,nv
      common /stdio/ lin,lout,lmess
      common/a/ recp
      common/b/ recs
      common /c/ flg
      common/d/ space
      data ltp,lts,lto /10,11,12/
      data space /'          '/
      data endrdp,endrds /.false.,.false./
c-----
      write(lmess,20) 
   20 format(/,'marry run')
      k=0
      kp=0
      ks=0
c-----command line
      if(iargc().eq.3) then
        ioff=0
        gap=.false.
      else
        ioff=1
        call getarg(1,flg)
        if (flg.ne.'-g') then
          write(lmess,23) flg
   23     format(' unknown option ',a) 
          call jobend
        endif
        gap=.true.
      endif
      call argopn(ltp,1+ioff,'old')
      call argopn(lts,2+ioff,'old')
      call argopn(lto,3+ioff,'new')
c-----read pairs of records
  100 call readch(irlp,iseqp,ltp,recp,endrdp,kp)
      if(endrdp) go to 900
      if(kp.eq.1) then
        write(lmess,21)ltp,irlp
   21   format(' file tape ',i3,' record length read = ',i7,
     +   ' characters')
      endif 
      if(irlp.gt.lr) then
        call exceed(4,irlp)
      endif
      call readch(irls,iseqs,lts,recs,endrds,ks)
      if (endrds) go to 910
      if(ks.eq.1) then
        write(lmess,21) lts,irls
      endif
      if(irls.gt.lr) then
        call exceed(4,irls)
      endif
c-----write concatenated pair
      k=k+1
      if(gap) then
        lgap=10-mod(irlp,10)
        irlo=irlp+irls+lgap
      else
        irlo=irlp+irls
      endif
      if(irlo.gt.lr) then
        call exceed(4,irlo)
      endif
      if(k.eq.1) then
        write(lmess,22) lto,irlo
   22   format(' file tape ',i3,' record length written = ',i10,
     + ' characters')
      endif
c-----
      if(gap) then
        write(lto,30) irlo,k,recp(1:irlp),space(1:lgap),recs(1:irls)
      else
        write(lto,30) irlo,k,recp(1:irlp),recs(1:irls)
   30   format(i3,i6,1x,a,a,a)
      endif
c-----
      go to 100
c-----eof on ltp
  900 call eofr(ltp,kp)
      call readch(irls,iseqs,lts,recs,endrds,ks)
      if(endrds) then
        call eofr(lts,ks)
      else
        write(lmess,25)
   25   format(' counts on primary and secondary files disagree',
     + ' - check match')
        call jobend
      endif
      go to 920
c----- eof on lts
  910 call eofr(lts,ks)
      call readch(irlp,iseqp,ltp,recp,endrdp,kp)
      if(endrdp) then
        call eofr(ltp,kp)
      else
        write(lmess,25)
        call jobend
      endif
c-----normal termination
  920 call eofw(lto,k)
      stop
      end
