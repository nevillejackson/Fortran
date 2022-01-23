      program nuform
c-----converts NOS coded charm files to UNIX charm files
c-----places record length & seq no at begin of record
c-----reduces record length by removing blank padding
c-----
      character*1000 rec
      character*1 blank
      character*2 flg
      logical endrd,gsw
      common /stdio/ lin,lout,lmess
      data blank/' '/
c-----
      write(lmess,20)
   20 format('0nuform executing')
c-----flag
      if(iargc().eq.2)then
        ioff=0
        gsw=.false.
      else
        ioff=1
        call getarg(1,flg)
        gsw=.true.
      endif
c-----files
      call argopn(10,1+ioff,'old')
      call argopn(11,2+ioff,'new')
c-----record length of old style records
      read (10,30) irl
   30 format(i10)
      write(lmess,200) irl
  200 format(' old record length = ',i10)
      if(gsw) then
        last=irl-3
      else
        last=irl
      endif
c-----scan file to check for blank padding
      k=0
   10 call recrd1(irl,10,rec,endrd,kseq)
      if (endrd) go to 99
      nb=0
      k=k+1
      do 1 i=1,last
      j=last-i+1
c     j goes from  last to 1
      if (rec(j:j).eq.blank) then
        nb=nb+1
      else
        go to 2
      endif
    1 continue
      write(lmess,31)
   31 format('0entire record blank')
      stop
c-----new record length jrl (incl gp no) of this record
c-----maxrl -- max of all records
    2 jrl=irl-nb
      maxrl=max0(maxrl,jrl)
      go to 10
c-----end scan -- start reformat 
   99 k=0
      write(lmess,100) 
  100 format(' finished scan -- starting reformat')
c-----jrl is record length excl gp no
      if (gsw) then
        jrl=maxrl-3
      else
        jrl=maxrl
      endif
      rewind 10
      endrd=.false.
      read(10,30) irl  
   40 call recrd1(irl,10,rec,endrd,kseq)
      if(endrd) go to 98
      k=k+1
c-----allow 5 cols for gp no 
      if(gsw) then
        rec(jrl+3:jrl+5)=rec(irl-2:irl)
        rec(jrl+1:jrl+2)='  '
        call recwr1(jrl+5,11,rec,kseq)
      else
        call recwr1(maxrl,11,rec,kseq)
      endif
      go to 40
c-----
   98 call eofr(10,k)
      call eofw(11,k)
      if(gsw) then
        write(lmess,201) jrl+5
  201   format(' new record length = ',i10)
      else
        write(lmess,201) maxrl
      endif
      stop
      end
