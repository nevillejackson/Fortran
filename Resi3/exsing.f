      logical function exsing(ini,inh,inf,ino,ntr,i)
c-----decides when a row of an ntr4*ntr4 cov matrix is expected
c-----to be dependent because the corresponding family is not
c-----being used
c-----
c-----returns  - true if row i expected to be dependent
c-----         - false if row i NOT expected to be dependent
c-----
      implicit double precision (a-h,o-z)
      common /limits/ mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      common /stdio/ lc,lp,lerr
      dimension ini(mvntr),inh(mvntr),inf(mvntr),ino(mvntr)
c-----
      j=(i-1)/ntr
      jrem=i-j*ntr
c----- I
      if(j.eq.0) then
        if(ini(jrem).eq.1) then
          exsing=.false.
        else
          exsing=.true.
        endif
c----- H
      else if(j.eq.1) then
        if(inh(jrem).eq.1) then
          exsing=.false.
        else
          exsing=.true.
        endif
c----- F
      else if(j.eq.2) then
        if(inf(jrem).eq.1) then
          exsing=.false.
        else
          exsing=.true.
        endif
c----- O
      else if(j.eq.3) then
        if(ino(jrem).eq.1) then
          exsing=.false.
        else
          exsing=.true.
        endif
      endif
      return
      end
