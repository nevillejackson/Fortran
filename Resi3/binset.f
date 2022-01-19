      subroutine binset(nr,q,u,ntr,ntr4,label,
     + ini,inh,inf,ino)
c----- set up matrices for Binet restrictions
c----- automatically for each trait_partition not defined by
c----- index parameters
      implicit double precision (a-h,o-z)
      dimension q(mvntr4,mvntr4),u(mvntr4)
      dimension ini(mvntr),inh(mvntr),inf(mvntr),ino(mvntr)
      character*10 label(mvntr)
      common /limits/ mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      common /stdio/ lc,lp,lerr
c-----
      do 1 i=1,nr
      u(i)=0.0
      do 1 j=1,ntr4
    1 q(i,j)=0.0
c-----
c-----note - ini() =0 -> trait NOT in index
c----              =1 -> trait IS in index
      knr=0
      do 2 i=1,ntr
      if(ini(i).eq.0) then
        knr=knr+1
        q(knr,i)=1.0
      endif
    2 continue
c-----
      do 3 i=1,ntr
      if(inh(i).eq.0) then
        knr=knr+1
        q(knr,i+ntr)=1.0
      endif
    3 continue
c-----
      do 4 i=1,ntr
        if(inf(i).eq.0) then
          knr=knr+1
          q(knr,i+2*ntr)=1.0
        endif
    4 continue
c-----
      do 5 i=1,ntr
        if(ino(i).eq.0) then
          knr=knr+1
          q(knr,i+3*ntr)=1.0
        endif
    5 continue
c-----
      if(nr.ne.knr) then
        write(lp,*)'resi3: nr.ne.knr in binset'
        stop
      endif
      return
      end
