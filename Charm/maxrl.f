      subroutine maxrl (idest,idesta,icl,icla,lemf,lemfa,radt,
     +rcdt,nad,ncd,lmax)
      dimension idest(50),idesta(50),icl(50),icla(50),lemf(50),
     +lemfa(50),rcdt(50),radt(50)
      character rcdt*1,radt*1
      common /limits/ lc,ll,lp,lr,nv
c-----estimates output file (tape11) record length from
c-----*all and first block of *code directives.
      do 1 i = 1,nad
      if (radt(i).eq.'e') then
      il = idesta(i)+lemfa(i)-1
      else
      il = idesta(i)+icla(i)-1
      end if
      if (i.eq.1) then
      lmax = il
      else if (il.gt.lmax) then
      lmax = il
      end if
    1 continue
      do 2 i = 1,ncd
      if (rcdt(i).eq.'e') then
      il = idest(i)+lemf(i)-1
      else
      il = idest(i)+icl(i)-1
      end if
      if (il.gt.lmax) then
      lmax = il
      end if
    2 continue
c-----check new record length within limits
      if (lmax.gt.lr) then
      call exceed (4,lmax)
      end if
      return
      end
