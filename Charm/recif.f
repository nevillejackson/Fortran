      subroutine recif (nad,idesta,iorba,iorea,icla,emfa,lemfa,
     +radt,ncd,idest,iorb,iore,icl,emf,lemf,rcdt,rec,lit,irl2)
      dimension idesta(50),iorba(50),iorea(50),icla(50),emfa(50)
     +,lemfa(50),radt(50),idest(50),iorb(50),iore(50),
     +icl(50),emf(50),lemf(50),rcdt(50)
      character emfa*10,emf*10,radt*1,rcdt*1,rec*1000,lit*1000
      common /stdio/ lin,lout,lmess
c-----recode record (lit) according to "*all" directives
      do 1 n = 1,nad
      j = idesta(n)
      if(radt(n).eq.'c')then
      k = iorba(n)
      kk = iorea(n)
      jj = j+icla(n)-1
      rec(j:jj) = lit(k:kk)
      else
      jj = j+lemfa(n)-1
      k = lemfa(n)
      rec(j:jj) = emfa(n)(1:k)
      end if
    1 continue
c-----recode record (lit) according to "*code" directives
      do 2 n = 1,ncd
      j = idest(n)
      if(rcdt(n).eq.'c')then
      k = iorb(n)
      kk = iore(n)
      jj = j+icl(n)-1
      rec(j:jj) = lit(k:kk)
      else
      jj = j+lemf(n)-1
      k = lemf(n)
      rec(j:jj) = emf(n)(1:k)
      end if
      if (jj.gt.irl2) goto 99
    2 continue
      return
   99 write (lmess,10) jj,irl2
   10 format (' Re-formatting to col',i4,' exceeds record length',i4)
      call jobend
      return
      end
