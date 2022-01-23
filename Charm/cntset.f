      subroutine cntset (chcnt)
c-----adds column count to first two lines of output
      dimension chcnt(2)
      character chcnt*120,aj*1,ak*1
      common /stdio/ lin,lout,lmess
      j = 0
      do 1 i = 20,120,10
      j = j+1
      if (j.eq.10) j=0
      write (aj,'(i1)')j
    1 chcnt(1)(i:i) = aj
      k = 0
      do 2 i = 11,120
      k = k+1
      if (k.eq.10) k=0
      write (ak,'(i1)')k
    2 chcnt(2)(i:i) = ak
      do 3 l = 1,2
      write (lmess,10) chcnt(l)
   10 format (' ',a)
    3 continue
      return
      end
