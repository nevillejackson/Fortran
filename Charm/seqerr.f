      subroutine seqerr (k,knt)
c-----sequence error in control field sorting
      common /stdio/ lin,lout,lmess
      write (lmess,10) knt,k
   10 format (' *** fatal *** sequence error on record',
     +i6,' field number',i3)
      call jobend
      return
      end
