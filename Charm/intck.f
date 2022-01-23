      subroutine intck (par)
      character par*1
      common /stdio/ lin,lout,lmess
c------check character is integer, if not call jobend
      if(par.ge.'0'.and.par.le.'9')goto 1
      write(lmess,10)
   10 format(' *** fatal *** integer/matching delimiter expected')
      call jobend
    1 return
      end
