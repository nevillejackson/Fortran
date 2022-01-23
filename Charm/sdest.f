      subroutine sdest (x,tsum,tsqsum,tsd)
c-----estimates standard deviation 
      real x
      double precision tsum,tsqsum,tsumsq,tnum,tsd
      common /stdio/ lin,lout,lmess
      if (x.le.1.0) then
      tsd = 0.0
      else
      tsumsq = (tsum*tsum)/x
      if (tsumsq.gt.tsqsum) then
      write (lmess,10)
   10 format (' *** -ve / zero tnum for this s.d. ***')
      tsd = 0.0
      else
      tnum = (tsqsum-tsumsq)/(x-1.0)
      tsd = dsqrt(tnum)
      end if
      end if
      return
      end
