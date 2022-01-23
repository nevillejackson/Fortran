
      subroutine triord(fvec,zvec,ftry,zz,big)
c=========================================================================

c     purpose : auxiliary routine to "dflmin" to add the 'point' zz 
c               to the current triplett of points, attempting to create
c               a bracket or otherwise merely discarding the
c               worst point (i.e with highest fvec(i)) and ordering
c               so that zvec(1) < zvec(2) < zvec(3)

c     parameters :
C               fvec  = vector of function values pertaining to zvec
c               zvec  = vector of points in triplett
c               ftry  = function value for new point
c               zz    = new point to be included

c---------------------------------------------------------km--10/90-------

      implicit double precision (a-h,o-z)
      dimension fvec(3),zvec(3)

c     check for bracket
      ff=dmax1(fvec(1),fvec(3))
      if(ff.ne.big .and. ftry.lt.fvec(1) .and. ftry.lt.fvec(2) .and. 
     *                     zz.gt.zvec(1) .and. zz.lt.zvec(2))then
         ind=2
         fvec(3)=fvec(2)
         zvec(3)=zvec(2)
      else if(ff.ne.big .and. ftry.lt.fvec(3) .and. ftry.lt.fvec(2)
     *                    .and. zz.gt.zvec(2) .and. zz.lt.zvec(3))then
         ind=2
         fvec(1)=fvec(2)
         zvec(1)=zvec(2)

c     discard worst point
      else if(fvec(1).gt.fvec(3))then
         if(zz.lt.zvec(2))then
            ind=1
         else if(zz.lt.zvec(3))then
            ind=2
         else
            ind=3
         end if
         do 1 i=1,ind-1
         zvec(i)=zvec(i+1)
1        fvec(i)=fvec(i+1)
      else if(fvec(3).gt.fvec(1))then
         if(zz.gt.zvec(2))then
            ind=3
         else if(zz.gt.zvec(1))then
            ind=2
         else
            ind=1
         end if
         do 2 i=3,ind+1,-1
         zvec(i)=zvec(i-1)
2        fvec(i)=fvec(i-1)
      else
         print *,zvec
         print *,fvec
         print *,zz
         stop 'triord'
      end if
      zvec(ind)=zz
      fvec(ind)=ftry
      print *,'z',zvec
      return
      end







