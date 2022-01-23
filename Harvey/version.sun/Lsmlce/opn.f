      subroutine opn (lun,fname,stat,formt)
c------given a filename argument (fname) open 
c------its logical unit (lun)
      common /stdio/ lin,lout,lmess
      integer lun
      logical xist
      character stat*4,fname*(*),formt*11
      xist=.false.
      if(stat(1:3).eq.'new') then
        inquire(file=fname,iostat=ios,err=80,exist=xist)
      endif
      if(xist) then
        open(lun,file=fname,status='old',err=80,iostat=ios,form=formt)
      else 
        open (lun,file=fname,status=stat,err=80,iostat=ios,form=formt)
      endif
c------normal exit
      write (lmess,90) lun,stat,fname
   90 format (' opened to unit ',i3,'  status = ',a,'  file = ',a)
      rewind lun
      return
c------error exit
   80 write (lmess,95) lun,ios,fname
   95 format (' could not open unit ',i4,' iostat = ',i4,'file = ',a)
      call jobend
      return
      end
