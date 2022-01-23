      program ckcalc
c-----written 24/9/87 to prevent two calc jobs running simultaneously with
c-----the second overwriting the first's program
      character string*25
      logical ex,run
      ex = .false.
      run = .true.
    1 inquire (file='unlockcalc',iostat=ios,exist=ex)
      if (ex) then
         string = 'mv unlockcalc lockcalc'
         lstat = system(string)
      else
         if (run) then
            write (0,10)
   10       format (' "calc run" in progress')
            run = .false.
         end if
c        call sleep(60)
         do 300 i=1,10000
  300    continue
         goto 1
      end if
      stop
      end
