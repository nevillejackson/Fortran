      subroutine jobend                                                         
c------UNIX subroutine 'exit' flushes all the process files, shell must
c------be run with '-e' option so subsequent programs in the jobdeck
c------will not run ie. aborts whole job not just the one program which
c------had the error
c     call exit (15)
      stop
      return                                                                    
      end                                                                       
