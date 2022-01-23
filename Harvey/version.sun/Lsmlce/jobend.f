      subroutine jobend                                                         
c------machine dependent routine                                                
c------aborts job process as distinct from fortran 'stop' state-                
c------ment which only aborts job step                                          
      idum=system('stty -raw echo')
c     call exit (1)
      stop
      return                                                                    
      end                                                                       
