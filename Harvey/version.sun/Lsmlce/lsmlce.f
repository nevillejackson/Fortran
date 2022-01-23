      program lsmlce
c     main program--------------------------------------                
c     least squares and maximum likelihood general purpose program      
c     mixed model version-----lsmlmw --- walter r. harvey               
c     lsmlmw driver deck                                                
c     --------------------------------------------------                
c     UNIX & CHARM version with extended dimensions
c     N. Jackson  -- Oct, 1988
c     --------------------------------------------------
      include 'decl1'
      include 'com1'
      include 'com2'
      include 'com3'
      include 'com4'
      include 'com5'
      include 'com6'
      include 'com7'
      include 'com8'
      character*80 fnam
      open(5,blank='zero')
    1 kput=0                                                            
      nsme=0                                                            
      ncas=0                                                            
      i30=0                                                             
      kb=0                                                              
      kc=0                                                              
      wtt=0.                                                            
      kd=0                                                              
      call param(ntitl,nnn)                                             
c-----
c-----skip file opens on 2nd. and subsequent runs
      if(nrn.eq.1)then
c-----
c-----unix file opening procedures
c-----datafile (unit 'in') 1st unix command line argument
      if(in.ne.05)then
      call argopf(in,fnam,1,'old','formatted')
      endif
c-----scratch files (units 12 & 13 ) 2nd & 3rd command line arguments
      call argopf(12,fnam,2,'new','formatted')
      call argopf(13,fnam,3,'new','formatted')
      endif
c-----matrix files
c-----
c-----
      if (mull.ne.0) go to 900                                          
      call rcbm(ntitl,nnn)                                              
      if (mull.eq.1) go to 900                                          
      if (liop.eq.6.or.kput.gt.0.or.liop.eq.7) go to 900                
      call lscomb(ntitl)                                                
      if (mull.eq.1) go to 900                                          
      if (mull.eq.2) go to 919                                          
      if (mull.eq.3) go to 913                                          
      kd=0                                                              
      if (mty.gt.1.and.nrun.eq.nrn) call suafmm                         
  900 if (kput.eq.2) write (6,1003)                                     
 1003 format (1h0,'class or subclass found with no observations (mis ni)
     1 or subclass missing in last row or column. analysis stopped')    
      if (mull.eq.0) go to 918                                          
      if (in.ne.5) go to 913                                            
  910 read (in,1000,end=912) lrec,kseq,ic(1:lrec)                       
      ntra=0                                                            
 1000 format (i3,i6,1x,a)                                               
      do 911 i=1,lrec                                                     
  911 if (ic(i:i).ne.blank) ntra=1                                         
      go to 915                                                         
  912 ntra=0                                                            
  915 if (ntra.eq.0) go to 913                                          
      go to 910                                                         
  913 if (nlc.eq.0) go to 918                                           
      do 914 i=1,nlc                                                    
  914 read (5,*) ncd,(tot2(j), j=1,ncd)                              
 1002 format (14x,i2,12f5.0,4x/(16f5.0))                                
  918 if (nrun.ne.nrn) go to 919                                        
      call polyab(ntitl)                                                
  919 if (mpop.eq.1) go to 1                                            
      stop                                                              
      end                                                               
