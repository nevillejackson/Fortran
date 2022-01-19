      subroutine parind(ij,head,ltr,ntr,new,ndg,nsr,nr,ntr4,
     + l,label,ini,inh,inf,ino,a,dg,gres,
     + listi,listh,listf,listo,nh,nf,no)
c-----reads index parameters for one job - 
c-----  - one job means one subset of parameters from library file
c-----  - can be several sets of ec wts or des gains - ie several indices
c-----
      implicit double precision (a-h,o-z)
      common /stdio/ lc,lp,lerrr
      common /limits/ mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
c-----
      character*10 label(mvntr)
      dimension ini(mvntr),inh(mvntr),inf(mvntr),ino(mvntr)
      dimension a(mvnw,mvntr),dg(mvndg,mvntr),gres(mvns,mvntr)
      dimension l(mvntr)
      character*100 head
      dimension listi(mvntr),listh(mvntr),listf(mvntr),listo(mvntr)
c-----
      write(lp,110)
  110 format(///' INDEX PARAMETERS AS READ FROM STANDARD INPUT FILE')
      read(lc,*)head
      write(lp,*)head
      read(lc,*)ntr,new,ndg,nsr
      write(lp,7) ij,ntr,new,ndg,nsr
    7 format('  job no = ',i3/'  no. traits = ',i3/
     + '  no. econ. wt. indices = ',i3/
     + '  no. desired gain indices = ',i3/
     + '  no. restricted indices = ',i3)
c-----
      read(lc,*)nh,nf,no
      write(lp,8) nh,nf,no
    8 format(' nh = no per half sib family = ',i3/
     + ' nf = no per full sib family = ',i3/
     + ' no = no per progeny test group = ',i3)
c-----
      nitr=0
      nhtr=0
      nftr=0
      notr=0
c-----read index trait parameters - defines subset of library traits
c-----                              to be used
c-----                              l(j) is library trait no = index for
c-----                              arrays setup by parlib subroutine
c-----                              gres(i,j) =1 if trait allowed to change
c-----                                        =0 if delta_G=0 for trait j
      if(ntr.eq.0) then
        write(lp,*) 'must be at least one trait'
        stop
      endif
c-----
      if(new.eq.0 .and. ndg.eq.0) then
        write(lp,*)' must be at least one set of wts or desired gains'
        stop
      endif
c-----
      ntr4=ntr*4
      nr=ntr4
        write(lp,100)
  100   format(' trait in index as - I - individual value'/
     +         18x,' - H - half sib family mean'/
     +         18x,' - F - full sib family mean'/
     +         18x,' - O - progeny test mean')
        write(lp,101)
  101   format(/19x,'  I  H  F  O')
c-----
      do 10 j=1,ntr
        if(new.gt.0 .and. ndg.gt.0 .and. nsr.gt.0) then
          read(lc,*)l(j),label(j),ini(j),inh(j),inf(j),ino(j)
     +    ,(a(inew,j),inew=1,new),(dg(idg,j),idg=1,ndg)
     +    ,(gres(ires,j),ires=1,nsr)
        else if(new.gt.0 .and. ndg.gt.0 .and. nsr.eq.0) then
          read(lc,*)l(j),label(j),ini(j),inh(j),inf(j),ino(j)
     +    ,(a(inew,j),inew=1,new),(dg(idg,j),idg=1,ndg)
        else if(new.gt.0 .and. ndg.eq.0 .and. nsr.gt.0) then
          read(lc,*)l(j),label(j),ini(j),inh(j),inf(j),ino(j)
     +    ,(a(inew,j),inew=1,new)
     +    ,(gres(ires,j),ires=1,nsr)
        else if(new.eq.0 .and. ndg.gt.0 .and. nsr.gt.0) then
          read(lc,*)l(j),label(j),ini(j),inh(j),inf(j),ino(j)
     +    ,(dg(idg,j),idg=1,ndg)
     +    ,(gres(ires,j),ires=1,nsr)
        else if(new.gt.0 .and. ndg.eq.0 .and. nsr.eq.0)then
          read(lc,*)l(j),label(j),ini(j),inh(j),inf(j),ino(j)
     +    ,(a(inew,j),inew=1,new)
        else if(new.eq.0 .and. ndg.gt.0 .and. nsr.eq.0) then
          read(lc,*)l(j),label(j),ini(j),inh(j),inf(j),ino(j)
     +    ,(dg(idg,j),idg=1,ndg)
        endif
c-----check trait no's
      if(l(j).le.0.or.l(j).gt.ltr.or.l(j).gt.mvltr)then
        write(lp,*)'resi3: library trait no ',l(j),' invalid'
        stop
      endif
c-----
c-----make lists of traits in I,H,F,O parts of combined index
        if(ini(j).eq.1) then
          nitr=nitr+1
          nr=nr-1
          listi(nitr)=j
        endif
        if(inh(j).eq.1) then
          nhtr=nhtr+1
          nr=nr-1
          listh(nhtr)=j
        endif
        if(inf(j).eq.1) then
          nftr=nftr+1
          nr=nr-1
          listf(nftr)=j
        endif
        if(ino(j).eq.1) then
          notr=notr+1
          nr=nr-1
          listo(notr)=j
        endif
c-----write traits + in?'s
        write(lp,102)j,l(j),label(j),ini(j),inh(j),inf(j),ino(j)
  102   format(2i4,1x,a10,4i3)
c-----
   10 continue
c-----
c-----write traits out in numerical order with ec wts, des gains, restrictions
      if(new.gt.0) then
        write(lp,20) new
   20   format(/' economic weights for indices 1 to ',i3)
        write(lp,21)(i,i=1,new)
   21   format('       trait  ',5('    index ',i2)/
     +        (12x,5('    index ',i2)))
        do 12 j=1,ntr
   12   write(lp,22)j,label(j),(a(inew,j),inew=1,new)
   22   format(' ',i2,1x,a10,5f12.4/(12x,5f12.4))
      endif
      if(ndg.gt.0) then
        write(lp,23) new+1,new+ndg
   23   format(/' desired gains for indices ',i3,' to ',i3)
        write(lp,21)(i,i=new+1,new+ndg)
        do 13 j=1,ntr
   13   write(lp,22)j,label(j),(dg(idg,j),idg=1,ndg)
      endif
      if(nsr.gt.0) then
        write(lp,24)nsr
   24   format(/' restricted indices 1 to ',i3)
        write(lp,25)(i,i=1,nsr)
   25   format('      trait   ',5('    index ',i2)/
     +   (12x,5('    index ',i2)))
        do 14 j=1,ntr
   14   write(lp,22)j,label(j),(gres(ires,j),ires=1,nsr)
      endif
c-----
      return
      end
