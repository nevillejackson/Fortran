      subroutine parlib(miop,ltr,hl,sigl,rgl,rpl,chl,cfl,rchl
     + ,rcfl)
c-----reads population parameters -four files - one per matrix
c-----file structure - free format,one i,j pair per record,
c-----               - any order of records
c-----               - i j rg(hsq if i.eq.j) rp(phenstdev if i.eq.j)
c-----use calls to matrix library to privide storage options
c-----
c-----returned - hl() vector of h^2 1,..ltr
c-----         - sigl() vector if phen.st.dev. 1,..ltr
c-----         - rgl() vector of Rg 1,..ltr*(ltr+1)/2
c-----         - rpl() vector of Rp 1,..ltr*(ltr+1)/2
c-----         - chl() vector of Ch^2 1,..ltr
c-----         - rchl() vector of Rch 1,..ltr*(ltr+1)/2
c-----         - cfl() vector of Cf^2 1,..ltr
c-----         - rcfl() vector of Rcf 1,..ltr*(ltr+1)/2
c-----
      implicit double precision (a-h,o-z)
      dimension hl(*),sigl(*),chl(*),cfl(*)
     +          ,rgl(*),rpl(*),rchl(*),rcfl(*)
      character*80 fin
      common /limits/ mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      common /stdio/ lc,lp,lerr
      common /files/ mi
      external iloc
c-----
c-----check miop
      if(miop.eq.1) then
c-----  ascii library file (one trait pair per line)
        if(iargc().ge.1) then
          call getarg(1,fin)
          open(mi,file=fin,status='old')
        else
          write(lp,*)'resi3: argument error'
          stop
        endif
        write(lp,100) fin
  100   format(///' POPULATION PARAMETERS AS READ FROM LIBRARY FILE '
     +   ,a80)
        write(lp,101)
  101   format(/' TRAIT-A   TRAIT-B    R(P) OR    R(G) OR   R(CH) OR'
     +  ,'   R(CF) OR')
        write(lp,102)
  102   format(18x,'     SD(P)        HSQ       CHSQ       CFSQ')
c-----
    1   read(mi,*,end=99,err=98,iostat=ios)i,j,a,b,c,d
        write(lp,103) i,j,a,b,c,d
  103   format(' ',i7,i10,4f11.4)
        ij=iloc(i,j,ltr,ltr,1)
        if(i.eq.j) then
          sigl(i)=a
          hl(i)=b
          chl(i)=c
          cfl(i)=d
          rpl(ij)=1.d0
          rgl(ij)=1.d0
          rchl(ij)=1.d0
          rcfl(ij)=1.d0
        else
          rpl(ij)=a
          rgl(ij)=b
          rchl(ij)=c
          rcfl(ij)=d
        endif
        go to 1
c-----
   98   write(lp,*)'resi3: read error on file ',fin,' iostat = ',ios
        stop
c-----
   99   close(mi)
c-----
      else if(miop.eq.2) then
c-----  matrix file blocked format
c-----  one file per matrix -> 4 or 8 files
        write(lp,*)' option miop=2 not available'
        stop
      endif
c-----
      return 
      end
