      subroutine covset(ij,liop,ltr,ntr,nh,nf,no
     + ,hl,sigl,rgl,rpl,chl,rchl,cfl,rcfl
     + ,ntr4,p,g,sigp,sigg,l,label,ch,cf)
c----- a)sets up subset of ntr library parameters chosen for 
c-----  current index (Ii,Ij) partition
c----- b)sets up H,F,O partitions of P & G
c-----  all partitions set up full size - mvntr4=4*mvntr
c-----  P & G square
c----- c)sigp() & sigg() set to Gii & Pii for I,H,F,O
c-----
c----- input: hl(),sigl(),rgl(),rpl()
c-----
c----- output:sigp(),sigg(),g(,),p(,)
c-----        old -> new
c-----        sig -> sigp
c-----          h -> sigg notation
c-----
      implicit double precision (a-h,o-z)
      dimension l(mvntr),ch(mvntr,mvntr),cf(mvntr,mvntr)
      dimension chl(mvltr),rchl(mvltr2),cfl(mvltr),rcfl(mvltr2)
      dimension p(mvntr4,mvntr4),g(mvntr4,mvntr4)
      dimension sigp(mvntr4),sigg(mvntr4)
      character*10 label(mvntr)
      dimension hl(mvltr),sigl(mvltr)
      dimension rgl(mvltr2),rpl(mvltr2)
c-----
      common /limits/mvltr,mvltr2,mvntr,mvnw,mvndg,mvns,mvntr4
      common /stdio/ lc,lp,lerr
      external iloc
c-----
c-----I,H,F,O partitions of P,G
      do 1 ip=1,4
        do 1 jp=1,4
          if(ip.eq.1 .and. jp.eq.1) then
c-----    IxI partition (and H,F common env cov's Ceh,Cef)
            do 2 i=1,ntr
              do 2 j=1,ntr
                ir=l(i)
                jr=l(j)
                p(i,j)=rpl(iloc(ir,jr,ltr,ltr,1))*sigl(ir)*sigl(jr)
                g(i,j)=rgl(iloc(ir,jr,ltr,ltr,1))*sigl(ir)*dsqrt(hl(ir))
     +                                 *sigl(jr)*dsqrt(hl(jr))
                ch(i,j)=rchl(iloc(ir,jr,ltr,ltr,1))
     +                                   *sigl(ir)*dsqrt(chl(ir))
     +                                   *sigl(jr)*dsqrt(chl(jr))
                cf(i,j)=rcfl(iloc(ir,jr,ltr,ltr,1))
     +                                   *sigl(ir)*dsqrt(cfl(ir))
     +                                   *sigl(jr)*dsqrt(cfl(jr))
    2       continue
c-----
          else if(ip.eq.1.and.jp.eq.2 .or. ip.eq.2.and.jp.eq.1) then
c-----    IxH or HxI partition
            do 3 i=1,ntr
              do 3 j=1,ntr
                ib=i+(ip-1)*ntr
                jb=j+(jp-1)*ntr
                if(nh.eq.0) then
                  p(ib,jb)=0.d0
                  g(ib,jb)=0.d0
                else
                  p(ib,jb)=(p(i,j)+(nh-1.)*(0.25*g(i,j)+ch(i,j)))/nh
                  g(ib,jb)=g(i,j)*(1.+0.25*(nh-1.))/nh
                endif
    3       continue
c-----
          else if(ip.eq.1.and.jp.eq.3 .or. ip.eq.3.and.jp.eq.1) then
c-----      IxF or FxI partition
            do 4 i=1,ntr
              do 4 j=1,ntr
                ib=i+(ip-1)*ntr
                jb=j+(jp-1)*ntr
                if(nf.eq.0) then
                  p(ib,jb)=0.d0
                  g(ib,jb)=0.d0
                else
                  p(ib,jb)=(p(i,j)+(nf-1.)*(0.5*g(i,j)+cf(i,j)))/nf
                  g(ib,jb)=g(i,j)*(1.+0.5*(nf-1.))/nf
                endif
    4       continue
c-----
          else if(ip.eq.1.and.jp.eq.4 .or. ip.eq.4.and.jp.eq.1) then
c-----      IxO or OxI partition (equal variances)
c-----          (ie variances equal across generations & 
c-----              no selection of individuals for progeny test)
            do 5 i=1,ntr
              do 5 j=1,ntr
                ib=i+(ip-1)*ntr
                jb=j+(jp-1)*ntr
                if(no.eq.0) then
                  p(ib,jb)=0.d0
                  g(ib,jb)=0.d0
                else
                  p(ib,jb)=0.5*g(i,j)
                  g(ib,jb)=0.5*g(i,j)
                endif
    5       continue
c-----
          else if(ip.eq.2.and.jp.eq.2) then
c-----      HxH partition
            do 6 i=1,ntr
              do 6 j=1,ntr
                ib=i+(ip-1)*ntr
                jb=j+(jp-1)*ntr
                if(nh.eq.0) then
                  p(ib,jb)=0.d0
                  g(ib,jb)=0.d0
                else
                  p(ib,jb)=(p(i,j)+(nh-1.)*(0.25*g(i,j)+ch(i,j)))/nh
                  g(ib,jb)=g(i,j)*(1.+0.25*(nh-1.))/nh
                endif
    6       continue
c-----
          else if(ip.eq.2.and.jp.eq.3 .or. ip.eq.3.and.jp.eq.2) then
c-----      HxF or FxH partition
            do 7 i=1,ntr
              do 7 j=1,ntr
                ib=i+(ip-1)*ntr
                jb=j+(jp-1)*ntr
                if(nh.eq.0 .or. nf.eq.0) then
                  p(ib,jb)=0.d0
                  g(ib,jb)=0.d0
                else        
                  p(ib,jb)=(p(i,j)+(nf-1.)*(0.5*g(i,j)+cf(i,j))
     +                          +(nh-nf)*(0.25*g(i,j)+ch(i,j)))/nh
                  g(ib,jb)=g(i,j)*(1.+0.5*(nf-1.)+0.25*(nh-nf))/nh
                endif
    7       continue
c-----
          else if(ip.eq.2.and.jp.eq.4 .or. ip.eq.4.and.jp.eq.2) then
c-----      HxO or OxH partition
            do 8 i=1,ntr
              do 8 j=1,ntr
                ib=i+(ip-1)*ntr
                jb=j+(jp-1)*ntr
                if(nh.eq.0 .or. no.eq.0) then
                  p(ib,jb)=0.d0
                  g(ib,jb)=0.d0
                else
                  p(ib,jb)=g(i,j)*(0.5+0.125*(nh-1.))/nh
                  g(ib,jb)=g(i,j)*(0.5+0.125*(nh-1.))/nh
                endif
    8       continue
c-----
          else if(ip.eq.3.and.jp.eq.3) then
c-----      FxF partition
            do 9 i=1,ntr
              do 9 j=1,ntr
                ib=i+(ip-1)*ntr
                jb=j+(jp-1)*ntr
                if(nf.eq.0) then
                  p(ib,jb)=0.d0
                  g(ib,jb)=0.d0
                else
                  p(ib,jb)=(p(i,j)+(nf-1.)*(0.5*g(i,j)+cf(i,j)))/nf
                  g(ib,jb)=g(i,j)*(1.+0.5*(nf-1.))/nf
                endif
    9       continue
c-----
          else if(ip.eq.3.and.jp.eq.4 .or. 
     +            ip.eq.4.and.jp.eq.3) then
c-----      FxO (or OxF) partition (equal variances)
            do 10 i=1,ntr
              do 10 j=1,ntr
                ib=i+(ip-1)*ntr
                jb=j+(jp-1)*ntr
                if(nf.eq.0 .or. no.eq.0) then
                  p(ib,jb)=0.d0
                  g(ib,jb)=0.d0
                else
                  p(ib,jb)=g(i,j)*(0.5+0.25*(nf-1.))/nf
                  g(ib,jb)=g(i,j)*(0.5+0.25*(nf-1.))/nf
                endif
   10       continue
c-----      OxO partition
          else if(ip.eq.4.and.jp.eq.4) then
            do 11 i=1,ntr
              do 11 j=1,ntr
                ib=i+(ip-1)*ntr
                jb=j+(jp-1)*ntr
                if(no.eq.0) then
                  p(ib,jb)=0.d0
                  g(ib,jb)=0.d0
                else
                  p(ib,jb)=(p(i,j)+0.25*(no-1.)*g(i,j))/no
                  g(ib,jb)=g(i,j)*(1.+0.25*(no-1.))/no
                endif
   11       continue
c-----
          endif
    1 continue
c----- write out all partitions of P & G
      if(liop.gt.0) then
        write(lp,40)
   40   format(/' phenotypic (co)variances - I,H,F,O partitions')
        do 41 i=1,ntr4
   41   write(lp,35)(p(i,j),j=1,i)
   35   format(' ',5f20.4)
        write(lp,42)
   42   format(/' genetic (co)variances - I,H,F,O partitions')
        do 43 i=1,ntr4
   43   write(lp,35)(g(i,j),j=1,i)
      endif
c-----
c-----setup sigp() & sigg()
      ntr4=4*ntr
      do 20 i=1,ntr4
        sigp(i)=dsqrt(p(i,i))
   20 sigg(i)=dsqrt(g(i,i))
c----- write out sigp() & sigg()
      if(liop.gt.0) then
        write(lp,44)
   44   format(/' phenotypic standard deviations - I,H,F,O partitions')
        write(lp,35)(sigp(i),i=1,ntr4)
        write(lp,45)
   45   format(/' genetic standard deviations - I,H,F,O partitions')
        write(lp,35)(sigg(i),i=1,ntr4)
      endif
c-----
      return
      end
