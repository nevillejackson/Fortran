      subroutine bxa(a,b,c,d,ta,tb,ach,bch)
      double precision a(*),b(*)
      character*10 c(*),d(*),ach(*),bch(*)
      logical ta(*),tb(*)
c-----
c----- a(1) W, a(2) D, a(3) B, a(4) dNLW, c(1) dlx code
c----- b(1) AMS Index = W/D^3 + B
c----- b(2) WP1.1, b(3) WP2.1, b(4) WP1.3, b(5) WP2.3
c-----
      character*4 dlx(23)
      real wm(23),dm(23),bm(23),dnlwm(23)
      data dlx /'8211','8212','8221','8222','8231','8232',
     +    '8311','8312','8321','8331','8332','8392',
     +    '8411','8421','8431','8432',
     +    '8511','8512','8521','8522','8531','8532','8592'/
      data wm /2.759,2.940,2.804,3.036,2.757,3.125,
     +    2.955,2.716,3.033,3.137,2.825,2.879,
     +    2.538,2.682,2.702,2.934,
     +    1.937,1.990,1.868,2.012,2.098,2.052,2.122/
      data dm /19.16,20.59,18.34,19.53,18.91,19.94,
     +    18.57,17.78,18.73,18.35,17.69,20.58,
     +    19.53,17.69,18.60,19.08,
     +    17.16,18.16,16.45,17.12,16.93,17.49,19.15/
      data bm /38.23,34.08,40.56,35.26,39.69,35.72,
     +    43.12,36.76,46.18,45.46,35.62,36.61,
     +    38.42,40.75,37.84,32.52,
     +    30.78,27.85,31.18,29.35,32.27,28.06,29.34/
      data dnlwm /1.14,1.15,1.18,1.19,1.05,1.13,
     +    1.23,1.40,1.39,1.10,1.12,1.32,
     +    1.30,1.23,1.47,1.27,
     +    1.39,1.31,1.40,1.37,1.12,1.29,1.40/
c-----
      if(ta(1).and.ta(2).and.ta(3).and.ta(4)) then
        do 1 i=1,5
    1   tb(i)=.true.
c----- ams index
        b(1)=1000.*a(1)/(a(2)**3) + a(3)
c-----woolplan indices
c----- drop,line,sex
        idlx=ifind(dlx,c(1),23)
        if(idlx.eq.0) then
          write(6,*)'item ',c(1), ' not in list'
          stop
        endif
c----- deviations
        w=a(1)-wm(idlx)
        di=a(2)-dm(idlx)
        bw=a(3)-bm(idlx)
        dnlw=a(4)-dnlwm(idlx)
c-----indices
        b(2)=5.54*w-0.99*di
        b(3)=5.61*w-0.29*di
        b(4)=4.51*w-1.05*di+1.24*dnlw+0.400*bw
        b(5)=4.64*w-0.28*di+1.20*dnlw+0.381*bw
c-----
        if(ta(5).and.ta(6).and.ta(7).and.ta(8).and.ta(9).and.ta(10)
     +    .and.ta(11)) then
          do 11 i=6,16
   11     tb(i)=.true.
c-----    dp-ds
          b(10)=a(5)-a(6)
          b(11)=b(10)*b(10)
c-----    sdp,sds,sd
          b(6)=a(8)*a(5)/100.
          b(7)=a(9)*a(6)/100.
          b(8)=sqrt((a(10)*b(6)**2 + a(11)*b(7)**2
     +              + b(11))/(a(10)+a(11)-1))
c-----    cvd
          b(9)=100.*b(8)/a(7)
c----     variances
          b(12)=b(6)*b(6)
          b(13)=b(7)*b(7)
          b(14)=b(8)*b(8)
c-----    primitive score & ratio (nb 8. sq mm approx)
          b(15)=sqrt(b(12)+(8.*a(10)/(8.*a(10)-1.))*b(11))
          b(16)=b(15)/b(7)
        else
          do 22 i=6,16
          tb(i)=.false.
   22     bch(i)='      '
        endif
      else
        do 2 i=1,5
          tb(i)=.false.
    2   bch(i)='      '
      endif
      return
      end
      integer function ifind(list,item,len)
      character*4 list(*),item
      do 1 i=1,len
      if(item.eq.list(i)) then
        ifind=i
        return
      endif
    1 continue
      ifind=0
      return
      end
