      program main
      dimension ksdco(30),bcoch(30),bcoch2(30),raizc1(30),raizc2(30),
     1raizco(30),sintec(30),pfpnc1(30),pfpnc2(30),bgscoh(30),singco(30)
      common /pps1/ksd
      open(unit=5,file='zsiinp.dat',status='old')
      open(unit=6,file='zsiout.dat',status='new')
      ak1=0.0
      do 4646 i=1,30
      ksdco(i)=0
      bcoch(i)=0.0
      bcoch2(i)=0.0
      raizc1(i)=0.0
      raizc2(i)=0.0
      raizco(i)=0.0
      sintec(i)=0.0
      pfpnc1(i)=0.0
      pfpnc2(i)=0.0
      bgscoh(i)=0.0
4646  singco(i)=0.0
      i=0
46461 i=i+1
      read(5,*)irlf,ksdco(i),bcoch(i),bcoch2(i),raizc1(i),raizc2(i),
     xraizco(i),pfpnc1(i),pfpnc2(i),nico
      if(nico.ne.0) goto 46461
      np=i
      do 36462 i=1,np
      if(bcoch2(i).gt..999) bcoch2(i)=1.0
      if(bcoch(i).gt..999) bcoch(i)=1.0
c**** wenn remontierungsreihe goto 4501
      ksd=ksdco(i)
      call zsi(bcoch(i),bcoch2(i),raizc1(i),raizc2(i),raizco(i),
     1sintec(i),pfpnc1(i),pfpnc2(i))
      bgscoh(i)=bcoch2(i)
      singco(i)=sintec(i)
36462 continue

      write(6,6853)
6853  format(1h1,10x/
     x11x,'select.   t e s t e d        %-selected    correlation-    '
     x,'   finit   g-value'/
     x11x,' path     a n i m a l s                    coefficient     '
     x,'  infinit  (zsi) '/
     x11x,'         1.stf     2.stf      b1     b2     r1   r2   r12  '
     x,'   ksd       g'/
     x11x,77(1h=))
      do 46462 i=1,np
      write(6,6854)i,pfpnc1(i),pfpnc2(i),bcoch(i),bcoch2(i),raizc1(i),
     xraizc2(i),raizco(i),ksdco(i),sintec(i)
46462 continue
6854  format(1h0,10x,i3,f10.1,1x,f8.1,5x,2f6.3,2x,3(f5.3,1x),i5,5x,
     x f7.4)
       end
      subroutine  ordin(alp,abs,ord,sint,z)
c=======================================================================
c**** unterprogramm zur ermittlung der selektionsintensitaet (niebel ...
c=======================================================================
      dimension p1(140),p2(130),p3(130),p4(100),ps(500)
      common/pps1/ksd
      data  p1/0.5,0.49601,0.49202,0.48803,0.48405,0.48006,0.476
     10800,0.4721,0.46812,0.46414,0.46017,0.4562,0.45224,0.44828,0.44433
     2,0.44038,0.43644,0.43251,0.42858,0.42465,0.42074,0.41683,0.41294,0
     3.40905,0.40517,0.40129,0.3974300,0.39358,0.38974,0.38591,0.38209,0
     4.37828,0.37448,0.37070,0.36693,0.36317,0.35942,0.35569,0.351970,0.
     534827,0.34458,0.3409,0.337240,0.3336,0.32997,0.32636,0.32276,0.319
     618,0.31561,0.31207,0.30854,0.30503,0.30153,0.29806,0.2946,0.29116,
     70.28774,0.28434,0.28096,0.277600,0.27425,0.27093,0.26763,0.26435,0
     8.26109,0.25785,0.25463,0.25143,0.24825,0.2451,0.24196,0.23885,0.23
     9576,0.2327,0.22965,0.22663,0.22363,0.22065,0.2177,0.21476,0.21186,
     a0.20897,0.20611,0.20327,0.20045,0.19766,0.19489,0.19215,0.18943,0.
     b18673,0.18406,0.18141,0.17879,0.17619,0.173610,0.17106,0.16853,0.1
     c66020,0.16354,0.16109,0.15866,0.15625,0.15386,0.15151,0.14917,0.14
     d686,0.14457,0.14231,0.140070,0.13786,0.13567,0.1335,0.13136,0.1292
     e4,0.12714,0.12507,0.12302,0.12100,0.119,0.11702,0.11507,0.113140,0
     f.11123,0.10935,0.10749,0.10565,0.10383,0.10204,0.10027,0.098525,0.
     g0968,0.095098,0.093418,0.091759,0.090123,0.088508,0.086915,0.08534
     h30,0.083793,0.082264 /
      data  p2/0.80757,0.7927,0.77804,0.76359,0.74934,0.73529,0.72145,0.
     170781,0.69437,0.6811200,0.66807,0.65522,0.64255,0.63008,0.6178,0.6
     20571,0.5938,0.58208,0.57053,0.55917,0.54799,0.53699,0.52616,0.5155
     31,0.50503,0.49471,0.48457,0.4746,0.46479,0.45514,0.44565,0.43633,0
     4.4271600,0.41815,0.4093,0.40059,0.39204,0.38364,0.37538,0.36727,0.
     53593,0.35148,0.3438,0.33625,0.32884,0.32157,0.31443,0.30742,0.3005
     6400,0.29379,0.28717,0.28067,0.27429,0.26803,0.2619,0.25588,0.24998
     7,0.24419,0.23852,0.23295,0.2275,0.2221600,0.21692,0.21178,0.20675,
     80.20182,0.19699,0.19226,0.18763,0.18309,0.17864,0.17429,0.17003,0.
     916586,0.16177,0.15778,0.15386,0.150030,0.14629,0.14262,0.13903,0.1
     a3553,0.13209,0.12874,0.12545,0.12224,0.11911,0.11604,0.11304,0.110
     b11,0.10724,0.10444,0.1017,0.09903,0.096419,0.093867,0.091375,0.088
     c94,0.086563,0.084242,0.081975,0.079763,0.077603,0.075494,0.073436,
     d0.071428,0.069469,0.067557,0.065691,0.0638720,0.06209700,0.060366,
     e0.0586770,0.057031,0.055426,0.053861,0.052336,0.050849,0.0494,0.04
     f7988,0.046612,0.045271,0.043965,0.042692,0.041453,0.0402460,0.0390
     g7,0.037926,0.036811,0.0357260/
      data  p3/0.3467,0.33642,0.32641,0.31667,0.3072,0.29798,0.28901,0.2
     18028,0.271790,0.26354,0.25551,0.24771,0.24012,0.23274,0.22557,0.21
     2860,0.21182,0.20524,0.198840,0.19262,0.18658,0.18071,0.17502,0.169
     348,0.16411,0.15889,0.15382,0.1489,0.14412,0.139490,0.13499,0.13062
     4,0.12639,0.12228,0.11829,0.11442,0.11067,0.10703,0.1035,0.10008,0.
     509676,0.093544,0.090426,0.087403,0.084474,0.081635,0.078885,0.0762
     619,0.073638,0.071136,0.068714,0.066367,0.064095,0.0618950,0.059765
     7,0.057703,0.055706,0.053774,0.051904,0.050094,0.048342,0.046648,0.
     8045009,0.043423,0.041889,0.040406,0.038971,0.037584,0.036243,0.034
     9946,0.033693,0.032481,0.031311,0.030179,0.029086,0.028029,0.027009
     a,0.026023,0.025071,0.024151,0.02326300,0.022405,0.021577,0.020778,
     b0.020006,0.019262,0.018543,0.017849,0.01718,0.016534,0.015911,0.01
     c531,0.01473,0.014171,0.013632,0.013112,0.012611,0.012128,0.011662,
     d0.011213,0.01078,0.010363,0.0099611,0.009574,0.009201,0.0088417,0.
     e0084957,0.0081624,0.0078414,0.0075324,0.0072348,0.0069483,0.006672
     f6,0.0064072,0.0061517,0.0059059,0.0056694,0.0054418,0.0052228,0.00
     g50122,0.0048096,0.0046148,0.0044274,0.00424730,0.0040741,0.0039076
     h,0.0037475,0.0035936,0.0034458,0.0033037 /
      data  p4/0.31671,0.30359,0.29099,0.27888,0.26726,0.25609,0.24536,0
     a.23507,0.22518,0.21569,0.20658,0.19783,0.18944,0.18138,0.17365,0.1
     b6624,0.15912,0.1523,0.14575,0.13948,0.13346,0.12769,0.12215,0.1168
     c5,0.11176,0.10689,0.10221,0.097736,0.093447,0.089337,0.085399,0.08
     d1627,0.078015,0.074555,0.071241,0.06807,0.065031,0.062123,0.059340
     e,            0.056675, 0.054125,0.051685,0.049350,0.047117,0.04497
     f9, 0.042935,0.040980,0.039110,0.037322,0.035612,0.033977,0.032414,
     g0.030920,0.029492,0.028127,0.026823,0.025577,0.024386,0.023249,0.0
     h22162,0.021125,0.020133,0.019187,0.018283,0.017420,0.016597, 0.015
     i810, 0.015060,0.014344,0.013660,0.013008,0.012386,0.011792,0.01122
     j6,0.010686,0.010171,0.0096796,0.0092113,0.0087648,0.0083391,0.0079
     k333,0.0075465,0.0071779,0.0068267,0.0064920,0.0061731,0.0058693,0.
     l0055799,0.0053043,0.0050418,0.0047918,0.0045538,0.0043272,0.004111
     m5, 0.0039061,0.0037107,0.0035247,0.0033476,0.0031792,0.0030190   /
      data  ibeg/1/
      if(ibeg.le.0)          go to 10
      do 12 i=141,270
      ps(i)=p2(i-140)/10.0
12    ps(i+130)=p3(i-140)/100.0
      do 13 i=1,140
13    ps(i)=p1(i)
      do 14 i=401,500
14    ps(i)=p4(i-400)/10000.0
      ibeg=0
10    if(alp.le.0.0)          go to 8
      if(alp.le.0.999)        go to 7
      if(alp-0.9999)          7,7,8
    8 alp=1.0
      sint=0.00000001
      ord=0.000001
      abs=-10.0
      return
    7 be=alp
      if(alp.gt.0.5)  be = 1-alp
      if(be.lt.ps(499))  be = ps(499)
      do 1 in = 50,500,50
      if (be.ge.ps(in))   go to 2
1     continue
2     in = in-50
      do 3 k = 5,50,5
      if (be.ge.ps(in+k))  goto 4
3     continue
4     m = in+k-5
      do 5 l = 1,5
      if(be.ge.ps(m+l))  go to 6
5     continue
6     n = m+l+1
      pol = (be-ps(n))/(ps(n-1)-ps(n))
      c = float(n)
      c = c-1.
      abs = (c-pol)*0.01
      ord = 0.3989422804d0*exp(-abs*abs/2.)
      sint = ord/alp
      if(ksd*z.lt.1.0)        go to 9
      x=z/(alp+0.0000001)
      if(x-z.lt.1.0d0) goto 9
      sint=sint-(x-z)/((x+1)*2.0*sint*z+0.00001)
9     neg = -1
      if(alp.gt.0.5) abs = abs*float(neg)
      return
      end
      subroutine zsi(b1,b2,r3,r4,r34,g,zb,zba)
c=======================================================================
c**** subroutine zur ermittlung der sel-int  in zweistufenselektion (nie
c=======================================================================
      rho=r34
      if(rho.lt.0.0)  rho=0.0
      if(rho.gt.0.999)  rho=0.999
      call ordin(b1,rk1,ord,s1,zb)
      call ordin(b2,rk2,ord,s2,zba)
      c=s1*(s1-rk1)
      div=10*s1+3*s2-1
      if(s1.le.0.11)  div=10*s1+3*s2+0.0001
         c1=.6*s1*rho**(rho*2.5*s1+1.7)/div*(2*(2*s2-1)+s1*(s2-1.6))
      hge=rho**2*c
      if(hge.ge.0.999) hge=0.999
      hg=1.-hge
      g=(r4-rho*r3*c)/sqrt(hg)*s2*(1.-c1)+r3*s1
      g1=r3*s1+r4*s2
      if(r3*r4.lt.0.)        g=g1
      return
      end

