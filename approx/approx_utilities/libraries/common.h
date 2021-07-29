
#include <stdio.h>
#include <math.h>
//#include <conio.h>
#include <stdlib.h>
//#include <dos.h> 


long n;
long k2;
long k3;
long n16;

struct Arrays
{
    double U[1001];
    double V[1001];
    double W[1001];
    double X[1001];
    double Y[1001];
    double Z[1001];
    double G[1001];
    double Du1[101];
    double Du2[101];
    double Du3[101];
    double Grd[1001];
    double Dex[1001];
    double Xi[1001];
    double Ex[1001];
    double Ex1[1001];
    double Dex1[1001];
    double Vx[1001];
    double Xx[1001];
    double Rx[1001];
    double Rh[2048];
    double Vsp[101];
    double Vstp[101];
    double Vxne[101];
    double Vxnd[101];
    double Ve3[101];
    double Vlr[101];
    double Vlin[101];
    double B5[101];
    double Plan[300];
    double D[300];
    double Sa[101];
    double Sb[101];     
    double P[512][4];
    double Px[101][25];
    double Cx[101][25];
    double Vy[25][101];
    double Vh[7][101];
    double Vf[7][101];
    double Vg[7][101];
    double Vs[7][101];
    double Za[7][101];
    double Zp[7][101];
    double Zq[7][101];
    double Zr[7][101];
    double Zm[7][101];
    double Zb[7][101];
    double Zu[7][101];
    double Zv[7][101];
    double Zz[7][101];               
    double B[64][64];
    double C[64][64];
    double H[64][64];     
    double U1[2][101][5];
    double U2[2][101][5];
    double U3[2][101][5];
    double Xtra[40];     
    long   E[96];
    long   F[96];
    long   Ix[1001];
    long   Ir[1001];
    long   Zone[301];
    double X0[1001];
    double W0[1001];
    double Px0[101][25];
    double P0[512][4];
    double H0[64][64];
    double Rh0[2048];
    double Vxne0[101];
    double Zr0[7][101];
    double Zu0[7][101];
    double Zv0[7][101];
    double Zz0[7][101];
    double Za0[101][25];
    double Stb50;               
    double Xx0;


}as1;
#define u        as1.U
#define v        as1.V
#define w        as1.W
#define x        as1.X
#define y        as1.Y
#define z        as1.Z
#define g        as1.G
#define du1      as1.Du1
#define du2      as1.Du2
#define du3      as1.Du3
#define grd      as1.Grd
#define dex      as1.Dex
#define xi       as1.Xi
#define ex       as1.Ex
#define ex1      as1.Ex1
#define dex1     as1.Dex1
#define vx       as1.Vx
#define xx       as1.Xx
#define rx       as1.Rx
#define rh       as1.Rh
#define vsp      as1.Vsp
#define vstp     as1.Vstp
#define vxne     as1.Vxne
#define vxnd     as1.Vxnd
#define ve3      as1.Ve3
#define vlr      as1.Vlr
#define vlin     as1.Vlin
#define b5       as1.B5
#define plan     as1.Plan
#define d        as1.D
#define sa       as1.Sa
#define sb       as1.Sb
#define p        as1.P
#define px       as1.Px
#define cx       as1.Cx
#define vy       as1.Vy
#define vh       as1.Vh
#define vf       as1.Vf
#define vg       as1.Vg
#define vs       as1.Vs
#define za       as1.Za
#define zb       as1.Zb
#define zp       as1.Zp
#define zq       as1.Zq
#define zr       as1.Zr
#define zm       as1.Zm
#define zz       as1.Zz
#define zu       as1.Zu
#define zv       as1.Zv
#define b        as1.B
#define c        as1.C
#define h        as1.H
#define u1       as1.U1
#define u2       as1.U2
#define u3       as1.U3
#define xtra     as1.Xtra
#define a11      as1.Xtra[1]
#define a12      as1.Xtra[2]
#define a13      as1.Xtra[3]
#define a21      as1.Xtra[4]
#define a22      as1.Xtra[5]
#define a23      as1.Xtra[6]
#define a31      as1.Xtra[7]
#define a32      as1.Xtra[8]
#define a33      as1.Xtra[9]
#define c0       as1.Xtra[12]
#define dk       as1.Xtra[15]
#define dm22     as1.Xtra[16]
#define dm23     as1.Xtra[17]
#define dm24     as1.Xtra[18]
#define dm25     as1.Xtra[19]
#define dm26     as1.Xtra[20]
#define dm27     as1.Xtra[21]
#define dm28     as1.Xtra[22]
#define expmax   as1.Xtra[26]
#define flx      as1.Xtra[27]
#define q        as1.Xtra[28]
#define r        as1.Xtra[30]
#define s        as1.Xtra[32]
#define sig      as1.Xtra[34]
#define stb5     as1.Xtra[35]
#define t        as1.Xtra[36]
#define xnm      as1.Xtra[39]   
#define e        as1.E  
#define f        as1.F
#define ix       as1.Ix
#define ir       as1.Ir
#define zone     as1.Zone


void iqranf();
void Init(long which) {
    //   long   lw;
    //   long   ipnt, ipntp, ii;
    //   double temp;
    //   long   nl1, nl2;
    //   long   kx, ky;
    //   double ar, br, cr;
    //   long   i, j, k, m;
    //   long   ip, i1, i2, j1, j2, j4, lb;
    //   long   ng, nz;
    //   double tmp;
    //   double scale, xnei, xnc, e3,e6;
    //   long   ink, jn, kn, kb5i;
    //   double di, dn;
    //   double qa;

    n16 = 75;

    long   i, j, k, l, m, nn;
    double ds, dw, rr, ss;
    double fuzz, fizz, buzz, scaled, one;  

    scaled =  (double)(10.0);
    scaled =  (double)(1.0) / scaled;
    fuzz =    (double)(0.0012345);
    buzz =    (double)(1.0) + fuzz;
    fizz =    (double)(1.1) * fuzz;
    one =     (double)(1.0);

    for ( k=0 ; k<19977 + 34132 ; k++)
    {
        if (k == 19977)
        {
            fuzz = (double)(0.0012345);
            buzz = (double) (1.0) + fuzz;
            fizz = (double) (1.1) * fuzz;
        }         
        buzz = (one - fuzz) * buzz + fuzz;
        fuzz = - fuzz;
        u[k] = (buzz - fizz) * scaled;
    }

    fuzz = (double)(0.0012345);
    buzz = (double) (1.0) + fuzz;
    fizz = (double) (1.1) * fuzz;

    for ( k=1 ; k<40 ; k++)
    {
        buzz = (one - fuzz) * buzz + fuzz;
        fuzz = - fuzz;
        xtra[k] = (buzz - fizz) * scaled;
    }

    ds = 1.0;
    dw = 0.5;
    for ( l=0 ; l<4 ; l++ )   
    {
        for ( i=0 ; i<512 ; i++ )
        {
            p[i][l] = ds;
            ds = ds + dw;
        }
    }
    for ( i=0 ; i<96 ; i++ )
    {
        e[i] = 1;
        f[i] = 1;
    }    


    iqranf();
    dw = -100.0;
    for ( i=0; i<1001 ; i++ )
    {
        dex[i] = dw * dex[i];
        grd[i] = ix[i];
    }     
    flx = 0.001;


    d[0]= 1.01980486428764;
    nn = n16; //n16 =  nloops[section][16];


    for ( l=1 ; l<300 ; l++ )
    {
        d[l] = d[l-1] + 1.000e-4 / d[l-1];
    }
    rr = d[nn-1];
    for ( l=1 ; l<=2 ; l++ )
    {
        m = (nn+nn)*(l-1);
        for ( j=1 ; j<=2 ; j++ )
        {
            for ( k=1 ; k<=nn ; k++ )
            {
                m = m + 1;
                ss = (double)(k);
                plan[m-1] = rr * ((ss + 1.0) / ss);
                zone[m-1] = k + k;
            }
        }
    }
    k = nn + nn + 1;
    zone[k-1] = nn;

    if (which == 16)
    {
        r = d[n-1];
        s = d[n-2];
        t = d[n-3];
        k3 = k2 = 0;
    }
    expmax = 20.0;
    if (which == 22)
    {
        u[n-1] = 0.99*expmax*v[n-1];
    }
    if (which == 24)
    {
        x[n/2] = -1.0e+10;
    }
}

void iqranf()
{

    long   inset, Mmin, Mmax, nn, i, kk;
    double span, spin, realn, per, scale1, qq, dkk, dp, dq;
    long seed = 256; //long   seed[3] = { 256, 12491249, 1499352848 };

    nn = 1001;
    Mmin = 1;
    Mmax = 1001;
    kk = seed;

    inset= Mmin;
    span= Mmax - Mmin;
    spin= 16807;
    per= 2147483647;
    realn= nn;
    scale1= 1.00001;
    qq= scale1 * (span / realn);
    dkk= kk;

    for ( i=0 ; i<nn ; i++)
    { 
        dp= dkk*spin;
        dkk= dp - (long)( dp/per)*per;
        dq= dkk*span;
        ix[i] = inset + ( dq/ per);
        if (ix[i] < Mmin | ix[i] > Mmax)
        {
            ix[i] = inset + i + 1 * qq;
        }
    }

    return;         
}

