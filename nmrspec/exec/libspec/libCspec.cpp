/*
  ID libCspec.cpp
  Copyright (C) 2015-2021 INRAE
  Authors: D. Jacob
*/
 
// See https://teuder.github.io/rcpp4everyone_en/
// https://knausb.github.io/2017/08/header-files-in-rcpp/

#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <string>
#include <math.h>
#include <float.h>

// [[Rcpp::plugins(openmp)]]

using namespace std;
using namespace Rcpp;

class data_info {
public:
    double  pmin;
    double  pmax;
    int size_c;
    int size_l;
};

double _abs(double x)
{
   if (x<0.0) x=-x;
   return x;
}

int indMaxC(SEXP x, int start, int end)
{
   NumericVector v(x);
   int n1 = (start>0) ? start : 0;
   int n2 = (end<v.size()) ? end : v.size();
   int indx = n1;
   for(int i = n1; i <= n2; i++) if (v[i]>v[indx]) indx=i;
   return indx;
}

int indMinC(SEXP x, int start, int end)
{
   NumericVector v(x);
   int n1 = (start>0) ? start : 0;
   int n2 = (end<v.size()) ? end : v.size();
   int indx = n1;
   for(int i = n1; i <= n2; i++) if (v[i]<v[indx]) indx=i;
   return indx;
}

double maxC(SEXP x, int start, int end)
{
   NumericVector v(x);
   return v[indMaxC(x, start, end)];
}
double minC(SEXP x, int start, int end)
{
   NumericVector v(x);
   return v[indMinC(x, start, end)];
}

// [[Rcpp::export]]
SEXP SDL(SEXP x, double Sigma)
{
   NumericVector X(x);
   int N = X.size();
   NumericVector Out(N);
   double v1, v2;
   for(int n = 0; n<N; n++)
   {
       v1 = X[n]*X[n]; v2 = Sigma*Sigma; 
       Out[n] = (12.0*v1-v2)/pow(4.0*v1+v2, 3);
   }
   return Out;
}

// ---------------------------------------------------
//  Read / Write the Matrix of spectra wihtin a binary file
// ---------------------------------------------------

// [[Rcpp::export]]
void C_write_pack (SEXP x, double pmin, double pmax, SEXP ff)
{
   // Matrix of spectra : 1 row = 1 spectrum, 1 column = a same value of ppm
   NumericMatrix xx(x);

   std::string fname = as<std::string>(ff); 

   // Header table
   data_info* inforec = new data_info();
   inforec->pmax=pmax;
   inforec->pmin=pmin;
   inforec->size_l=xx.ncol();
   inforec->size_c=xx.nrow()+2;

   // Open the file as binary mode
   std::ofstream outBinFile;
   outBinFile.open(fname.c_str(), std::ios::out | std::ios::binary);

   // write the header table
   outBinFile.write( (char *)inforec, sizeof(data_info) );

   // Buffer
   double* buf = new double[inforec->size_c];

   // write data by column
   buf[0] = buf[xx.nrow()+1] = 0.0;
   for(int i = 0; i<xx.ncol(); i++) {
      for(int k = 1; k<=xx.nrow(); k++) buf[k] = xx(k-1, i);
      outBinFile.write( (char *)buf, (unsigned)(inforec->size_c)*sizeof(double) );
   }

   outBinFile.flush();
   outBinFile.close(); 
}

// [[Rcpp::export]]
SEXP C_read_pack (SEXP ff)
{
   string fname = as<string>(ff); 

   // Header table
   data_info* inforec = new data_info();
   
   // Open the file as binary mode
   ifstream inBinFile;
   inBinFile.open(fname.c_str(), ios::in | ios::binary);
   inBinFile.seekg (0, ios::beg);
   
   // read the header table
   inBinFile.read( (char *)inforec, sizeof(data_info) );
   double pmax = inforec->pmax;
   double pmin = inforec->pmin;
   int ncol = (unsigned)(inforec->size_l);
   int nrow = (unsigned)(inforec->size_c)-2;

   // Matrix of spectra : 1 row = 1 spectrum, 1 column = a same value of ppm
   NumericMatrix M(nrow, ncol);

   // Buffer
   double* buf = new double[inforec->size_c];

   // Read data by column
   buf[0] = buf[nrow+1] = 0.0;
   for(int i = 0; i<ncol; i++) {
       inBinFile.read( (char *)buf, (unsigned)(inforec->size_c)*sizeof(double) );
       for(int k = 1; k<=nrow; k++) M(k-1, i) = buf[k];
   }

   inBinFile.close(); 

   return Rcpp::List::create(_["int"] = M,
                             _["nspec"] = nrow,
                             _["size"] = ncol,
                             _["ppm_min"] = pmin,
                             _["ppm_max"] = pmax );

}

// ---------------------------------------------------
//  Baseline Correction Routines
// ---------------------------------------------------

// [[Rcpp::export]]
SEXP C_GlobSeg (SEXP v, int dN, double sig)
{
    NumericVector specR(v);
    int N = specR.size();
    int n1, n2, i, k, count;
    double a,Vmin;
    NumericVector S(N);

    S[0]=specR[0];
    S[N-1]=specR[N-1];

    if (S[0]>S[N-1]) {
        count=0;
        while (count<N) {
           n1=count;
           n2=N-1;
           Vmin=DBL_MAX;
           for (i=(n1+1); i<N; i++)
               if (specR[i]<Vmin) { Vmin=specR[i]; n2=i; }
           a=(specR[n2]-specR[n1])/(n2-n1);
           k=(n1+n2)/2;
           if ( (specR[k] - a*(k-n1))>sig && n1>2*dN) {
              n1 -= dN;
              a=(specR[n2]-specR[n1])/(n2-n1);
           }
           for (k=n1; k<=n2; k++) S[k]=a*(k-n1)+specR[n1];
           count=n2;
           if (count==(N-1))
               count++;
        }
    }
     else {
        count=N-1;
        while (count>0) {
           n2=count;
           n1=0;
           Vmin=DBL_MAX;
           for (i=(n2-1); i>0; i--)
               if (specR[i]<Vmin) { Vmin=specR[i]; n1=i; }
           a=(specR[n2]-specR[n1])/(n2-n1);
           k=(n1+n2)/2;
           if ( (specR[k] - a*(n2-k))>sig && n2<(N-2*dN)) {
              n2 += dN;
              a=(specR[n2]-specR[n1])/(n2-n1);
           }
           for (k=n1; k<=n2; k++) S[k]=a*(k-n1)+specR[n1];
           count=n1;
           if (count==1)
               count--;
        }
    }

    return S;
}

// [[Rcpp::export]]
SEXP lowpass1 (SEXP x, double alpha)
{
   int k,N;
   NumericVector VecIn(x);
   N = VecIn.size();
   NumericVector VecOut(N);
   VecOut[0]=VecIn[0];
   for (k=1; k<N; k++) VecOut[k] = VecOut[k-1] + alpha * (VecIn[k] - VecOut[k-1]);
   return(VecOut);
}

// [[Rcpp::export]]
double WinMoy (SEXP v, int n1, int n2)
{
    NumericVector specR(v);
    int k;
    double  moy=0.0;
    for (k=n1; k<=n2; k++) moy += specR[k];
    moy /= (double)(n2-n1+1);
    return moy;
}

// [[Rcpp::export]]
SEXP Smooth (SEXP v, int n)
{
    NumericVector V(v);
    int N = V.size();
    NumericVector S(N);
    
    double Wk=V[0];
    S[0]=V[0];
    for (int k=1; k<(N-1); k++) {
        if (k<=n)              { Wk += (V[2*k]   + V[2*k-1]);    S[k] = Wk/(2*k+1);     }
        if (k>n && k<=(N-n-1)) { Wk += (V[k+n]   - V[k-n-1]);    S[k] = Wk/(2*n+1);     }
        if (k>(N-n-1))         { Wk -= (V[2*k-N] - V[2*k-N-1]);  S[k] = Wk/(2*(N-k)+1); }
    }
    S[N-1]=V[N-1];
    return S;
}

// [[Rcpp::export]]
void fitLines (SEXP s, SEXP b, int n1, int n2)
{
    NumericVector specR(s), lb(b);
    int k,ni;
    double  a,diff,diff_max,lb_line;

    a=(lb[n2]-lb[n1])/(n2-n1);
    diff_max=0.0; ni=n1;
    for (k=n1; k<n2; k++) {
        lb_line = a*(k-n1)+lb[n1];
        diff = specR[k]< lb_line ? lb_line - specR[k] : 0.0 ;
        if (diff>diff_max) { diff_max=diff; ni=k; }
    }
    if (ni>n1 && ni<n2) {
        a=(specR[ni]-lb[n1])/(ni-n1);
        for (k=n1+1; k<=ni; k++)
            lb[k]=a*(k-n1)+lb[n1];
        fitLines(specR,lb,ni,n2);
    }
    else
        for (k=n1; k<n2; k++)
            lb[k]=a*(k-n1)+lb[n1];
}

// [[Rcpp::export]]
SEXP C_Estime_LB (SEXP s, int istart, int iend, double WS, double NEIGH, double sig)
{
   NumericVector specR(s);
   int count,n1,n2,k,cnt;
   int TD = specR.size();
   int N = round(log2(TD));
   int ws = N>15 ? 2 : 1;
   int edgesize=10;

   // Create the BL vector initialize with spectrum values
   NumericVector lb(TD), m1(TD), m2(TD);

   // (s1,neigh) = (50,35) => soft, (25,15) => intermediate, (10,5) => hard

   m1=Smooth(specR,WS*ws);
   m2=Smooth(specR,4*ws);

   cnt=n1=n2=0;
   for (count=0; count<TD; count++) {
        if (count<istart || count>iend ) {
            lb[count]=0;  n1=count; n2=count; cnt=0; continue;
        }
        if (count<(istart+edgesize)) {
            lb[count]=m1[count];  n1=count; n2=count; cnt=0; continue;
        }
        if ((_abs(m2[count] - m1[count])) <= sig) {
            if (cnt==0) n2=count;
            cnt++;
        }
        else {
            if (cnt<NEIGH*ws) { cnt=0; continue; }
            for (k=n2; k<count; k++) lb[k] = m1[k];
            if (n1<n2) {
                fitLines(specR,lb,n1,n2);
            }
            n1=count-1;
            cnt=0;
        }
   }
   if (cnt>0) for (k=n2; k<count; k++) lb[k] = m1[k];
   if (n1<n2) fitLines(specR,lb,n1,iend-1);
   return(lb);
}

// [[Rcpp::export]]
SEXP C_Estime_LB2 (SEXP s, int istart, int iend, double WS, double NEIGH, double sig)
{
   NumericVector specR(s);
   int count,n1,n2,k,cnt;
   int TD = specR.size();
   int N = round(log2(TD));
   int ws = N>15 ? 2 : 1;

   // Create the lb vector initialize with spectrum values
   NumericVector lb(TD), m1(TD), m2(TD);

   m1=Smooth(specR,WS*ws);
   m2=Smooth(specR,4*ws);

   cnt=n1=n2=0;
   for (count=0; count<TD; count++) {
        if (count<istart || count>iend ) {
            lb[count]=m1[count];  n1=count; n2=count; cnt=0; continue;
        }
        if ((_abs(m2[count] - m1[count])) <= sig) {
            if (cnt==0) n2=count;
            cnt++;
        }
        else {
            if (cnt<NEIGH*ws) { cnt=0; continue; }
            for (k=n2; k<count; k++) lb[k] = m1[k];
            if (n1<n2) {
               double a=(lb[n2]-lb[n1])/(n2-n1);  for (k=n1; k<n2; k++) lb[k]=a*(k-n1)+lb[n1];
            }
            n1=count-1;
            cnt=0;
        }
   }
   if (cnt>0) for (k=n2; k<TD; k++) lb[k] = m1[k];
   if (n1<n2) {
//       n2=iend-1;
       double a=(lb[n2]-lb[n1])/(n2-n1);  for (k=n1; k<n2; k++) lb[k]=a*(k-n1)+lb[n1];
   }
   return(lb);
}

// ---------------------------------------------------
//  Noise estimation (cf. Bruker command 'sino' - TopSpin 3.0)
// ---------------------------------------------------

// [[Rcpp::export]]
SEXP C_noise_estimate (SEXP x, int n1, int n2, int flg)
{
   NumericMatrix VV(x);
   int n_specs = VV.nrow();
   int size_m = n2-n1+1;
   int size_half = size_m/2;
   int count,k, i1,i2;
   double SQ, Som, SD;

   // Create the Noise vector
   NumericVector Vnoise(n_specs);

   // for each spectrum
   for (k=0; k<n_specs; k++) {
       SQ=Som=0.0;
       for(count=n1; count<=n2; count++) {
           SQ += VV(k,count)*VV(k,count);
           Som += VV(k,count);
       }
       Som = _abs(Som);
       if (flg==0) {
           Vnoise[k] =  sqrt( (SQ - Som*Som/_abs(size_m))/_abs(size_m-1) );
       } 
       else {
           SD=0.0;
           for(count=0; count<size_half; count++) {
              i1 = n1 + size_half + count;
              i2 = n1 + size_half - count - 1 ;
              SD += (count+1)*( VV(k,i1) - VV(k,i2) );
           }
           SD = _abs(SD);
           Vnoise[k] =  sqrt(( SQ - ( Som*Som + 3*SD*SD/_abs(size_m*size_m-1) )/_abs(size_m) )/_abs(size_m-1) );
       }
   }

   return(Vnoise);
}

// ---------------------------------------------------
//  Reference Spectrun
// ---------------------------------------------------

// [[Rcpp::export]]
SEXP C_spec_ref_interval (SEXP x, int istart, int iend, IntegerVector v)
{
   NumericMatrix VV(x);
   int n_specs = VV.nrow();
   int size_m = iend-istart+1;
   int count,k;
   int bounds = v.length()>0 ? v.length() : n_specs ;

   NumericVector vref(size_m);

   for(count=0; count<size_m; count++) {
        vref[count]=0.0;
        for (k=0; k<bounds; k++) {
            vref[count] += VV(v.length()>0 ? v[k] : k, istart+count);
        }
   }
   for (count=0; count<size_m; count++) vref[count] /= (double)(bounds);

   return(vref);
}

// [[Rcpp::export]]
SEXP C_spec_ref (SEXP x, IntegerVector v)
{
   NumericMatrix VV(x);
   int count_max = VV.ncol();
   NumericVector vref = C_spec_ref_interval (x, 0, count_max-1, v);
   return(vref);
}

// [[Rcpp::export]]
SEXP C_MedianSpec(SEXP x)
{
   NumericMatrix VV(x);
   int n_specs = VV.nrow();
   int count_max = VV.ncol();
   int position = n_specs / 2; // Euclidian division
   NumericVector out(count_max);
   for (int j = 0; j < count_max; j++) { 
        NumericVector y = VV(_,j); // Copy column -- original will not be mod
        std::nth_element(y.begin(), y.begin() + position, y.end()); 
        out[j] = y[position];  
   }
   return out;
}

// ---------------------------------------------------
//  Alignment Algorithms
// ---------------------------------------------------

/* Calcul de la derivee (ordre 4 centree) */
// [[Rcpp::export]]
SEXP C_Derive1 (SEXP v)
{
   NumericVector specR(v);
   int count_max = specR.size();
   int count;

   NumericVector D(count_max);

   for (count=0; count<count_max; count++) D[count]=0.0;
//   D[1]=specR[1]-specR[0]; D[0]=specR[1];
//   for (count=2; count<count_max-2; count++)
//       D[count] = (specR[count-2]-8*specR[count-1]+8*specR[count+1]-specR[count+2])/12;
   for (count=5; count<count_max-5; count++)
       D[count] = (42*(specR[count+1]-specR[count-1]) + 
                   48*(specR[count+2]-specR[count-2]) + 
                   27*(specR[count+3]-specR[count-3]) +
                    8*(specR[count+4]-specR[count-4]) +
                       specR[count+5]-specR[count-5] )/512;
   return (D);
}

// [[Rcpp::export]]
SEXP C_Derive (SEXP x)
{
   NumericMatrix VV(x);
   int n_specs = VV.nrow();
   int count_max = VV.ncol();
   int k, count;

   NumericMatrix M(n_specs, count_max);

   for (k=0; k<n_specs; k++) {
       for (count=0; count<count_max; count++) M(k,count)=0.0;
       M(k,1)=VV(k,1)-VV(k,0); M(k,0)=VV(k,1);
       for (count=2; count<count_max-2; count++)
            M(k,count)=(VV(k,count-2)-8*VV(k,count-1)+8*VV(k,count+1)-VV(k,count+2))/12;
   }
   return (M);
}

/* Calcul de l'integrale */
// [[Rcpp::export]]
SEXP C_Integre (SEXP x, int istart, int iend)
{
   NumericMatrix VV(x);
   int n_specs = VV.nrow();
   int count_max = VV.ncol();
   int k, count;

   NumericMatrix M(n_specs, count_max);

   for (k=0; k<n_specs; k++) {
       for (count=0; count<count_max; count++) M(k,count)=0.0;
       for (count=istart; count<iend-2; count++)
            M(k,count)= 0.5*( VV(k,count) + VV(k,count+1) );
   }
   return (M);
}

/* Apodisation par "sigmoides symétriques" aux extremites de la zone d'alignement */
void _apodize (SEXP x, int k, int n)
{
   NumericMatrix VV(x);
   double lambda=2.0;
   int N=4;
   int i;
   for (i=n-N; i<n; i++) VV(k, i) *= 1.0/(1.0+exp(-lambda*(n-N/2-i)));
   VV(k, n)=0.0;
   for (i=n+1; i<(n+N); i++) VV(k, i) *= 1.0/(1.0+exp(-lambda*(i-n-N/2)));
}

int find_optim_decal( SEXP x, SEXP y, int decal )
{
   NumericVector vref(x);
   NumericVector vk(y);
   int size_m = vref.size();
   int i,j, ij, optim_decal;
   double min_sse, sse;

   /* Recherche le shift qui minimise la Somme des Erreurs Quadratiques (SSE) */
   min_sse=DBL_MAX;
   optim_decal=0;
   for (j=-decal; j<=decal; j++) {
       sse=0.0;
       for (i=0; i<size_m; i++) {
           ij = i + j;
           if ( ij<size_m && ij>=0 )
              sse +=  10.0*(vref[i]-vk[ij])*(vref[i]-vk[ij]);
           else 
              sse +=  10.0*vref[i]*vref[i];
       }
       if (sse<min_sse) { optim_decal=j; min_sse=sse; }
   }
   return(optim_decal);
}


// [[Rcpp::export]]
SEXP C_segment_shifts (SEXP x, int idx_vref, int decal_max, int istart, int iend, IntegerVector v)
{
   NumericMatrix VV(x);
   int n_specs = VV.nrow();
   int size_m = iend-istart+1;
   int i, k, decal, idx;
   double somref, somk;
   int bounds = v.length()>0 ? v.length() : n_specs ;

   /* Spectre de reference Vref */
   NumericVector vref(size_m);

   NumericVector vk(size_m);
   NumericVector shift_v(n_specs);

   if (idx_vref==0) {
       vref = C_spec_ref_interval (x, istart, iend, v);
   } else {
       for (i=0; i<size_m; i++) vref[i]=VV(idx_vref-1,i);
   }
   for (k=0; k<n_specs; k++) shift_v[k]=0;

   somref=0.0;
   for (i=0; i<size_m; i++) somref +=  vref[i];
   for (i=0; i<size_m; i++) vref[i] = 100.0*vref[i]/somref;

   /* Taille de la fenetre de glissement entre les deux massifs Vref et Vk */
   decal= (int)(size_m/3);
   if (decal_max>0 && decal_max<size_m) decal = decal_max;

   /* Pour chaque spectre */
   for (k=0; k<bounds; k++) {

       /* init */
       idx = v.length()>0 ? v[k] : k ;
       shift_v[idx]=0;

       /* Segment du spectre Vk a aligner */
       somk=0.0;
       for (i=0; i<size_m; i++) somk +=  VV(idx, istart+i);
       if (somk==0.0) continue;
       for (i=0; i<size_m; i++) vk[i] = 100.0*(VV(idx, istart+i)/somk);

       /* Recherche le shift qui minimise la Somme des Erreurs Quadratiques (SSE) */
       shift_v[idx] = find_optim_decal(vref,vk,decal);

   }
   return(shift_v);
}

// [[Rcpp::export]]
int C_align_segment (SEXP x, SEXP s, int istart, int iend, int apodize, IntegerVector v)
{
   NumericMatrix VV(x);
   NumericVector shift_v(s);

   int n_specs = VV.nrow();
   int size_m = iend-istart+1;
   int i, k, ij, delta, moy_shift, idx;
   int bounds = v.length()>0 ? v.length() : n_specs ;

   NumericVector vk(size_m);

   /* Calcul le shift moyen */
   moy_shift = 0;
   for (k=0; k<bounds; k++) moy_shift += shift_v[v.length()>0 ? v[k] : k];
   moy_shift = (int)(moy_shift/bounds);

   /* Translate les massifs */
   for (k=0; k<bounds; k++) {
       idx = v.length()>0 ? v[k] : k ;
       delta = shift_v[idx]-moy_shift;
       delta = shift_v[idx];
       if (delta==0) continue;
       for (i=0; i<size_m; i++) {
           //vk[i]=0.0;
           ij=i+delta;
           if ( ij<0 )
               vk[i]=VV(idx, istart);
           else if ( ij>=size_m )
               vk[i]=VV(idx, iend);
           else if ( ij>=0 && ij<size_m )
               vk[i]=VV(idx, istart+ij);
       }
       for (i=0; i<size_m; i++) VV(idx,istart+i)=vk[i];
       if (apodize>0) {
          _apodize(x, idx,istart);
          _apodize(x, idx,iend);
       }
   }
   return(moy_shift);
}


// ---------------------------------------------------
//  Binning Algorithms
// ---------------------------------------------------

struct BinData {
   int VREF;
   int n_buckets;
   int inoise_start;
   int inoise_end;
   double delta_ppm;
   double ppm_max;
   double R;
   double ynoise;
   double vnoise;
   double noise_fac;
   double bin_fac;
   double peaknoise_rate;
   double BUCMIN;
   double DELTAPPM;
};

struct ErvaData {
   int n_buckets;
   double bucketsize;
   double delta_ppm;
   double ppm_min;
   double R;
   double THRESBUC;
   double BUCMIN;
   double noise_fac;
};

// [[Rcpp::export]]
double C_noise_estimation(SEXP x, int n1, int n2)
{
   NumericVector V(x);
   double  ym, sum_y, sum_y2, y_noise;
   int i;
   sum_y=sum_y2=0.0;
   for (i=n1; i<n2; i++) {
       sum_y2 += V[i]*V[i];
       sum_y  += V[i];
   }
   ym=_abs(sum_y);
   y_noise = sqrt(( sum_y2 - ym*ym/_abs(n2-n1) )/_abs(n2-n1-1));
   return y_noise;
}

/*-------- Bin Evaluation Criterion (BEC)----------------------------------*/
double bin_value(SEXP x, SEXP v, struct BinData *bdata, int n1, int n2)
{
   NumericMatrix VV(x);
   NumericVector vref(v);
   int n_specs = VV.nrow();
   int i,k;
   double Amax=0.0;
   double vb=0.0;

   if (bdata->VREF==1) {
       for(i=n1; i<=n2; i++) if (vref[i]>Amax) Amax=vref[i];
       vb += pow((Amax-vref[n1])*(Amax-vref[n2]),bdata->R);
   } else {
        for (k=0; k<n_specs; k++) {
            Amax=0.0;
            for(i=n1; i<=n2; i++) if (VV(k,i)>Amax) Amax=VV(k,i);
            vb += pow((Amax-VV(k,n1))*(Amax-VV(k,n2)),bdata->R);
        }
        vb /= n_specs;
   }
   return vb;
}

void save_bucket(SEXP b, SEXP v, struct BinData *bdata, int n1, int n2)
{
   NumericMatrix buckets(b);
   NumericVector vref(v);
   int i, flg;
   while (vref[n1]==0.0) n1++;
   while (vref[n2]==0.0) n2--;
   flg=0;
   for(i=n1; i<=n2; i++)
      if (vref[i]>bdata->peaknoise_rate*bdata->ynoise) { flg=1; break; }
   if (flg==0) return;
   if (C_noise_estimation(vref, n1, n2) < bdata->noise_fac*bdata->ynoise) return;
   if ( ( _abs(n1-n2)*bdata->delta_ppm )<bdata->BUCMIN ) return;
   if ( ( _abs(n1-n2)*bdata->delta_ppm )>1 ) return;
   //Rprintf("\tsave bucket [%03d]: range %d - %d\n",bdata->n_buckets+1, n1, n2);
   buckets(bdata->n_buckets,0)=n1;
   buckets(bdata->n_buckets,1)=n2;
   bdata->n_buckets++;
}

int find_aibin_buckets(SEXP x, SEXP b, SEXP v, struct BinData *bdata, int n1, int n2)
{
   NumericMatrix buckets(b);
   NumericVector vref(v);
   int count,ncut,nmin;
   double vb1,vb2,vbsum, vbmax;
   vbmax=bin_value(x, vref,bdata,n1,n2);
   nmin=(int)(bdata->BUCMIN/bdata->delta_ppm);
   ncut=0;
   for(count=(n1+nmin); count<(n2-nmin); count++) {
        vb1=bin_value(x, vref,bdata,n1,count);
        vb2=bin_value(x, vref,bdata,count,n2);
        vbsum=vb1+vb2;
        if (vbsum>vbmax && vb1>bdata->vnoise && vb2>bdata->vnoise) {
            vbmax=vbsum;
            ncut=count;
        }
   }
   //Rprintf("find bucket: range %d - %d, vbmax=%f, nmin=%d, ncut=%d\n",n1,n2, vbmax, nmin, ncut);
   if (ncut>0) {
        if (find_aibin_buckets(x, buckets,vref,bdata,n1,ncut)==0) save_bucket(buckets,vref,bdata,n1,ncut);
        if (find_aibin_buckets(x, buckets,vref,bdata,ncut,n2)==0) save_bucket(buckets,vref,bdata,ncut,n2);
   }
   return ncut;
}

// [[Rcpp::export]]
SEXP C_aibin_buckets(SEXP x, SEXP b, SEXP v, SEXP l, int n1, int n2)
{
   NumericMatrix buckets(b);
   NumericVector vref(v);
   List blist(l);
   struct BinData bdata;
   int i;

   bdata.n_buckets=0;
   bdata.VREF = as<int>(blist["VREF"]);
   bdata.R = as<double>(blist["R"]);
   bdata.noise_fac = as<double>(blist["noise_fac"]);
   bdata.bin_fac = as<double>(blist["bin_fac"]);
   bdata.peaknoise_rate = as<double>(blist["peaknoise_rate"]);
   bdata.BUCMIN = as<double>(blist["BUCMIN"]);
   bdata.delta_ppm = as<double>(blist["dppm"]);
   bdata.ynoise = as<double>(blist["ynoise"]);
   bdata.inoise_start = as<int>(blist["inoise_start"]);
   bdata.inoise_end = as<int>(blist["inoise_end"]);
   bdata.vnoise = bdata.bin_fac*bin_value(x, vref, &bdata, bdata.inoise_start, bdata.inoise_end);
   // Rprintf("AIBIN: range %d - %d, vnoise=%f\n",n1,n2, bdata.vnoise);

   find_aibin_buckets(x, buckets,vref,&bdata,n1-1,n2-1);
   // Rprintf("Returned Value = %d, number of buckets found = %d\n",ret, bdata.n_buckets);
   if (bdata.n_buckets==0) return R_NilValue;

   NumericMatrix M(bdata.n_buckets, 2);
   for (i=0; i<bdata.n_buckets; i++) { M(i,0) = buckets(i,0)+1; M(i,1) = buckets(i,1)+1; }
   return M;
}

// ---------------------------------------------------
//  ERVA
// ---------------------------------------------------

// ---------------------------------------------------
//  Convolution with the second derivative of a Lorentzian function 
// ---------------------------------------------------
double func_lorentz(double x,double x0, double s) {  return s*s/(s*s+(x-x0)*(x-x0)); }

// Calculation of the derivative (order 4 centered)
void Derivation (double *v1, double *v2, int count_max)
{
    int    count;

    for (count=0; count<=count_max; count++) v2[count]=0.0;
    for (count=6; count<=count_max-5; count++)
        v2[count] = (42*(v1[count+1]-v1[count-1]) +
                     48*(v1[count+2]-v1[count-2]) +
                     27*(v1[count+3]-v1[count-3]) +
                      8*(v1[count+4]-v1[count-4]) +
                         v1[count+5]-v1[count-5] )/512;

}

// [[Rcpp::export]]
SEXP C_SDL_convolution (SEXP x, SEXP y, double sigma)
{
   NumericVector X(x);
   NumericVector Y(y);
   int  n=X.size();
   NumericVector V(n);
   int ltzwin=500;
   int n1,n2,k,count;
   for (count=0; count<n; count++) {
        V[count]=0;
        n1 = count<ltzwin ? 0 : count-ltzwin;
        n2 = count>(n-ltzwin-1) ? n-1 : count+ltzwin;
        for (k=n1; k<=n2; k++) {
            V[count] += Y[k]*func_lorentz(X[k], X[count], sigma);
        }
   }
   for (k=0; k<100; k++) { V[k]=0.0; V[n-k-1]=0.0; }
   V = C_Derive1(V);
   V = C_Derive1(V);
   return(V);
}

int find_erva_buckets(SEXP x, SEXP b, SEXP v, struct ErvaData *edata, int n1, int n2)
{
   NumericMatrix VV(x);
   NumericMatrix buckets(b);
   NumericVector vref(v);

   int i1, i2, k, count, ltzwin, flg;
   int n_points = VV.ncol();

   double ppm, ppm0, f2buc;
   double sigma = edata->bucketsize;
   double dppm=edata->BUCMIN;
   double max_vref=0.0;

   double *v2=(double *) malloc((unsigned) (n_points+1)*sizeof(double));
   double *v1=(double *) malloc((unsigned) (n_points+1)*sizeof(double));

   ltzwin=1000;
   for (count=1; count<=n_points; count++) {
       ppm0 = edata->ppm_min + (count-1)*edata->delta_ppm;
       v1[count]=0;
       i1 = count<ltzwin ? 1 : count-ltzwin+1;
       i2 = count>(n_points-ltzwin) ? n_points : count+ltzwin;
       for (k=i1; k<=i2; k++) {
           ppm = edata->ppm_min + (k-1)*edata->delta_ppm;
           v1[count] += 100000.0*vref[k-1]*func_lorentz(ppm, ppm0, sigma);
       }
   }
   Derivation(v1,v2,n_points);
   Derivation(v2,v1,n_points);

   edata->n_buckets=0;
   count = n1;
   flg=0;
   while(count<n2) {
       count++;
       if (v1[count+1]<0.0)
           f2buc=-1.0*v1[count+1];
       else
           f2buc=0.0;
       if (flg==0 && f2buc>0.0) {
           buckets(edata->n_buckets,0)=count-(int)(dppm/edata->delta_ppm);
           max_vref=0.0;
           flg=1;
           continue;
       }
       if (flg==1 && f2buc>0.0) {
           if (vref[count]>max_vref) max_vref=vref[count];
           continue;
       }
       if (flg==1 && f2buc==0.0) {
           buckets(edata->n_buckets,1)=count+(int)(dppm/edata->delta_ppm);
           if ((buckets(edata->n_buckets,1)-buckets(edata->n_buckets,0))*edata->delta_ppm>=edata->noise_fac*dppm)
               edata->n_buckets++;
           flg=0;
       }
   }

   free(v1);
   free(v2);

   return edata->n_buckets;
}

// [[Rcpp::export]]
SEXP C_erva_buckets(SEXP x, SEXP b, SEXP v, SEXP l, int n1, int n2)
{
   NumericMatrix buckets(b);
   NumericVector vref(v);
   List blist(l);
   struct ErvaData edata;
   int i;

   edata.n_buckets=0;
   edata.bucketsize = as<double>(blist["bucketsize"]);
   edata.BUCMIN = as<double>(blist["BUCMIN"]); // 0.001
   edata.noise_fac = as<double>(blist["noise_fac"]); // 3
   edata.delta_ppm = as<double>(blist["dppm"]);
   edata.ppm_min = as<double>(blist["ppm_min"]);

   find_erva_buckets(x, buckets,vref,&edata,n1-1,n2-1);
   // Rprintf("Returned Value = %d, number of buckets found = %d\n",ret, edata.n_buckets);
   if (edata.n_buckets==0) return R_NilValue;

   NumericMatrix M(edata.n_buckets, 2);
   for (i=0; i<edata.n_buckets; i++) {
       M(i,0) = buckets(i,0)+1; M(i,1) = buckets(i,1)+1;
   }
   return M;
}


// [[Rcpp::export]]
SEXP C_spectra_integrate (SEXP x, int istart, int iend)
{
   NumericMatrix VV(x);
   int n_specs = VV.nrow();
   int i,k;

   //Vector resulting of the integration : 1 cell = 1 spectrum
   NumericVector Vint(n_specs);

   // for each spectrum
   for (k=0; k<n_specs; k++) {
       Vint[k] = 0.0;
       for (i=istart; i<iend; i++) Vint[k] += 0.5*( VV(k,i) + VV(k,i+1) );
   }

   return(Vint);
}

// [[Rcpp::export]]
SEXP C_buckets_integrate (SEXP x, SEXP b, int mode)
{
   NumericVector V(x);
   NumericMatrix Buc(b);
   int n_bucs = Buc.nrow();
   int i, m;

   //Vector resulting of the integration : 1 cell = 1 bucket
   NumericVector Vbuc(n_bucs);

   // for each bucket
   for (m=0; m<n_bucs; m++) {
       // for each point
       Vbuc[m] = 0.0;
       for (i=(Buc(m,0)-1); i<(Buc(m,1)-1); i++) Vbuc[m] += 0.5*( V[i] + V[i+1] );
       if (mode == -1) Vbuc[m] /= ( Buc(m,1) - Buc(m,0) + 1 );
       if (mode ==  1) Vbuc[m] *= ( Buc(m,1) - Buc(m,0) + 1 );
   }

   return(Vbuc);
}


// [[Rcpp::export]]
SEXP C_all_buckets_integrate (SEXP x, SEXP b, int mode)
{
   NumericMatrix VV(x);
   NumericMatrix Buc(b);
   int n_specs = VV.nrow();
   int n_bucs = Buc.nrow();
   int k;

   //Matrix of the Buckets' integration : 1 row = 1 spectrum, 1 column = 1 bucket
   NumericMatrix M(n_specs, n_bucs);

   // for each spectrum
   for (k=0; k<n_specs; k++) {
        NumericVector Y = VV(k,_);
        NumericVector Z = C_buckets_integrate( Y, Buc, mode );
        M(k,_) = Z;
   }

   return(M);
}

// [[Rcpp::export]]
SEXP C_maxval_buckets (SEXP x, SEXP b)
{
   NumericMatrix VV(x);
   NumericMatrix Buc(b);
   int n_specs = VV.nrow();
   int n_bucs = Buc.nrow();
   int k,m;

   //Matrix of the Buckets' maxval : 1 row = 1 spectrum, 1 column = 1 bucket
   NumericMatrix M(n_specs, n_bucs);

   // for each spectrum
   for (k=0; k<n_specs; k++) {
        NumericVector Y = VV(k,_);

        // for each bucket
        for (m=0; m<n_bucs; m++)
           M(k,m) = maxC(Y, Buc(m,0), Buc(m,1));
   }
   return(M);
}

// [[Rcpp::export]]
SEXP C_ppmIntMax_buckets (SEXP x, SEXP b)
{
   NumericMatrix VV(x);
   NumericMatrix Buc(b);
   int n_specs = VV.nrow();
   int n_bucs = Buc.nrow();
   int k,m,i;
   double Y, sumS;

   //ppm values for each bucket of the maximum of the average intensity of all spectra 
   NumericVector P(n_bucs);

   // for each bucket
   for (m=0; m<n_bucs; m++) {
      Y=0;
   // for each point in the bucket range
      for (i=Buc(m,0); i<Buc(m,1); i++) {
          // for each spectrum
          sumS=0; for (k=0; k<n_specs; k++) sumS += VV(k,i);
          // Keep the ppm value if it the current intensity is over than previous.
          if (sumS>Y) { Y=sumS; P[m]=i; }
      }       
   }
   return(P);
}

// [[Rcpp::export]]
SEXP C_buckets_CSN_normalize (SEXP b)
{
   NumericMatrix buckets(b);
   int n_specs = buckets.nrow();
   int n_bucs = buckets.ncol();
   NumericMatrix M(n_specs, n_bucs);
   int k, m;
   double sumS;

   // for each spectrum
   for (k=0; k<n_specs; k++) {
       // for each bucket
       sumS=0.0;
       for (m=0; m<n_bucs; m++) sumS += buckets(k,m);
       for (m=0; m<n_bucs; m++) M(k,m) = 100000.0 * buckets(k,m)/sumS;
   }
   return(M);
}


// ---------------------------------------------------
//  Spectra pre-processing
// ---------------------------------------------------

// [[Rcpp::export]]
double C_estime_sd(SEXP x, int cut)
{
   NumericVector X(x);
   const size_t n = (size_t)(X.size());
   const size_t n2 = n/cut;
   size_t i,k;

   //double v[n2];
   NumericVector v(n2);
   double sdx, sdev;

   // Sdev estimation
   sdev=0;
   for (i=2; i<(size_t)(cut-1); ++i) {
       for (k=0; k<n2; ++k) v[k] = X[i*n2+k];
       sdx = Rcpp::sd(v);
       if (i==2 || sdx<sdev) sdev=sdx;
   }
   return sdev;
}

// ajustBL: (Yre <- x)
//   n <- round(length(Yre)/32)
//   sv <- simplify2array(lapply( c(3:29), function(x) { sd(Yre[((x-1)*n):(x*n)]); }))
//   mv <- simplify2array(lapply( c(3:29), function(x) { median(Yre[((x-1)*n):(x*n)]); }))
//   Yre <- Yre - mv[which.min(sv)]
//   if (flg==1) {
//      n <- round(length(Yre)/24)
//      Yre <- Yre[n:(23*n)]
//   }

// [[Rcpp::export]]
SEXP ajustBL (SEXP x, int flg) {

   NumericVector X(x);
   const size_t n = (size_t)(X.size());
   const size_t n2 = n/32;
   const size_t n3 = (size_t)(n/24);
   size_t i,k;

   NumericVector Y( X.size() );
   NumericVector m(n2);

   double mx, sdx, med, sdev;

   Rcpp::Environment base("package:stats"); 
   Rcpp::Function median_r = base["median"];

   sdx=0; mx=0;
   for (i=3; i<30; ++i) {
       for (k=0; k<n2; ++k) m[k] = X[i*n2+k];
       NumericVector res = median_r(m);
       med = res[0];
       sdev=Rcpp::sd(m);
       if (i==3 || sdev<sdx) { mx=med; sdx=sdev; }
   }
   for (i=0; i<n; ++i)
     if (flg==0 || i>n3 || i<(n-n3))
         Y[i] = X[i]-mx;
     else
         Y[i] = 0;

   return(Y);
}


// C_corr_spec_re( l=list(spec1r$re, spec1r$im, phc0, phc1) )
//   m <- length(spec1r)
//   Omega <- (0:(m-1))/m
//   phi <- phc0 + phc1*2*pi*Omega
//   Yrot <- spec1r * exp(complex(real=0,imaginary=phi)) # spectrum rotation

// [[Rcpp::export]]
SEXP C_corr_spec_re (SEXP l)
{
   List spec(l);
   NumericVector re = as<NumericVector>(spec["re"]);
   NumericVector im = as<NumericVector>(spec["im"]);
   double phc0 = as<double>(spec["phc0"]);
   double phc1 = as<double>(spec["phc1"]);
   int n = re.size();
   double phi;

   NumericVector S_re(n);
   NumericVector S_im(n);
   for (int i=0; i<n; i++)
   {
       //phi = phc0 + phc1*(i/n-0.5);
       phi = phc0 + phc1*i/n;
       S_re[i] = cos(phi)*re[i] - sin(phi)*im[i];
       S_im[i] = cos(phi)*im[i] + sin(phi)*re[i];

   }
   List new_spec = List::create(
      _["re"] = S_re,
      _["im"] = S_im 
   );

   return new_spec;
}

// [[Rcpp::export]]
double Fmin(SEXP par, SEXP re, SEXP im, int blphc, double B, int flg=0)
{
   NumericVector P(par);
   NumericVector Re(re);
   NumericVector Im(im);
   double phc0  = P[0];
   double phc1  = P[1];;
   const size_t n = (size_t)(Re.size());
   const size_t n2 = (size_t)(n/24);
   size_t i;
   double phi, Xmin, Xmax, SS;

   NumericVector X(n);
   for (i=0; i<n; i++) {
       //phi = phc0 + phc1*(i/n-0.5);
       phi = phc0 + phc1*i/n;
       X[i] = cos(phi)*Re[i] - sin(phi)*Im[i];
   }
   X=ajustBL (X, 0);

   NumericVector lb(n);
   if (blphc>0)
      lb=C_Estime_LB2(X, 1, n-1, blphc, blphc, B);

   Xmax=Xmin=0;
   for (i=n2; i<(n-n2); i++) {
      if (blphc==0 && i>n/2 && i<n) X[i]=0;
      if (blphc>0) X[i] -= lb[i];
      if (X[i]<Xmin) Xmin=X[i];
      if (X[i]>Xmax) Xmax=X[i];
   }

   SS=0;
   for (i=n2; i<(n-n2); i++) {
       if (flg==0) SS +=  X[i] < 0 ? pow(X[i]/Xmax,2) : 0;
       if (flg==1) SS +=  pow(X[i]/Xmax,2);
       if (flg==2) SS +=  X[i] < 0 ? pow((X[i]-Xmin)/Xmax,2) : pow(Xmin,2);
       if (flg==3) SS +=  pow(abs(X[i]/Xmax),0.5);
   }
   return(SS);
}

// [[Rcpp::export]]
double Fentropy(SEXP par, SEXP re, SEXP im, int blphc, int neigh, double B, double Gamma)
{
   NumericVector P(par);
   NumericVector Re(re);
   NumericVector Im(im);
   double phc0  = P[0];
   double phc1  = P[1];
   const size_t n = (size_t)(Re.size());
   size_t i;
   double phi, sumD, H1, Pfun, sumax, sumax2;

   // X = real( data * exp(1j * (phase0 + phase1 * x)) )
   NumericVector X(n);
   for (i=0; i<n; i++) {
       phi = phc0 + phc1*i/n;
       X[i] = cos(phi)*Re[i] - sin(phi)*Im[i];
   }
   
   // Ignore 1st N and last N points of the spectrum
   size_t N=1000;
   size_t n2=n-2*N;
   NumericVector X2(n2);
   for (i=0; i<n2; i++) X2[i] = X[i+N];

   // X <- X - baseline
   NumericVector lb(n2);
   X2=ajustBL (X2, 0);
   if (blphc>0) {
      lb=C_Estime_LB2(X2, 1, n2-1, blphc, neigh, B);
      for (i=0; i<n2; i++) X2[i] -= lb[i];
   }

   // D = abs((X[3:n] - X[1:n - 2]) / 2.0)
   NumericVector D(n2-2);
   sumD = 0.0;
   for (i=0; i<(n2-2); i++) { D[i] = _abs(X2[i+2]-X2[i])/2.0; sumD += D[i]; }

   // p1 = D / sum(D)
   // p1[where(p1 == 0)] = 1
   NumericVector p1(n2-2);
   for (i=0; i<(n2-2); i++) { p1[i] = D[i]/sumD; if (p1[i]==0) p1[i] = 1; }

   // H1 = sum(-p1 * log(p1))
   H1 = 0.0;
   for (i=0; i<(n2-2); i++) H1 += -p1[i]*log(p1[i]);

   // Pfun = 0.0
   Pfun = 0.0;
   
   // sumax = sum(X - abs(X))
   // sumax2 = sum((X - abs(X))^2)
   sumax = sumax2 = 0.0;
   for (i=0; i<n2; i++) { sumax += (X2[i] - _abs(X2[i])); sumax2 += pow(X2[i] - _abs(X2[i]),2); }
 
   // if real(sumax) < 0:
   //     Pfun = sumax2 / (2*n)^2
   if (sumax<0) Pfun += sumax2/(4*pow(n2,2));

   // return H1 + 1000 * Pfun
   return(H1 + Gamma * Pfun);
}

