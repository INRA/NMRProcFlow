// See https://teuder.github.io/rcpp4everyone_en/
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
    NumericVector specR(v);
    int n1,n2,N;
    N = specR.size();
    NumericVector S(N);
    for (int count=0; count<N; count++) {
        n1 = count >= n ? count - n : 0;
        n2 = count <= N - n - 1 ? count + n : N - 1;
        S[count] = WinMoy(specR,n1,n2);
    }
    return S;
}

// [[Rcpp::export]]
void Ajust_LB (SEXP s, SEXP b, int n1, int n2)
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
        Ajust_LB(specR,lb,ni,n2);
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
                Ajust_LB(specR,lb,n1,n2);
            }
            n1=count-1;
            cnt=0;
        }
   }
   if (cnt>0) for (k=n2; k<count; k++) lb[k] = m1[k];
   if (n1<n2) Ajust_LB(specR,lb,n1,iend-1);
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
   int edgesize=10;

   // Create the lb vector initialize with spectrum values
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
               double a=(lb[n2]-lb[n1])/(n2-n1);  for (k=n1; k<n2; k++) lb[k]=a*(k-n1)+lb[n1];
            }
            n1=count-1;
            cnt=0;
        }
   }
   if (cnt>0) for (k=n2; k<count; k++) lb[k] = m1[k];
   if (n1<n2) {
       n2=iend-1;
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
SEXP C_MedianSpec(SEXP x) {
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
   D[1]=specR[1]-specR[0]; D[0]=specR[1];
   for (count=2; count<count_max-2; count++)
       D[count] = (specR[count-2]-8*specR[count-1]+8*specR[count+1]-specR[count+2])/12;
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
              sse +=  10.0*pow(vref[i]-vk[ij],2);
           else 
              sse +=  10.0*pow(vref[i],2);
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
int C_align_segment (SEXP x, SEXP s, int istart, int iend, IntegerVector v)
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
       if (delta==0) continue;
       for (i=0; i<size_m; i++) {
           vk[i]=0.0;
           ij=i+delta;
           if ( ij>=0 && ij<size_m)
               vk[i]=VV(idx, istart+ij);
       }
       for (i=0; i<size_m; i++) VV(idx,istart+i)=vk[i];
       _apodize(x, idx,istart);
       _apodize(x, idx,iend);
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
   double R;
   double ynoise;
   double vnoise;
   double noise_fac;
   double bin_fac;
   double peaknoise_rate;
   double BUCMIN;
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

int find_buckets(SEXP x, SEXP b, SEXP v, struct BinData *bdata, int n1, int n2)
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
        if (find_buckets(x, buckets,vref,bdata,n1,ncut)==0) save_bucket(buckets,vref,bdata,n1,ncut);
        if (find_buckets(x, buckets,vref,bdata,ncut,n2)==0) save_bucket(buckets,vref,bdata,ncut,n2);
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

   find_buckets(x, buckets,vref,&bdata,n1-1,n2-1);
   // Rprintf("Returned Value = %d, number of buckets found = %d\n",ret, bdata.n_buckets);
   if (bdata.n_buckets==0) return R_NilValue;

   NumericMatrix M(bdata.n_buckets, 2);
   for (i=0; i<bdata.n_buckets; i++) { M(i,0) = buckets(i,0)+1; M(i,1) = buckets(i,1)+1; }
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
   //double m[n2];
   double mx, sdx, med, sdev;

  Rcpp::Environment base("package:stats"); 
  Rcpp::Function median_r = base["median"];

  // Call the function and receive its list output

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

// C_corr_spec_re( l=list(spec1r$re, spec1r$im, phc0, phc1, alpha) )
//   m <- length(spec1r)
//   dm <- round(m*fracppm)
//   Omega <- (-dm:(m-1-dm))/m
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
   double alpha = as<double>(spec["alpha"]);
   int n = re.size();
   double phi, dn;

   NumericVector S_re(n);
   NumericVector S_im(n);
   dn=n*alpha;
   for (int i=0; i<n; i++)
   {
       phi = phc0 + phc1*(i-dn)/n;
       S_re[i] = cos(phi)*re[i] - sin(phi)*im[i];
       S_im[i] = cos(phi)*im[i] + sin(phi)*re[i];

   }
   List new_spec = List::create(
      _["re"] = S_re,
      _["im"] = S_im 
   );

   return new_spec;
}

// fmin (l=list(spec1r$re, spec1r$im, phc0, phc1, fracppm, p=alpha), x, flg)
// depending on flg, the x value serves to evaluate the optimization criterium for either phc0 (flg=0) or phc1(flg=0)
//   m <- length(spec1r)
//   dm <- round(m*fracppm)
//   Omega <- (-dm:(m-1-dm))/m
//   phi <- phc0 + phc1*2*pi*Omega
//   Yrot <- spec1r * exp(complex(real=0,imaginary=phi)) # spectrum rotation
//   Yre <- Re(Yrot)
//   n <- round(length(Yre)/32)
//   sv <- simplify2array(lapply( c(3:29), function(x) { sd(Yre[((x-1)*n):(x*n)]); }))
//   mv <- simplify2array(lapply( c(3:29), function(x) { median(Yre[((x-1)*n):(x*n)]); }))
//   Yre <- Yre - mv[which.min(sv)]
//   n <- round(length(Yre)/24)
//   Yre <- Yre[n:(23*n)]
//   si <- sign(Yre)  # sign of intensities
//   Yre[abs(Yre) >= quantile(abs(Yre), p)] <- quantile(abs(Yre), p)  # trim the values
//   Yre <- abs(Yre) * si  # spectral trimmed values
//   YrePos <- Yre[Yre >= 0]  # select positive intensities
//   POSss <- sum((YrePos)^2)  # SS for positive intensities
//   ss <- sum((Yre)^2)  #  SS for all intensities
//   ret <- 1- POSss/ss  # criterion : SS for positive values / SS for all intensities
//}

double fmin(SEXP l, double x, int flg)
{
   List spec(l);
   NumericVector re = as<NumericVector>(spec["re"]);
   NumericVector im = as<NumericVector>(spec["im"]);
   double phc0  = as<double>(spec["phc0"]);
   double phc1  = as<double>(spec["phc1"]);
   double alpha = as<double>(spec["alpha"]);
   int    blphc = as<int>(spec["blphc"]);
   double p     = as<double>(spec["p"]);
   const size_t n = (size_t)(re.size());
   const size_t n2 = (size_t)(n/24);
   size_t i;

   // depending on flg, the x value serves to evaluate the optimization criterium for either phc0 (flg=0) or phc1(flg=1)
   if (flg==0) phc0=x;
   if (flg==1) phc1=x;

   double quant, SSpos, SStot, ysign, ytrim;
   double phi, dn;

   NumericVector X(n), Yf(n), lb(n), V(n), P(1);
   dn=n*alpha;
   for (i=0; i<n; i++) {
       phi = phc0 + phc1*(i-dn)/n;
       X[i] = cos(phi)*re[i] - sin(phi)*im[i];
   }
   if (blphc==1) {
      double sig = C_estime_sd(X,128);
      lb=C_Estime_LB2(X, 1, n-1, 100, 100, 3.0*sig);
   }
   for (i=0; i<n; ++i) {
     if (i>n2 || i<(n-n2))
         if (blphc==1) {
             Yf[i] = X[i]-lb[i];
         } else Yf[i] = X[i];
     else
         Yf[i] = 0;
     V[i]= (Yf[i] > 0 ? Yf[i]:-Yf[i]);
   }

  Rcpp::Environment base("package:stats"); 
  Rcpp::Function quantile_r = base["quantile"];
  P[0]=p;
  NumericVector res = quantile_r(V, _["probs"] = P);
  quant=res[0];

   SSpos=SStot=0;
   for (i=0; i<n; ++i) {
      ysign = Yf[i] > 0 ? 1:-1;
      ytrim = Yf[i]*ysign > quant ? quant*ysign:Yf[i];
      SStot += ytrim*ytrim;
      if (Yf[i] > 0) SSpos += ytrim*ytrim;
   }

   return(1 - SSpos/SStot);
}

// C_optim_phc: Adapted directly from R devel optimize.c
// See http://docs.rexamine.com/R-devel/optimize_8c_source.html
//
// interval = (ax, bx)
// l:  list(spec1r$re, spec1r$im, phc0, phc1, fracppm, p=alpha) (see fmin)
// flg: depending on flg, the optimization criterium is evaluated for either phc0 (flg=0) or phc1(flg=0)
// tol: tolerance

// [[Rcpp::export]]
SEXP C_optim_phc(double ax, double bx, SEXP l, int flg, double tol)
{
    /*  c is the squared inverse of the golden ratio */
    const double c = (3. - sqrt(5.)) * .5;

    /* Local variables */
    double a, b, d, e, p, q, r, u, v, w, x;
    double t2, fu, fv, fw, fx, xm, eps, tol1, tol3;

/*  eps is approximately the square root of the relative machine precision. */
    eps = DBL_EPSILON;
    tol1 = eps + 1.;/* the smallest 1.000... > 1 */
    eps = sqrt(eps);
    a = ax;
    b = bx;
    v = a + c * (b - a);
    w = x = v;
    e = d = 0.; /* -Wall */
    fx = fmin(l,x,flg);
    fv = fw = fx;
    tol3 = tol / 3.;

/*  main loop starts here ----------------------------------- */
for(;;) {
   xm = (a + b) * .5;
   tol1 = eps * fabs(x) + tol3;
   t2 = tol1 * 2.;

   /* check stopping criterion */

   if (fabs(x - xm) <= t2 - (b - a) * .5) break;
   p = q = r = 0.;
   if (fabs(e) > tol1) { /* fit parabola */
       r = (x - w) * (fx - fv);
       q = (x - v) * (fx - fw);
       p = (x - v) * q - (x - w) * r;
       q = (q - r) * 2.;
       if (q > 0.) p = -p; else q = -q;
       r = e;
       e = d;
   }

   if (fabs(p) >= fabs(q * .5 * r) || p <= q * (a - x) || p >= q * (b - x)) { /* a golden-section step */
       if (x < xm) e = b - x; else e = a - x;
       d = c * e;
   }
   else { /* a parabolic-interpolation step */
       d = p / q;
       u = x + d;
       /* f must not be evaluated too close to ax or bx */
       if (u - a < t2 || b - u < t2) {
          d = tol1;
          if (x >= xm) d = -d;
       }
   }

   /* f must not be evaluated too close to x */
   if (fabs(d) >= tol1)
       u = x + d;
   else if (d > 0.)
       u = x + tol1;
   else
       u = x - tol1;

   fu = fmin(l,u,flg);

   /*  update  a, b, v, w, and x */
   if (fu <= fx) {
       if (u < x) b = x; else a = x;
       v = w;   w = x;   x = u;
       fv = fw; fw = fx; fx = fu;
   } else {
       if (u < x) a = u; else b = u;
       if (fu <= fw || w == x) {
          v = w; fv = fw;
          w = u; fw = fu;
       }
       else if (fu <= fv || v == x || v == w) {
          v = u; fv = fu;
       }
   }
}
/* end of main loop */

   List best = List::create(
      _["minimum"] = x,
      _["objective"] = fv 
   );
   return best;
}
