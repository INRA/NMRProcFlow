/* bin4gp_1r.c - DJ - mars 2013 - INRA */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
/*#include <unistd.h>*/
#include <fcntl.h>
#include <math.h>
#include <sys/resource.h>

#define PROG "bin4gp_1r"
#define VERSION "1.0"
#define STACKSIZE     128L * 1024L * 1024L   /* min stack size = 128 Mb */

int DEBUG=0;
int PTSMAX=2048;

double ppm_min=-0.05;
double ppm_max=9.5;
char *sep;

typedef struct {
    double  pmin;
    double  pmax;
    int size_c;
    int size_l;
} data_info;

int help(ret)
{
    fprintf(stderr,"\nusage: %s packfile binfile [options]\n",PROG);
    fprintf(stderr,"options are:\n");
    fprintf(stderr,"  -h                   This help\n");
    fprintf(stderr,"  -d                   some DEBUG information (stderr)\n");
    fprintf(stderr,"  -min  num            spectral windows to consider (min); default value = %f\n",ppm_min);
    fprintf(stderr,"  -max  num            spectral windows to consider (max); default value = %f\n",ppm_max);
    fprintf(stderr,"  -pts  num            number of points; 0 means all points; default value = %d\n",PTSMAX);
    exit(ret);
}

int streq(char *a, char *b)
{
        while(*a){
           if(*a!=*b)return 0;
       a++; b++;
    }
    return 1;
}

int numeric(char *s)
{
        char c;
    while(c=*s++) {
       if((c<='9' && c>='0') || c=='+' || c=='-' || c=='.') continue;
       return 0;
    }
    return 1;
}

int inc_rlimit ()
{
    const rlim_t kStackSize = STACKSIZE;
    struct rlimit rl;
    int result;

    result = getrlimit(RLIMIT_STACK, &rl);
    if (result == 0)
    {
        if (rl.rlim_cur < kStackSize)
        {
            rl.rlim_cur = kStackSize;
            result = setrlimit(RLIMIT_STACK, &rl);
            if (result != 0)
            {
                fprintf(stderr, "setrlimit returned result = %d\n", result);
            }
        }
    }

    return result;
}

/* get_parameter - process one command line option
        (return # parameters used) */
int get_parameter(int argc, char **argv)
{
    int i;
    if (streq(*argv,"-h"))
        { help(0); return 1;}
    else if(streq(*argv,"-d"))
        { DEBUG=1; return 1; }
    else if(streq(*argv,"-min"))
        {if((argc>1) && numeric(argv[1])) ppm_min=atof(argv[1]);
        return 2;
        }
    else if(streq(*argv,"-max"))
        {if((argc>1) && numeric(argv[1])) ppm_max=atof(argv[1]);
        return 2;
        }
    else if(streq(*argv,"-pts"))
        {if((argc>1) && numeric(argv[1])) PTSMAX=atof(argv[1]);
        return 2;
        }
    else { fprintf(stderr,"%s\n","Syntaxe incorrect"); help(1); }
    return 0;
}

/* ------- definition Matrix  -----------------------------------------------*/
double **matrix2(int l, int c)
{
    int i;
    double** m = (double**) malloc((unsigned)(l+1) * sizeof(double));
    for (i = 0; i <=l; i++)
        m[i] = (double*) malloc((unsigned)(c+1) * sizeof(double));
    return m;
}
void free_matrix (double **m, int l)
{
    int i;
    for (i = 0; i <=l; i++)
        free(m[i]);
    free(m);
}


/* =========================================================================
       Programme Principal
   =========================================================================*/

int main(argc,argv)
    int     argc;
    char    *argv[];
{
    char    *file_in, *file_out;
    char    binfile[1024]="";
    char    *ext = malloc (sizeof (*ext) * 8);
    char    *sid = malloc (sizeof (*sid) * 8);
    FILE    *fin,*fout;
    int     i, j, k, n, p, nstart, nstop, count, kmin, kmax, size, sizerec;
    long    foffset;
    double  xmin, xmax, ymin, ymax, delta_ppm;
    float   fvalue, min_val, max_val;
    double  **VV, **BB;

    fprintf(stderr,"%s   version %s\n",PROG,VERSION);

/* ------- parametres en entree ----------------------------------------------*/
    if (argc==2) {
        argv++;
        get_parameter(argc,argv);
    }
    if (argc<3 ) {
        fprintf(stderr,"%s\n","Syntaxe incorrect");
        help(1);
    }

    file_in  = argv[1];
    file_out = argv[2];

    argc -= 3; argv +=3;
    while(argc>0) {
         i=get_parameter(argc,argv);
         argv += i; argc -= i;
    }

   /* Increase RLIMIT_STACK up to STACKSIZE */
   /*if (inc_rlimit() != 0) exit(1);*/

/* ------- Lecture des donnees -----------------------------------------------*/
    data_info   inforec;
    fin = fopen(file_in, "rb");
    fread(&inforec,sizeof(inforec),1,fin);
    fprintf(stderr,"Min = %f,  Max = %f, Nb Cols = %d, Nb Lines = %d\n",inforec.pmin,inforec.pmax,inforec.size_c,inforec.size_l);
    sizerec=(unsigned)(inforec.size_c) * sizeof(double);
    delta_ppm=(inforec.pmax - inforec.pmin)/(inforec.size_l-1);
    nstart = (inforec.pmax - ppm_max)/delta_ppm + 1;
    nstop  = (inforec.pmax - ppm_min)/delta_ppm + 1;
    nstop = nstop > inforec.size_l ? inforec.size_l : nstop;
    foffset =  (long)(nstart * sizerec);
    count = nstop - nstart + 1 ;
    fprintf(stderr,"Nstart = %d, Nstop = %d, Count = %d, Sizerec = %d, Offset = %d bytes\n",nstart, nstop, count, sizerec, foffset);
    if( (fseek(fin,foffset,SEEK_CUR))!= 0) {
         printf("Error\n");
         exit(0);
    }

    double ppm (int n) { return inforec.pmax - (double)(nstart + n - 1)*delta_ppm; }
    fprintf(stderr,"ppm(%d) = %f, ppm(%d) = %f, delta ppm = %f \n",nstart,ppm(1),nstop,ppm(count-1),delta_ppm);

    VV = matrix2(count+1,inforec.size_c);
    for (i=1; i<count; i++) if (fread(VV[i],(long)sizerec,1,fin) == 0) { i--; break; }
    count=i;
    close(fin);
    fprintf(stderr,"OK read data: %d bytes\n",(count+1)*sizerec);

    BB = matrix2(2*PTSMAX,inforec.size_c);

    //Extract a small part just for graph
    size = (int)(count/PTSMAX) + 1;
    fprintf(stderr,"Count = %d, Pts = %d, step size = %d\n",count,PTSMAX,size);
    n=inforec.size_c-1;
    p=1;
    for (i=1; i<count; i+=size) {
           xmin=0; ymin=VV[i][n]; xmax=0; ymax=VV[i][n]-1.0;
           for (k=0; k<size; k++) {
              if ((i+k)>count) break;
              if (VV[i+k][n]<=ymin) { xmin = ppm(i+k); ymin = VV[i+k][n]; kmin=k; }
              if (VV[i+k][n]>ymax)  { xmax = ppm(i+k); ymax = VV[i+k][n]; kmax=k; }
           }
           BB[p][1]=xmin;
           for (j=1; j<n; j++) BB[p][j+1] = VV[i+kmin][j];
           p++;
           if (xmax>xmin) {
              BB[p][1]=xmax;
              for (j=1; j<n; j++) BB[p][j+1] = VV[i+kmax][j];
              p++;
           }
    }
    p--;
    fprintf(stderr,"OK Extract\n");

    // populate output binary files : one per spectrum
    strcpy(ext,".bin");
    min_val=0; max_val=0;
    for (j=2; j<=n; j++) {
        strcpy(binfile,file_out);
        sprintf(sid,"%d",j-1);
        strcat(binfile,sid);
        strcat(binfile,ext);
        fprintf(stderr,"file = %s\n",binfile);
        fout = fopen(binfile,"wb");
        for (i=1; i<=p; i++) {
            fvalue = (float)(BB[i][1]); fwrite(&fvalue,sizeof(float),1,fout);
            fvalue = (float)(BB[i][j]); fwrite(&fvalue,sizeof(float),1,fout);
            if (fvalue<min_val) min_val=fvalue;
            if (fvalue>max_val) max_val=fvalue;
        }
        close(fout);
    }
    
    fprintf(stdout,"record=%dx%d\n",p,n);
    fprintf(stdout,"min=%f\n",min_val);
    fprintf(stdout,"max=%f\n",max_val);

    free_matrix(BB,2*PTSMAX);
    free_matrix(VV,count+1);

    exit(0);
}
