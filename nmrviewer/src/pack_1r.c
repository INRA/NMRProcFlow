/* pack_1r.c - DJ - Fev 2013 - INRA */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
/*#include <unistd.h>*/
#include <fcntl.h>
#include <math.h>
#include <sys/resource.h>

#define PROG "pack_1r"
#define VERSION "1.0"
#define COUNT_MAX     129*1024  /* Nb de point max. permis */
#define STACKSIZE     256L * 1024L * 1024L   /* min stack size = 256 Mb */
#define NBSPEC_MAX    256

int  PTSMAX=2048;
int  DEBUG=0;
char *sep;

double ppm_min=-0.05;
double ppm_max=9.5;

struct s_multispec {
     char   **specs;
     double **VV;
     int    count_max;
     int    n_specs;
     double ppm_max;
     double ppm_min;
     double zone_max;
     double zone_min;
     double delta_ppm;
     double som_moy;
};

typedef struct {
    double  pmin;
    double  pmax;
    int size_c;
    int size_l;
} data_info;

struct  s_multispec msp;

int help(ret)
{
    fprintf(stderr,"\nusage: %s <list_file_1r> <pack_file> [options]\n",PROG);
    fprintf(stderr,"options are:\n");
    fprintf(stderr,"  -h                   This help\n");
    fprintf(stderr,"  -d                   some DEBUG information (stderr)\n");
    fprintf(stderr,"  -min  num            spectral windows to consider (min); default value = %f\n",ppm_min);
    fprintf(stderr,"  -max  num            spectral windows to consider (max); default value = %f\n",ppm_max);
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
    double v1,v2;
    if (streq(*argv,"-h"))
        { help(0); return 1;}
    else if(streq(*argv,"-d"))
        { DEBUG=1; return 1; }
    else if(streq(*argv,"-min"))
        {if((argc>1) && numeric(argv[1])) msp.zone_min=atof(argv[1]);
        return 2;
        }
    else if(streq(*argv,"-max"))
        {if((argc>1) && numeric(argv[1])) msp.zone_max=atof(argv[1]);
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
void free_matrix2 (double **m, int l)
{
    int i;
    for (i = 0; i <=l; i++)
        free(m[i]);
    free(m);
}

/* ------- Lecture des spectres ---------------------------------------------*/
void lecture_spectres (struct s_multispec *msp)
{
    FILE *fsp;
    int count,k,decal;
    char ligne[128];
    float xs,ys;
    double som;

    // Init som_moy
    msp->som_moy=0.0;

    /* Pour chaque fichier de spectre */
    for (k=1; k<=msp->n_specs; k++) {
        if (DEBUG) fprintf(stderr,"Reading  %-3d:%s ...",k, msp->specs[k]);
        for (count=1; count<=msp->count_max; count++) msp->VV[count][k]=0.0;

        fsp = fopen(msp->specs[k], "rt");
        count=0; decal=0; som=0.0;

        /* lecture fichier */
        while (!feof(fsp)) {
            if (fgets(ligne,128,fsp) == NULL) break;
            count++;
            sscanf(ligne,"%f%*c%f%*c",&xs,&ys);

            /* Ajustement (décalage) de l'échelle des ppm / au 1er spectre */
            if (k>1 && count==1) {
                decal=(int)((xs-msp->ppm_max)/msp->delta_ppm);
                if (DEBUG) fprintf(stderr,"PPM_MAX=%f, decal=%d, ",xs,decal);
            }
            if (decal>0 && count<=decal) continue;

            msp->VV[count-decal][k]=ys;
            som += ys;
            /* Echelle des ppm / 1er spectre */
            if (k==1) {
                if (xs<msp->ppm_min) msp->ppm_min=xs;
                if (xs>msp->ppm_max) msp->ppm_max=xs;
            }
            else if ((count-decal)>=msp->count_max) break;
        }
        close(fsp);

        if (k==1) {
            msp->count_max=count;
            msp->delta_ppm=(msp->ppm_max-msp->ppm_min)/(msp->count_max-1);
        }
        for (count=1; count<=msp->count_max; count++) msp->VV[count][k] = msp->VV[count][k]/som;
        msp->som_moy += som;

        if (DEBUG) fprintf(stderr,"(%d) ",count);
        if (DEBUG) fprintf(stderr,"OK\n");
    }

    msp->som_moy /= msp->n_specs;
    for (count=1; count<=msp->count_max; count++) {
        msp->VV[count][msp->n_specs+1]=0.0;
        for (k=1; k<=msp->n_specs; k++) {
            msp->VV[count][k] = msp->som_moy*msp->VV[count][k];
            msp->VV[count][msp->n_specs+1] += msp->VV[count][k];
        }
        msp->VV[count][msp->n_specs+1] /= (double)(msp->n_specs);
    }
}



/* =========================================================================
       Programme Principal
   =========================================================================*/

int main(argc,argv)
    int     argc;
    char    *argv[];
{
    char    *file_in, *file_out;
    FILE    *fin,*fout;

    double  y, ppm, xmin, xmax, ymin, ymax, c_norm;
    int     count, cnt1, cnt2, size, n, k, i, j, jmin, jmax;
    char    *ext = malloc (sizeof (*ext) * 8);
    char ligne[128], file_sp[128];

    /* Increase RLIMIT_STACK up to 64Mb */
    /*if (inc_rlimit() != 0) exit(1);*/

    fprintf(stderr,"%s   version %s\n",PROG,VERSION);

    /* Init : some default values */
    msp.zone_min=ppm_min;
    msp.zone_max=ppm_min;
    sep = malloc (8);
    strcpy(sep,"\t");

    if (argc>1 && argc<3) {
        argv++;
        get_parameter(argc,argv);
    }

    /* parametres en entree */
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

    /* Lecture de la liste des spectres */
    fin = fopen(file_in, "rt");
    msp.n_specs=0;
    msp.specs = malloc(NBSPEC_MAX * sizeof *msp.specs);
    fprintf(stderr,"Reading spectrum list: ");
    while (!feof(fin)) {
        if (fgets(ligne,128,fin) == NULL) break;
        sscanf(ligne,"%s*c",&file_sp);
        msp.n_specs++;
        msp.specs[msp.n_specs]=malloc(strlen(file_sp)+1);
        strcpy(msp.specs[msp.n_specs],file_sp);
    }
    close(fin);
    fprintf(stderr,": %d spectrum\n",msp.n_specs);

    /* Allocation memoire */
    msp.VV=matrix2(COUNT_MAX,msp.n_specs+1);

    fprintf(stderr,"OK malloc : %d\n",COUNT_MAX);

    /* Lecture des spectres */
    msp.ppm_min=0;
    msp.ppm_max=0;
    lecture_spectres(&msp);

    fprintf(stderr,"Nb points : %d, ppm_min=%f, ppm_max=%f, delta_ppm=%f\n",msp.count_max,msp.ppm_min,msp.ppm_max,msp.delta_ppm);
    fprintf(stderr,"Interest Zone : min=%f, max=%f\n",msp.zone_min,msp.zone_max);

    /* ------- Ecriture des donnees (mode binary) --------------------------------*/

    data_info   inforec;

    // Output file
    fout = fopen(file_out,"w");

    inforec.pmax=msp.ppm_max;
    inforec.pmin=msp.ppm_min;
    inforec.size_l=msp.count_max;
    inforec.size_c=msp.n_specs+2;
    fwrite(&inforec,sizeof(inforec),1,fout);

    for (i=1; i<=msp.count_max; i++) {
           fwrite(msp.VV[i],(unsigned)(inforec.size_c) * sizeof(double),1,fout);
    }

    close(fout);

    free_matrix2(msp.VV,COUNT_MAX);

    exit(0);
}
