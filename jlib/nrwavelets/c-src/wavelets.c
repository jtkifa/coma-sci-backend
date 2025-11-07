/* public domain numerical recipes wavelets routines converted
   to double-float, and original single float

   note that some of the coefficients are not expressed in full double
   float precision, so that these routines may not be very good */

#include <stdio.h>
#include <stdlib.h>


#define NR_END 1
#define FREE_ARG char*


void nrerror(char error_text[])
/* Numerical Recipes standard error handler */
{
        fprintf(stderr,"Numerical Recipes run-time error...\n");
        fprintf(stderr,"%s\n",error_text);
        fprintf(stderr,"...now exiting to system...\n");
        exit(1);
}



/****************************************************************/
/*  DOUBLE FLOAT VERSION  ***************************************/
/****************************************************************/


void free_dvector(double *v, long nl, long nh)
/* free a double vector allocated with dvector() */
{
        free((FREE_ARG) (v+nl-NR_END));
}



double *dvector(long nl, long nh)
/* allocate a double vector with subscript range v[nl..nh] */
{
        double *v;

        v=(double *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(double)));
        if (!v) nrerror("allocation failure in dvector()");
        return v-nl+NR_END;
}





typedef struct {
        int ncof,ioff,joff;
        double *cc,*cr;
} dwavefilt;


/*
void dwt1(double a[], unsigned long n, int isign,
        void (*wtstep)(double [], unsigned long, int, *dwavefilt), dwavewfilt *wfilt)
{
        unsigned long nn;

        if (n < 4) return;
        if (isign >= 0) {
                for (nn=n;nn>=4;nn>>=1) (*wtstep)(a,nn,isign, *wfilt);
        } else {
                for (nn=4;nn<=n;nn<<=1) (*wtstep)(a,nn,isign, *wfilt);
        }
}*/


/*wavefilt wfilt;*/

void dpwtset(int n, dwavefilt *wfilt)
{
        void nrerror(char error_text[]);
        int k;
        double sig = -1.0;
        static double c4[5]={0.0,0.4829629131445341,0.8365163037378079,
                        0.2241438680420134,-0.1294095225512604};
        static double c12[13]={0.0,0.111540743350, 0.494623890398, 0.751133908021
,
                0.315250351709,-0.226264693965,-0.129766867567,
                0.097501605587, 0.027522865530,-0.031582039318,
                0.000553842201, 0.004777257511,-0.001077301085};
        static double c20[21]={0.0,0.026670057901, 0.188176800078, 0.527201188932
,
                0.688459039454, 0.281172343661,-0.249846424327,
                -0.195946274377, 0.127369340336, 0.093057364604,
                -0.071394147166,-0.029457536822, 0.033212674059,
                0.003606553567,-0.010733175483, 0.001395351747,
                0.001992405295,-0.000685856695,-0.000116466855,
                0.000093588670,-0.000013264203};
        static double c4r[5],c12r[13],c20r[21];

        wfilt->ncof=n;
        if (n == 4) {
                wfilt->cc=c4;
                wfilt->cr=c4r;
        }
        else if (n == 12) {
                wfilt->cc=c12;
                wfilt->cr=c12r;
        }
        else if (n == 20) {
                wfilt->cc=c20;
                wfilt->cr=c20r;
        }
        else nrerror("unimplemented value n in dpwtset");
        for (k=1;k<=n;k++) {
                wfilt->cr[wfilt->ncof+1-k]=sig*wfilt->cc[k];
                sig = -sig;
        }
        wfilt->ioff = wfilt->joff = -(n >> 1);
}




void dpwt(double a[], unsigned long n, int isign, dwavefilt *wfilt)
{
        double ai,ai1,*wksp;
        unsigned long i,ii,j,jf,jr,k,n1,ni,nj,nh,nmod;

        if (n < 4) return;
        wksp=dvector(1,n);
        nmod=wfilt->ncof*n;
        n1=n-1;
        nh=n >> 1;
        for (j=1;j<=n;j++) wksp[j]=0.0;
        if (isign >= 0) {
                for (ii=1,i=1;i<=n;i+=2,ii++) {
                        ni=i+nmod+wfilt->ioff;
                        nj=i+nmod+wfilt->joff;
                        for (k=1;k<=wfilt->ncof;k++) {
                                jf=n1 & (ni+k);
                                jr=n1 & (nj+k);
                                wksp[ii] += wfilt->cc[k]*a[jf+1];
                                wksp[ii+nh] += wfilt->cr[k]*a[jr+1];
                        }
                }
        } else {
                for (ii=1,i=1;i<=n;i+=2,ii++) {
                        ai=a[ii];
                        ai1=a[ii+nh];
                        ni=i+nmod+wfilt->ioff;
                        nj=i+nmod+wfilt->joff;
                        for (k=1;k<=wfilt->ncof;k++) {
                                jf=(n1 & (ni+k))+1;
                                jr=(n1 & (nj+k))+1;
                                wksp[jf] += wfilt->cc[k]*ai;
                                wksp[jr] += wfilt->cr[k]*ai1;
                        }
                }
        }
        for (j=1;j<=n;j++) a[j]=wksp[j];
        free_dvector(wksp,1,n);
}


void dwtn(double a[], unsigned long nn[], int ndim, int isign,
	  void (*wtstep)(double [], unsigned long, int, dwavefilt*), dwavefilt *wfilt)
{
        unsigned long i1,i2,i3,k,n,nnew,nprev=1,nt,ntot=1;
        int idim;
        double *wksp;

        for (idim=1;idim<=ndim;idim++) ntot *= nn[idim];
        wksp=dvector(1,ntot);
        for (idim=1;idim<=ndim;idim++) {
                n=nn[idim];
                nnew=n*nprev;
                if (n > 4) {
		  for (i2=0;i2<ntot;i2+=nnew) {
		    for (i1=1;i1<=nprev;i1++) {
		      for (i3=i1+i2,k=1;k<=n;k++,i3+=nprev) wksp[k]=a[i3];
		      if (isign >= 0) {
			for(nt=n;nt>=4;nt >>= 1)
			  (*wtstep)(wksp,nt,isign,wfilt);
		      } else {
			for(nt=4;nt<=n;nt <<= 1)
			  (*wtstep)(wksp,nt,isign,wfilt);
		      }
		      
		      for (i3=i1+i2,k=1;k<=n;k++,i3+=nprev) a[i3]=wksp[k];
		    }
		  }
                }
                nprev=nnew;
        }
        free_dvector(wksp,1,ntot);
}


/* our specific versions of wtn for wavelets of 4,12,20 - note that
   these take arrays beginning with a[0]...a[n-1] */
void dwtn4(double a[], unsigned long nn[], int ndim, int isign)
{
  dwavefilt wf;
  dpwtset(4,&wf);
  a--;  
  nn--;
  dwtn(a,nn,ndim,isign,dpwt,&wf);
}

void dwtn12(double a[], unsigned long nn[], int ndim, int isign)
{
  dwavefilt wf;
  dpwtset(12,&wf);
  a--;
  nn--;
  dwtn(a,nn,ndim,isign,dpwt,&wf);
}

void dwtn20(double a[], unsigned long nn[], int ndim, int isign)
{
  dwavefilt wf;
  dpwtset(20,&wf);
  a--;
  nn--;
  dwtn(a,nn,ndim,isign,dpwt,&wf);
}


/****************************************************************/
/*  SINGLE FLOAT VERSION  ***************************************/
/****************************************************************/


void free_fvector(float *v, long nl, long nh)
/* free a float vector allocated with fvector() */
{
        free((FREE_ARG) (v+nl-NR_END));
}



float *fvector(long nl, long nh)
/* allocate a float vector with subscript range v[nl..nh] */
{
        float *v;

        v=(float *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(float)));
        if (!v) nrerror("allocation failure in fvector()");
        return v-nl+NR_END;
}





typedef struct {
        int ncof,ioff,joff;
        float *cc,*cr;
} fwavefilt;





/*wavefilt wfilt;*/

void fpwtset(int n, fwavefilt *wfilt)
{
        void nrerror(char error_text[]);
        int k;
        float sig = -1.0;
        static float c4[5]={0.0,0.4829629131445341,0.8365163037378079,
                        0.2241438680420134,-0.1294095225512604};
        static float c12[13]={0.0,0.111540743350, 0.494623890398, 0.751133908021
,
                0.315250351709,-0.226264693965,-0.129766867567,
                0.097501605587, 0.027522865530,-0.031582039318,
                0.000553842201, 0.004777257511,-0.001077301085};
        static float c20[21]={0.0,0.026670057901, 0.188176800078, 0.527201188932
,
                0.688459039454, 0.281172343661,-0.249846424327,
                -0.195946274377, 0.127369340336, 0.093057364604,
                -0.071394147166,-0.029457536822, 0.033212674059,
                0.003606553567,-0.010733175483, 0.001395351747,
                0.001992405295,-0.000685856695,-0.000116466855,
                0.000093588670,-0.000013264203};
        static float c4r[5],c12r[13],c20r[21];

        wfilt->ncof=n;
        if (n == 4) {
                wfilt->cc=c4;
                wfilt->cr=c4r;
        }
        else if (n == 12) {
                wfilt->cc=c12;
                wfilt->cr=c12r;
        }
        else if (n == 20) {
                wfilt->cc=c20;
                wfilt->cr=c20r;
        }
        else nrerror("unimplemented value n in fpwtset");
        for (k=1;k<=n;k++) {
                wfilt->cr[wfilt->ncof+1-k]=sig*wfilt->cc[k];
                sig = -sig;
        }
        wfilt->ioff = wfilt->joff = -(n >> 1);
}




void fpwt(float a[], unsigned long n, int isign, fwavefilt *wfilt)
{
        float ai,ai1,*wksp;
        unsigned long i,ii,j,jf,jr,k,n1,ni,nj,nh,nmod;

        if (n < 4) return;
        wksp=fvector(1,n);
        nmod=wfilt->ncof*n;
        n1=n-1;
        nh=n >> 1;
        for (j=1;j<=n;j++) wksp[j]=0.0;
        if (isign >= 0) {
                for (ii=1,i=1;i<=n;i+=2,ii++) {
                        ni=i+nmod+wfilt->ioff;
                        nj=i+nmod+wfilt->joff;
                        for (k=1;k<=wfilt->ncof;k++) {
                                jf=n1 & (ni+k);
                                jr=n1 & (nj+k);
                                wksp[ii] += wfilt->cc[k]*a[jf+1];
                                wksp[ii+nh] += wfilt->cr[k]*a[jr+1];
                        }
                }
        } else {
                for (ii=1,i=1;i<=n;i+=2,ii++) {
                        ai=a[ii];
                        ai1=a[ii+nh];
                        ni=i+nmod+wfilt->ioff;
                        nj=i+nmod+wfilt->joff;
                        for (k=1;k<=wfilt->ncof;k++) {
                                jf=(n1 & (ni+k))+1;
                                jr=(n1 & (nj+k))+1;
                                wksp[jf] += wfilt->cc[k]*ai;
                                wksp[jr] += wfilt->cr[k]*ai1;
                        }
                }
        }
        for (j=1;j<=n;j++) a[j]=wksp[j];
        free_fvector(wksp,1,n);
}


void fwtn(float a[], unsigned long nn[], int ndim, int isign,
	  void (*wtstep)(float [], unsigned long, int, fwavefilt*), fwavefilt *wfilt)
{
        unsigned long i1,i2,i3,k,n,nnew,nprev=1,nt,ntot=1;
        int idim;
        float *wksp;

        for (idim=1;idim<=ndim;idim++) ntot *= nn[idim];
        wksp=fvector(1,ntot);
        for (idim=1;idim<=ndim;idim++) {
                n=nn[idim];
                nnew=n*nprev;
                if (n > 4) {
		  for (i2=0;i2<ntot;i2+=nnew) {
		    for (i1=1;i1<=nprev;i1++) {
		      for (i3=i1+i2,k=1;k<=n;k++,i3+=nprev) wksp[k]=a[i3];
		      if (isign >= 0) {
			for(nt=n;nt>=4;nt >>= 1)
			  (*wtstep)(wksp,nt,isign,wfilt);
		      } else {
			for(nt=4;nt<=n;nt <<= 1)
			  (*wtstep)(wksp,nt,isign,wfilt);
		      }
		      
		      for (i3=i1+i2,k=1;k<=n;k++,i3+=nprev) a[i3]=wksp[k];
		    }
		  }
                }
                nprev=nnew;
        }
        free_fvector(wksp,1,ntot);
}


/* our specific versions of wtn for wavelets of 4,12,20 - note that
   these take arrays beginning with a[0]...a[n-1] */
void fwtn4(float a[], unsigned long nn[], int ndim, int isign)
{
  fwavefilt wf;
  fpwtset(4,&wf);
  a--;  
  nn--;
  fwtn(a,nn,ndim,isign,fpwt,&wf);
}

void fwtn12(float a[], unsigned long nn[], int ndim, int isign)
{
  fwavefilt wf;
  fpwtset(12,&wf);
  a--;
  nn--;
  fwtn(a,nn,ndim,isign,fpwt,&wf);
}

void fwtn20(float a[], unsigned long nn[], int ndim, int isign)
{
  fwavefilt wf;
  fpwtset(20,&wf);
  a--;
  nn--;
  fwtn(a,nn,ndim,isign,fpwt,&wf);
}

