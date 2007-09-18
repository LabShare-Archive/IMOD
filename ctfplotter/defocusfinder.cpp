#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "defocusfinder.h"

#define DEF_START -1.0
#define DEF_END -20.0
#define ZERO_START 0.2
#define ZERO_END 0.8
#define MY_PI 3.1415926

DefocusFinder::DefocusFinder(int volt, double pSize,
    double ampContrast, double inputCs, 
    int dim, double expDef): voltage(volt), pixelSize(pSize), 
  ampRatio(ampContrast), cs(inputCs), nDim(dim), expDefocus(expDef)
{ 
  expDefocus=expDefocus/1000.0; //convert to microns;
  //wavelength in nm;
  wavelength=1.226/sqrt( 1000.0*voltage*(1+0.0009788*voltage) ); 
  csOne=sqrt(cs*wavelength); // deltaZ=-deltaZ'/cs1;  In microns
  csTwo=sqrt(sqrt(1000000.0*cs/wavelength)); //theta=theta'*cs2;
  expZero=sqrt(csOne/expDefocus)*pixelSize*2.0/(wavelength*csTwo);
  defocus=-1000.0;
}

int DefocusFinder::findZero(const double* simplexRes, const double* linearRes,
    int x1, int x2, double* zero)
{
  if( (x2-x1)<2 ){
    printf("findZero() error: range<2\n");
    return -1;
  }
  int dim=x2-x1+1;
  double *diff=(double*)malloc(dim*sizeof(double));
  double minDiff; 
  int i, minIndex; 
  for(i=x1;i<x1+dim;i++){
    diff[i-x1]=simplexRes[i]-linearRes[i];
  }

  int middle=dim/2;

  //find the minimiun;
  minDiff=fabs(diff[0]); 
  minIndex=0;
  for(i=1;i<middle;i++){
    if( fabs(diff[i])<minDiff ){
      minDiff=fabs(diff[i]);
      minIndex=i;
    }
  }

  //confirm it is a zerocrossing;
  if( (minIndex-1)<0 || (minIndex+1)>(dim-1) ){
    printf("findZero() error: the sign of values can not be determined.  \
        zeroIndex=%d\n", minIndex);
    free(diff);
    return -1;
  }

  if( diff[minIndex-1]*diff[minIndex+1]<0.0 ){ // it is a crossing
    //linear interpolation
    double interpolate=-(diff[minIndex+1]+diff[minIndex-1])/
                     (diff[minIndex+1]-diff[minIndex-1]);
    *zero=(double)(x1+minIndex+interpolate)/(nDim-1);
    zeroCrossing=*zero;
    free(diff);
    return 0;
  }else{
    minDiff=fabs(diff[middle]);
    minIndex=middle;
    for(i=middle;i<dim;i++){
      if( fabs(diff[i])<minDiff ){
        minDiff=fabs(diff[i]);
        minIndex=i;
      }
    }
    if( (minIndex-1)<0 || (minIndex+1)>(dim-1) ){
      printf("findZero() error: the sign of values can not be determined.  \
          zeroIndex=%d\n", minIndex);
      free(diff);
      return -1;
    }
    if( diff[minIndex-1]*diff[minIndex+1]<0.0 ){ // it is a crossing
      //linear interpolation
      double interpolate=-(diff[minIndex+1]+diff[minIndex-1])/
                         (diff[minIndex+1]-diff[minIndex-1]);
      *zero=(double)(x1+minIndex+interpolate)/(nDim-1);
      zeroCrossing=*zero;
      free(diff);
      return 0;
    }
  }//else

  printf("findZero(): no intersection found\n");
  free(diff);
  return -1;
}

void DefocusFinder::setExpDefocus(double expDef)
{
  expDefocus=expDef/1000.0; //convert to microns;
  // convert relative frequency to absolute;
  double q=(0.5/pixelSize)*zeroCrossing; 
  // theta=theta'(cs/lambda)^0.25, theta'=q*lambda;
  double theta=q*wavelength*csTwo; 

  double defInc=0.001; //microns;
  int dim=(ZERO_END-ZERO_START)/defInc+1;
  double *wtheta=new double[dim];
  int i,j;
  for(i=0;i<dim;i++){
    q=(0.5/pixelSize)*(i*defInc+ZERO_START);
    theta=q*wavelength*csTwo;
    wtheta[i]=2.0*MY_PI*( theta*theta*theta*theta/4.0-(theta*theta/2.0)
              *(expDef/csOne) );
    //wtheta=2*pi*(theta**4/4.0 - theta**2 * deltaZ); deltaZ=-deltaZ'/cs1;
  }

  printf("Searching first zero from %4.2f to %4.2f micron with %d knots\n",
      ZERO_START, ZERO_END,dim);

  printf("Zero: Original wtheta=");
  for(i=0;i<6;i++) printf("%f \t", wtheta[i]);
  printf("\n");

  
  for(i=0;i<dim;i++)
   wtheta[i]= fabs( sqrt(1-ampRatio*ampRatio)*-sin(wtheta[i]) + 
                    ampRatio*cos(wtheta[i]) );
 
  //sorting;
  int tempIndex;
  double temp;
  int *index=new int[dim];
  for(i=0;i<dim;i++) index[i]=i;
  for(i=0;i<dim;i++)
   for(j=i+1;j<dim;j++){
      if( wtheta[i]>wtheta[j] ){
        temp=wtheta[i];
        wtheta[i]=wtheta[j];
        wtheta[j]=temp;

        tempIndex=index[i];
        index[i]=index[j];
        index[j]=tempIndex;
      }
   } 

  printf("Zero: Sorted wtheta=");
  for(i=0;i<6;i++) printf("%f \t", wtheta[i]);
  printf("\n");

  printf("Zero: index=");
  for(i=0;i<6;i++) printf("%d \t", index[i]);
  printf("\n");

  //select the smallest value of index[0...5];
  tempIndex=index[0];
  for(i=1;i<6;i++) if(tempIndex>index[i]) tempIndex=index[i];

  expZero=(ZERO_START+tempIndex*defInc);
  delete[] wtheta;
  delete[] index;
  return ;
}


/*void DefocusFinder::setExpDefocus(double expDef){
    expDefocus=expDef;
    expZero=sqrt(csOne/expDefocus)*pixelSize*2.0/(wavelength*csTwo);
}*/

/*int DefocusFinder::findDefocus(double *focus)
{
  *focus=zeroCrossing*wavelength*csTwo/(pixelSize*2.0);
  *focus=csOne/((*focus)*(*focus));
  defocus=*focus;
}*/

int DefocusFinder::findDefocus(double *focus)
{
  // convert relative frequency to absolute;
  double q=(0.5/pixelSize)*zeroCrossing; 
  // theta=theta'(cs/lambda)^0.25, theta'=q*lambda;
  double theta=q*wavelength*csTwo; 

  double defInc=0.01; //microns;
  int dim=-(DEF_END-DEF_START)/defInc+1;
  double *wtheta=new double[dim];
  int i,j;
  for(i=0;i<dim;i++)
    wtheta[i]=2.0*MY_PI*( theta*theta*theta*theta/4.0-(theta*theta/2.0)
                         *-(DEF_START-i*defInc)/csOne );
    //wtheta=2*pi*(theta**4/4.0 - theta**2 * deltaZ); deltaZ=-deltaZ'/cs1;

  printf("Defocus without considering amp contrast=%f cs1=%f  cs2=%f \n",
      0.5*(theta*theta*theta*theta+2)*csOne/(theta*theta), csOne, csTwo );
  printf("First zero=%f (1/nm) theta=%f voltage=%d Kv  wavelength=%f nm \n", 
      q, theta, voltage, wavelength);
  printf("Searching defocus from %4.2f to %4.2f micron with %d knots\n", 
      DEF_START, DEF_END,dim);

  printf("Original wtheta=");
  for(i=0;i<6;i++) printf("%f \t", wtheta[i]);
  printf("\n");

  
  for(i=0;i<dim;i++)
   wtheta[i]= fabs( sqrt(1-ampRatio*ampRatio)*-sin(wtheta[i]) + 
                         ampRatio*cos(wtheta[i]) );
 
  //sorting;
  int tempIndex;
  double temp;
  int *index=new int[dim];
  for(i=0;i<dim;i++) index[i]=i;
  for(i=0;i<dim;i++)
   for(j=i+1;j<dim;j++){
      if( wtheta[i]>wtheta[j] ){
        temp=wtheta[i];
        wtheta[i]=wtheta[j];
        wtheta[j]=temp;

        tempIndex=index[i];
        index[i]=index[j];
        index[j]=tempIndex;
      }
   } 

  printf("Sorted wtheta=");
  for(i=0;i<6;i++) printf("%f \t", wtheta[i]);
  printf("\n");

  printf("index=");
  for(i=0;i<6;i++) printf("%d \t", index[i]);
  printf("\n");

  //select the smallest value of index[0...5];
  tempIndex=index[0];
  for(i=1;i<6;i++) if(tempIndex>index[i]) tempIndex=index[i];

  *focus=-(DEF_START-tempIndex*defInc);
  defocus=*focus; 
  delete[] wtheta;
  delete[] index;
  return 0;
}
