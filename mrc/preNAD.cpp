/** @file preNAD.cpp
 *
 * \brief Pre-NAD filter adapted for IMOD.
 *
 * \mainpage Pre-NAD filter in IMOD.
 * Electron tomography produces highly magnified 3D image volumes useful for investigating the structure and function of cellular components. Image quality is degraded by multiple scattering events and quantum noise, which depend on the angle at which individual tilt projections are collected. We have adapted a biomedical imaging approach to improve image quality by enhancing individual tilt projections prior to volumetric reconstruction. Here is implemented a Non-linear Anisotropic Diffusion (NAD) filter parameterized by the tilt angle.
 *
 * \tableofcontents
 *
 * @param inputFile
 * @param "-input OR -InputAlignedTilt"   Input Aligned tilt stack file
 * @param "-output OR -OutputFileName"    Output Aligned tilt stack file
 * @param "-angles OR -AnglesFile"	  tlt file (file with the tilt angles)
 * @param "-MVD OR -MaskedVarianceDifference"  (optional) Float Masked Variance Difference
 * @param "-s OR -sigma"  Float \f$\sigma\f$ value
 * @param "-minite OR -MinIterations"   minimum number of iterations
 * @param "-maxite OR -MaxIterations"   maximum number of iterations
 *
 *
 *  @author   Mauro Maiorca, of the Biochemistry & Molecular Biology Department, Bio21 institute, University of Melbourne, Australia, contributed the preNAD program. 
 *  It uses recursive line filter routines from Gregoire Malandain, covered by version 3 of the GPL (see GPL-3.0.txt). 
 *  Please quote Maiorca, M., et al., J Struct Biol (2012) 180, 132-42. 
 *  This work was supported by funding from the Australian Research Council and the National Health and Medical Research Council.
 *
 */
#include <limits>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <iterator>
#include <string>
#include <vector>

#include "b3dutil.h"
#include "iimage.h"
#include "sliceproc.h"
#include "ilist.h"
#include "ctfutils.h"
#include "parse_params.h"
#include "nrutil.c"
#include "dsyev3.h"
#include "cfsemshare.h"
#include "lapackc.h"
#include "recline.h"

#define MIN_ANGLE 1.0e-6	//tilt Angle less than this is treated as 0.0;
#define MY_PI 3.141592653589
#define MAXLINE 160

/**
 * TiltProjectionType: support structure for iterating through slices.
 *
 */
typedef struct
{
  /// Current iteration
  int currentIteration;

  /// Masked Variance Difference at the current iteration
  double MVD;

  /// angle in radiants (\f$\phi\f$)
  double angleRadiant;

  /// angle \f$\phi\f$ in degrees
  double angleDegrees;

  /// \f$\sigma\f$ for the current tilt projection 
  double sigma;

  /// \f$\skip\f$ the current tilt projection won't be processed if this is set to true
  bool skipMe;


  /// Current tilt projection
  int TiltNumber;

  /// pointer to the processed tilt projection data at the current iteration
  float **Data;

  /// pointer to the original tilt projection data (prior to processing)
  float **DataOriginal;

} TiltProjectionType;



/**
 * fills the border with a padding
 *
 */
void FillingPadding (float **I,	// INPUT/OUTPUT image
		     unsigned int nx,	// size INPUT in x direction
		     unsigned int ny,	// size INPUT in y direction
		     unsigned int paddingSize	//padding size
  )
{

  unsigned int p, i, j;		// loop variables
  if (nx > paddingSize + 1 && ny > paddingSize + 1)
    for (p = 0; p < paddingSize; p++)
      {
	for (i = 0; i < nx + 2 * paddingSize; i++)
	  I[i][paddingSize + ny + p] = I[i][ny - 1 - p];
	for (i = 0; i < nx + 2 * paddingSize; i++)
	  I[i][p] = I[i][2 * paddingSize - p];
	for (j = 0; j < ny + 2 * paddingSize; j++)
	  I[paddingSize + nx + p][j] = I[nx - 1 - p][j];
	for (j = 0; j < ny + 2 * paddingSize; j++)
	  I[p][j] = I[2 * paddingSize - p][j];
      }

  return;
}





/**
 * Returns the index of the mean Masked Variance Difference (MVD) value
 *
 */
unsigned int
getQuartileMVDIndex (TiltProjectionType * array, int size)
{

  unsigned int medianIndex = 0;
  int i, j;
  float temp, tmpIndex;

  float *values = vector (0, size);
  int *indexes = ivector (0, size);

  //copy the vector and the index
  for (i = 0; i < size; i++)
    {
      values[i] = array[i].MVD;
      indexes[i] = i;
    }

  ///The function uses bubblesort algorithm to order the MVD values.
  for (i = (size - 1); i > 0; i--)
    {
      for (j = 1; j <= i; j++)
	{
	  if (values[j - 1] > values[j])
	    {
	      temp = values[j - 1];
	      values[j - 1] = values[j];
	      values[j] = temp;
	      tmpIndex = indexes[j - 1];
	      indexes[j - 1] = indexes[j];
	      indexes[j] = tmpIndex;

	    }
	}
    }

  ///The median is then extracted from the ordered array.
  //get the first quartileInstead
  medianIndex = indexes[(unsigned int) (size / 4.0)];

  free_vector (values, 0, size);
  free_ivector (indexes, 0, size);

  return medianIndex;
}




/**
 * gaussRecursiveDerivatives1D is an accessory function for computing 1D zero order and first order gaussian derivatives of a 2D image.
 *
 */
void gaussRecursiveDerivatives1D (
double    sigma,				 //!< standard deviation of Gaussian
unsigned int     nx,			 //!< image dimension in x direction
unsigned int     ny,			 //!< image dimension in y direction
unsigned int padding,			 //the size of the padding
double    hx,					 //!< pixel size in x direction
double    hy,					 //!< pixel size in y direction
unsigned int direction,			 //!< 0 for X, 1 for Y
unsigned int DerivativeOrder,	 //!< 0 smoothing, 1 first derivative, 2 second derivative
float    **f,					 //!< input: original image ;
float    **fo = NULL			 //!<output: smoothed (if null, input overwritten)
)								 // input: original image ;  output: smoothed
{

	unsigned int    i, j, k;	 // loop variables

	FillingPadding(f, nx,ny,padding);

	if ( fo == NULL )
	{
		fo = f;
	}
	

	//if less then a certain threshold, then just don't smooth, copy the image
	const double sigmaThreshold = 0.1;
	if (sigma < sigmaThreshold){
		for (i=padding; i<nx+padding; i++)
			for (j=padding; j<ny+padding; j++)
				fo[i][j] = f[i][j];
		FillingPadding(fo,nx,ny,padding);
		return;
	}


	//for each line, put the line in a buffer and process it.
	double * bufferIn = NULL;
	double * bufferOut = NULL;
	double * bufferTmp0 = NULL;
	double * bufferTmp1 = NULL;
	//float filterCoefs;

	//recursiveFilterType recFilter = ALPHA_DERICHE;
	recursiveFilterType recFilter = GAUSSIAN_DERICHE;
	derivativeOrder derivOrder = NODERIVATIVE;
	if (DerivativeOrder == 0)
	{
		derivOrder = SMOOTHING;
	}
	else if (DerivativeOrder == 1)
	{
		derivOrder = DERIVATIVE_1;
	}
	else if (DerivativeOrder == 2)
	{
		derivOrder = DERIVATIVE_2;
	}
	else if (DerivativeOrder == 3)
	{
		derivOrder = DERIVATIVE_3;
	}
	RFcoefficientType *rfc = InitRecursiveCoefficients( sigma, recFilter, derivOrder );
  if (!rfc)
    exitError("Allocation structure for recursive filter");

	//fill the buffer
	if (direction==0)			 //along X
	{


		bufferIn   = (double *)malloc((nx+2*padding)*sizeof(double));
		bufferOut  = (double *)malloc((nx+2*padding)*sizeof(double));
		bufferTmp0  = (double *)malloc((nx+2*padding)*sizeof(double));
		bufferTmp1  = (double *)malloc((nx+2*padding)*sizeof(double));
		
		if (bufferIn == NULL || bufferOut == NULL || bufferTmp0 == NULL || bufferTmp1 == NULL)
			exitError ("error allocating memory");

		for (j=0; j<ny+2*padding; j++)
		{
			for (k=0; k<nx+2*padding; k++)
			{
				bufferIn[k]=(double)f[k][j];
				bufferOut[k]=(double)f[k][j];
				bufferTmp0[k]=(double)f[k][j];
				bufferTmp1[k]=(double)f[k][j];
			}
			RecursiveFilter1D( rfc, bufferIn, bufferOut, bufferTmp0, bufferTmp1, nx + 2*padding);

			for (k=0; k<nx+2*padding; k++)
			{
				fo[k][j]=(float)bufferOut[k];
			}

		}

		free(bufferIn);
		free(bufferOut);
		free(bufferTmp0);
		free(bufferTmp1);



	}							 //along Y
	if (direction==1)
	{


		bufferIn   = (double *)malloc((ny+2*padding)*sizeof(double));
		bufferOut  = (double *)malloc((ny+2*padding)*sizeof(double));
		bufferTmp0  = (double *)malloc((ny+2*padding)*sizeof(double));
		bufferTmp1  = (double *)malloc((ny+2*padding)*sizeof(double));
		if (bufferIn == NULL || bufferOut == NULL || bufferTmp0 == NULL || bufferTmp1 == NULL)
			exitError ("error allocating memory");


		for (j=0; j<nx+2*padding; j++)
		{
			for (k=0; k<ny+2*padding; k++)
			{
				bufferIn[k]=(double)f[j][k];
				bufferOut[k]=(double)f[j][k];
				bufferTmp0[k]=(double)f[j][k];
				bufferTmp1[k]=(double)f[j][k];
			}
			RecursiveFilter1D(rfc, bufferIn, bufferOut, bufferTmp0, bufferTmp1, ny + 2*padding );

			for (k=0; k<ny+2*padding; k++)
			{
				fo[j][k]=(float)bufferOut[k];
			}

		}
		free(bufferIn);
		free(bufferOut);
		free(bufferTmp0);
		free(bufferTmp1);




	}

	FillingPadding(fo, nx,ny,padding);
  free(rfc);

	return;

}								 //gaussRecursiveDerivatives1D







/**
 *  pre-NAD one tilt
 *  it's the core function of the algorithm
 *  \return This function will return the Masked Variance Difference (MVD) \f$\delta\f$ between Iout and I0
 */
double
preNAD1Tilt (unsigned int nx,	/*!< Dimension (in pixels) along the axis \f$x\f$ */
	     unsigned int ny,	/*!< Dimension (in pixels) along the axis \f$y\f$ */
	     unsigned int padding,	//padding size
	     double spacingX,	/*!< Size (in nm) of a pixel along the axis \f$x\f$ */
	     double spacingY,	/*!< Size (in nm) of a pixel along the axis \f$y\f$ */
	     double sigma,	/*!< \f$\sigma\f$ */
	     double lambdaE,	/*!< Edge Enhancing diffusion parameter, \f$\lambda_e\f$ */
	     double lambdaC,	/*!< Coherence Enhancing diffusion parameter, \f$\lambda_c\f$ */
	     double lambdaH,	/*!< Hybrid diffusion parameter, \f$\lambda_h\f$ */
	     double angleRadiant,	/*!< angle of the ongoing tilt projection (in radiants) */
	     float **I,		/*!< Input image */
	     float **Iout,	/*!< Output image */
	     const float **I0 = NULL	/*!< Image for calculate the masked variance difference, by default is I */
  )
{

  if (Iout == NULL)
    {
      Iout = I;
    }

  if (I0 == NULL)
    {
      I0 = (const float **) I;
    }

  //unsigned int padding = paddingInput + sigma + 1.0;
  float **dx = matrix (0, nx + 2 * padding, 0, ny + 2 * padding);
  float **dy = matrix (0, nx + 2 * padding, 0, ny + 2 * padding);
//  float **IBlurred = matrix (0, nx + 2 * padding, 0, ny + 2 * padding);
  float **gradientMagnitudeSquare = matrix (0, nx + 2 * padding, 0, ny + 2 * padding);

  //memory for eigenvectors
  float **eV1X = matrix (0, nx + 2 * padding, 0, ny + 2 * padding);
  float **eV1Y = matrix (0, nx + 2 * padding, 0, ny + 2 * padding);
  float **eV2X = matrix (0, nx + 2 * padding, 0, ny + 2 * padding);
  float **eV2Y = matrix (0, nx + 2 * padding, 0, ny + 2 * padding);

  //memory for lambdas (EED, CED)
  float **lEED1 = dx; //re-use the memory (avoid unnecessary new allocations of memory)
  //float **lEED2 = matrix (0, nx + 2 * padding, 0, ny + 2 * padding);  //not necessary,  lEED2 is always equal to 1;
  //float **lCED1 = matrix (0, nx + 2 * padding, 0, ny + 2 * padding); //not necessary,  lCED1 is always equal to Alpha;
  float **lCED2 = dy; //re-use the memory (avoid unnecessary new allocations of memory)

  //memory for The diffusion tensor
  //D = U*I*U' = |a b|
  //             |c d|
  //    where b==c
  float **a = matrix (0, nx + 2 * padding, 0, ny + 2 * padding);
  float **b = matrix (0, nx + 2 * padding, 0, ny + 2 * padding);
  float **d = matrix (0, nx + 2 * padding, 0, ny + 2 * padding);

  //memory for epsilon image (edge condition)
  float **edgeConditionImage =  b;  //re-use the memory (avoid unnecessary new allocations of memory)


  double ContrastParameterLambdaEED = 30;
  double contrastParameterLambdaCED = 30;
  double ContrastParameterLambdaHybrid = 30;
  double contrastParameterLambdaCEDSquare = pow (contrastParameterLambdaCED, 2.0);
  double ThresholdParameterC = 3.31488;
  double zeroValueTolerance = 1e-15;
  double Alpha = 0.001;
  double timeStep = 0.125;
  double rxx = timeStep / (2.0 * spacingX * spacingX);
  double ryy = timeStep / (2.0 * spacingY * spacingY);
  double rxy = timeStep / (4.0 * spacingX * spacingY);

  //structure tensor enhancers
  const unsigned int convolutionPrecision = sigma + 1.0;
  const double magnifyGradientRatio = (1.0 / convolutionPrecision) * log ((double) convolutionPrecision);
  //const double StructureTensorEnhancingRatio = 4 * log ((double) (16 * convolutionPrecision + 1));


  //gaussian blurred image (it might no be needed)
//  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 0, 0, I, IBlurred);
//  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 1, 0, IBlurred, IBlurred);

  //gaussian first order derivative
  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 0, 1, I, dx);
  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 1, 1, I, dy);

	/**
	 * The grayscale intensity, I, of each image is regarded as a density that is redistributed by a conservative diffusive process. The equilibration of intensity due to its inhomogeneity is determined by Fick's law of diffusion, so the time-evolution of I is obtained by solving the diffusion equation:
	 * \f{eqnarray*}{
	 *   \partial_{t}I = \nabla \cdot (\mathbf{D}\cdot \nabla I )
	 * \f}
	 * where \f$\mathbf{D}\f$ is the diffusion tensor. If it can be assumed that \f$\mathbf{D}\f$ is constant over the entire domain of I, the diffusion process is described as homogeneous and if it depends only on position it is described as homogeneous. Here, however, it is assumed that D depends on the local nature of the time evolution of I and the diffusion process that defines the filtering procedure is consequently described as non-linear, inhomogeneous and anisotropic.
	 */
  //double mean = 0;

  double meanSquared = 0.0;
  const double counter = (nx) * (ny);

  for (unsigned int i = 0; i < nx + 2 * padding; i++)
    for (unsigned int j = 0; j < ny + 2 * padding; j++)
      {

	//Iout[i][j]=dy[i][j];
		/**
		 * The diffusion tensor (\f$\mathbf{D}\f$) can be designed starting from the diffusion tensor :
		 *
		 *  \f{eqnarray*}{
		 *  \mathbf{J}=\nabla I \cdot \nabla I^T
		 *  \f}
		 *
		 */
	// D= | a b |
	//    | b d | (Hermitian Matrix)
	a[i][j] = pow (dx[i][j], (float) 2.0);
	b[i][j] = dx[i][j] * dy[i][j];
	d[i][j] = pow (dy[i][j], (float) 2.0);
	gradientMagnitudeSquare[i][j] = magnifyGradientRatio * (a[i][j] + d[i][j]);
      }


  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 0, 0, a, a);
  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 1, 0, a, a);

  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 0, 0, b, b);
  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 1, 0, b, b);

  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 0, 0, d, d);
  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 1, 0, d, d);

  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 0, 0, gradientMagnitudeSquare,  gradientMagnitudeSquare);
  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 1, 0, gradientMagnitudeSquare, gradientMagnitudeSquare);



  double mean = 0;
  for (unsigned int i = 0; i < nx + 2 * padding; i++)
    for (unsigned int j = 0; j < ny + 2 * padding; j++)
      {

		/** The structure tensor matrix is thus a positive definite symmetric matrix. It is possible to use eigen-analysis to rewrite \f$\mathbf{J}\f$ as :
		 *  \f{eqnarray*}{
		 *  \mathbf{J}=[\mathbf{u_1},\, \mathbf{u_2}] \cdot \left[ \begin{array}{cc} \mu_1 & 0 \\ 0& \mu_2 \end{array}\right] \cdot [\mathbf{u_1},\mathbf{u_2}]^T
		 *  \f}
		 *  In our implementation we used the Jacobi method for computing eigenvalues \f$\mu_1\f$ and \f$\mu_2\f$ (\f$\mu_1\f$ < \f$\mu_2\f$), and the corresponding eigenvectors \f$\mathbf{u_1}\f$ and \f$\mathbf{u_2}\f$.			 */

	/*
	 *  DLAEV2 computes the eigendecomposition of a 2-by-2 symmetric matrix
	 *     [  A   B  ]
	 *     [  B   C  ].
	 *  On return, RT1 is the eigenvalue of larger absolute value, RT2 is the
	 *  eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right
	 *  eigenvector for RT1, giving the decomposition
	 *
	 *     [ CS1  SN1 ] [  A   B  ] [ CS1 -SN1 ]  =  [ RT1  0  ]
	 *     [-SN1  CS1 ] [  B   C  ] [ SN1  CS1 ]     [  0  RT2 ].
	 */

	//note: dlaev2 already sorts the eigenvalues by absolute value
	double mu1 = 0.0, mu2 = 0.0;
	double cs1 = 0.0, sn1 = 0.0;
	dlaev2 ((double *) &(a[i][j]), (double *) &(b[i][j]),
		(double *) &(d[i][j]), &mu1, &mu2, &cs1, &sn1);
	mu1 = fabs (mu1);
	mu2 = fabs (mu2);
	eV1X[i][j] = cs1;
	eV1Y[i][j] = sn1;
	eV2X[i][j] = -sn1;
	eV2Y[i][j] = cs1;



	//(alternative) calculate eigenvalues/vector using dsyev
/*		double valuesInput[4] ={ a[i][j], 0.0, b[i][j], d[i][j] };
		int NN = 2, lwork=-1, LDA = 2, info;
		double w[NN];
		double wkopt;
		double* work;
		dsyev("Vectors", "Upper", &NN, valuesInput, &LDA, w, &wkopt, &lwork, &info );
		lwork = (int)wkopt;
		work = (double*)malloc( lwork*sizeof(double) );
		dsyev("Vectors", "Upper", &NN, valuesInput, &LDA, w, &wkopt, &lwork, &info );
		mu1=fabs(w[0]);
		//mu2=fabs(StructureTensorEnhancingRatio*w[1]);
		mu2=fabs(w[1]);
		eV1X[i][j] = valuesInput[0];
		eV1Y[i][j] = valuesInput[2];
		eV2X[i][j] = valuesInput[1];
		eV2Y[i][j] = valuesInput[3];
		free( (void*)work );
*/

		/**
		 * The Diffusion tensor \f$\mathbf{D}\f$ is also calculated using eigen analysis,
		 *  \f{eqnarray*}{
		 *  \mathbf{D} = [\mathbf{v_1},\, \mathbf{v_2}] \cdot\left[ \begin{array}{cc} \lambda_1(\epsilon(\lambda_c(\mu_1,\mu_2,\phi),\lambda_e(\mu_1,\mu_2,\phi) )) &0\\0&\lambda_2(\epsilon(\lambda_c(\mu_1,\mu_2,\phi),\lambda_e(\mu_1,\mu_2,\phi))) \end{array} \right]\cdot[ \mathbf{v_1},\, \mathbf{v_2}]^T
		 *  \f}
		 * where (\f$\lambda_c\f$) and (\f$\lambda_e\f$) are the Coherence Enhancing Diffusion (CED) and Edge Enhancing Diffusion (EED) parameter functions.
		 */

	//compute Lambdas for EED
		/**
		 *  In particular, the Edge Enhancing Diffusion (EED) parameters are:
		 *  \f{eqnarray*}{
		 *  \lambda_{e_1}(\phi)  &=&  \begin{cases}{ 1  & \textrm{ if } |\nabla I_{\sigma(\phi)}|^2 = 0 \cr
		 *  1-e^{\frac{-C}{(|\nabla I_{\sigma(\phi)}|^2/{\lambda_{e}}^2)^4}}  & $\textrm{ if } | \nabla I_{\sigma(\phi)}|^2 > 0 }\end{cases}\cr
		 *  \lambda_{e_2}(\phi)  &=&  1
		 *  \f}
		 * where C = 3.31488 is a threshold parameter and lambda_e (= 30 by default) is a user supplied parameter.
		 */
	double LambdaEED1 = 1.0;
	//double LambdaEED2 = 1.0;

	double gradientMagnitude = sqrt (gradientMagnitudeSquare[i][j]);
	double ratio;
	double expVal;
	double kappa;

	if (gradientMagnitude > zeroValueTolerance)
	  {
	    ratio = (gradientMagnitudeSquare[i][j]) / pow (ContrastParameterLambdaEED, 2.0);
	    expVal = exp ((-1.0 * ThresholdParameterC) / (pow (ratio, 4.0)));
	    LambdaEED1 = 1.0 - expVal;
	  }

	//compute Lambda's for CED
		/**
		 *  The Coherence Enhancing Diffusion (CED) parameters are defined as
		 *  \f{eqnarray*}{
		 *  \lambda_{c_2}(\phi)  &=&  1\cr
		 *  \lambda_{c_1}(\phi)  &=&  \begin{cases}{ 1  & \textrm{ if } \mu_2 = 0 \cr
		 *  \alpha+(i-\alpha)-e^{-\frac{log(2){\lambda_{c}}^2}{K(\phi)}}  & \textrm{ otherwise } }\end{cases}
		 *  \f}
		 * where
		 *  \f{eqnarray*}{
		 *  K (\phi ) =\left[\frac{\mu_1(\phi)}{\alpha+\mu_2(\phi)}\right]^4
		 *  \f}
		 */
	//double LambdaCED1 = Alpha;
	double LambdaCED2 = 1.0;
	double tmp;

	//(always rememember: mu2>mu1)
	if (fabs (mu1) > zeroValueTolerance)
	  {
	    kappa = pow (mu1 / (Alpha + mu2), 1.0);
	    tmp = (-1.0 * (log (2.0) * contrastParameterLambdaCEDSquare) / kappa);
	    expVal = exp (tmp);
	    LambdaCED2 = Alpha + (1.0 - Alpha) * expVal;
	  }

	// Compute the final lambdas for the continous switch
		/**
		 *  The edge condition formula is calculated using
		 *  \f{eqnarray*}{
		 *  \xi (\phi) = 4log(1+\alpha+\mu_1(\phi) -\mu_2(\phi) ) + 4log(1+\alpha+\mu_1 (\phi) /( \alpha+\mu_2(\phi) )).
		 *  \f}
		 * which leads to the continuous switch formula (\f$\epsilon\f$).
		 */
	double edgeCondition = 4.0 * log (1.0 + Alpha + fabs (mu2 - mu1)) + 4.0 * log (1.0 +  Alpha +  (mu2) /  (Alpha +  mu1));

	//fill eigenvectors
	//eV1X[i][j] = V[1][1];
	//eV1Y[i][j] = V[2][1];
	//eV2X[i][j] = V[1][2];
	//eV2Y[i][j] = V[2][2];

	//fill lambdas (EED, CED)
	if (LambdaEED1 < Alpha) LambdaEED1 = 0.0;
	if (LambdaCED2 < Alpha) LambdaCED2 = 0.0;
	lEED1[i][j] = LambdaEED1;
	lCED2[i][j] = LambdaCED2;

	//fill epsilon image (edge condition)
	edgeConditionImage[i][j] = edgeCondition;

	//some statistics for computing the final epsilon
	mean = mean + edgeCondition;
	meanSquared = edgeCondition * edgeCondition;
      }

  mean = mean / (1.0 + counter);
  double variance = 0;

  for (unsigned int i = 0; i < nx + 2 * padding; i++)
    for (unsigned int j = 0; j < ny + 2 * padding; j++)
      {
	variance += fabs (edgeConditionImage[i][j] - mean);
      }
  variance = variance / (nx * ny - 1.0);

  double counterMVD = 0;
  double meanMVD = 0;
  double varianceMVD = 0;

  for (unsigned int i = 0; i < nx + 2 * padding; i++)
    for (unsigned int j = 0; j < ny + 2 * padding; j++)
      {
		/**
		 *  The continuous switch formula discriminates is a continuous manner edges from uniform (coherent) regions
		 *  \f{eqnarray*}{
		 *    \epsilon (\phi) = \frac{e}{e+e^{-\frac{(\xi (\phi) - \bar{\xi} (\phi))}{\lambda_{h_i,\phi}} } }
		 *  \f}
		 * \f$\bar{\xi}(\phi)\f$ is the mean value of \f$\xi(\phi)\f$ taken over all the pixels in the tilt projection at angle \f$\phi\f$ and \f$\xi (\phi)\f$.
		 */
	double t = edgeConditionImage[i][j];

	//note: K little => more edges. Use little K with noisy images (ET)
	//ContrastParameterLambdaHybrid also known as K
	t = (t - mean) / ContrastParameterLambdaHybrid;
	double continuousSwitch = exp (1.0) / (exp (1.0) + exp (-1.0 * t));

	if (t > mean)
	  {
	    meanMVD += t;
	    counterMVD++;
	  }

	if (continuousSwitch >= 1.0)
	  continuousSwitch = 1.0;
	if (continuousSwitch < Alpha)
	  continuousSwitch = 0.0;

	//1 are edges, 0 are coherence
	double lEED2 = 1.0;
	double lCED1 = Alpha;
	double Lambda1 = (1.0 - continuousSwitch) * lCED1 + continuousSwitch * lEED1[i][j];
	double Lambda2 = (1.0 - continuousSwitch) * lCED2[i][j] + continuousSwitch * lEED2;
	if (Lambda1 < Alpha)  Lambda1 = Alpha;
	if (Lambda2 < Alpha)  Lambda2 = Alpha;
	if (Lambda1 > 1.0 - Alpha)  Lambda1 = 1.0;
	if (Lambda2 > 1.0 - Alpha)  Lambda2 = 1.0;

	//D = U*I*U' = |a b|
	//             |c d|
	//    where b==c
	a[i][j] = Lambda1 * pow (eV1X[i][j], float (2.0)) + Lambda2 * pow (eV1Y[i][j], float (2.0));
	b[i][j] = Lambda1 * (eV1X[i][j] * eV2X[i][j]) + Lambda2 * (eV1Y[i][j] * eV2Y[i][j]);
	d[i][j] = Lambda1 * pow (eV2X[i][j], float (2.0)) + Lambda2 * pow (eV2Y[i][j], float (2.0));

      }

  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 0, 0, a, a);
  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 1, 0, a, a);

  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 0, 0, b, b);
  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 1, 0, b, b);

  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 0, 0, d, d);
  gaussRecursiveDerivatives1D (sigma, nx, ny, padding, spacingX, spacingY, 1, 0, d, d);

  counterMVD = 0;
  for (unsigned int i = padding; i < nx + padding; i++)
    for (unsigned int j = padding; j < ny + padding; j++)
      {
		/**
		 * The final approximation for the diffusion tensor  \f$\mathbf{D}\f$ is finally performed using the central difference theorem.
		 * \f{eqnarray*}{
		 *  \mathbf{D} = [\mathbf{v_1},\, \mathbf{v_2}] \cdot\left[ \begin{array}{cc} \lambda_1&0\\0&\lambda_2\end{array} \right]\cdot[ \mathbf{v_1},\, \mathbf{v_2}]^T
		 *  \f}
		 */

	/* //CLASSIC EVOLUTION (OBSOLETE METHOD)
				double wXX = rxx*( a[i+1][j]*(I[i+1][j]-I[i][j]) -a[i-1][j]*(I[i][j]-I[i-1][j]) );
				double wXY = rxy*( b[i+1][j]*(I[i+1][j+1]-I[i+1][j-1]) -b[i-1][j]*(I[i-1][j+1]-I[i-1][j-1]) );
				double wYX = rxy*( b[i][j+1]*(I[i+1][j+1]-I[i-1][j+1]) -b[i][j-1]*(I[i+1][j-1]-I[i-1][j-1]) );
				double wYY = ryy*( d[i][j+1]*(I[i][j+1]-I[i][j]) -d[i][j-1]*(I[i][j]-I[i][j-1]) );
				Iout[i][j]= I[i][j] + wXX + wXY + wYX + wYY;
	*/
	//EVOLUTION
	double wE  =   rxx * (a[i+1][j]   + a[i][j]) - rxy * (sqrt(b[i+1][j] * b[i+1][j]) + sqrt(b[i][j] * b[i][j]));
	double wW  =   rxx * (a[i-1][j]   + a[i][j]) - rxy * (sqrt(b[i-1][j] * b[i-1][j]) + sqrt(b[i][j] * b[i][j]));
	double wS  =   ryy * (d[i][j+1]   + d[i][j]) - rxy * (sqrt(b[i][j+1] * b[i][j+1]) + sqrt(b[i][j] * b[i][j]));
	double wN  =   ryy * (d[i][j-1]   + d[i][j]) - rxy * (sqrt(b[i][j-1] * b[i][j-1]) + sqrt(b[i][j] * b[i][j]));
	double wSE =   rxy * (b[i+1][j+1] + b[i][j] + sqrt(b[i+1][j+1] * b[i+1][j+1]) + sqrt(b[i][j] * b[i][j]));
	double wNW =   rxy * (b[i-1][j-1] + b[i][j] + sqrt(b[i-1][j-1] * b[i-1][j-1]) + sqrt(b[i][j] * b[i][j]));
	double wNE =   rxy * ( - b[i+1][j-1] - b[i][j] + sqrt(b[i+1][j-1] * b[i+1][j-1]) + sqrt(b[i][j] * b[i][j]));
	double wSW =   rxy * ( - b[i-1][j+1] - b[i][j] + sqrt(b[i-1][j+1] * b[i-1][j+1]) + sqrt(b[i][j] * b[i][j]));
	Iout[i][j] = I[i][j]
		+ wE  * (I[i+1][j]   - I[i][j])
		+ wW  * (I[i-1][j]   - I[i][j])
		+ wS  * (I[i][j+1]   - I[i][j])
		+ wN  * (I[i][j-1]   - I[i][j])
		+ wSE * (I[i+1][j+1] - I[i][j])
		+ wNW * (I[i-1][j-1] - I[i][j])
		+ wSW * (I[i-1][j+1] - I[i][j])
		+ wNE * (I[i+1][j-1] - I[i][j]);

	//apply the mask
	//no edge image
	if (edgeConditionImage[i][j] > mean)
	  {
	    meanMVD += fabs (Iout[i][j] - I0[i][j]);
	    counterMVD++;
	  }
      }
  meanMVD = meanMVD / (counterMVD + 1);

  varianceMVD = 0.0;

  for (unsigned int i = padding; i < nx + padding; i++)
    for (unsigned int j = padding; j < ny + padding; j++)
      {
	//there is no edge (coherence region)
	if (edgeConditionImage[i][j] > mean)
	  {
	    varianceMVD =
	      varianceMVD + pow (fabs (Iout[i][j] - I0[i][j]) - meanMVD, 2);
	  }
      }
  varianceMVD = sqrt (varianceMVD) / (counterMVD + 1);


  //clear buffers
  free_matrix (dx, 0, nx + 2 * padding, 0, ny + 2 * padding);
  free_matrix (dy, 0, nx + 2 * padding, 0, ny + 2 * padding);
//  free_matrix (IBlurred, 0, nx + 2 * padding, 0, ny + 2 * padding);
  free_matrix (gradientMagnitudeSquare, 0, nx + 2 * padding, 0, ny + 2 * padding);
//  free_matrix (edgeConditionImage, 0, nx + 2 * padding, 0, ny + 2 * padding);

  free_matrix (eV1X, 0, nx + 2 * padding, 0, ny + 2 * padding);
  free_matrix (eV1Y, 0, nx + 2 * padding, 0, ny + 2 * padding);
  free_matrix (eV2X, 0, nx + 2 * padding, 0, ny + 2 * padding);
  free_matrix (eV2Y, 0, nx + 2 * padding, 0, ny + 2 * padding);

//  free_matrix (lEED1, 0, nx + 2 * padding, 0, ny + 2 * padding);
//  free_matrix (lEED2, 0, nx + 2 * padding, 0, ny + 2 * padding);
//  free_matrix (lCED1, 0, nx + 2 * padding, 0, ny + 2 * padding);
//  free_matrix (lCED2, 0, nx + 2 * padding, 0, ny + 2 * padding);

  free_matrix (a, 0, nx + 2 * padding, 0, ny + 2 * padding);
  free_matrix (b, 0, nx + 2 * padding, 0, ny + 2 * padding);
  free_matrix (d, 0, nx + 2 * padding, 0, ny + 2 * padding);

  return varianceMVD;
}


/**
 *  Automatic Stop condition and iteration through the whole dataset
 *
 */
void
automaticPreNAD (
/// stack in
		  TiltProjectionType * stackIn,
/// stack out
		  TiltProjectionType * stackOut,
/// dimension along the axis \f$x\f$
		  unsigned int nx,
/// dimension along the axis \f$y\f$
		  unsigned int ny,
/// number of tilt projections
		  unsigned int nz,
/// the size of border padding
		  unsigned int padding,
/// minimum number of iterations
		  unsigned int MinIterations,
/// maximum number of iterations
		  unsigned int MaxIterations,
/// spacing (in nm) along  \f$x\f$
		  double spacingX,
/// spacing (in nm) along  \f$y\f$
		  double spacingY,
/// \f$\lambda_e\f$
		  double lambdaE,
/// \f$\lambda_c\f$
		  double lambdaC,
/// \f$\lambda_h\f$
		  double lambdaH)
{

  unsigned int ii = 0;

  printf ("START (t=tilt, a=angle, i=iteration, MVD=masked variance difference)\n");

  //We have to reach the min number of iterations for each tilt angles
  for (ii = 0; ii < nz; ii++)
    {
      for (unsigned int it = 0; it < MinIterations && !stackIn[ii].skipMe; it++)
	{
	  stackOut[ii].MVD =
	    preNAD1Tilt (nx, ny, padding, spacingX, spacingY,
			 stackOut[ii].sigma, lambdaE, lambdaC, lambdaH,
			 stackOut[ii].angleRadiant, stackOut[ii].Data,
			 stackOut[ii].Data,
			 (const float **) stackIn[ii].Data);
	  printf ("[t=%d,a=%f,i=%d,MVD=%f]", ii, stackOut[ii].angleDegrees, it, stackOut[ii].MVD);
	}
      printf ("\n");
    }

  printf ("\n\nSearch for the maximum(minimum MVD)\n");

  printf ("\n(iteration, tilt, MVD)=\n");
  unsigned int indexTargetMVD = getQuartileMVDIndex (stackOut, nz);

  double stopMVD = stackOut[indexTargetMVD].MVD;

  for (unsigned int it = MinIterations; it < MaxIterations && !stackIn[ii].skipMe; it++)
    {
      stopMVD = stackOut[indexTargetMVD].MVD;
      stackOut[indexTargetMVD].MVD =
	preNAD1Tilt (nx, ny, padding, spacingX, spacingY,
		     stackOut[indexTargetMVD].sigma, lambdaE, lambdaC,
		     lambdaH, stackOut[ii].angleRadiant,
		     stackOut[indexTargetMVD].Data,
		     stackOut[indexTargetMVD].Data,
		     (const float **) stackIn[indexTargetMVD].Data);
      stackOut[indexTargetMVD].currentIteration = it;
      stopMVD = (stopMVD + stackOut[indexTargetMVD].MVD) / 2;
      printf ("[t=%d,a=%f,i=%d,MVD=%f] ", ii, stackOut[ii].angleDegrees, it, stackOut[ii].MVD);
    }

  printf ("\n");
  printf ("(stopMVD)=%f\n", stopMVD);

  printf ("\n\n\nClassic computation (t=tilt, a=angle, i=iteration, MVD=masked variance difference):\n");
  for (unsigned int ii = 0; ii < nz; ii++)
    {
      for (unsigned int it = MinIterations; it < MaxIterations && !stackIn[ii].skipMe; it++)
	{
	  if (stackOut[ii].currentIteration < (int) MaxIterations && stackOut[ii].MVD <= stopMVD)
	    {
	      stackOut[ii].currentIteration = it;
	      stackOut[ii].MVD =
		preNAD1Tilt (nx, ny, padding, spacingX, spacingY,
			     stackOut[ii].sigma, lambdaE, lambdaC, lambdaH,
			     stackOut[ii].angleRadiant, stackOut[ii].Data,
			     stackOut[ii].Data,
			     (const float **) stackIn[ii].Data);
	      printf ("[t=%d,a=%f,i=%d,MVD=%f] ", ii, stackOut[ii].angleDegrees, it, stackOut[ii].MVD);
	    }
	}
      printf ("\n");
    }
    printf("\n");

}


/**
 *  Main()
 *
 */
int
main (int argc, char *argv[])
{

  char *stackFn, *angleFn, *outFn, *ViewsToProcess;
  int startingView;
  int MinIterations = -1;
  int MaxIterations = -1;
  float MaskedVarianceDifference = -1.0;

  char *progname = imodProgName (argv[0]);

  int numOptArgs, numNonOptArgs;
  int numOptions = 8;
  float sigma = 1.0;
  unsigned int padding = 40;
  bool rangeRequested = false;
  bool noTiltAngle = false;


  //parselist of views
  int *anglesToProcess;
  int nAnglesToProcess = 0;

  const char *options[] = {
    "input:InputStack:FN:",
    "output:OutputFileName:FN:",
    "angles:AnglesFile:FN:",
    "MVD:MaskedVarianceDifference:F:",
    "s:sigma:F:",
    "minite:MinIterations:I:",
    "maxite:MaxIterations:I:",
    "views:ViewsToProcess:LI:"
  };

  printf ("\n [Mauro Maiorca, of the Biochemistry & Molecular Biology Department, Bio21 institute, University of Melbourne, Australia, contributed the preNAD program (adapted for IMOD). ");
  printf ("It uses recursive line filter routines from Gregoire Malandain, covered by version 3 of the GPL (see GPL-3.0.txt). ");
  printf ("Please quote Maiorca, M., et al., J Struct Biol (2012) 180, 132-42. This work was supported by funding from the Australian Research Council and the National Health and Medical Research Council.]\n\n");
  printf (" examples of use:\n  standard:\n    preNAD -input myInputImage.mrc  -output myOutputImage.ali -angles myTiltAnglesFile.tlt -s 3 -minite 6 -maxite 8\n");
  printf ("  only tilt projections number 1 2 8 15 18 19 20:\n    preNAD -input myInputImage.mrc  -output myOutputImage.ali -angles myTiltAnglesFile.tlt -s 3 -minite 6 -maxite 8 -views 1-2,8,15,18-20\n\n\n");

  PipReadOrParseOptions (argc, argv, options, numOptions, progname, 1, 0, 0, &numOptArgs, &numNonOptArgs, NULL);
  if (PipGetString ("InputStack", &stackFn))
    exitError ("No stack specified");
  if (PipGetString ("OutputFileName", &outFn))
    exitError ("OutputFileName is not specified");
  if (!PipGetString ("ViewsToProcess", &ViewsToProcess)){
      rangeRequested = true;
      anglesToProcess=parselist(ViewsToProcess, &nAnglesToProcess);
  }
  if (PipGetString ("AnglesFile", &angleFn))
    {
      angleFn = NULL;
      printf ("[Warning] No angle file is specified, tilt angles are assumed to be all 0.0 degrees.\n");
      noTiltAngle = true;
    }
  if (PipGetFloat ("sigma", &sigma))
    {
      sigma = 1.0;
      printf ("No sigma file is specified, sigma is assumed to be 1.0\n");
    }



  PipGetInteger ("MinIterations", &MinIterations);
  PipGetInteger ("MaxIterations", &MaxIterations);
  PipGetFloat ("MaskedVarianceDifference", &MaskedVarianceDifference);

  //manual
  if (MaskedVarianceDifference > 0.0)
    {
      if (MaxIterations < 0)
	{
	  MaxIterations = 6;
	  printf ("MaxIterations not properly specified, is assumed to be %d\n",  MaxIterations);
	}
    }
  else
    {
      if (!(MaxIterations > 0 && MinIterations > 0 && MaxIterations >= MinIterations))
	{
	  if (MaxIterations < 0 && MinIterations < 0)
	    {
	      MinIterations = 3;
	      MaxIterations = 6;
	      printf ("MaxIterations and MinIterations are not properly specified, assumed to be %d and %d\n", MinIterations, MaxIterations);
	    }
	  else if (MaxIterations < 0 && MinIterations > 0)
	    {
	      MaxIterations = MinIterations + 3;
	      printf ("MaxIterations is not properly specified, assumed to be %d\n", MaxIterations);
	    }
	  else if (MaxIterations > 3 && MinIterations < 0)
	    {
	      MinIterations = MaxIterations - 3;
	      printf ("MinIterations is not properly specified, assumed to be %d\n", MinIterations);
	    }
	  else if (MaxIterations >= 1 && MinIterations < 0)
	    {
	      MinIterations = 1;
	      printf ("MinIterations is not properly specified, assumed to be %d\n", MinIterations);
	    }
	  else if (MaxIterations == 0 || MinIterations == 0)
	    {
	      printf ("Warning: you might have no iterations for some tilt projections, I assume you know what you are doing\n");
	    }
	  else
	    {
	      exitError ("MaxIterations/MinIterations not properly specified\n");
	    }
	}
    }

  printf ("parameters:\n");
  printf ("       stackFn = \"%s\", outFn=\"%s\",  angleFn=\"%s\" ", stackFn,
	  outFn, angleFn);
  printf ("\n       ");
  if (MinIterations > 0)
    printf ("minite=%d ", MinIterations);
  if (MaxIterations > 0)
    printf ("maxite=%d ", MaxIterations);
  if (MaskedVarianceDifference > 0)
    printf ("MaskedVarianceDifference=%f", MaskedVarianceDifference);
  printf ("\n");

  FILE *fpStack;
  if ((fpStack = iiFOpen (stackFn, "rb")) == 0)
    {
      printf ("error 0.5\n");
      exitError ("could not open input file %s", stackFn);
    }

  FILE *foutput;

  MrcHeader header;
  MrcHeader outHeader;
  int sliceMode;
  /* read header */
  if (mrc_head_read (fpStack, &header))
    {
      printf ("Error 2\n");
      exitError ("reading header of input file %s", stackFn);

    }

  if (mrc_head_read (fpStack, &outHeader))
    {
      printf ("Error 3\n");
      exitError ("reading header of input file %s", stackFn);
    }

  //check the file
  sliceMode = sliceModeIfReal (header.mode);
  if (sliceMode < 0)
    exitError ("File mode is %d; only byte, short integer, or real allowed", header.mode);
  printf ("slice mode=%d\n", sliceMode);

  startingView = 1;
  int numSlides = header.nz;
  printf ("Number of slides=%d\n", numSlides);

  //write the header
  unsigned int nx = header.nx;
  unsigned int ny = header.ny;
  Islice *currSlice;

  float angleSign = 1;
  float minAngle, maxAngle;
  minAngle = -20;
  maxAngle = 20;

  float *tiltAngles = NULL;
  printf ("reading angle file=%s\n", angleFn);
  if (!noTiltAngle)
	tiltAngles =  readTiltAngles (angleFn, header.nz, angleSign, minAngle, maxAngle);


  // printf("minAngle=%f, maxAngle=%f\n",minAngle,maxAngle);
  foutput = iiFOpen (outFn, "wb");

  mrc_head_label (&outHeader, "PreNAD filtered image");
  mrc_head_label(&outHeader, "PreNAD filtered image, options here");
  mrc_head_write(foutput, &outHeader);



  TiltProjectionType *stackIn = NULL;
  TiltProjectionType *stackOut = NULL;
  stackIn = (TiltProjectionType *) malloc (header.nz * sizeof (TiltProjectionType));
  stackOut = (TiltProjectionType *) malloc (header.nz * sizeof (TiltProjectionType));

  if (stackIn == NULL || stackOut == NULL)
	exitError ("error allocating memory");


  double spacingX = 1.0;
  double spacingY = 1.0;
  double lambdaE = 30.0, lambdaC = 30.0, lambdaH = 30.0;
  unsigned int ii = 0;

  //set the angles to process
  for (ii = 0; ii < (unsigned int) header.nz && rangeRequested; ii++)
      stackIn[ii].skipMe = true;
  
  if (rangeRequested)
    {
	for (ii = 0; ii < nAnglesToProcess; ii++)
		if ( anglesToProcess[ii] > 0 && anglesToProcess[ii] <= (int) header.nz ){
			stackIn[(anglesToProcess[ii])-1].skipMe = false;
		}else{
			printf ("[WARNING] projection %d not in the views, ignored!\n", anglesToProcess[ii]);
		}

    }


  for (ii = 0; ii < (unsigned int) header.nz; ii++)
    {
      float angle;
      if (noTiltAngle){
        angle = 0.0;
      }else{
        angle = tiltAngles[ii];
      }
      


      currSlice = sliceCreate (nx, ny, sliceMode);
      if (!currSlice)
	exitError ("Creating slice for input");
      //get the type of data we are dealing with
      if (mrc_read_slice (currSlice->data.b, fpStack, &header, ii, 'Z'))
	exitError ("Reading slice %d", ii);

      // Convert slice to floats
      if (sliceMode != SLICE_MODE_FLOAT)
	if (sliceNewMode (currSlice, SLICE_MODE_FLOAT) < 0)
	  exitError ("Converting slice to float");

      stackIn[ii].Data = matrix (0, nx + padding + padding, 0, ny + padding + padding);
      stackOut[ii].Data = matrix (0, nx + padding + padding, 0, ny + padding + padding);
      stackOut[ii].angleRadiant = (angle * MY_PI) / 180.0;
      stackOut[ii].angleDegrees = angle;
      stackOut[ii].MVD = 0.0;
      stackOut[ii].currentIteration = 0;
      stackOut[ii].TiltNumber = ii;
      stackOut[ii].sigma = sigma * cosf (stackOut[ii].angleRadiant);

      // Copy data into array
      for (unsigned int j = 0; j < ny; j++)
	for (unsigned int i = 0; i < nx; i++)
	  {
	    (stackIn[ii].Data)[i + padding][j + padding] =
	      currSlice->data.f[i + j * nx];
	    (stackOut[ii].Data)[i + padding][j + padding] =
	      currSlice->data.f[i + j * nx];
	  }

      FillingPadding (stackIn[ii].Data, nx, ny, padding);
      FillingPadding (stackOut[ii].Data, nx, ny, padding);

    }

  printf ("start processing\n");
  // ********************************************************************
  // ********************** PRE NON-LINEAR ANISOTROPIC DIFFUSION
  // ********************************************************************

  automaticPreNAD (stackIn, stackOut, nx, ny, header.nz, padding,
		   MinIterations, MaxIterations, spacingX, spacingY, lambdaE,
		   lambdaC, lambdaH);

  // ********************************************************************
  // ****  END  ***************  PRE NON-LINEAR ANISOTROPIC DIFFUSION
  // ********************************************************************
  printf ("\n***************\n write image data and close file \n***************\n");
  for (ii = 0; ii < (unsigned int) header.nz; ii++)
    {
      currSlice = sliceCreate (nx, ny, SLICE_MODE_FLOAT);
      if (!currSlice) exitError ("Creating slice for output");
      for (unsigned int j = 0; j < ny; j++)
	for (unsigned int i = 0; i < nx; i++){
	  currSlice->data.f[i + j * nx] = (stackOut[ii].Data)[i + padding][j + padding];
	}

      // Convert if necessary and write slice
      if (sliceMode != SLICE_MODE_FLOAT) if (sliceNewMode (currSlice, sliceMode) < 0)
	  exitError ("Converting slice to short");
      if (mrc_write_slice (currSlice->data.b, foutput, &outHeader, ii, 'Z'))
	exitError ("Writing slice %d", ii);

      sliceFree (currSlice);
      free_matrix (stackIn[ii].Data, 0, nx + padding + padding, 0, ny + padding + padding);
      free_matrix (stackOut[ii].Data, 0, nx + padding + padding, 0, ny + padding + padding);
    }


  free (stackIn);
  free (stackOut);

  iiFClose (foutput);
  iiFClose (fpStack);

}



