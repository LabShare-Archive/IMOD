/* diffusion.c
 * Written By:     Alejandro Cantarero
 * 
 * Provides a set of functions to perform anisotropic
 *   diffusion (Perona and Malik model) on an image
 *
 * algorithm is parallelized and uses MPI
 *
 */

#include <math.h>

/* routine that computes I^{n+1} */
void updateMatrix(double **image, double **imageOld, int m, int n,
		  int CC, double k, double lambda, int p) {
    extern int  myRank;
    double      diffN;
    double      diffS;
    double      diffE;
    double      diffW;
    double      cN;
    double      cS;
    double      cE;
    double      cW;
    double      normConst;
    int         i, j;
    
    /* the following code is for a
     *    block-row decomposition
     */
    
    /* if I am not a boundary process ( rank 0 or p-1 )
     *    I can process all rows of the matrix
     *
     * No process can process all columns
     */
    if ( myRank != 0 && myRank != p-1 ) {
	for (i = 1; i < m+1; i++) {
	    for (j = 2; j < n; j++) {
		diffN = imageOld[i-1][j] - imageOld[i][j];
		diffS = imageOld[i+1][j] - imageOld[i][j];
		diffE = imageOld[i][j+1] - imageOld[i][j];
		diffW = imageOld[i][j-1] - imageOld[i][j];
		
		if (CC == 1) {
		    cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		    cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		    cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		    cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
		}
		else if (CC == 2) {
		    cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		    cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		    cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		    cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
		}
		else if (CC == 3) { /* use Tukey Biweight */

		    /* assumes image has maximum value
		     *    of 1 and minimum value of 0
		     */
		    normConst = 2 / (( 1 - (1/k)*(1/k) ) *
			             ( 1 - (1/k)*(1/k) ));

		    if ( fabs(diffN) > k )
			cN = 0;
		    else {
			cN = 0.5*(1 - ( fabs(diffN) / k )*
				  ( fabs(diffN) / k ) ) *
			    (1 - ( fabs(diffN) / k )*
			     ( fabs(diffN) / k ) );
		    }

		    if ( fabs(diffS) > k )
			cS = 0;
		    else {
			cS = 0.5*(1 - ( fabs(diffS) / k )*
				  ( fabs(diffS) / k ) ) *
			    (1 - ( fabs(diffS) / k )*
			     ( fabs(diffS) / k ) );
		    }

		    if ( fabs(diffE) > k )
			cE = 0;
		    else {
			cE = 0.5*(1 - ( fabs(diffE) / k )*
				  ( fabs(diffE) / k ) ) *
			    (1 - ( fabs(diffE) / k )*
			     ( fabs(diffE) / k ) );
		    }

		    if ( fabs(diffW) > k )
			cW = 0;
		    else {
			cW = 0.5*(1 - ( fabs(diffW) / k )*
				  ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		    }		    
		}
		else
		    ; /* no-op */
		
		image[i][j] = imageOld[i][j] + lambda*(cN*diffN + cS*diffS +
						       cE*diffE + cW*diffW);
	    }
	}

	/* process the first and last columns of the matrix */
    
	/* left column */
	for (i = 1; i < m+1; i++) {
	    diffN = imageOld[i-1][1] - imageOld[i][1];
	    diffS = imageOld[i+1][1] - imageOld[i][1];
	    diffE = imageOld[i][2]   - imageOld[i][1];
	    diffW = imageOld[i][1]   - imageOld[i][1];
	    
	    if (CC == 1) {
		cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	    }
	    else if (CC == 2) {
		cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	    }
	    else if (CC == 3) { /* use Tukey Biweight */
		
		/* assumes image has maximum value
		 *    of 1 and minimum value of 0
		 */
		normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				 ( 1 - (1/k)*(1/k) ));
		
		if ( fabs(diffN) > k )
		    cN = 0;
		else {
		    cN = 0.5*(1 - ( fabs(diffN) / k )*
			      ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
		}
		
		if ( fabs(diffS) > k )
		    cS = 0;
		else {
		    cS = 0.5*(1 - ( fabs(diffS) / k )*
				  ( fabs(diffS) / k ) ) *
			(1 - ( fabs(diffS) / k )*
			 ( fabs(diffS) / k ) );
		}
		
		if ( fabs(diffE) > k )
		    cE = 0;
		else {
		    cE = 0.5*(1 - ( fabs(diffE) / k )*
			      ( fabs(diffE) / k ) ) *
			(1 - ( fabs(diffE) / k )*
			 ( fabs(diffE) / k ) );
		}
		
		if ( fabs(diffW) > k )
		    cW = 0;
		else {
		    cW = 0.5*(1 - ( fabs(diffW) / k )*
			      ( fabs(diffW) / k ) ) *
			(1 - ( fabs(diffW) / k )*
			 ( fabs(diffW) / k ) );
		}		    
	    }
	    else
		; /* no-op */
	    
	    image[i][1] = imageOld[i][1] + lambda*(cN*diffN + cS*diffS +
						   cE*diffE + cW*diffW);
	}
	
	/* right column */
	for (i = 1; i < m+1; i++) {	   
	    diffN = imageOld[i-1][n] - imageOld[i][n];
	    diffS = imageOld[i+1][n] - imageOld[i][n];
	    diffE = imageOld[i][n]   - imageOld[i][n];
	    diffW = imageOld[i][n-1] - imageOld[i][n];
	    
	    if (CC == 1) {
		cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	    }
	    else if (CC == 2) {
		cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	    }
	    else if (CC == 3) { /* use Tukey Biweight */
		
		/* assumes image has maximum value
		 *    of 1 and minimum value of 0
		 */
		normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				 ( 1 - (1/k)*(1/k) ));
		
		if ( fabs(diffN) > k )
		    cN = 0;
		else {
		    cN = 0.5*(1 - ( fabs(diffN) / k )*
			      ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
		}
		
		if ( fabs(diffS) > k )
		    cS = 0;
		else {
		    cS = 0.5*(1 - ( fabs(diffS) / k )*
			      ( fabs(diffS) / k ) ) *
			(1 - ( fabs(diffS) / k )*
			 ( fabs(diffS) / k ) );
		}
		
		if ( fabs(diffE) > k )
		    cE = 0;
		else {
		    cE = 0.5*(1 - ( fabs(diffE) / k )*
			      ( fabs(diffE) / k ) ) *
			(1 - ( fabs(diffE) / k )*
			 ( fabs(diffE) / k ) );
		}
		
		if ( fabs(diffW) > k )
		    cW = 0;
		else {
		    cW = 0.5*(1 - ( fabs(diffW) / k )*
			      ( fabs(diffW) / k ) ) *
			(1 - ( fabs(diffW) / k )*
			 ( fabs(diffW) / k ) );
		}		    
	    }	    
	    else
		; /* no-op */
	    
	    image[i][n] = imageOld[i][n] + lambda*(cN*diffN + cS*diffS +
						       cE*diffE + cW*diffW);
	}

    } else if ( p == 1 ) {
	/* if p == 1, then the top row, bottom row and all four
	 *       corners require special processing
	 */
	
	for (i = 2; i < m; i++) {
	    for (j = 2; j < n; j++) {
		diffN = imageOld[i-1][j] - imageOld[i][j];
		diffS = imageOld[i+1][j] - imageOld[i][j];
		diffE = imageOld[i][j+1] - imageOld[i][j];
		diffW = imageOld[i][j-1] - imageOld[i][j];
		
		if (CC == 1) {
		    cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		    cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		    cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		    cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
		}
		else if (CC == 2) {
		    cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		    cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		    cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		    cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
		}
		else if (CC == 3) { /* use Tukey Biweight */

		    /* assumes image has maximum value
		     *    of 1 and minimum value of 0
		     */
		    normConst = 2 / (( 1 - (1/k)*(1/k) ) *
			             ( 1 - (1/k)*(1/k) ));
		    
		    if ( fabs(diffN) > k )
			cN = 0;
		    else {
			cN = 0.5*(1 - ( fabs(diffN) / k )*
				  ( fabs(diffN) / k ) ) *
			    (1 - ( fabs(diffN) / k )*
			     ( fabs(diffN) / k ) );
		    }

		    if ( fabs(diffS) > k )
			cS = 0;
		    else {
			cS = 0.5*(1 - ( fabs(diffS) / k )*
				  ( fabs(diffS) / k ) ) *
			    (1 - ( fabs(diffS) / k )*
			     ( fabs(diffS) / k ) );
		    }

		    if ( fabs(diffE) > k )
			cE = 0;
		    else {
			cE = 0.5*(1 - ( fabs(diffE) / k )*
				  ( fabs(diffE) / k ) ) *
			    (1 - ( fabs(diffE) / k )*
			     ( fabs(diffE) / k ) );
		    }

		    if ( fabs(diffW) > k )
			cW = 0;
		    else {
			cW = 0.5*(1 - ( fabs(diffW) / k )*
				  ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		    }		    
		}
		else
		    ; /* no-op */
		
		image[i][j] = imageOld[i][j] + lambda*(cN*diffN + cS*diffS +
						       cE*diffE + cW*diffW);
	    }
	}
	
	/* top row */
	for (j = 2; j < n; j++) {
	    diffN = imageOld[1][j]   - imageOld[1][j];
	    diffS = imageOld[2][j]   - imageOld[1][j];
	    diffE = imageOld[1][j+1] - imageOld[1][j];
	    diffW = imageOld[1][j-1] - imageOld[1][j];
	    
	    if (CC == 1) {
		cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	    }
	    else if (CC == 2) {
		cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	    }
	    else if (CC == 3) { /* use Tukey Biweight */
		
		/* assumes image has maximum value
		 *    of 1 and minimum value of 0
		 */
		normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				 ( 1 - (1/k)*(1/k) ));
		
		if ( fabs(diffN) > k )
		    cN = 0;
		else {
		    cN = 0.5*(1 - ( fabs(diffN) / k )*
			      ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
		}
		
		if ( fabs(diffS) > k )
		    cS = 0;
		else {
		    cS = 0.5*(1 - ( fabs(diffS) / k )*
			      ( fabs(diffS) / k ) ) *
			(1 - ( fabs(diffS) / k )*
			 ( fabs(diffS) / k ) );
		}
		
		if ( fabs(diffE) > k )
		    cE = 0;
		else {
		    cE = 0.5*(1 - ( fabs(diffE) / k )*
			      ( fabs(diffE) / k ) ) *
			(1 - ( fabs(diffE) / k )*
			 ( fabs(diffE) / k ) );
		}
		
		if ( fabs(diffW) > k )
		    cW = 0;
		else {
			cW = 0.5*(1 - ( fabs(diffW) / k )*
				  ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		}		    
	    }
	    else
		; /* no-op */
	    
	    image[1][j] = imageOld[1][j] + lambda*(cN*diffN + cS*diffS +
						   cE*diffE + cW*diffW);
	}

	/* process upper left corner */	
	diffN = imageOld[1][1] - imageOld[1][1];
	diffS = imageOld[2][1] - imageOld[1][1];
	diffE = imageOld[1][2] - imageOld[1][1];
	diffW = imageOld[1][1] - imageOld[1][1];

	if (CC == 1) {
	    cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
	    cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
	    cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
	    cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	}
	else if (CC == 2) {
	    cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
	    cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
	    cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
	    cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	}
	else if (CC == 3) { /* use Tukey Biweight */
	    
	    /* assumes image has maximum value
	     *    of 1 and minimum value of 0
	     */
	    normConst = 2 / (( 1 - (1/k)*(1/k) ) *
			     ( 1 - (1/k)*(1/k) ));
	    
	    if ( fabs(diffN) > k )
		cN = 0;
	    else {
		cN = 0.5*(1 - ( fabs(diffN) / k )*
			  ( fabs(diffN) / k ) ) *
		    (1 - ( fabs(diffN) / k )*
		     ( fabs(diffN) / k ) );
	    }
	    
	    if ( fabs(diffS) > k )
		cS = 0;
	    else {
		cS = 0.5*(1 - ( fabs(diffS) / k )*
			  ( fabs(diffS) / k ) ) *
		    (1 - ( fabs(diffS) / k )*
		     ( fabs(diffS) / k ) );
	    }
	    
	    if ( fabs(diffE) > k )
		cE = 0;
	    else {
		cE = 0.5*(1 - ( fabs(diffE) / k )*
			  ( fabs(diffE) / k ) ) *
		    (1 - ( fabs(diffE) / k )*
		     ( fabs(diffE) / k ) );
	    }
	    
	    if ( fabs(diffW) > k )
		cW = 0;
	    else {
		cW = 0.5*(1 - ( fabs(diffW) / k )*
			  ( fabs(diffW) / k ) ) *
		    (1 - ( fabs(diffW) / k )*
		     ( fabs(diffW) / k ) );
	    }		    
	}
	else
	    ; /* no-op */
	
	image[1][1] = imageOld[1][1] + lambda*(cN*diffN + cS*diffS +
					       cE*diffE + cW*diffW);
	

	/* process upper right corner */
	diffN = imageOld[1][n]   - imageOld[1][n];
	diffS = imageOld[2][n]   - imageOld[1][n];
	diffE = imageOld[1][n]   - imageOld[1][n];
	diffW = imageOld[1][n-1] - imageOld[1][n];
	
	if (CC == 1) {
	    cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
	    cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
	    cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
	    cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	}
	else if (CC == 2) {
	    cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
	    cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
	    cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
	    cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	}
	else if (CC == 3) { /* use Tukey Biweight */
	    
	    /* assumes image has maximum value
	     *    of 1 and minimum value of 0
	     */
	    normConst = 2 / (( 1 - (1/k)*(1/k) ) *
			     ( 1 - (1/k)*(1/k) ));
	    
	    if ( fabs(diffN) > k )
		cN = 0;
	    else {
		cN = 0.5*(1 - ( fabs(diffN) / k )*
			  ( fabs(diffN) / k ) ) *
		    (1 - ( fabs(diffN) / k )*
		     ( fabs(diffN) / k ) );
	    }
	    
	    if ( fabs(diffS) > k )
		cS = 0;
	    else {
		cS = 0.5*(1 - ( fabs(diffS) / k )*
			  ( fabs(diffS) / k ) ) *
		    (1 - ( fabs(diffS) / k )*
		     ( fabs(diffS) / k ) );
	    }
	    
	    if ( fabs(diffE) > k )
		cE = 0;
	    else {
		cE = 0.5*(1 - ( fabs(diffE) / k )*
			  ( fabs(diffE) / k ) ) *
		    (1 - ( fabs(diffE) / k )*
		     ( fabs(diffE) / k ) );
	    }
	    
	    if ( fabs(diffW) > k )
		cW = 0;
	    else {
		cW = 0.5*(1 - ( fabs(diffW) / k )*
			  ( fabs(diffW) / k ) ) *
		    (1 - ( fabs(diffW) / k )*
		     ( fabs(diffW) / k ) );
	    }		    
	}
	else
	    ; /* no-op */
	
	image[1][n] = imageOld[1][n] + lambda*(cN*diffN + cS*diffS +
					       cE*diffE + cW*diffW);


	/* left column */
	for (i = 2; i < m; i++) {
	    diffN = imageOld[i-1][1] - imageOld[i][1];
	    diffS = imageOld[i+1][1] - imageOld[i][1];
	    diffE = imageOld[i][2]   - imageOld[i][1];
	    diffW = imageOld[i][1]   - imageOld[i][1];
	    
	    if (CC == 1) {
		cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	    }
	    else if (CC == 2) {
		cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	    }
	    else if (CC == 3) { /* use Tukey Biweight */

		/* assumes image has maximum value
		 *    of 1 and minimum value of 0
		 */
		normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				 ( 1 - (1/k)*(1/k) ));
		
		if ( fabs(diffN) > k )
		    cN = 0;
		else {
		    cN = 0.5*(1 - ( fabs(diffN) / k )*
			      ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
		}
		
		if ( fabs(diffS) > k )
		    cS = 0;
		else {
		    cS = 0.5*(1 - ( fabs(diffS) / k )*
			      ( fabs(diffS) / k ) ) *
			(1 - ( fabs(diffS) / k )*
			 ( fabs(diffS) / k ) );
		}
		
		if ( fabs(diffE) > k )
		    cE = 0;
		else {
		    cE = 0.5*(1 - ( fabs(diffE) / k )*
			      ( fabs(diffE) / k ) ) *
			(1 - ( fabs(diffE) / k )*
			 ( fabs(diffE) / k ) );
		}
		
		if ( fabs(diffW) > k )
		    cW = 0;
		else {
		    cW = 0.5*(1 - ( fabs(diffW) / k )*
				  ( fabs(diffW) / k ) ) *
			(1 - ( fabs(diffW) / k )*
			 ( fabs(diffW) / k ) );
		}		    
	    }
	    else
		; /* no-op */
	    
	    image[i][1] = imageOld[i][1] + lambda*(cN*diffN + cS*diffS +
						   cE*diffE + cW*diffW);
	}
	
	/* right column */
	for (i = 2; i < m; i++) {	   
	    diffN = imageOld[i-1][n] - imageOld[i][n];
	    diffS = imageOld[i+1][n] - imageOld[i][n];
	    diffE = imageOld[i][n]   - imageOld[i][n];
	    diffW = imageOld[i][n-1] - imageOld[i][n];
	    
	    if (CC == 1) {
		cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	    }
	    else if (CC == 2) {
		cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	    }
	    else if (CC == 3) { /* use Tukey Biweight */
		
		/* assumes image has maximum value
		 *    of 1 and minimum value of 0
		 */
		normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				 ( 1 - (1/k)*(1/k) ));
		
		if ( fabs(diffN) > k )
		    cN = 0;
		else {
		    cN = 0.5*(1 - ( fabs(diffN) / k )*
			      ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
		}
		
		if ( fabs(diffS) > k )
		    cS = 0;
		else {
		    cS = 0.5*(1 - ( fabs(diffS) / k )*
			      ( fabs(diffS) / k ) ) *
			(1 - ( fabs(diffS) / k )*
			 ( fabs(diffS) / k ) );
		}
		
		if ( fabs(diffE) > k )
		    cE = 0;
		else {
		    cE = 0.5*(1 - ( fabs(diffE) / k )*
			      ( fabs(diffE) / k ) ) *
			(1 - ( fabs(diffE) / k )*
			 ( fabs(diffE) / k ) );
		}
		
		if ( fabs(diffW) > k )
		    cW = 0;
		else {
		    cW = 0.5*(1 - ( fabs(diffW) / k )*
			      ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		}		    
	    }
	    else
		; /* no-op */
	    
	    image[i][n] = imageOld[i][n] + lambda*(cN*diffN + cS*diffS +
						   cE*diffE + cW*diffW);
	}

	/* bottom row */
	for (j = 2; j < n; j++) {
	    diffN = imageOld[m-1][j] - imageOld[m][j];
	    diffS = imageOld[m][j]   - imageOld[m][j];
	    diffE = imageOld[m][j+1] - imageOld[m][j];
	    diffW = imageOld[m][j-1] - imageOld[m][j];
	    
	    if (CC == 1) {
		cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	    }
	    else if (CC == 2) {
		cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	    }
	    else if (CC == 3) { /* use Tukey Biweight */
		
		/* assumes image has maximum value
		 *    of 1 and minimum value of 0
		 */
		normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				 ( 1 - (1/k)*(1/k) ));
		
		if ( fabs(diffN) > k )
		    cN = 0;
		else {
		    cN = 0.5*(1 - ( fabs(diffN) / k )*
			      ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
		}
		
		if ( fabs(diffS) > k )
		    cS = 0;
		else {
		    cS = 0.5*(1 - ( fabs(diffS) / k )*
			      ( fabs(diffS) / k ) ) *
			(1 - ( fabs(diffS) / k )*
			 ( fabs(diffS) / k ) );
		}
		
		if ( fabs(diffE) > k )
		    cE = 0;
		else {
		    cE = 0.5*(1 - ( fabs(diffE) / k )*
			      ( fabs(diffE) / k ) ) *
			(1 - ( fabs(diffE) / k )*
			 ( fabs(diffE) / k ) );
		}
		
		if ( fabs(diffW) > k )
		    cW = 0;
		else {
		    cW = 0.5*(1 - ( fabs(diffW) / k )*
			      ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		}		    
	    }
	    else
		; /* no-op */
	    
	    image[m][j] = imageOld[m][j] + lambda*(cN*diffN + cS*diffS +
						   cE*diffE + cW*diffW);    
	}

	/* process lower left corner */
	diffN = imageOld[m-1][1] - imageOld[m][1];
	diffS = imageOld[m][1]   - imageOld[m][1];
	diffE = imageOld[m][2]   - imageOld[m][1];
	diffW = imageOld[m][1]   - imageOld[m][1];
	
	if (CC == 1) {
	    cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
	    cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
	    cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
	    cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	}
	else if (CC == 2) {
	    cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
	    cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
	    cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
	    cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	}
	else if (CC == 3) { /* use Tukey Biweight */
	    
	    /* assumes image has maximum value
	     *    of 1 and minimum value of 0
	     */
	    normConst = 2 / (( 1 - (1/k)*(1/k) ) *
			     ( 1 - (1/k)*(1/k) ));
	    
	    if ( fabs(diffN) > k )
		cN = 0;
	    else {
		cN = 0.5*(1 - ( fabs(diffN) / k )*
			  ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
	    }
	    
	    if ( fabs(diffS) > k )
		cS = 0;
	    else {
		cS = 0.5*(1 - ( fabs(diffS) / k )*
			  ( fabs(diffS) / k ) ) *
		    (1 - ( fabs(diffS) / k )*
		     ( fabs(diffS) / k ) );
	    }
	    
	    if ( fabs(diffE) > k )
		cE = 0;
	    else {
		cE = 0.5*(1 - ( fabs(diffE) / k )*
			  ( fabs(diffE) / k ) ) *
		    (1 - ( fabs(diffE) / k )*
		     ( fabs(diffE) / k ) );
	    }
	    
	    if ( fabs(diffW) > k )
		cW = 0;
	    else {
		cW = 0.5*(1 - ( fabs(diffW) / k )*
			  ( fabs(diffW) / k ) ) *
		    (1 - ( fabs(diffW) / k )*
		     ( fabs(diffW) / k ) );
	    }		    
	}
	else
	    ; /* no-op */
	
	image[m][1] = imageOld[m][1] + lambda*(cN*diffN + cS*diffS +
					       cE*diffE + cW*diffW);

	/* process lower right corner */
	diffN = imageOld[m-1][n] - imageOld[m][n];
	diffS = imageOld[m][n]   - imageOld[m][n];
	diffE = imageOld[m][n]   - imageOld[m][n];
	diffW = imageOld[m][n-1] - imageOld[m][n];
	
	if (CC == 1) {
	    cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
	    cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
	    cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
	    cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	}
	else if (CC == 2) {
	    cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
	    cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
	    cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
	    cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	}
	else if (CC == 3) { /* use Tukey Biweight */
	    
	    /* assumes image has maximum value
	     *    of 1 and minimum value of 0
	     */
	    normConst = 2 / (( 1 - (1/k)*(1/k) ) *
			     ( 1 - (1/k)*(1/k) ));
	    
	    if ( fabs(diffN) > k )
		cN = 0;
	    else {
		cN = 0.5*(1 - ( fabs(diffN) / k )*
			  ( fabs(diffN) / k ) ) *
		    (1 - ( fabs(diffN) / k )*
		     ( fabs(diffN) / k ) );
	    }
	    
	    if ( fabs(diffS) > k )
		cS = 0;
	    else {
		cS = 0.5*(1 - ( fabs(diffS) / k )*
			  ( fabs(diffS) / k ) ) *
		    (1 - ( fabs(diffS) / k )*
		     ( fabs(diffS) / k ) );
	    }
	    
	    if ( fabs(diffE) > k )
		cE = 0;
	    else {
		cE = 0.5*(1 - ( fabs(diffE) / k )*
			  ( fabs(diffE) / k ) ) *
		    (1 - ( fabs(diffE) / k )*
		     ( fabs(diffE) / k ) );
	    }
	    
	    if ( fabs(diffW) > k )
		cW = 0;
	    else {
		cW = 0.5*(1 - ( fabs(diffW) / k )*
			  ( fabs(diffW) / k ) ) *
		    (1 - ( fabs(diffW) / k )*
		     ( fabs(diffW) / k ) );
	    }		    
	}
	else
	    ; /* no-op */
	
	image[m][n] = imageOld[m][n] + lambda*(cN*diffN + cS*diffS +
					       cE*diffE + cW*diffW);


    } else if ( myRank == 0 ) {
	/* the top row requires special processing */

	for (i = 2; i < m+1; i++) {
	    for (j = 2; j < n; j++) {
		diffN = imageOld[i-1][j] - imageOld[i][j];
		diffS = imageOld[i+1][j] - imageOld[i][j];
		diffE = imageOld[i][j+1] - imageOld[i][j];
		diffW = imageOld[i][j-1] - imageOld[i][j];
		
		if (CC == 1) {
		    cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		    cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		    cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		    cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
		}
		else if (CC == 2) {
		    cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		    cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		    cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		    cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
		}
		else if (CC == 3) { /* use Tukey Biweight */
		    
		    /* assumes image has maximum value
		     *    of 1 and minimum value of 0
		     */
		    normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				     ( 1 - (1/k)*(1/k) ));
		    
		    if ( fabs(diffN) > k )
			cN = 0;
		    else {
			cN = 0.5*(1 - ( fabs(diffN) / k )*
				  ( fabs(diffN) / k ) ) *
			    (1 - ( fabs(diffN) / k )*
			     ( fabs(diffN) / k ) );
		    }
		    
		    if ( fabs(diffS) > k )
			cS = 0;
		    else {
			cS = 0.5*(1 - ( fabs(diffS) / k )*
				  ( fabs(diffS) / k ) ) *
			    (1 - ( fabs(diffS) / k )*
			     ( fabs(diffS) / k ) );
		    }
		    
		    if ( fabs(diffE) > k )
			cE = 0;
		    else {
			cE = 0.5*(1 - ( fabs(diffE) / k )*
				  ( fabs(diffE) / k ) ) *
			    (1 - ( fabs(diffE) / k )*
			     ( fabs(diffE) / k ) );
		    }
		    
		    if ( fabs(diffW) > k )
			cW = 0;
		    else {
			cW = 0.5*(1 - ( fabs(diffW) / k )*
				  ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		    }		    
		}
		else
		    ; /* no-op */
		
		image[i][j] = imageOld[i][j] + lambda*(cN*diffN + cS*diffS +
						       cE*diffE + cW*diffW);
	    }
	}
	
	/* top row */
	for (j = 2; j < n; j++) {
	    diffN = imageOld[1][j]   - imageOld[1][j];
	    diffS = imageOld[2][j]   - imageOld[1][j];
	    diffE = imageOld[1][j+1] - imageOld[1][j];
	    diffW = imageOld[1][j-1] - imageOld[1][j];
	    
	    if (CC == 1) {
		cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	    }
	    else if (CC == 2) {
		cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	    }
	    else if (CC == 3) { /* use Tukey Biweight */
		
		/* assumes image has maximum value
		 *    of 1 and minimum value of 0
		 */
		normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				 ( 1 - (1/k)*(1/k) ));
		
		if ( fabs(diffN) > k )
		    cN = 0;
		else {
		    cN = 0.5*(1 - ( fabs(diffN) / k )*
			      ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
		}
		
		if ( fabs(diffS) > k )
		    cS = 0;
		else {
		    cS = 0.5*(1 - ( fabs(diffS) / k )*
			      ( fabs(diffS) / k ) ) *
			(1 - ( fabs(diffS) / k )*
			 ( fabs(diffS) / k ) );
		}
		
		if ( fabs(diffE) > k )
		    cE = 0;
		else {
		    cE = 0.5*(1 - ( fabs(diffE) / k )*
			      ( fabs(diffE) / k ) ) *
			(1 - ( fabs(diffE) / k )*
			 ( fabs(diffE) / k ) );
		}
		
		if ( fabs(diffW) > k )
		    cW = 0;
		else {
		    cW = 0.5*(1 - ( fabs(diffW) / k )*
			      ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		}		    
	    }
	    else
		; /* no-op */
	    
	    image[1][j] = imageOld[1][j] + lambda*(cN*diffN + cS*diffS +
						   cE*diffE + cW*diffW);
	}

	/* process upper left corner */	
	diffN = imageOld[1][1] - imageOld[1][1];
	diffS = imageOld[2][1] - imageOld[1][1];
	diffE = imageOld[1][2] - imageOld[1][1];
	diffW = imageOld[1][1] - imageOld[1][1];

	if (CC == 1) {
	    cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
	    cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
	    cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
	    cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	}
	else if (CC == 2) {
	    cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
	    cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
	    cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
	    cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	}
	else if (CC == 3) { /* use Tukey Biweight */
	    
	    /* assumes image has maximum value
	     *    of 1 and minimum value of 0
	     */
	    normConst = 2 / (( 1 - (1/k)*(1/k) ) *
			     ( 1 - (1/k)*(1/k) ));
	    
	    if ( fabs(diffN) > k )
		cN = 0;
	    else {
		cN = 0.5*(1 - ( fabs(diffN) / k )*
			  ( fabs(diffN) / k ) ) *
		    (1 - ( fabs(diffN) / k )*
		     ( fabs(diffN) / k ) );
	    }
	    
	    if ( fabs(diffS) > k )
		cS = 0;
	    else {
		cS = 0.5*(1 - ( fabs(diffS) / k )*
			  ( fabs(diffS) / k ) ) *
		    (1 - ( fabs(diffS) / k )*
		     ( fabs(diffS) / k ) );
	    }
	    
	    if ( fabs(diffE) > k )
		cE = 0;
	    else {
		cE = 0.5*(1 - ( fabs(diffE) / k )*
			  ( fabs(diffE) / k ) ) *
		    (1 - ( fabs(diffE) / k )*
		     ( fabs(diffE) / k ) );
	    }
	    
	    if ( fabs(diffW) > k )
		cW = 0;
	    else {
		cW = 0.5*(1 - ( fabs(diffW) / k )*
			  ( fabs(diffW) / k ) ) *
		    (1 - ( fabs(diffW) / k )*
		     ( fabs(diffW) / k ) );
	    }		    
	}
	else
	    ; /* no-op */
	
	image[1][1] = imageOld[1][1] + lambda*(cN*diffN + cS*diffS +
					       cE*diffE + cW*diffW);


	/* process upper right corner */
	diffN = imageOld[1][n]   - imageOld[1][n];
	diffS = imageOld[2][n]   - imageOld[1][n];
	diffE = imageOld[1][n]   - imageOld[1][n];
	diffW = imageOld[1][n-1] - imageOld[1][n];
	
	if (CC == 1) {
	    cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
	    cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
	    cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
	    cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	}
	else if (CC == 2) {
	    cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
	    cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
	    cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
	    cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	}
	else if (CC == 3) { /* use Tukey Biweight */
	    
	    /* assumes image has maximum value
	     *    of 1 and minimum value of 0
	     */
	    normConst = 2 / (( 1 - (1/k)*(1/k) ) *
			     ( 1 - (1/k)*(1/k) ));
	    
	    if ( fabs(diffN) > k )
		cN = 0;
	    else {
		cN = 0.5*(1 - ( fabs(diffN) / k )*
			  ( fabs(diffN) / k ) ) *
		    (1 - ( fabs(diffN) / k )*
		     ( fabs(diffN) / k ) );
	    }
	    
	    if ( fabs(diffS) > k )
		cS = 0;
	    else {
		cS = 0.5*(1 - ( fabs(diffS) / k )*
			  ( fabs(diffS) / k ) ) *
		    (1 - ( fabs(diffS) / k )*
		     ( fabs(diffS) / k ) );
	    }
	    
	    if ( fabs(diffE) > k )
		cE = 0;
	    else {
		cE = 0.5*(1 - ( fabs(diffE) / k )*
			  ( fabs(diffE) / k ) ) *
		    (1 - ( fabs(diffE) / k )*
		     ( fabs(diffE) / k ) );
	    }
	    
	    if ( fabs(diffW) > k )
		cW = 0;
	    else {
		cW = 0.5*(1 - ( fabs(diffW) / k )*
			  ( fabs(diffW) / k ) ) *
		    (1 - ( fabs(diffW) / k )*
		     ( fabs(diffW) / k ) );
	    }		    
	}
	else
	    ; /* no-op */
	
	image[1][n] = imageOld[1][n] + lambda*(cN*diffN + cS*diffS +
					       cE*diffE + cW*diffW);


	/* left column */
	for (i = 2; i < m+1; i++) {
	    diffN = imageOld[i-1][1] - imageOld[i][1];
	    diffS = imageOld[i+1][1] - imageOld[i][1];
	    diffE = imageOld[i][2]   - imageOld[i][1];
	    diffW = imageOld[i][1]   - imageOld[i][1];
	    
	    if (CC == 1) {
		cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	    }
	    else if (CC == 2) {
		cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	    }
	    else if (CC == 3) { /* use Tukey Biweight */
		
		/* assumes image has maximum value
		 *    of 1 and minimum value of 0
		 */
		normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				 ( 1 - (1/k)*(1/k) ));
		
		if ( fabs(diffN) > k )
		    cN = 0;
		else {
		    cN = 0.5*(1 - ( fabs(diffN) / k )*
			      ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
		}
		
		if ( fabs(diffS) > k )
		    cS = 0;
		else {
		    cS = 0.5*(1 - ( fabs(diffS) / k )*
			      ( fabs(diffS) / k ) ) *
			(1 - ( fabs(diffS) / k )*
			 ( fabs(diffS) / k ) );
		}
		
		if ( fabs(diffE) > k )
		    cE = 0;
		else {
		    cE = 0.5*(1 - ( fabs(diffE) / k )*
			      ( fabs(diffE) / k ) ) *
			(1 - ( fabs(diffE) / k )*
			 ( fabs(diffE) / k ) );
		}
		
		if ( fabs(diffW) > k )
		    cW = 0;
		else {
		    cW = 0.5*(1 - ( fabs(diffW) / k )*
			      ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		}		    
	    }
	    else
		; /* no-op */
	    
	    image[i][1] = imageOld[i][1] + lambda*(cN*diffN + cS*diffS +
						   cE*diffE + cW*diffW);
	}
	
	/* right column */
	for (i = 2; i < m+1; i++) {	   
	    diffN = imageOld[i-1][n] - imageOld[i][n];
	    diffS = imageOld[i+1][n] - imageOld[i][n];
	    diffE = imageOld[i][n]   - imageOld[i][n];
	    diffW = imageOld[i][n-1] - imageOld[i][n];
	    
	    if (CC == 1) {
		cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	    }
	    else if (CC == 2) {
		cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	    }
	    else if (CC == 3) { /* use Tukey Biweight */
		
		/* assumes image has maximum value
		 *    of 1 and minimum value of 0
		 */
		normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				 ( 1 - (1/k)*(1/k) ));
		
		if ( fabs(diffN) > k )
		    cN = 0;
		else {
		    cN = 0.5*(1 - ( fabs(diffN) / k )*
			      ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
		}
		
		if ( fabs(diffS) > k )
		    cS = 0;
		else {
		    cS = 0.5*(1 - ( fabs(diffS) / k )*
			      ( fabs(diffS) / k ) ) *
			(1 - ( fabs(diffS) / k )*
			 ( fabs(diffS) / k ) );
		}
		
		if ( fabs(diffE) > k )
		    cE = 0;
		else {
		    cE = 0.5*(1 - ( fabs(diffE) / k )*
			      ( fabs(diffE) / k ) ) *
			(1 - ( fabs(diffE) / k )*
			 ( fabs(diffE) / k ) );
		}
		
		if ( fabs(diffW) > k )
		    cW = 0;
		else {
		    cW = 0.5*(1 - ( fabs(diffW) / k )*
			      ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		}		    
	    }
	    else
		; /* no-op */
	    
	    image[i][n] = imageOld[i][n] + lambda*(cN*diffN + cS*diffS +
						   cE*diffE + cW*diffW);
	}
	
	
    } else { /* myRank is p-1 */	
	/* the bottom row requires special processing */

	for (i = 1; i < m; i++) {
	    for (j = 2; j < n; j++) {
		diffN = imageOld[i-1][j] - imageOld[i][j];
		diffS = imageOld[i+1][j] - imageOld[i][j];
		diffE = imageOld[i][j+1] - imageOld[i][j];
		diffW = imageOld[i][j-1] - imageOld[i][j];
		
		if (CC == 1) {
		    cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		    cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		    cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		    cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
		}
		else if (CC == 2) {
		    cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		    cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		    cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		    cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
		}
		else if (CC == 3) { /* use Tukey Biweight */
		    
		    /* assumes image has maximum value
		     *    of 1 and minimum value of 0
		     */
		    normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				     ( 1 - (1/k)*(1/k) ));
		    
		    if ( fabs(diffN) > k )
			cN = 0;
		    else {
			cN = 0.5*(1 - ( fabs(diffN) / k )*
				  ( fabs(diffN) / k ) ) *
			    (1 - ( fabs(diffN) / k )*
			     ( fabs(diffN) / k ) );
		    }
		    
		    if ( fabs(diffS) > k )
			cS = 0;
		    else {
			cS = 0.5*(1 - ( fabs(diffS) / k )*
				  ( fabs(diffS) / k ) ) *
			    (1 - ( fabs(diffS) / k )*
			     ( fabs(diffS) / k ) );
		    }
		    
		    if ( fabs(diffE) > k )
			cE = 0;
		    else {
			cE = 0.5*(1 - ( fabs(diffE) / k )*
				  ( fabs(diffE) / k ) ) *
			    (1 - ( fabs(diffE) / k )*
			     ( fabs(diffE) / k ) );
		    }
		    
		    if ( fabs(diffW) > k )
			cW = 0;
		    else {
			cW = 0.5*(1 - ( fabs(diffW) / k )*
				  ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		    }		    
		}		
		else
		    ; /* no-op */
	    
		image[i][j] = imageOld[i][j] + lambda*(cN*diffN + cS*diffS +
						       cE*diffE + cW*diffW);
	    }
	}
    
	/* bottom row */
	for (j = 2; j < n; j++) {
	    diffN = imageOld[m-1][j] - imageOld[m][j];
	    diffS = imageOld[m][j]   - imageOld[m][j];
	    diffE = imageOld[m][j+1] - imageOld[m][j];
	    diffW = imageOld[m][j-1] - imageOld[m][j];
	    
	    if (CC == 1) {
		cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	    }
	    else if (CC == 2) {
		cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	    }
	    else if (CC == 3) { /* use Tukey Biweight */
		
		/* assumes image has maximum value
		 *    of 1 and minimum value of 0
		 */
		normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				 ( 1 - (1/k)*(1/k) ));
		
		if ( fabs(diffN) > k )
		    cN = 0;
		else {
		    cN = 0.5*(1 - ( fabs(diffN) / k )*
			      ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
		}
		
		if ( fabs(diffS) > k )
		    cS = 0;
		else {
		    cS = 0.5*(1 - ( fabs(diffS) / k )*
			      ( fabs(diffS) / k ) ) *
			(1 - ( fabs(diffS) / k )*
			 ( fabs(diffS) / k ) );
		}
		
		if ( fabs(diffE) > k )
		    cE = 0;
		else {
		    cE = 0.5*(1 - ( fabs(diffE) / k )*
			      ( fabs(diffE) / k ) ) *
			(1 - ( fabs(diffE) / k )*
			 ( fabs(diffE) / k ) );
		}
		
		if ( fabs(diffW) > k )
		    cW = 0;
		else {
		    cW = 0.5*(1 - ( fabs(diffW) / k )*
			      ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		}		    
	    }
	    else
		; /* no-op */
	    
	    image[m][j] = imageOld[m][j] + lambda*(cN*diffN + cS*diffS +
						   cE*diffE + cW*diffW);    
	}

	/* process lower left corner */
	diffN = imageOld[m-1][1] - imageOld[m][1];
	diffS = imageOld[m][1]   - imageOld[m][1];
	diffE = imageOld[m][2]   - imageOld[m][1];
	diffW = imageOld[m][1]   - imageOld[m][1];
	
	if (CC == 1) {
	    cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
	    cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
	    cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
	    cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	}
	else if (CC == 2) {
	    cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
	    cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
	    cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
	    cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	}
	else if (CC == 3) { /* use Tukey Biweight */
	    
	    /* assumes image has maximum value
	     *    of 1 and minimum value of 0
	     */
	    normConst = 2 / (( 1 - (1/k)*(1/k) ) *
			     ( 1 - (1/k)*(1/k) ));
	    
	    if ( fabs(diffN) > k )
		cN = 0;
	    else {
		cN = 0.5*(1 - ( fabs(diffN) / k )*
			  ( fabs(diffN) / k ) ) *
		    (1 - ( fabs(diffN) / k )*
		     ( fabs(diffN) / k ) );
	    }
	    
	    if ( fabs(diffS) > k )
		cS = 0;
	    else {
		cS = 0.5*(1 - ( fabs(diffS) / k )*
			  ( fabs(diffS) / k ) ) *
		    (1 - ( fabs(diffS) / k )*
		     ( fabs(diffS) / k ) );
	    }
	    
	    if ( fabs(diffE) > k )
		cE = 0;
	    else {
		cE = 0.5*(1 - ( fabs(diffE) / k )*
			  ( fabs(diffE) / k ) ) *
		    (1 - ( fabs(diffE) / k )*
		     ( fabs(diffE) / k ) );
	    }
	    
	    if ( fabs(diffW) > k )
		cW = 0;
	    else {
		cW = 0.5*(1 - ( fabs(diffW) / k )*
			  ( fabs(diffW) / k ) ) *
		    (1 - ( fabs(diffW) / k )*
		     ( fabs(diffW) / k ) );
	    }		    
	}
	else
	    ; /* no-op */
	
	image[m][1] = imageOld[m][1] + lambda*(cN*diffN + cS*diffS +
					       cE*diffE + cW*diffW);

	/* process lower right corner */
	diffN = imageOld[m-1][n] - imageOld[m][n];
	diffS = imageOld[m][n]   - imageOld[m][n];
	diffE = imageOld[m][n]   - imageOld[m][n];
	diffW = imageOld[m][n-1] - imageOld[m][n];
	
	if (CC == 1) {
	    cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
	    cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
	    cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
	    cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	}
	else if (CC == 2) {
	    cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
	    cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
	    cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
	    cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	}
	else if (CC == 3) { /* use Tukey Biweight */
	    
	    /* assumes image has maximum value
	     *    of 1 and minimum value of 0
	     */
	    normConst = 2 / (( 1 - (1/k)*(1/k) ) *
			     ( 1 - (1/k)*(1/k) ));
	    
	    if ( fabs(diffN) > k )
		cN = 0;
	    else {
		cN = 0.5*(1 - ( fabs(diffN) / k )*
			  ( fabs(diffN) / k ) ) *
		    (1 - ( fabs(diffN) / k )*
		     ( fabs(diffN) / k ) );
	    }
	    
	    if ( fabs(diffS) > k )
		cS = 0;
	    else {
		cS = 0.5*(1 - ( fabs(diffS) / k )*
			  ( fabs(diffS) / k ) ) *
		    (1 - ( fabs(diffS) / k )*
		     ( fabs(diffS) / k ) );
	    }
	    
	    if ( fabs(diffE) > k )
		cE = 0;
	    else {
		cE = 0.5*(1 - ( fabs(diffE) / k )*
			  ( fabs(diffE) / k ) ) *
		    (1 - ( fabs(diffE) / k )*
		     ( fabs(diffE) / k ) );
	    }
	    
	    if ( fabs(diffW) > k )
		cW = 0;
	    else {
		cW = 0.5*(1 - ( fabs(diffW) / k )*
			  ( fabs(diffW) / k ) ) *
		    (1 - ( fabs(diffW) / k )*
		     ( fabs(diffW) / k ) );
	    }		    
	}
	else
	    ; /* no-op */
	
	image[m][n] = imageOld[m][n] + lambda*(cN*diffN + cS*diffS +
					       cE*diffE + cW*diffW);


	/* left column */
	for (i = 1; i < m; i++) {
	    diffN = imageOld[i-1][1] - imageOld[i][1];
	    diffS = imageOld[i+1][1] - imageOld[i][1];
	    diffE = imageOld[i][2]   - imageOld[i][1];
	    diffW = imageOld[i][1]   - imageOld[i][1];
	    
	    if (CC == 1) {
		cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	    }
	    else if (CC == 2) {
		cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	    }
	    else if (CC == 3) { /* use Tukey Biweight */
		
		/* assumes image has maximum value
		 *    of 1 and minimum value of 0
		 */
		normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				 ( 1 - (1/k)*(1/k) ));
		
		if ( fabs(diffN) > k )
		    cN = 0;
		else {
		    cN = 0.5*(1 - ( fabs(diffN) / k )*
			      ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
		}
		
		if ( fabs(diffS) > k )
		    cS = 0;
		else {
		    cS = 0.5*(1 - ( fabs(diffS) / k )*
			      ( fabs(diffS) / k ) ) *
			(1 - ( fabs(diffS) / k )*
			 ( fabs(diffS) / k ) );
		}
		
		if ( fabs(diffE) > k )
		    cE = 0;
		else {
		    cE = 0.5*(1 - ( fabs(diffE) / k )*
			      ( fabs(diffE) / k ) ) *
			(1 - ( fabs(diffE) / k )*
			 ( fabs(diffE) / k ) );
		}
		
		if ( fabs(diffW) > k )
		    cW = 0;
		else {
		    cW = 0.5*(1 - ( fabs(diffW) / k )*
			      ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		}		    
	    }
	    else
		; /* no-op */
	    
	    image[i][1] = imageOld[i][1] + lambda*(cN*diffN + cS*diffS +
						   cE*diffE + cW*diffW);
	}
	
	/* right column */
	for (i = 1; i < m; i++) {	   
	    diffN = imageOld[i-1][n] - imageOld[i][n];
	    diffS = imageOld[i+1][n] - imageOld[i][n];
	    diffE = imageOld[i][n]   - imageOld[i][n];
	    diffW = imageOld[i][n-1] - imageOld[i][n];
	    
	    if (CC == 1) {
		cN = exp(-((fabs(diffN)/k)*(fabs(diffN)/k)));
		cS = exp(-((fabs(diffS)/k)*(fabs(diffS)/k)));
		cE = exp(-((fabs(diffE)/k)*(fabs(diffE)/k)));
		cW = exp(-((fabs(diffW)/k)*(fabs(diffW)/k)));
	    }
	    else if (CC == 2) {
		cN = 1/(1+(fabs(diffN)/k)*(fabs(diffN)/k));
		cS = 1/(1+(fabs(diffS)/k)*(fabs(diffS)/k));
		cE = 1/(1+(fabs(diffE)/k)*(fabs(diffE)/k));
		cW = 1/(1+(fabs(diffW)/k)*(fabs(diffW)/k));
	    }
	    else if (CC == 3) { /* use Tukey Biweight */
		
		/* assumes image has maximum value
		 *    of 1 and minimum value of 0
		 */
		normConst = 2 / (( 1 - (1/k)*(1/k) ) *
				 ( 1 - (1/k)*(1/k) ));
		
		if ( fabs(diffN) > k )
		    cN = 0;
		else {
		    cN = 0.5*(1 - ( fabs(diffN) / k )*
			      ( fabs(diffN) / k ) ) *
			(1 - ( fabs(diffN) / k )*
			 ( fabs(diffN) / k ) );
		}
		
		if ( fabs(diffS) > k )
		    cS = 0;
		else {
		    cS = 0.5*(1 - ( fabs(diffS) / k )*
			      ( fabs(diffS) / k ) ) *
			(1 - ( fabs(diffS) / k )*
			 ( fabs(diffS) / k ) );
		}
		
		if ( fabs(diffE) > k )
		    cE = 0;
		else {
		    cE = 0.5*(1 - ( fabs(diffE) / k )*
			      ( fabs(diffE) / k ) ) *
			(1 - ( fabs(diffE) / k )*
			 ( fabs(diffE) / k ) );
		}
		
		if ( fabs(diffW) > k )
		    cW = 0;
		else {
		    cW = 0.5*(1 - ( fabs(diffW) / k )*
			      ( fabs(diffW) / k ) ) *
			    (1 - ( fabs(diffW) / k )*
			     ( fabs(diffW) / k ) );
		}		    
	    }
	    else
		; /* no-op */
	    
	    image[i][n] = imageOld[i][n] + lambda*(cN*diffN + cS*diffS +
						   cE*diffE + cW*diffW);
	}	
    }    
}
