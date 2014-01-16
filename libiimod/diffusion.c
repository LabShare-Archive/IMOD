/* diffusion.c
 * Based on a parallel version written by Alejandro Cantarero
 * Modified by David Mastronarde
 *
 */
/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

#include <math.h>

/* routine that computes I^{n+1} */
void updateMatrix(float **image, float **imageOld, int m, int n,
                  int CC, double k, double lambda) {
  double      diffN;
  double      diffS;
  double      diffE;
  double      diffW;
  double      cN;
  double      cS;
  double      cE;
  double      cW;
  double      gradN, gradS, gradE, gradW;
  /*double      wCen = 0.5;
    double      wSide = 0.25; */
  double      ksq = k * k;
  int         i, j, ip1, im1, jp1, jm1;
    
  for (i = 1; i < m+1; i++) {
    ip1 = i == m ? m : i + 1;
    im1 = i == 1 ? 1 : i - 1;
    for (j = 1; j < n + 1; j++) {
      jp1 = j == n ? n : j + 1;
      jm1 = j == 1 ? 1 : j - 1;
      
      gradN = diffN = imageOld[im1][j] - imageOld[i][j];
      gradS = diffS = imageOld[ip1][j] - imageOld[i][j];
      gradE = diffE = imageOld[i][jp1] - imageOld[i][j];
      gradW = diffW = imageOld[i][jm1] - imageOld[i][j];

      /* Trying to compute a more robust gradient eliminates more diagonal
         noise and leaves horizontal/vertical artifacts */
      /*gradN = wCen * diffN + wSide * (imageOld[im1][jp1] - imageOld[i][jp1] +
                                      imageOld[im1][jm1] - imageOld[i][jm1]);
      gradS = wCen * diffS + wSide * (imageOld[ip1][jp1] - imageOld[i][jp1] +
                                      imageOld[ip1][jm1] - imageOld[i][jm1]);
      gradE = wCen * diffE + wSide * (imageOld[ip1][jp1] - imageOld[ip1][j] +
                                      imageOld[im1][jp1] - imageOld[im1][j]);
      gradW = wCen * diffW + wSide * (imageOld[ip1][jm1] - imageOld[ip1][j] +
      imageOld[im1][jm1] - imageOld[im1][j]);*/
      if (CC == 1) {
        cN = exp(-gradN * gradN / ksq);
        cS = exp(-gradS * gradS / ksq);
        cE = exp(-gradE * gradE / ksq);
        cW = exp(-gradW * gradW / ksq);
      }
      else if (CC == 2) {
        cN = 1 / (1 + gradN * gradN / ksq);
        cS = 1 / (1 + gradS * gradS / ksq);
        cE = 1 / (1 + gradE * gradE / ksq);
        cW = 1 / (1 + gradW * gradW / ksq);
      }
      else { /* use Tukey Biweight */
            
        if ( fabs(diffN) > k )
          cN = 0;
        else
          cN = 0.5 * (1 - gradN * gradN / ksq) * (1 - gradN * gradN / ksq);

        if ( fabs(diffS) > k )
          cS = 0;
        else
          cS = 0.5 * (1 - gradS * gradS / ksq) * (1 - gradS * gradS / ksq);

        if ( fabs(diffE) > k )
          cE = 0;
        else
          cE = 0.5 * (1 - gradE * gradE / ksq) * (1 - gradE * gradE / ksq);

        if ( fabs(diffW) > k )
          cW = 0;
        else
          cW = 0.5 * (1 - gradW * gradW / ksq) * (1 - gradW * gradW / ksq);
      }
                
      image[i][j] = imageOld[i][j] + lambda*(cN*diffN + cS*diffS +
                                             cE*diffE + cW*diffW);
    }
  }
}
