#include "xplax.h"
void plax_initialize(char *string, int strsize)
{
  realgraphicsmain_();
  exit(0);
}

int plax_open(void) {}
void plax_close(void) {}
void plax_erase(void) {}
void plax_flush(void) {}
void plax_mapcolor(int *color, int *ired, int *igreen, int *iblue) {}
void plax_box(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2) {}
void plax_boxo(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2) {}
void plax_vect(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2) {}
void plax_vectw(int *linewidth, int *cindex,
		int *ix1, int *iy1, int *ix2, int *iy2) {}
void plax_circ(int *cindex, int *radius, int *ix, int *iy) {}
void plax_circo(int *cindex, int *radius, int *ix, int *iy) {}
void plax_poly(int *cindex, int *size, short *vec) {}
void plax_polyo(int *cindex, int *size, short *vec) {}
void plax_sctext(int *thickness,
		 int *xsize,
		 int *iysize,
		 int *cindex,
		 int *ix, int *iy,
#ifdef F77STRING
		 FString *f77str) {}
#else
                 char *string, int strsize) {}
#endif
