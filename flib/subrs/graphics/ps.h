#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

typedef struct
{
  FILE *fp;
  double dpi;  /* resolution in dots per inch */
  
  double cx, cy;  /* Current position. */
  double xoffset, yoffset;
  double lineWidth;
  int    font_center;
  double fontSize;
  double pointSize;
  int    pointStyle;
  int    red;
  int    green;
  int    blue;
  char   fontName[80];
    
}PS;

/* Text placement defines. */
#define PS_CENTERED        4
#define PS_LEFT_JUST       3
#define PS_RIGHT_JUST      2
#define PS_VERT_CENTERED   0
#define PS_VERT_LEFT_JUST -1
#define PS_VERT_RIGHT_JUST 1

/* Superscript, subscript, and symbol font defines */
#define ESC_CHAR  '^'
#define SUB_CHAR  'B'
#define SUP_CHAR  'P'
#define SYM_CHAR  'S'
#define PS_REGULAR      0
#define PS_SUPERSCRIPT  1
#define PS_SUBSCRIPT    2
#define PS_SYMBOL       3
#define MAXSEG 20
#define SUPSCALE  0.8f
#define SUBSCALE  0.8f
#define SYMSCALE  1.0f
#define SUPRISE   0.33f
#define SUBDROP   0.33f

PS   *PSopen(char *filename, double dpi, double x, double y);
void  PSpage(PS *ps);   /* create a new page, send old page to output device.*/
void  PSclose(PS *ps);  /* close ps file and send to output device. */

/* low level drawing */
void  PSsetPoint(PS *ps, double x, double y);
void  PSdrawPoint(PS *ps, double x, double y);
void  PSdrawVector(PS *ps, double x, double y);
void  PSsetLineWidth(PS *ps, double width);
void  PSdrawCircle(PS *ps, double x, double y, double rad, int fill);
void  PSdrawTriangle(PS *ps, double *x, double *y, int fill);
void  PSdrawQuadrangle(PS *ps, double *x, double *y, int fill);
void  PSsetColor(PS *ps, int red, int green, int blue);

/*void  PSdrawLine(PS *ps, double x1, double y1, double x2, double y2); */


/* text drawing */
void  PSsetFont(PS *ps, char *name, int size);
void  PSdrawText(PS *ps, char *text, double x, double y,
                      int rotation, int placement);
