#include <stdio.h>
#include <math.h>

/* All units are in inches. */

typedef struct
{
     FILE *fp;
     double dpi;  /* resolution in dots per inch */
     double scale;

     double cx, cy;  /* Current position. */
     double xoffset, yoffset;
     int    font_center;
     double fontSize;
     double pointSize;
     int    pointStyle;
}PS;

/* Text placement defines. */
#define PS_CENTERED        4
#define PS_LEFT_JUST       3
#define PS_RIGHT_JUST      2
#define PS_VERT_CENTERED   0
#define PS_VERT_LEFT_JUST -1
#define PS_VERT_RIGHT_JUST 1

PS   *PSnew(FILE *fp, double dpi, double x, double y);
PS   *PSopen(char *filename, double dpi, double x, double y);
void  PSpage(PS *ps);   /* create a new page, send old page to output device.*/
void  PSclose(PS *ps);  /* close ps file and send to output device. */

void  PScolor(PS *ps, double red, double green, double blue);

/* low level drawing */
void  PSsetPoint(PS *ps, double x, double y);
void  PSdrawPoint(PS *ps, double x, double y);
void  PSdrawVector(PS *ps, double x, double y);

void  PSdrawLine(PS *ps, double x1, double y1, double x2, double y2);
void  PSdrawFilledCircle(PS *ps, double x, double y, double r);
void  PSdrawCircle(PS *ps, double x, double y, double r);
     

/* text drawing */
void  PSsetFont(PS *ps, char *name, int size);
void  PSdrawText(PS *ps, char *text, double x, double y,
                      int rotation, int placement);
