#include <stdlib.h>
#include "ps.h"

static void PSpageHeader(PS *inPS);

PS *PSopen(char *filename, double dpi, double x, double y)
{
    FILE *fp;

    fp = fopen(filename, "w");
    if (!(fp)){
	perror("libps: error getting output file. ");
	return(NULL);
    }
    return(PSnew(fp, dpi, x, y));
}

PS *PSnew(FILE *fp, double dpi, double x, double y)
{
    PS *ps = (PS *)malloc(sizeof(PS));
    if (!ps){
	fprintf(stderr, "libps: error getting memory\n");
	return(NULL);
    }
    ps->fp = fp;

     ps->dpi = dpi;
     ps->scale = 72.0 / dpi;
     ps->xoffset = x;
     ps->yoffset = y;
     ps->cx = ps->cy = 0.0;
     ps->pointSize = 1.0/dpi;

     fprintf(ps->fp, "%%!PS-Adobe-3.0 EPSF-3.0\n");
     fprintf(ps->fp, "%%Boulder-Laboratory-for-3D-Fine-Structure\n");
     fprintf(ps->fp, "%%%%Created by the BL3DFS PS Library. Compiled %s %s\n",
	     __DATE__, __TIME__);
     PSpageHeader(ps);
     return(ps);
}

void PSpageHeader(PS *ps)     
{
     fprintf(ps->fp, "%%BoundingBox: 0 0 %g %g\n",
	     8.5 * 72.0, 11.0 * 72.0);

     fprintf(ps->fp, "/inch {%g mul} def\n", ps->dpi);
     fprintf(ps->fp, "%g %g scale\n", 72.0 / ps->dpi, 72.0 / ps->dpi);
     fprintf(ps->fp, "%g inch %g inch translate\n", 
	     ps->xoffset, ps->yoffset);

     fprintf(ps->fp, "\n%%%%-----Define procedures-----\n");
     fprintf(ps->fp, "/drawpoint %% stack: x y\n"
	     "{newpath moveto 1 0 rlineto 0 1 rlineto -1 0 rlineto "
	     "closepath fill} def\n");
     fprintf(ps->fp, "/drawline { newpath moveto lineto stroke } def\n");
     fprintf(ps->fp, "/rightshow %%stk: string\n"
	     "{dup stringwidth pop 0 exch sub 0 rmoveto show } def\n");
     fprintf(ps->fp, "/centershow %%stk: string\n"
	     "{dup stringwidth pop 0 exch sub 2 div 0 rmoveto show } def\n");
     fprintf(ps->fp, "/drawcircle %% stack: x y r\n"
	     "{ newpath 0 360 arc stroke } def\n");
     fprintf(ps->fp, "/drawfcircle %% stack: x y r\n"
	     "{ newpath 0 360 arc fill } def\n");
     fprintf(ps->fp, "\n%%%%-----Graphics output-----\n");
}

void  PSclose(PS *ps)
{
     fprintf(ps->fp, "showpage\n");
     fclose(ps->fp);
     free(ps);
}

void  PSpage(PS *ps)
{
     fprintf(ps->fp, "showpage\n");
     PSpageHeader(ps);
}

void  PSdrawPoint(PS *ps, double x, double y)
{
/*     fprintf(ps->fp, "%g inch %g inch drawpoint\n", x, y);*/
     fprintf(ps->fp, "%g %g drawpoint\n", x * ps->dpi, y * ps->dpi);
}

void  PSdrawCircle(PS *ps, double x, double y, double r)
{
/*     fprintf(ps->fp, "%g inch %g inch %g inch drawcircle\n",
	     x, y, r);
*/
     fprintf(ps->fp, "%g %g %g drawcircle\n",
             x * ps->dpi, y * ps->dpi, r * ps->dpi);
}
void  PSdrawFilledCircle(PS *ps, double x, double y, double r)
{
/*     fprintf(ps->fp, "%g inch %g inch %g inch drawfcircle\n",
	     x, y, r);
*/
     fprintf(ps->fp, "%g %g %g drawfcircle\n",
	     x * ps->dpi, y * ps->dpi, r * ps->dpi);
}

void  PSsetPoint(PS *ps, double x, double y)
{
     ps->cx = x;
     ps->cy = y;
}

void  PSdrawVector(PS *ps, double x, double y)
{
/*     fprintf(ps->fp, "%g inch %g inch %g inch %g inch drawline\n",
	     ps->cx, ps->cy, x, y);
*/
     fprintf(ps->fp, "%g %g %g %g drawline\n",
	     ps->cx * ps->dpi, ps->cy * ps->dpi, 
	     x * ps->dpi, y * ps->dpi);
     ps->cx = x;
     ps->cy = y;
}

void  PSsetFont(PS *ps, char *name, int size)
{
     ps->fontSize = (double)(size) * ps->dpi / 72.0;
     fprintf(ps->fp, "/%s findfont %g scalefont setfont\n",
	     name, ps->fontSize);
}

void PScolor(PS *ps, double red, double green, double blue)
{
     fprintf(ps->fp, "%g %g %g setrgbcolor\n",
	     red, green, blue);
}

void  PSdrawText(PS *ps, char *text, 
		 double x, double y,
		 int rotation, int placement)
{
     if (ps->fontSize <= 0.0){
	  fprintf(stderr, "libps: bad font size\n");
	  return;
     }

     fprintf(ps->fp, "%g inch %g inch moveto\n", x, y );

     if (rotation)
	  fprintf(ps->fp, "gsave\n %d rotate\n", rotation);

     switch(placement){

	case PS_CENTERED:
	  fprintf(ps->fp, "(%s) centershow\n", text);
	  break;
	  
	case PS_RIGHT_JUST:
	  fprintf(ps->fp, "(%s) rightshow\n", text);
	  break;
	  
	case PS_LEFT_JUST:
	  fprintf(ps->fp, "(%s) show\n", text);
	  break;
	  
	case PS_VERT_CENTERED:
	  fprintf(ps->fp, "0 %g rmoveto\n", ps->fontSize * -0.33);
	  fprintf(ps->fp, "(%s) centershow\n", text);
	  break;

	case PS_VERT_RIGHT_JUST:
	  fprintf(ps->fp, "0 %g rmoveto\n", ps->fontSize * -0.33);
	  fprintf(ps->fp, "(%s) rightshow\n", text);
	  break;
	  
	case PS_VERT_LEFT_JUST:
	  fprintf(ps->fp, "0 %g rmoveto\n", ps->fontSize * -0.33);
	  fprintf(ps->fp, "(%s) show\n", text);
	  break;
     }

     if (rotation)
	  fprintf(ps->fp, "grestore\n");
}



