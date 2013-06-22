/* Postscript drawing routines
 *
 *  Copyright (C) 1995-2012 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include "ps.h"
#include "b3dutil.h"

PS *PSopen(char *filename, double dpi, double x, double y)
{
  PS *ps = (PS *)malloc(sizeof(PS));

  if (!ps){
    fprintf(stderr, "PSopen: error getting memory\n");
    return(NULL);
  }

  ps->fp = fopen(filename, "w");
  if (!(ps->fp)){
    free(ps);
    perror("PSopen: error getting output file. ");
    return(NULL);
  }

  ps->dpi = dpi;
  ps->xoffset = x;
  ps->yoffset = y;
  ps->cx = ps->cy = 0.0;
  ps->pointSize = 1.0/dpi;
  ps->lineWidth = 1;

  fprintf(ps->fp, "%%!PS-Boulder-Laboratory-for-3D-EM-of-Cells\n");
  fprintf(ps->fp, "%%%%Created by the BL3DEMC PS module. Compiled %s %s\n",
          __DATE__, __TIME__);
     
  fprintf(ps->fp, "/inch {%g mul} def\n", dpi);
  fprintf(ps->fp, "%g %g scale\n", 72.0/dpi, 72.0/dpi);
  fprintf(ps->fp, "%g inch %g inch translate\n", x, y);
  fprintf(ps->fp, "1 setlinewidth\n0 setgray\n");

  fprintf(ps->fp, "\n%%%%-----Define procedures-----\n");
  fprintf(ps->fp, "/drawpoint %% stack: x y\n"
          "{newpath moveto 1 0 rlineto 0 1 rlineto -1 0 rlineto "
          "closepath fill} def\n");
  fprintf(ps->fp, "/drawline { newpath moveto lineto stroke } def\n");
  fprintf(ps->fp, "/opencircle { newpath 0 360 arc stroke } def\n");
  fprintf(ps->fp, "/fillcircle { newpath 0 360 arc fill } def\n");
  fprintf(ps->fp, "/opentri { newpath moveto lineto lineto closepath "
          "stroke } def\n");
  fprintf(ps->fp, "/filltri { newpath moveto lineto lineto closepath "
          "fill } def\n");
  fprintf(ps->fp, "/openquad { newpath moveto lineto lineto lineto closepath "
          "stroke } def\n");
  fprintf(ps->fp, "/fillquad { newpath moveto lineto lineto lineto closepath "
          "fill } def\n");
  fprintf(ps->fp, "/rightshow %%stk: string\n"
          "{dup stringwidth pop neg 0 rmoveto show } def\n");
  fprintf(ps->fp, "/centershow %%stk: string\n"
          "{dup stringwidth pop neg 2 div 0 rmoveto show } def\n");
  fprintf(ps->fp, "\n%%%%-----Graphics output-----\n");
  return(ps);
}

void  PSclose(PS *ps)
{
  fclose(ps->fp);
  free(ps);
}

void  PSpage(PS *ps)
{
  fprintf(ps->fp, "showpage\n");
  fprintf(ps->fp, "%g %g scale\n", 72.0/ps->dpi, 72.0/ps->dpi);
  fprintf(ps->fp, "%g inch %g inch translate\n", ps->xoffset, ps->yoffset);
}

void  PSdrawPoint(PS *ps, double x, double y)
{
  fprintf(ps->fp, "%g inch %g inch drawpoint\n", x, y);
}

void  PSsetPoint(PS *ps, double x, double y)
{
  ps->cx = x;
  ps->cy = y;
}

void PSsetLineWidth(PS *ps, double width)
{
  if (width != ps->lineWidth)
    fprintf(ps->fp, "%g setlinewidth\n", width);
  ps->lineWidth = width;
}

void  PSdrawVector(PS *ps, double x, double y)
{
  fprintf(ps->fp, "%g inch %g inch %g inch %g inch drawline\n",
          ps->cx, ps->cy, x, y);
  ps->cx = x;
  ps->cy = y;
}

void  PSdrawCircle(PS *ps, double x, double y, double rad, int fill)
{
    fprintf(ps->fp, "%g inch %g inch %g inch %scircle\n", x, y, rad, 
            fill ? "fill" : "open");
}

void  PSdrawTriangle(PS *ps, double *x, double *y, int fill)
{
    fprintf(ps->fp, "%g inch %g inch %g inch %g inch %g inch %g inch %stri\n",
            x[0], y[0], x[1], y[1], x[2], y[2], fill ? "fill" : "open");
}

void  PSdrawQuadrangle(PS *ps, double *x, double *y, int fill)
{
    fprintf(ps->fp, "%g inch %g inch %g inch %g inch %g inch %g inch "
            "%g inch %g inch %squad\n", x[0], y[0], x[1], y[1], x[2], y[2],
            x[3], y[3], fill ? "fill" : "open");
}

void  PSsetFont(PS *ps, char *name, int size)
{
  ps->fontSize = (double)(size) * ps->dpi / 72.0;
  strcpy(ps->fontName, name);
  fprintf(ps->fp, "/%s findfont %g scalefont setfont\n",
          name, ps->fontSize);
}

void  PSsetColor(PS *ps, int red, int green, int blue)
{
  if (ps->red == red && ps->green == green && ps->blue == blue)
    return;
  ps->red = B3DMAX(0, B3DMIN(255, red));
  ps->green = B3DMAX(0, B3DMIN(255, green));
  ps->blue = B3DMAX(0, B3DMIN(255, blue));
  fprintf(ps->fp, "%f %f %f setrgbcolor\n", ps->red / 255., ps->green / 255., 
          ps->blue / 255.);
}

void  PSdrawText(PS *ps, char *text, 
                 double x, double y,
                 int rotation, int placement)
{
  int indstr[MAXSEG], indend[MAXSEG], typestrng[MAXSEG];
  char *temp;
  char *tcpy;
  int i, ii, octcode, ndig, j, lentext, iseg;
  int nseg = 0;
  int lasttext = 0;
  char code;

  if (ps->fontSize <= 0.0){
    fprintf(stderr, "PSdrawText: bad font size\n");
    return;
  }

  fprintf(ps->fp, "%g inch %g inch moveto\n", x, y );

  if (rotation)
    fprintf(ps->fp, "gsave\n %d rotate\n", rotation);

  /* Copy text string, putting \ before any parentheses to keep PS happy 
     and converting \nnn to single character */ 

  lentext = strlen(text);
  tcpy = (char *) malloc(lentext + 20);
  temp = (char *) malloc(lentext + 20);
  j = 0;
  for (i = 0; i < lentext; i++) {
    if (text[i] == '(' || text[i] == ')') tcpy[j++] = '\\';
    if (text[i] == '\\') {
      ii = i + 1;
      ndig = 0;
      octcode = 0;
      while (ndig < 3 && ii < lentext) {
        if (text[ii] >= '0' && text[ii] <= '9') {
          octcode = 8 * octcode + ( (int)text[ii] - 48);
          ndig++;
          ii++;
        }
        else
          break;
      }
      if (ndig == 3) {
        tcpy[j++] = octcode;
        i = ii - 1;
      }
      else
        tcpy[j++] = text[i];
    }
    else
      tcpy[j++] = text[i];
  }
  lentext = j;
  tcpy[j] = '\0';

  /* Parse the string into regular text and special characters */

  i = 0;
  do  {
    if ( tcpy[i++] == ESC_CHAR ) {
      if ((code = tcpy[i++]) == SUP_CHAR || code == SUB_CHAR || 
          code == SYM_CHAR) {
        if ( i - 3 >= lasttext ) {
          typestrng[nseg] = PS_REGULAR;
          indstr[nseg] = lasttext;
          indend[nseg++] = i - 3;
        }
        switch (code) {
         
        case SUP_CHAR:
          typestrng[nseg] = PS_SUPERSCRIPT;
          break;
        case SUB_CHAR:
          typestrng[nseg] = PS_SUBSCRIPT;
          break;
        case SYM_CHAR:
          typestrng[nseg] = PS_SYMBOL;
          break;
        }
        indstr[nseg++] = i++;
        lasttext = i;
      }
    }

  } while (i < lentext );

  if (lasttext < lentext) {
    typestrng[nseg] = PS_REGULAR;
    indstr[nseg] = lasttext;
    indend[nseg++] = lentext-1;
  }

  switch(placement){
  case PS_VERT_CENTERED:
  case PS_VERT_RIGHT_JUST:
  case PS_VERT_LEFT_JUST:
    fprintf(ps->fp, "0 %g rmoveto\n", ps->fontSize * -0.33);
    break;
  }

  /* Use the defined procedures for pure text */

  if (nseg < 2 && typestrng[0] == PS_REGULAR) {

    switch(placement){

    case PS_CENTERED:
    case PS_VERT_CENTERED:
      fprintf(ps->fp, "(%s) centershow\n", tcpy);
      break;
      
    case PS_RIGHT_JUST:
    case PS_VERT_RIGHT_JUST:
      fprintf(ps->fp, "(%s) rightshow\n", tcpy);
      break;
      
    case PS_LEFT_JUST:
    case PS_VERT_LEFT_JUST:
      fprintf(ps->fp, "(%s) show\n", tcpy);
      break;
      
    }
  }

  else {

    /* Otherwise first output commands to add up the string width */

    if (placement != PS_LEFT_JUST && placement != PS_VERT_LEFT_JUST) {
      fprintf(ps->fp, "0\n");
      for (iseg = 0; iseg < nseg; iseg++) {

        if (typestrng[iseg] == PS_REGULAR) {
          strncpy(temp, tcpy+indstr[iseg], indend[iseg] + 1 - indstr[iseg]);
          temp[indend[iseg] + 1 - indstr[iseg]] = '\0';
          fprintf(ps->fp, "(%s) stringwidth pop add\n", temp);
        }
        else {
          switch (typestrng[iseg]) {

          case PS_SUPERSCRIPT:
            fprintf(ps->fp, "/%s findfont %g scalefont setfont\n",
                    ps->fontName, ps->fontSize * SUPSCALE);
            break;

          case PS_SUBSCRIPT:
            fprintf(ps->fp, "/%s findfont %g scalefont setfont\n",
                    ps->fontName, ps->fontSize * SUBSCALE);
            break;   

          case PS_SYMBOL:
            fprintf(ps->fp, "/Symbol findfont %g scalefont setfont\n",
                    ps->fontSize * SYMSCALE);
            break;
          }

          fprintf(ps->fp, "(%c) stringwidth pop add\n", tcpy[indstr[iseg]]);
          fprintf(ps->fp, "/%s findfont %g scalefont setfont\n",
                  ps->fontName, ps->fontSize);
        }
      }

      /* Shift for centered and right justified text */
     
      if (placement == PS_CENTERED || placement == PS_VERT_CENTERED)
        fprintf(ps->fp, "neg 2 div 0 rmoveto\n");
      else
        fprintf(ps->fp, "neg 0 rmoveto\n");
    }

    /* Finally, output commands to do all text and special characters */

    for (iseg = 0; iseg < nseg; iseg++) {
     
      if (typestrng[iseg] == PS_REGULAR) {
        strncpy(temp, tcpy+indstr[iseg], indend[iseg] + 1 - indstr[iseg]);
        temp[indend[iseg] + 1 - indstr[iseg]] = '\0';
        fprintf(ps->fp, "(%s) show\n", temp);
      }
      else {
        switch (typestrng[iseg]) {
         
        case PS_SUPERSCRIPT:
          fprintf(ps->fp, "/%s findfont %g scalefont setfont\n",
                  ps->fontName, ps->fontSize * SUPSCALE);
          fprintf(ps->fp, "0 %g rmoveto\n", ps->fontSize * SUPRISE);
          fprintf(ps->fp, "(%c) show\n", tcpy[indstr[iseg]]);
          fprintf(ps->fp, "0 %g rmoveto\n", -ps->fontSize * SUPRISE);
          break;

        case PS_SUBSCRIPT:
          fprintf(ps->fp, "/%s findfont %g scalefont setfont\n",
                  ps->fontName, ps->fontSize * SUBSCALE);
          fprintf(ps->fp, "0 %g rmoveto\n", -ps->fontSize * SUBDROP);
          fprintf(ps->fp, "(%c) show\n", tcpy[indstr[iseg]]);
          fprintf(ps->fp, "0 %g rmoveto\n", ps->fontSize * SUBDROP);
          break; 

        case PS_SYMBOL:
          fprintf(ps->fp, "/Symbol findfont %g scalefont setfont\n",
                  ps->fontSize * SYMSCALE);
          fprintf(ps->fp, "(%c) show\n", tcpy[indstr[iseg]]);
          break;
        }
        fprintf(ps->fp, "/%s findfont %g scalefont setfont\n",
                ps->fontName, ps->fontSize);
      }
    }
  }
  if (rotation)
    fprintf(ps->fp, "grestore\n");
  free(tcpy);
  free(temp);

}


