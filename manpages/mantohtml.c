/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.1  2003/10/08 17:22:16  mast
Added a return value at the end fo main

*/
#include <stdio.h>

void putspec(int c);

int main(int argc , char **argv)
{
  int c;
  int last = 0;
  int bold = 0;
  int italic = 0;

  char *htitle = "Man Page";

  if (argc > 1)
    htitle = argv[1];

  printf("<html><head><title>%s</title></head><body><pre>\n", htitle);

  while (last != EOF) {
    c = getchar();
    if (c == '\b') {

      /* If it is backspace, get next character */
      c = getchar();
      if (c == last) {

        /* If characters match, it is bold; turn off italic and turn on bold */
        if (italic)
          printf("</i>");
        if (!bold)
          printf("<b>");
        italic = 0;
        bold = 1;
        putspec(c);
        last = getchar();
        continue;

      } else if (last == '_') {

        /* Or if last was _, it is italic; turn off bold and turn on italic */
        if (bold)
          printf("</b>");
        if (!italic)
          printf("<i>");
        italic = 1;
        bold = 0;
        putspec(c);
        last = getchar();
        continue;
      }
    }
    
    /* Neither case: turn off bold and italic, dump last character */
    if (bold)
      printf("</b>");
    if (italic)
      printf("</i>");
    bold = 0;
    italic = 0;
    putspec(last);
    last = c;
  }

  printf("</pre></body></html>\n");
  return 0;
}

void putspec(int c)
{
  if (c){
    switch(c){
    case '<':
      printf("&lt;");
      break;
    case '>':
      printf("&gt;");
      break;
    case '&':
      printf("&amp;");
      break;
    case '\"':
      printf("&quot;");
      break;
    default:
      putchar(c);
    }
  }
}
