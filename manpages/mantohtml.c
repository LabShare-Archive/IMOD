/* 
 *  $Id$
 */

#include <stdio.h>
#include <string.h>
#include "proglist.h"

void putspec(int c);
int nextchar();

#define INSIZE 1000
#define PROCSIZE 10000

static char *htitle = "Man Page";

int main(int argc , char **argv)
{
  int c;
  int last = 0;
  int bold = 0;
  int italic = 0;



  if (argc > 1)
    htitle = argv[1];

  printf("<html><head><title>%s</title></head><body><a name=\"TOP\"></a><pre>\n", htitle);

  while (last != EOF) {
    c = nextchar();
    if (c == '\b') {

      /* If it is backspace, get next character */
      c = nextchar();
      if (c == last) {

        /* If characters match, it is bold; turn off italic and turn on bold */
        if (italic)
          printf("</i>");
        if (!bold)
          printf("<b>");
        italic = 0;
        bold = 1;
        putspec(c);
        last = nextchar();
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
        last = nextchar();
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

/* Translate special characters and translate protected characters back */
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
    case 240:
      printf("<");
      break;
    case 241:
      printf(">");
      break;
    case 242:
      printf("\"");
      break;
    default:
      putchar(c);
    }
  }
}

int nextchar()
{
  static unsigned char inbuf[INSIZE];
  static unsigned char procbuf[PROCSIZE];
  static int bufind = -1;
  static int buflen = -1;
  char candidate[50], link[100];
  char *result;
  unsigned char *parstr;
  int list, i, progind, parind, dosub, ncomp, copyStart, caps;

  if (bufind >= 0 && bufind < buflen)
    return procbuf[bufind++];
  result = fgets(inbuf, INSIZE, stdin);
  if (!result)
    return EOF;
  
  buflen = 0;
  copyStart = 0;
  while (1) {
    parstr = strstr(&inbuf[copyStart], "(1)");

    /* If can't find substring copy the rest of buffer */
    if (!parstr) {
      while (inbuf[copyStart] != 0 && buflen < PROCSIZE - 1)
        procbuf[buflen++] = inbuf[copyStart++];
      procbuf[buflen] = 0x00;
      break;
    }

    /* Back up until previous space or start of uncopied string */
    parind = parstr - &inbuf[0];
    progind = parind;
    while (progind > copyStart && inbuf[progind - 1] != ' ')
      progind--;

    /* Copy to a separate string and compare with program names, but exclude
       self */
    dosub = 0;
    ncomp = parind - progind;
    if (ncomp > 0 && ncomp < 40) {
      strncpy(candidate, &inbuf[progind], ncomp);
      candidate[ncomp] = 0x00;
      caps = 0;
      if (candidate[0] >= 65 && candidate[0] <= 90) {
        candidate[0] += 32;
        caps = 1;
      }
      if (strcmp(candidate, htitle)) {
        for (list = 0; programs[list]; list++) {
          if (!strcmp(candidate, programs[list])) {
            dosub = 1;
            if (caps)
              candidate[0] -= 32;
            break;
          }
        }
      }
    }
    if (dosub) {
      
      /* To do substitution, print the link, copy the stuff up to start of
         program name, and copy the link */
      sprintf(link, "<A HREF=\"%s.html\">%s</A>", programs[list],  candidate);
      while (copyStart < progind && buflen < PROCSIZE - 1)
        procbuf[buflen++] = inbuf[copyStart++];
      i = 0;

      /* Convert characters to protect them from special conversions */
      while (link[i] && buflen < PROCSIZE - 1) {
        if (link[i] == '<')
          procbuf[buflen++] = 240;
        else if (link[i] == '>')
          procbuf[buflen++] = 241;
        else if (link[i] == '\"')
          procbuf[buflen++] = 242;
        else
          procbuf[buflen++] = link[i];
        i++;
      }

    } else {

      /* Otherwise copy through the (1) */
      while (copyStart < parind + 3 && buflen < PROCSIZE - 1)
        procbuf[buflen++] = inbuf[copyStart++];
    }
    copyStart = parind + 3;
  }

  /* Finally, return first character on line */
  bufind = 1;
  return (procbuf[0]);
}
