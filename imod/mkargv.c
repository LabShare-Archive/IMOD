#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
     int c;
     FILE *fin, *fout;
     
     if (argc < 4)
	  exit(1);
     
     fin = fopen(argv[1], "r");
     if (!fin)
	  exit(3);
     
     fout = fopen(argv[2], "w");
     if (!fout)
	  exit(3);
     
     fprintf(fout, "char *%s = {\n", argv[3]);



     fprintf(fout, "\"");
     while ((c = fgetc(fin)) != EOF ){
	  
	  switch(c){
	       
	     case '\n':
	       fprintf(fout,"\\n\",\n\"");
	       break;
	       
	     case '\t':
	       fprintf(fout, "\\t");
	       break;
	       
	     case '\b':
	       if (!((c = fgetc(fin)) != EOF )){
                    fprintf(fout, "\",\nNULL};\n");
		    exit(0);
	       }
	       break;

	     case '\\':
	       fprintf(fout, "\\\\");
	       break;

	     case '_':
	       c = fgetc(fin);
	       if (c != '\b')
		    fprintf(fout, "_%c", c);
	       break;
	       
	     default:
	       fprintf(fout, "%c", c);
	       break;
	       
           }
     }
     fprintf(fout, "\",\n NULL};\n");
     exit(0);
     return(0);
}

