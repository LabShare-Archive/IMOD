/*  $Author$

    $Date$

    $Revision$

    $Log$
*/
#include <stdio.h>

int main(int argc , char **argv)
{
     int c;
     int last = 0;

     char *htitle = "Man Page";

     if (argc > 1)
	  htitle = argv[1];

     printf("<html><head><title>%s</title></head><body><pre>\n", htitle);

     while(EOF != (c = getchar())){
	  
	  switch(c){

	     case '\b':
	       if (last == '_'){
		    printf("<i>");
		    do{
			 c = getchar();
			 putchar(c);
			 c = getchar();

		    }while( (c == '_'));

		    printf("</i>");
	       }else{
		    int lastput = 0;
		    printf("<b>");
		    do{
			 c = getchar();
			 if (!lastput){
			      putchar(c);
			      lastput = c;
			 }
			 if (lastput != c){
			      putchar(c);
			      lastput = c;
			 }
			 c = getchar();
			 
		    }while( (c == '\b'));

		    printf("</b>");
	       }
	       last = c;
	       break;

	     default:
	       if (last){
		    switch(last){
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
			 putchar(last);
		    }

	       }
	       last = c;
	  }
     }

     printf("</pre></body></html>\n");
     return 0;
}
