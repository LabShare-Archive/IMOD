/* standard unix C functions not standard on VMS */
#include <stdio.h>
#include <string.h>

char *strdup(char *inString)
{
    char *dupstr;
    int len;
    if (!inString) return NULL;
    len = strlen(inString);
    dupstr = (char *)malloc(len+4);
    strcpy(dupstr, inString);
    return(dupstr);
}


char *optarg;
int  optind = 0;
static int  subopt = 0;

int getopt(int argc, char **argv, char *options)
{
    
    int i, mi;

    mi = strlen(options);

    optind++;
    if (optind == argc) return EOF;

    optarg = argv[optind];

    if (optarg[0] != '-') return EOF;
    if (optarg[1] == '-') return EOF;

    for(i = 0; i < mi; i++){
	if (optarg[1] == options[i]){
	    if (options[i+1] == ':'){
		optind++;
		if (optind == argc) return EOF;
		optarg = argv[optind];
		return(options[i]);
	    }
	    return(options[i]);
	}
	    
    }

    return('?');
}
