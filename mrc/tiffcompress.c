#include "tiffio.h"
int main(int argc, char **argv)
{
    int ci = 1;
    int dircount = 1;

    TIFF* tifin = TIFFOpen(argv[ci], "r");
    ci++;
    if (!tifin) exit(-1);

    TIFF* tifout = TIFFOpen(argv[ci], "w");
    if (!tifout) exit(-1);

    while (TIFFReadDirectory(tifin)){
	
    }


    TIFFClose(tifin);
    TIFFClose(tifout);
}
