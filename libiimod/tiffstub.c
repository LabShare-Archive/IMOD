#include "notiffio.h"
#include <stdarg.h>

char *version = "IMODSTUB";

int TIFFSetDirectory(TIFF *d, tdir_t t)
{
    return -1;
}
int TIFFReadDirectory(TIFF *d)
{
    return 0;
}
int TIFFWriteDirectory(TIFF *d)
{
    return 0;
}
int TIFFGetField(TIFF *d, ttag_t t, ...)
{
    return 0;
}

int TIFFSetField(TIFF *d, ttag_t t, ...)
{
    return 0;
}

tstrip_t TIFFNumberOfStrips(TIFF *d)
{
    return 0;
}

tsize_t TIFFStripSize(TIFF* tif)
{
     return 0;
}

tsize_t TIFFReadEncodedStrip(TIFF *t, tstrip_t s, tdata_t d, tsize_t z)
{
    return 0;
}

tsize_t TIFFWriteEncodedStrip(TIFF *t, tstrip_t s, tdata_t d, tsize_t z)
{
    return 0;
}

tsize_t TIFFTileSize(TIFF* tif)
{
     return 0;
}

tsize_t TIFFReadEncodedTile(TIFF *t, tstrip_t s, tdata_t d, tsize_t z)
{
    return 0;
}

int TIFFReadScanline(TIFF *t, tdata_t d, uint32 u, tsample_t s)
{
    return 0;
 }

TIFF* TIFFOpen(const char *a, const char *b)
{
    return NULL;
}

void TIFFClose(TIFF *d)
{

}

typedef void (*TIFFWarningHandler)(const char *module, const char *fmt,
                                   va_list ap);

TIFFWarningHandler TIFFSetWarningHandler(TIFFWarningHandler handler)
{
  return NULL;
}

TIFFErrorHandler TIFFSetErrorHandler(TIFFErrorHandler handler)
{
  return NULL;
}

const char* TIFFGetVersion(void)
{
  return version;
}
