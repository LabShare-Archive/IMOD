#include <ilight.h>

Ilight *imodLightNew(void)
{
     Ilight *light = (Ilight *)malloc(sizeof(Ilight));
     if (!light) return(NULL);
     light->normal.x  = 0.0f;
     light->normal.y  = 0.0f;
     light->normal.z  = 1.0f;
     light->dist      = 0.0f;
     light->constant  = 1.0f;
     light->linear    = 0.0f;
     light->quadratic = 0.0f;
     return(light);
}

