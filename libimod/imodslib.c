#include "imodel.h"
#include <string.h>

struct Mod_Mesh *imodMeshAdd(struct Mod_Mesh *nmesh,
			     struct Mod_Mesh *mray, int *size);

Imod *imodNew(void)
{
     Imod *model;
     model = (Imod *)malloc(sizeof(Imod));
     if (!model)
	  return(model);

     imodDefault(model);
     return(model);
}

int imodDefault(Imod *model)
{
     model->file = NULL;
     model->objsize = 0;
     model->objsize  = 0;
     model->flags = 0;
     model->name[0]    = 0x00;
     model->drawmode   = 1;
     model->mousemode  = IMOD_MMOVIE;
     model->blacklevel = 0;
     model->whitelevel = 255;
     
     model->xoffset = 0;
     model->yoffset = 0;
     model->zoffset = 0;
     
     model->xscale = 1;
     model->yscale = 1;
     model->zscale = 1;
     
     model->cindex.object  = -1;
     model->cindex.contour = -1;
     model->cindex.point   = -1;
     
     model->res = 3;
     model->thresh = 128;
     model->pixsize = 1.0;
     model->units = 0;      /* if unit is 0, pixsize is undefined */
     
     model->csum  = 0;
     model->ex3  = 0;
     model->ex2  = 0;
     model->ex1  = 0;
     
     model->alpha = 0.0f;
     model->beta  = 0.0f;
     model->gamma = 0.0f;
     return(0);
}



void imodFree(Imod *imod)
{
     int ob,co;

     if (!imod)
	  return;
     for(ob = 0; ob < imod->objsize; ob++){
	  for(co = 0; co < imod->obj[ob].contsize; co++){
	       if (imod->obj[ob].cont[co].pts)
		    free(imod->obj[ob].cont[co].pts);
	  }
	  free(imod->obj[ob].cont);
	  /* todo free meshes */
     }
     free(imod->obj);
     free(imod);
     return;
}


Iobj *imodObjectsNew(int size)
{
     Iobj *obj;
     int ob;
     
     obj = (Iobj *)malloc(size * sizeof(Iobj));
     if (!obj)
	  return(NULL);
     
     for(ob = 0; ob < size; ob++){
	  obj[ob].cont      = NULL;
	  obj[ob].mesh      = NULL;
	  obj[ob].vcont     = NULL;
	  obj[ob].vmesh     = NULL;
	  obj[ob].name[0]   = 0x00;
	  obj[ob].contsize  = 0;
	  obj[ob].flags     = 0;
	  obj[ob].axis      = 0;
	  obj[ob].drawmode  = 1;
	  obj[ob].red       = 0.5f;
	  obj[ob].green     = 0.5f;
	  obj[ob].blue      = 0.5f;
	  obj[ob].pdrawsize = 1;
	  obj[ob].linewidth = 1;
	  obj[ob].trans     = 0.0f;
	  obj[ob].meshsize  = 0;
	  obj[ob].surfsize  = 0;
	  
	  obj[ob].clip  = 0;
	  obj[ob].rclip = 0;
	  obj[ob].pclip = 0;
	  obj[ob].aclip = 0;
	  
	  obj[ob].ambient   = 102;
	  obj[ob].diffuse   = 255;
	  obj[ob].specular  = 127;
	  obj[ob].shininess = 4;
	  obj[ob].mat1 = 0;
	  obj[ob].mat2 = 0;
	  obj[ob].mat3 = 0;
     }
     return(obj);
}


Icont *imodContoursNew(int size)
{
     Icont *cont;
     int co;
     
     if (size <= 0)
	  return(NULL);
     
     cont = (Icont *)malloc( size * sizeof(Icont));
     if (!cont)
	  return(NULL);
     
     for(co = 0; co < size; co++){
	  cont[co].pts   = NULL;
	  cont[co].psize = 0;
	  cont[co].flags = 0;
	  cont[co].type  = 0;
	  cont[co].surf  = 0;
	  cont[co].label = NULL;
	  cont[co].sizes = NULL;
     }
     return(cont);
     
}

int imodContourCopy(Icont *from, Icont *to)
{
     if (!from)
	  return(-1);
     if (!to)
	  return(-1);
     memcpy(to, from, sizeof(Icont));
     return(0);
}


Imesh *imodMeshesNew(int size)
{
     Imesh *mesh;
     int sh;
     
     if (size <= 0)
	  return(NULL);
     
     mesh = (Imesh *)malloc(size * sizeof(Imesh));
     if (!mesh)
	  return(NULL);
     
     for (sh = 0; sh < size; sh++){
	  mesh[sh].vert  = NULL;
	  mesh[sh].list  = NULL;
	  mesh[sh].vsize = 0;
	  mesh[sh].lsize = 0;
	  mesh[sh].flag  = 0;
	  mesh[sh].type  = 0;
	  mesh[sh].pad   = 0;
     }
     return(mesh);
}


int imodObjectVirtOut(Iobj *obj)
{
     if (!obj)
	  return(-1);
     if (!iobjVirtual(obj->flags))
	  return(-1);
     if ((!obj->vcont) && (!obj->vmesh)){
	  obj->flags &= ~IMOD_OBJFLAG_VIRT;
	  return(-1);
     }
     
     obj->contsize++;
     obj->cont = (Icont *)realloc(obj->cont, sizeof(Icont) * obj->contsize);
     obj->cont[obj->contsize-1].pts   = NULL;
     obj->cont[obj->contsize-1].psize = 0;
     obj->cont[obj->contsize-1].flags = 0;
     obj->cont[obj->contsize-1].type  = 0;
     obj->cont[obj->contsize-1].surf  = 0;
     if (obj->vcont){
	  imodContourCopy(obj->vcont, &(obj->cont[obj->contsize - 1]));
	  free(obj->vcont);
     }
     if (!obj->vmesh)
	  obj->vmesh = imodMeshesNew(1);
     
     obj->mesh = imodMeshAdd(obj->vmesh, obj->mesh, &(obj->meshsize));
     free(obj->vmesh);
     
     return(0);
}

/* Converts from file to runtime format */
int imodObjectVirtIn(Iobj *obj)
{
     
     if (!obj)
	  return(-1);
     if (!iobjVirtual(obj->flags))
	  return(-1);
     
     if (!obj->contsize){
	  obj->vcont = NULL;
	  obj->flags &= ~IMOD_OBJFLAG_VIRT;
	  return(-1);
     }
     obj->vcont = imodContoursNew(1);
     if (!obj->vcont)
	  return(-1);
     imodContourCopy(&(obj->cont[obj->contsize - 1]), obj->vcont);
     obj->contsize--;
     if (!obj->contsize)
	  free(obj->cont);
     else
	  obj->cont = (Icont *)realloc(obj->cont,
				       sizeof(Icont) * obj->contsize);
     
     if (!obj->meshsize){
	  obj->vmesh = NULL; /* use contour instead in 3-d drawing. */
	  /*        obj->flags &= ~IMOD_OBJFLAG_VIRT; */
	  /*        imodContourDelete(obj->vcont); */
	  /*        obj->vcont = NULL; */
	  return(1);
     }
     obj->vmesh = imodMeshesNew(1);
     if (!obj->vmesh)
	  return(-1);
     imodMeshCopy(&(obj->mesh[obj->meshsize - 1]), obj->vmesh);
     obj->meshsize--;
     if (!obj->meshsize)
	  free(obj->mesh);
     else
	  obj->mesh = (Imesh *)realloc(obj->mesh,
				       sizeof(Imesh) * obj->meshsize);
     return(0);
}


int imodMeshCopy(Imesh *from, Imesh *to)
{
     if (!from)
	  return(-1);
     if (!to)
	  return(-1);
     memcpy(to, from, sizeof(Imesh));
     return(0);
}

struct Mod_Mesh *imodMeshAdd(struct Mod_Mesh *nmesh,
				 struct Mod_Mesh *mray, int *size)
{
     struct Mod_Mesh *nmray;
     if (nmesh == NULL){
	  return(mray);
     }
     
     if ((mray == NULL) && (!*size)){
	  nmray = (struct Mod_Mesh *)malloc(sizeof(struct Mod_Mesh));
     }else{
	  nmray = (struct Mod_Mesh *)
               realloc((struct Mod_Mesh *)mray,
                       (*size + 1) * sizeof(struct Mod_Mesh));
     }
     
     if (nmray == NULL)
          return(mray);

     nmray[*size].vert = nmesh->vert;
     nmray[*size].list = nmesh->list;
     nmray[*size].vsize = nmesh->vsize;
     nmray[*size].lsize = nmesh->lsize;
     nmray[*size].flag = nmesh->flag;
     nmray[*size].type = nmesh->type;
     nmray[*size].pad  = nmesh->pad;
     (*size)++;
     return(nmray);
}



char *imodUnits(Imod *mod)
{
     int units = mod->units;
     char *retval = NULL;

     switch(units){
	case IMOD_UNIT_PIXEL:
	  retval ="pixels";
	  break;
	case IMOD_UNIT_KILO:
	  retval = "km";
	  break;
	case IMOD_UNIT_METER:
	  retval = "m";
	  break;
	case IMOD_UNIT_CM:
	  retval = "cm";
	  break;
	case IMOD_UNIT_MM:
	  retval = "mm";
	  break;
	case IMOD_UNIT_UM:
	  retval = "um";
	  break;
	case IMOD_UNIT_NM:
	  retval = "nm";
	  break;
	case IMOD_UNIT_ANGSTROM:
	  retval = "A";
	  break;
	case IMOD_UNIT_PM:
	  retval = "pm";
	  break;
	default:
	  retval = "unknown units";
	  break;
     }
     return(retval);
}

int substr(char bs[], char ls[])
{
     int i,len;
     
     len = strlen(ls);
     
     for (i = 0; i < len; i++){
	  if (bs[i] == ls[i])
	       continue;
	  else
	       return(0);
     }
     return(1);
}
