#include <math.h>
#include <ilist.h>

typedef struct
{
     char *label;
     int bornon;
     int lastseen;
     float x,y,z;

}CellStat;
typedef Ilist CellStatList;

CellStatList *CellStatNew(void);
void CellStatClear(CellStatList *inlist);
void CellStatDelete(CellStatList *inlist);


void CellStatAddLTX(CellStatList *inlist, char *label, int time,
		    double x, double y);
int CellStatRelative(CellStatList *inlist,  char *label);

