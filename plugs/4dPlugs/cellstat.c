
#include <string.h>
#include "cellstat.h"


CellStatList *CellStatNew(void)
{
    return((CellStatList *)ilistNew(sizeof(CellStat), 100));
}

void CellStatAddLTX(CellStatList *inlist, char *label, int time, 
		    double x, double y)
{
    CellStat *cs;
    CellStat incs;

    for(cs = (CellStat *)ilistFirst(inlist); cs; 
	cs = (CellStat *)ilistNext(inlist)){
	
	 if (strcmp(label, cs->label) == 0){
	      cs->lastseen = time;
	      cs->x = x;
	      cs->y = y;
	      return;
       }
    }
    incs.label  = strdup(label);
    incs.bornon = time;
    incs.lastseen = time;
    incs.x     = x;
    incs.y     = y; 
    ilistAppend(inlist, &incs);
}

int CellStatRelative(CellStatList *inlist,  char *label)
{
    int current = inlist->current;
    CellStat *cs;
    int inlen = strlen(label);
    int len;

    for(cs = (CellStat *)ilistFirst(inlist); cs;
	cs = (CellStat *)ilistNext(inlist)){
	 len = strlen(cs->label);
	 if (len <= inlen)
	      continue;
	 if (strncmp(label, cs->label, inlen) == 0){
	      inlist->current = current;
	      return 1;
	 }
    }
    inlist->current = current;
    return 0;
}

void CellStatClear(CellStatList *inlist)
{
    CellStat *cs;
    
    for(cs = (CellStat *)ilistFirst(inlist); cs; 
	cs = (CellStat *)ilistNext(inlist)){
	if (cs->label)
	    free(cs->label);
    }
    inlist->current = 0;
    inlist->size    = 0;
}

void CellStatDelete(CellStatList *inlist)
{
    CellStat *cs;

    for(cs = (CellStat *)ilistFirst(inlist); cs; 
	cs = (CellStat *)ilistNext(inlist)){
	if (cs->label)
	    free(cs->label);
    }
    ilistDelete(inlist);
}

