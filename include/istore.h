/*  IMOD VERSION 2.02
 *
 *  istore.h -- Generic storage for IMOD Model files.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1997 by Boulder Laboratory for 3-Dimensional Fine         *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/

#ifndef ISTORE_H
#define ISTORE_H

typedef Ilist *Istore;

typedef void*(*)(char *inData, int inSize, int *size) IstoreReadProc;
typedef void*(*)(int *size) IstoreWriteProc;

typedef struct
{
     int type;

     /* structured data. */
     char *data;
     int   size;

     /* Raw data to/from file. */
     char *rdata;
     int  rsize;

     int   rawData;
     IstoreReadProc  readProc;
     IstoreWriteProc writeProc;

}Ipod; /* Piece Of Data */


#ifdef __cplusplus
extern "C" {
#endif


Istore  imodStoreNew();
void    imodStoreDelete(Istore inStore);
Ilist  *imodStoreGetPodList(Istore inStore, int inType);
void    imodStoreAddPod(Istore inStore, Ipod *inComp);
int     imodStoreGetPodSize(Istore *inStore, int inType);
Ipod   *imodStoreGetPod(Istore *inStore, int inType, int inIndex);

Icomponent *imodComponentNew(int inType, char *inData, int inSize, int inRaw);
void        imodComponentDelete(Icomponent *inComponent);
void        imodComponentSet(Icomponent *inComp,
			     int inType, char *inData, int inSize, int inRaw);
void        imodComponentSetProcs(Icomponent *inComp,
				  IstoreWriteProc inWriteProc,
				  IstoreReadProc inReadProc);
void       *imodComponentUpdateRawData(Icomponent *inComp,
				       IstoreWriteProc inWriteProc);
void       *imodComponentUpdateData(Iconponent *inComp,
				    IstoreReadProc inReadProc);

void       *imodComponentGetRawData(Icomponent *inComp);
void       *imodComponentGetData(Icomponent *inComp);

#ifdef __cplusplus
}
#endif
#endif
