/*****************************************************************************
* SYSTEM.....:
*
* APPLICATION: 
*
* MODULE.....: environ.h
*
* PURPOSE....: 
*
* TAG: ENV
*
* AUTHOR: Ross Dargahi
*
*******************************************************************************/
/*Include Test*/
 
#ifndef ENVIRON_H
#define ENVIRON_H
 
/******************************************************************************
Include Files
*******************************************************************************/
/* DNM 8/13/00: add this, make all special cases come through here, remove
 defines for VMS and SEEK64BIT (was SGI64) from section below */

#include "imodconfig.h"
 
/******************************************************************************
Public Definitions
*******************************************************************************/
 
#define FALSE       0           /*false for boolean*/
#define PROTECTED               /*protected type*/
#define PRIVATE     static      /*private type*/
#define PUBLIC                  /*public type*/
#define TRUE        !FALSE      /*true for boolean*/
 
/******************************************************************************
Public Data Structures
*******************************************************************************/
 
/******************************************************************************
Public Typedefs
*******************************************************************************/
 
typedef char           BOOLEAN;
typedef char           CHAR;
typedef unsigned char  UCHAR;
typedef unsigned short USHORT;      
typedef unsigned int   UINT;
typedef unsigned long  ULONG;      
typedef float          FLOAT;
typedef double         DOUBLE;
 
/******************************************************************************/
#endif
 
 
