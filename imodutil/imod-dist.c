/*************************************************************************
**************************************************************************
**  Program: imod-dist
**
**  Description: imod-dist computes the distance between a reference
**  location and other points in an IMOD model.  The reference object can
**  be either a single point in a IMOD object or the centroid of the object.
**  The euclidean distance is reported for all points in the object list
**  specified.
**
** $Id$
**
**
**************************************************************************
*************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "imodel.h"
#include "b3dutil.h"
#include "parse_params.h"

static void showArgumentState();
static int checkArguments();
static int calcReferenceCentroid(Imod * inputModel,
                          int idxReferenceObject,
                          Ipoint * referencePoint);
static int getReferencePoint(Imod *inputModel,
                      int idxReferenceObject,
                      int idxReferenceContour,
                      int idxReferencePoint,
                      Ipoint *referencePoint);
static float * calcAllPointDistance(Iobj * object, Ipoint reference, int * nPoints);

static char * imodUnitsToString(int units);
static void imodApplyModelZScale(Imod * iModel);
static void imodApplyObjectZScale(Iobj * iObject, float zScale);
static void imodApplyContourZScale(Icont * iContour, float zScale);

/*  Command line arguments variables */
char * inputFilename = "";
char * outputFilename = "";
char * strObjectList = "";
int idxReferenceObject = -1;
int referenceCentroid = 0;
int referenceContour = 1;
int idxReferencePoint = -1;
int verbose = 0;
int veryVerbose = 0;

int main(int argc, char * argv[]) {
  Imod* inputModel;
  Ipoint referencePoint;
  int * objectList;
  int nObjects;
  int i, j, numOptArgs, numNonOptArgs;
  float * dist;
  int nDist;
  char *progname = imodProgName(argv[0]);

  /* Fallbacks from    ../manpages/autodoc2man 2 1 imod-dist  */
  int numOptions = 10;
  const char *options[] = {
    "i:InputModel:FN:", "o:OutputFile:FN:", "c:CentroidReference::",
    "l:ContourReference:I:", "m:Measure:LI:", "p:PointReference:I:",
    "r:ReferenceObject:I:", "v:verbose:B:", "V:VeryVerbose:B:", "help:usage:B:"};

  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
                        3, 1, 0, &numOptArgs, &numNonOptArgs, imodUsageHeader);
  if (!PipGetBoolean("usage", &i)) {
    PipPrintHelp(progname, 0, 1, 0);
    exit(0);
  }
  PipGetInOutFile("InputModel", 0, &inputFilename);
  PipGetInOutFile("OutputFile", 1, &outputFilename);
  PipGetBoolean("CentroidReference", &referenceCentroid);
  PipGetInteger("ContourReference", &referenceContour);
  PipGetString("Measure", &strObjectList);
  PipGetInteger("PointReference", &idxReferencePoint);
  PipGetInteger("ReferenceObject", &idxReferenceObject);
  PipGetBoolean("verbose", &verbose);
  PipGetBoolean("VeryVerbose", &veryVerbose);

  /*  Check to if the required arguments are present */
  if(checkArguments() != 0) {
    printf("\n\n\n");
    PipPrintHelp(progname, 0, 1, 0);
    exit(1);
  }

  if(veryVerbose) {
    showArgumentState();
  }

  /*  Parse the list of objects to be measured */
  objectList = parselist(strObjectList, & nObjects);
  if(objectList == NULL) {
    fprintf(stderr, "Object list syntax error");
    exit(1);
  }

  /* Read in the model file */
  inputModel = imodRead(inputFilename);
  if(inputModel == NULL) {
    perror("imod-dist: Unable to open input model file");
    fprintf(stderr, "Input model file %s\n", inputFilename);
    exit(1);
  }

  if(verbose) {
    printf("Model info:\n");
    printf("  name: %s\n", inputModel->name);
    printf("  # objects: %d\n", inputModel->objsize);
    printf("  scale factors: %f %f %f\n",
           inputModel->xscale, inputModel->yscale, inputModel->zscale);
    printf("  units: %s\n", imodUnitsToString(inputModel->units));
    printf("  pixel size: %f\n\n", inputModel->pixsize);
  } 

  /*  Apply the zScale to the whole model so that pixels are isotropic */
  imodApplyModelZScale(inputModel);

  if(referenceCentroid == 1) {
    /*  Calculate the coordinate reference with respect to the centroid of */
    /*  of the reference object */
    if(calcReferenceCentroid(inputModel,
                             idxReferenceObject,
                             &referencePoint)) {
      exit(1);
    }
  }
  else {
    /*  Get the selected reference point */
    if (getReferencePoint(inputModel,
                          idxReferenceObject,
                          referenceContour,
                          idxReferencePoint,
                          &referencePoint)) {
      exit(1);
    }
  }

  if(verbose) {
    printf("Reference object:\n");
    printf("  index: %d\n", idxReferenceObject);
    printf("  name: %s\n", inputModel->obj[idxReferenceObject-1].name);
    printf("  reference location: %f %f %f\n\n", referencePoint.x, 
           referencePoint.y, referencePoint.z);
  }

  /* Walk through the object list, the -1 follwing the object list is */
  /* to preserve indexing relative to 1 for the user. */
  for(i = 0; i < nObjects; i++) {
    if(verbose) {
      printf("Measured object:\n");
      printf("  index: %d\n", objectList[i]);
      printf("  name: %s\n", inputModel->obj[objectList[i]-1].name);
    }

    dist = calcAllPointDistance(& inputModel->obj[objectList[i]-1],
                                referencePoint, & nDist);
    if(dist == NULL) {
      fprintf(stderr, "Unable to allocate memory for distance calculation\n");
      exit(1);
    }
    for(j = 0; j < nDist; j++) {
      if(verbose) {
        printf("    point: %d\tdistance: ", j);
      }
      printf("%f\n", dist[j] * inputModel->pixsize);
    }

    free(dist);
  }

  /*  Success */
  exit(0);
}


static void showArgumentState() {
  printf("inputFilename: %s\n", inputFilename);
  printf("outputFilename: %s\n", outputFilename);
  printf("idxReferenceObject: %d\n", idxReferenceObject);
  printf("referenceCentroid: %d\n", referenceCentroid);
  printf("referenceContour: %d\n", referenceContour);
  printf("referencePoint: %d\n", idxReferencePoint);
  printf("strObjectList: %s\n", strObjectList);
}


/*************************************************************************
**  checkArguments  Check the command line arguments, returning 0 if they
**  are good or a non-zero code if they are bad
**
**  usage errorCode = checkArguments();
*************************************************************************/
static int checkArguments() {
  int errorCode = 0;

  /*Check to see if the input model file is specified */
  if(strcmp("", inputFilename) == 0) {
    fprintf(stderr, "Input model file name is missing\n");
    errorCode |= 0x0001;
  }
  if(strcmp("", strObjectList) == 0) {
    fprintf(stderr, "Object list is missing\n");
    errorCode |= 0x0002;
  }
  if(idxReferenceObject == -1) {
    fprintf(stderr, "Reference object is missing\n");
    errorCode |= 0x0004;
  }
  if((referenceCentroid == 0) & (idxReferencePoint == -1)) {
    fprintf(stderr,
      "Specify either the reference point or use the centroid reference\n");
    errorCode |= 0x0008;
  }
  return errorCode;
}


static int calcReferenceCentroid(Imod * inputModel,
                          int idxReferenceObject,
                          Ipoint * referencePoint) {

  if(idxReferenceObject < 1 || idxReferenceObject - 1 >= inputModel->objsize) {
    fprintf(stderr, "Reference object index out of range\n");
    fprintf(stderr, "There are %d objects in the model\n",
            inputModel->objsize);
    return -1;
  }

  if (imodel_object_centroid(&inputModel->obj[idxReferenceObject-1],
                             referencePoint)) {
    fprintf(stderr, "An error occurred calculating the centroid of the "
            "reference object\n");
    return -1;
  }
  return 0;
}

static int getReferencePoint(Imod *inputModel,
                      int idxReferenceObject,
                      int idxReferenceContour,
                      int idxReferencePoint,
                      Ipoint *referencePoint)
{
  Icont *cont;
  if(idxReferenceObject < 1 || idxReferenceObject - 1 >= inputModel->objsize) {
    fprintf(stderr, "Reference object index out of range\n");
    return -1;
  }
  if(idxReferenceContour < 1 || idxReferenceContour - 1 >= 
     inputModel->obj[idxReferenceObject - 1].contsize) {
    fprintf(stderr, "Reference contour index out of range for reference "
            "object\n");
    return -1;
  }
    
  cont = &inputModel->obj[idxReferenceObject - 1].cont[idxReferenceContour-1];
  if (idxReferencePoint -1 >=  cont->psize) {
    fprintf(stderr, "Reference point index out of range for "
            "contour %d of reference object\n", idxReferenceContour);
    return -1;
  }
  *referencePoint = cont->pts[idxReferencePoint -1];
  return 0;
}

/*************************************************************************
** calcAllPointDistance Calculate the distance to all points in a object
**
** dist = calcAllPointDistance(Iobj * object, Ipoint reference, int * nPoints)
**
** dist         An array of floats containing the distance from the
**              reference to each point in the object.  This is allocated
**              within the function and is the callers responsibility to
**              free.
**
** reference    An Ipoint object containing the refernce for the
**              measurements.
**
** nPoints      The number of points in the distance array.
**
*************************************************************************/
static float * calcAllPointDistance(Iobj * object, Ipoint reference, int * nPoints) {
  int i, j;
  int idxPoint = 0;
  float * dist = NULL;
  float dx, dy, dz;
  Icont * currentContour;
  /*  Walk through the object to count all of the points */
  * nPoints = 0;
  for(i = 0; i < object->contsize; i++) {
    * nPoints += object->cont[i].psize;
  }

  dist = (float *) calloc(* nPoints, sizeof(float));
  if(dist == NULL)
    return dist;

  /*  Loop over all of the points in all of the contours calculating */
  /*  the distance from the reference */
  for(i = 0; i < object->contsize; i++) {
    currentContour = & object->cont[i];
    for(j = 0; j < currentContour->psize; j++) {
      dx = currentContour->pts[j].x - reference.x;
      dy = currentContour->pts[j].y - reference.y;
      dz = currentContour->pts[j].z - reference.z;
      dist[idxPoint++] = sqrt((dx * dx) + (dy * dy) + (dz * dz));
    }
  }
  return dist;
}

/*************************************************************************
**  imodApplyModelZScale  Apply the zscale to all objects in model
**
**  iModel     the object to rescale
**
**  imodApplyModelZScale applies the model zscale factor to all objects
**  in the model by multiply each points z value by the zscale factor
*******************************************************************************/
static void imodApplyModelZScale(Imod * iModel) {
  int i;

  for(i = 0; i < iModel->objsize; i++) {
    imodApplyObjectZScale(& iModel->obj[i], iModel->zscale);
  }
}


/*************************************************************************
**  imodApplyObjectZScale  Apply the zscale to a object
**
**  iObject     the object to rescale
**
**  zscale      the scale factor to apply
**
**  imodApplyObjectZScale applies the scale factor zScale to all points
**  in the object by multiplying each z value by the zScale value
*******************************************************************************/
static void imodApplyObjectZScale(Iobj * iObject, float zScale) {
  int i;

  for(i = 0; i < iObject->contsize; i++) {
    imodApplyContourZScale(& iObject->cont[i], zScale);
  }
}


/*************************************************************************
**  imodApplyContourZScale  Apply the zscale to a contour
**
**  iContour    the contour to rescale
**
**  zscale      the scale factor to apply
**
**  imodApplyContourZScale applies the scale factor zScale to all points
**  in the contour by multiplying each z value by the zScale value
*******************************************************************************/
static void imodApplyContourZScale(Icont * iContour, float zScale) {
  int i;

  for(i = 0; i < iContour->psize; i++) {
    iContour->pts[i].z *= zScale;
  }
}


/*************************************************************************
**  imodUnitsToString   Return a string description of the units used
**
**  strUnits = imodUnitsToString(int units)
**
**  strUnits    the string description of the units in the model
**
**  units       the integer unit code from the Imod structure
*******************************************************************************/
static char * imodUnitsToString(int units) {
  char * strUnits = "";
  switch(units) {
  case IMOD_UNIT_PIXEL:
    strUnits = "pixel";
    break;
  case IMOD_UNIT_KILO:
    strUnits = "kilometer";
    break;
  case IMOD_UNIT_METER:
    strUnits = "meter";
    break;
  case IMOD_UNIT_CM:
    strUnits = "centimeter";
    break;
  case IMOD_UNIT_MM:
    strUnits = "millimeter";
    break;
  case IMOD_UNIT_UM:
    strUnits = "micrometer";
    break;
  case IMOD_UNIT_NM:
    strUnits = "nanometer";
    break;
  case IMOD_UNIT_ANGSTROM:
    strUnits = "angstrom";
    break;
  case IMOD_UNIT_PM:
    strUnits = "picometer";
    break;
  }

  return strUnits;
}


