
/*
 *  Example usage of the libimod.so library.
 *  
 *  Version 1.01
 */

#include <stdio.h>
#include <stdlib.h>

#include "model.h"

int main(int argc, char **argv)
{
     Imod   *theModel;
     Iobj   *theObject;
     Icont  *theContour;
     Imesh  *theMesh;

     int maxObject;
     int maxContour;
     int maxMesh;
     int ob, co, msh;

     /* Check for input file name.
      */
     if (argc != 2){
	  fprintf(stderr, "usage: minfo <model>\n");
	  exit(1);
     }

     /* Load the model.
      */
     if (NULL == (theModel = imodFileRead(argv[1]))){
	  fprintf(stderr, "%s: model read failed for %s\n",
		  argv[0], argv[1]);
	  perror("read model");
	  exit(2);
     }

     /* Print some information about the model.
      */

     /* loop through all the objects. */
     for(ob = 1, theObject = imodObjectGetFirst(theModel);
	 theObject != NULL;
	 theObject = imodObjectGetNext(theModel), ob++){

	  /* print the number and name of the object.
	   * note the number is the index plus one.
	   */
	  printf("\nObject %d : %s\n", 
		 ob, imodObjectGetName(theObject));

	  
	  /* 
	   * Print our Contour information.
	   * get the number of contours in this object.
	   */
	  maxContour = imodObjectGetValue(theObject, IobjMaxContour);

	  /*
	   *  Now loop through all of the contours.
	   */
	  for(co = 1, theContour = imodContourGetFirst(theModel);
	      theContour != NULL;
	      theContour = imodContourGetNext(theModel), co++){

	       /* print some information about the contour. */
	       printf("\tContour %d: %s  has %d points\n",
		      co,
		      imodContourGetName(theContour),
		      imodContourGetMaxPoint(theContour));
	  }

	  /*
	   * Print out mesh data.
	   */
	  maxMesh    = imodObjectGetValue(theObject, IobjMaxMesh);
	  
	  if (maxMesh > 0){
	      printf("\n\tNumber of meshes = %d\n", maxMesh);
	      for(msh = 0; msh < maxMesh; msh++){
		  theMesh = imodObjectGetMesh(theObject, msh);
		  if (theMesh != NULL){
		      printf("\tMesh %3d has %3d indices and %3d vertices.\n",
			     msh+1,
			     imodMeshGetMaxIndex(theMesh),
			     imodMeshGetMaxVert(theMesh));
			     
		  }
	      }
	      
	  }
	  printf("\n");
     }
     printf("\n");
     return(0);
}
