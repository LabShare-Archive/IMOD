/*
 *  Example usage of the libimod.so library.
 *  Create a custom mesh.
 *  
 *  Version 1.01
 */

#include <stdio.h>
#include <stdlib.h>

#include "model.h"

#define TRUE 1

int main(int argc, char **argv)
{
    FILE *fin;

    Imod   *theModel;
    Iobj   *theObject;
    Imesh  *theMesh;
    Ipoint  theVert;
    Ipoint  theNormal;

    int theMeshIndex;
    int index;

    /* Check for proper input args.
     */
    if (argc != 3){
	fprintf(stderr, "usage: meshcustom <indata> <model>\n");
	exit(1);
    }
    
    /*  Open up input file here. (to do: check for error)
     */
    fin = fopen(argv[1], "r");

    /*
     * Create a new IMOD model.
     */
    theModel = imodNew();

    /*
     * Create a mesh to store our data in.
     */
    theMesh      = imodMeshNew();
    
    /* 
     * Add nodes here in a loop.
     * theNormal is needed for lighting calculations.
     * You can scale the normals so that they have magnitudes
     * ranging between 0.001 and 1.0 these will be used for
     * the normals have magnitude flag.
     */
    do{
	 imodMeshAddVert(theMesh, &theVert);
	 imodMeshAddVert(theMesh, &theNormal);
    }while(0); /* While there is nodes to add. */


    imodMeshAddIndex(theMesh, IMOD_MESH_BGNTRINORM);
    do {
	 imodMeshAddIndex(theMesh, index);   /* Node or vertex. */
	 imodMeshAddIndex(theMesh, index+1); /* Normal index */

	 /* index = next vertex in triangle. */
	 imodMeshAddIndex(theMesh, index);
	 imodMeshAddIndex(theMesh, index +1);

	 /* index = last vertex in triangle. */
	 imodMeshAddIndex(theMesh, index);
	 imodMeshAddIndex(theMesh, index);
    }while(0); /* While there are mesh triangles to add. */    
    imodMeshAddIndex(theMesh, IMOD_MESH_ENDPOLY);

    /*
     *  Close off the mesh.
     */
    imodMeshAddIndex(theMesh, IMOD_MESH_END);

    /*
     * Create an Object to store our mesh in.
     */
    imodNewObject(theModel);
    theObject    = imodObjectGetFirst(theModel);
    theMeshIndex = imodObjectAddMesh(theObject, theMesh);

    /*
     * Set some of the object attributes.
     */
    imodObjectSetName(theObject, "myObject");
    imodObjectSetValue(theObject, IobjFlagFilled, TRUE);
    imodObjectSetValue(theObject, IobjFlagMesh, TRUE);

    /*
     * Write the model out to disk.
     */
    imodFileWrite(theModel, argv[2]);
}
