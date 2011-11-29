/*   vertexbuffer.h  -  declarations for vertexbuffer.cpp and definition of VertBufData
 *  $Id$
 */
#define GL_GLEXT_PROTOTYPES
#include <qgl.h>

#define BUFFER_OFFSET(bytes) ((GLubyte*) NULL + (bytes))

/*
 * Definition of data structure for drawing with VBO's
 */
typedef struct Vert_Buf_Data {
  b3dUInt32 vbObj;         // Generated identifier of vertex buffer object
  b3dUInt32 ebObj;         // Generated identifier of element buffer pbject
  int vboSize;             // Allocated sizes of data buffers
  int eboSize;
  int numIndDefault;       // Number of indexes for default segment
  int numSpecialSets;      // Number of non-default sets of indices
  int *numIndSpecial;      // Number of indices in each set
  b3dUInt32 *rgbtSpecial;  // rgbt values of each set
  int specialSize;         // Allocated sizes of these arrays
  int *remnantIndList;     // List of remaining indices
  int remListSize;         // Allocated size
  int numRemnant;          // Number in list
  Ilist *remnantStore;     // List of Istores that go with remnant indices
  float zscale;            // Z scale at which vertices were loaded
  int fillType;            // Whether it is for fill draw or not
  int useFillColor;        // Which kind of color was used
  b3dUInt32 defaultRGBT;   // RGBT of the default drawing
  double checksum;         // Checksum of the store
} VertBufData;

void vbPackRGBT(float red, float green, float blue, int trans, b3dUInt32 &rgbtVal);
void vbPackRGBT(DrawProps *props, int useFill, b3dUInt32 &rgbtVal);
void vbUnpackRGBT(b3dUInt32 rgbtVal, float &red, float &green, float &blue, int &trans);
void vbUnpackRGBT(b3dUInt32 rgbtVal, int useFill, DrawProps *props);

void vbCleanupVBD(Imesh *mesh);
void vbCleanupVBD(Iobj *obj);
void vbCleanupVBD(Imod *mod);
int vbAnalyzeMesh(Imesh *mesh, float zscale, int fillType, int useFillColor,
                  DrawProps *defProps);
void vbClearTempArrays();
int vbCheckAllTrans(Iobj *obj, VertBufData *vbd, int &remnantMatchesTrans);

