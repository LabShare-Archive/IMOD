/*   vertexbuffer.h  -  declarations for vertexbuffer.cpp and definition of VertBufData
 *  $Id$
 */
#ifndef VERTEXBUFFER_H
#define VERTEXBUFFER_H

#include "b3dgfx.h"
#include <map>
using namespace std;

struct RGBTindices {
  int firstElement;
  int numInds;
  int numFanInds;
};

typedef map<b3dUInt32,RGBTindices> RGBTmap;

#define BUFFER_OFFSET(bytes) ((GLubyte*) NULL + (bytes))
#define RESTART_INDEX 0x7fffffff
#ifndef GL_PRIMITIVE_RESTART
#define GL_PRIMITIVE_RESTART              0x8F9D
#endif

/*
 * Definition of data structure for drawing with VBO's
 */
typedef struct Vert_Buf_Data {
  b3dUInt32 vbObj;         // Generated identifier of vertex buffer object
  b3dUInt32 ebObj;         // Generated identifier of element buffer object
  int vboSize;             // Allocated sizes of data buffers
  int eboSize;
  int numIndDefault;       // Number of indexes for default segment
  int numSpecialSets;      // Number of non-default sets of indices
  int *numIndSpecial;      // Number of indices in each set
  b3dUInt32 *rgbtSpecial;  // rgbt values of each set
  int specialSize;         // Allocated sizes of these arrays
  int *remnantIndList;     // List of remaining indices or contours
  int remListSize;         // Allocated size
  int numRemnant;          // Number in list
  Ilist *remnantStore;     // List of Istores that go with remnant indices
  int numFanIndDefault;    // Number of default fan indices
  int *numFanIndSpecial;   // Number of fan indices in special sets
  b3dUInt32 defaultRGBT;   // RGBT of the default drawing
  int fanIndStart;         // Starting index of fan indices
  float zscale;            // Z scale at which vertices were loaded
  int fillType;            // Whether it is for fill draw or not
  int useFillColor;        // Which kind of color was used
  int checkTime;           // Whether time was checked, and if so current time
  int thickenCont;         // Flag that selected conts were thickened (remnants)
  int checkStipple;        // Whether stipple was checked
  float scrnScale;         // Screen scaling
  float pdrawsize;         // Default point size
  int quality;             // Sphere drawing quality
  double checksum;         // Checksum of the store
} VertBufData;

// Global functions for cleaning up VBD's
void vbCleanupVBD(Imesh *mesh);
void vbCleanupVBD(Iobj *obj);
void vbCleanupVBD(Imod *mod);
void vbCleanupSphereVBD(Iobj *obj);
void vbCleanupContVBD(Iobj *obj);
void vbCleanupMeshVBD(Iobj *obj);

// The class for mangaging VBO's
class VertBufManager {
 public:
  VertBufManager();
  ~VertBufManager() {clearTempArrays();};
  void unpackRGBT(b3dUInt32 rgbtVal, float &red, float &green, float &blue, int &trans);
  void unpackRGBT(b3dUInt32 rgbtVal, int useFill, DrawProps *props);

  int analyzeMesh(Imesh *mesh, float zscale, int fillType, int useFillColor,
                  DrawProps *defProps);
  int analyzeConts(Iobj *obj, int obNum, int thickenCont, int checkStipple,
                   int checkTime);
  int analyzeSpheres(Iobj *obj, int obNum, float zscale, int xybin, float scrnScale,
                     int quality, int fillType, int useFillColor, int thickenCont,
                     int checkTime);
  void clearTempArrays();
  int checkAllTrans(Iobj *obj, VertBufData *vbd, int &remnantMatchesTrans);

 private:
  void packRGBT(float red, float green, float blue, int trans, b3dUInt32 &rgbtVal);
  void packRGBT(DrawProps *props, int useFill, b3dUInt32 &rgbtVal);
  int loadVertexNormalArray(Imesh *mesh, float zscale, int fillType);
  int allocateSpecialSets(VertBufData *vbd, int numSets, int cumInd, int sphere);
  int processMap(VertBufData *vbd, RGBTmap *colors, int &cumInd,
                  int indPerItem, int &cumFanInd);
  VertBufData *allocateVBDIfNeeded(VertBufData **vbdp);
  int genAndBindBuffers(VertBufData *vbd, int numVerts, int cumInd) ;
  int allocateTempVerts(int numVerts);
  int allocateTempInds(int cumInd) ;
  int allocateDefaultSphere(int numVerts, int numInds, int needNorm);
  inline void loadNormal(float x, float y, float z);
  inline void loadVertex(float x, float y, float z);
  int makeSphere(float radius, int slices, int stacks, GLfloat *vertex, GLuint *index,
                 int &indVert, int &indQuad, int &indFan, int needNorm, float xadd,
                 float yadd, float zadd);
  int sphereCounts(int slices, int stacks, int fillType, int &numQuad, int &numFan);
  void copyDefaultSphere(GLfloat *defVert, GLuint *defInd, int numVert, int numQuad,
                         int numFan, int needNorm, GLfloat *vertex, GLuint *index, 
                         int &indVert, int &indQuad, int &indFan, float xadd, 
                         float yadd, float zadd);
  int checkSelectedAreRemnants(VertBufData *vbd, int obNum);

  // Arrays to be allocated as needed during a draw and freed once at end
  GLuint *mInds;
  int mIndSize;
  GLfloat *mVerts;
  int mVertSize;
  GLuint *mDefSphInds;
  int mDefSphIndSize;
  GLfloat *mDefSphVerts;
  int mDefSphVertSize;

  // Variables to be set when building spheres
  GLfloat *mVertex;
  int mIndVert;
  float mXadd, mYadd, mZadd;
  int mNormOffset;

};
#endif
