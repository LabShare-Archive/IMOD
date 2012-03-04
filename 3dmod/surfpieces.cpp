#include<algorithm> //std::sort
#include<vector>
#include "imodel.h"
#include "imodconfig.h"
#include "surfpieces.h"

static int maximum_triangle_vertex_index(const int *tarray, int tc)
{
  int vmax = 0;
  int s0 = 3, s1 = 1;
  const int *tv = tarray;
  for (int t = 0 ; t < tc ; ++t)
    {
      int v0 = tv[s0*t], v1 = tv[s0*t+s1], v2 = tv[s0*t+2*s1];
      if (v0 > vmax) vmax = v0;
      if (v1 > vmax) vmax = v1;
      if (v2 > vmax) vmax = v2;
    }
  return vmax;
}

inline int min_connected_vertex(int v, int *vmap)
{
  int v0 = v;
  while (vmap[v0] < v0)
    v0 = vmap[v0];
  // Collapse chain for efficiency of future traversals.
  for (int v1 = v ; v1 > v0 ; v1 = vmap[v1])
    vmap[v1] = v0;
  return v0;
}

// ----------------------------------------------------------------------------
//
static int calculate_components(const int * tarray, int tc, int *vmap, int vc)
{
  for (int v = 0 ; v < vc ; ++v)
    vmap[v] = vc;

  int s0 = 3, s1 = 1;
  const int *tv = tarray;
  for (int t = 0 ; t < tc ; ++t)
    {
      int v0 = min_connected_vertex(tv[s0*t], vmap);
      int v1 = min_connected_vertex(tv[s0*t+s1], vmap);
      int v2 = min_connected_vertex(tv[s0*t+2*s1], vmap);
      int v01 = (v0 < v1 ? v0 : v1);
      int vmin = (v2 < v01 ? v2 : v01);
      vmap[v0] = vmap[v1] = vmap[v2] = vmin;
    }

  // Make each vertex map to a connected component number 0,1,2,....
  int cc = 0;
  for (int v = 0 ; v < vc ; ++v)
    {
      int vm = vmap[v];
      if (vm < v)
	vmap[v] = vmap[vm];
      else if (vm == v)
	vmap[v] = cc++;
    }

  return cc;
}

Surface_Piece::Surface_Piece( std::vector<int> *vl , std::vector<int> *tl )
{
  vList=vl;
  tList=tl;
}  

bool sortbyArea(const Surface_Piece & left, const Surface_Piece& right)
{
return left.area < right.area;
}

//caution: vertex_xyz has normals in it;
//on return tarray is the new triangle list;
Surface_Pieces::Surface_Pieces(const Ipoint *vertex_xyz, const b3dInt32 *tarray,
    int tc, b3dInt32 *sortedTriangle)
{
  int tCounter=0, t, nT, ci;
  if( tc<=0 ) 
    return;

  // Create map of vertices to lowest number connected vertex.
  int vc = maximum_triangle_vertex_index(tarray, tc) + 1;
  int *vmap = new int[vc];
  int cc = calculate_components(tarray, tc, vmap, vc);

  // Allocate surface piece vertex and triangle index arrays.
  for (int c = 0 ; c < cc ; ++c)
    pieces.push_back(  Surface_Piece(new std::vector<int>, new
            std::vector<int>) );
    //pieces.push_back( *(new Surface_Piece(new std::vector<int>, new
    //        std::vector<int>)) );

  // Fill vertex index piece arrays.
  for (int v = 0 ; v < vc ; ++v)
    if (vmap[v] < vc)
      pieces[vmap[v]].vList->push_back(v);

  // Fill triangle index piece arrays.
  int  s0 = 3;
  const int *tv = tarray;
  for (t = 0 ; t < tc ; ++t)
    pieces[vmap[tv[s0*t]]].tList->push_back(t);


  float temp;
   Ipoint v0, v1, v2;
   float a, b, c, s;
   std::vector<int> currTList;
   
   for(ci=0;ci<cc;++ci){
     nT=pieces[ci].tList->size();
     tCounter+=nT;
     temp=0;
     //printf("nT=%d\n", nT);
     /*for(t=0;t<nT;++t){
       //times 2 since vertex_xyz has normals;
       currTList=*(pieces[ci].tList);
       v0=vertex_xyz[ 2*tarray[ 3*currTList[t]  ] ]; 
       v1=vertex_xyz[ 2*tarray[ 3*currTList[t] +1 ]];
       v2=vertex_xyz[ 2*tarray[ 3*currTList[t] +2 ]];
       a=sqrtf( (v1.x-v0.x)*(v1.x-v0.x) +
                 (v1.y-v0.y)*(v1.y-v0.y) +
                 (v1.y-v0.y)*(v1.y-v0.y) );
       b=sqrtf( (v2.x-v0.x)*(v2.x-v0.x) +
                 (v2.y-v0.y)*(v2.y-v0.y) +
                 (v2.y-v0.y)*(v2.y-v0.y) );
       c=sqrtf( (v2.x-v1.x)*(v2.x-v1.x) +
                 (v2.y-v1.y)*(v2.y-v1.y) +
                 (v2.y-v1.y)*(v2.y-v1.y) );
       s=0.5*(a+b+c);
       temp+=sqrtf( s*(s-a)*(s-b)*(s-c) );
     }
     pieces[ci].area=temp;*/
     pieces[ci].area=nT;
   }
  //printf("before sort: total # of triangle is: %d\n", tCounter);
   
  std::sort( pieces.begin(), pieces.end(), sortbyArea );

  tCounter=0;
  for(ci=0;ci<cc;++ci){
    nT=pieces[ci].tList->size();
    currTList=*(pieces[ci].tList);
    for( t=0;t<nT;++t){
      sortedTriangle[tCounter++]=tarray[ 3*currTList[t] ];
      sortedTriangle[tCounter++]=tarray[ 3*currTList[t]+1 ];
      sortedTriangle[tCounter++]=tarray[ 3*currTList[t]+2 ];
    }
  }
  //printf("after sort: total # of triangle is: %d\n", tCounter/3);

  //for(int ci=0;ci<cc;++ci)
  //  printf("piece[%d] area is: %f\n", ci, pieces[ci].area);

  delete [] vmap;

}


Surface_Pieces::~Surface_Pieces()
{
  int pc = static_cast<int>(pieces.size());
  for (int p = 0 ; p < pc ; ++p)
    {
      delete pieces[p].vList;
      delete pieces[p].tList;
    }

}

