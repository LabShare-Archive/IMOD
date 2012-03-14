// ----------------------------------------------------------------------------
// Calculate constant intensity surfaces for volumes.
// Uses the marching cubes algorithm.
//
/*
 * Module originally from Chimera, developed at the Resource for Biocomputing,
 * Visualization, and Informatics, UCSF
 * Modified for use in IMOD by Quanren Xiong and David Mastronarde
 *
 *  $Id$
 */

//#include <iostream>		// use std::cerr for debugging

#include <math.h>		// use sqrt()

#include "mappingtable.h"	// use cube_edges, triangle_table

#include "imodel.h"             // Added: for Ipoint definition
#include "mkmesh.h"             // Added: for imeshNormal

#define CONTOUR_ARRAY_BLOCK_SIZE 1048576

// ----------------------------------------------------------------------------
// Use array broken into blocks for storing vertices and triangles.
//
template <class T> class Block_Array
{
public:
  Block_Array(Index block_size);
  ~Block_Array();
  Index size() { return ae + afsize; }
  T element(Index k)
    { return (k >= afsize ? a[k-afsize] : alist[k/bsize][k%bsize]); }
  void set_element(Index k, T e)
    { if (k >= afsize) a[k-afsize] = e;
      else alist[k/bsize][k%bsize] = e; }
  void add_element(T e)
    {
      if (ae == bsize || alsize == 0) next_block();
      a[ae++] = e;
    }
  void array(T *carray);  // Contiguous array.
  void reset() // Does not deallocate memory.
    { ae = anxt = afsize = 0; a = (alist ? alist[0] : a); }
private:
  Index bsize; // block size in elements.
  Index ae;    // elements in last block in use.
  Index anxt;  // next unused block number.
  Index ale;   // number of blocks allocated.
  Index alsize; // size of array of block pointers.
  Index afsize; // number of elements in full blocks.
  T *a;		// last block in use.
  T **alist;	// pointers to allocated blocks.
  void next_block();
};

// ----------------------------------------------------------------------------
//
template <class T> Block_Array<T>::Block_Array(Index block_size)
{
  this->bsize = block_size;
  this->ae = this->anxt = this->ale = this->alsize = this->afsize = 0;
  this->a = NULL;
  this->alist = NULL;
}

// ----------------------------------------------------------------------------
//
  template <class T> Block_Array<T>::~Block_Array()
{
  for (Index k = 0 ; k < ale ; ++k)
    delete [] alist[k];
  delete [] alist;
}

// ----------------------------------------------------------------------------
//
template <class T> void Block_Array<T>::next_block()
{
  if (anxt >= ale)
    {
      if (alsize == 0)
	{
	  this->alsize = 1024;
	  this->alist = new T*[alsize];
	}
      if (ale == alsize)
	{
	  T **alist2 = new T*[2*alsize];
	  for (Index k = 0 ; k < alsize ; ++k)
	    alist2[k] = alist[k];
	  delete [] alist;
	  this->alist = alist2;
	  this->alsize *= 2;
	}
      alist[ale++] = new T[bsize];
    }
  this->a = alist[anxt++];
  this->ae = 0;
  this->afsize = (anxt - 1) * bsize;
}

// ----------------------------------------------------------------------------
//
template <class T> void Block_Array<T>::array(T *carray)
{
  Index k = 0;
  for (Index i = 0 ; i+1 < anxt ; ++i)
	{
	  T *b = alist[i];
	  for (Index j = 0 ; j < bsize ; ++j, ++k)
	    carray[k] = b[j];
	}
  for (Index j = 0 ; j < ae ; ++j, ++k)
    carray[k] = a[j];
}

// ----------------------------------------------------------------------------
//
template <class Data_Type>
class CSurface : public Contour_Surface
{
public:
  CSurface(const Data_Type *grid, const Index size[3], const Index stride[3],
	   float threshold, bool cap_faces, Index block_size)
    : grid(grid), threshold(threshold), cap_faces(cap_faces),
      vxyz(3*block_size), tvi(3*block_size)
    {
      for (int a = 0 ; a < 3 ; ++a)
	{ this->size[a] = size[a]; this->stride[a] = stride[a]; }
      compute_surface();
    }
  virtual ~CSurface() {}

  virtual Index vertex_count() { return vxyz.size()/3; }
  virtual Index triangle_count() { return tvi.size()/3; }
  virtual void geometry(float *vertex_xyz, Index *triangle_vertex_indices, int
      *origin, int binNum)
    { 
      //added by Quanren Xiong for imodv
      int n=vertex_count();
      for (int i=0;i<n;i++){
          vertex_xyz[6*i]=vxyz.element(3*i) *binNum + origin[0];
          vertex_xyz[6*i+1]=vxyz.element(3*i+1)*binNum + origin[1];
          vertex_xyz[6*i+2]=vxyz.element(3*i+2)*binNum+ origin[2];
      }

      //vxyz.array(vertex_xyz); 
      tvi.array(triangle_vertex_indices); 
    }
  virtual void normals(float *normals);

private:
  const Data_Type *grid;
  Index size[3], stride[3];
  float threshold;
  bool cap_faces;
  Block_Array<float> vxyz;
  Block_Array<Index> tvi;

  void compute_surface();
  void add_triangle_corner(Index v) { tvi.add_element(v); }
  Index create_vertex(float x, float y, float z)
    { vxyz.add_element(x); vxyz.add_element(y); vxyz.add_element(z);
      return vertex_count()-1; }
  void make_cap_triangles(int face, int bits, Index *cell_vertices)
    {
      int fbits = face_corner_bits[face][bits];
      int *t = cap_triangle_table[face][fbits];
      for (int v = *t ; v != -1 ; ++t, v = *t)
	add_triangle_corner(cell_vertices[v]);
    }
};

// ----------------------------------------------------------------------------
//
class Grid_Cell
{
public:
  Index k0, k1;		// Cell position in xy plane.
  Index vertex[20];	// Vertex numbers for 12 edges and 8 corners.
  bool boundary;	// Contour reaches boundary.
};

// ----------------------------------------------------------------------------
//
class Grid_Cell_List
{
public:
  Grid_Cell_List(Index size0, Index size1) : cells(CONTOUR_ARRAY_BLOCK_SIZE)
  {
    Index i;                // DNM 8/19/08: declare here for old Intel compiler
    this->rsize = size0+2;	// Pad by one grid cell.
    Index csize = size1+2;
    Index size = rsize * csize;
    this->cell_count = 0;
    this->cmin = 2;
    this->cell_table = new Index[size];
    for (i = 0 ; i < size ; ++i)
      cell_table[i] = no_cell;
    for (i = 0 ; i < rsize ; ++i)
      cell_table[i] = cell_table[size-i-1] = out_of_bounds;
    for (i = 0 ; i < size ; i += rsize)
      cell_table[i] = cell_table[i+rsize-1] = out_of_bounds;
  }
  ~Grid_Cell_List()
    {
      delete_cells();
      delete [] cell_table;
    }
  void set_edge_vertex(Index k0, Index k1, Index e, Index v)
  {
    Grid_Cell *c = cell(k0,k1);
    if (c)
      c->vertex[e] = v;
  }
  Grid_Cell *cell(Index k0, Index k1)
  {
    Index i = k0+1 + (k1+1)*rsize;
    Index c = cell_table[i];
    if (c == out_of_bounds)
      return NULL;

    Grid_Cell *cp;
    if (c != no_cell && c >= cmin)
      cp = cells.element(c-cmin);
    else
      {
	cell_table[i] = cmin + cell_count;
	if (cell_count < cells.size())
	  cp = cells.element(cell_count);
	else
	  cells.add_element(cp = new Grid_Cell);
	cp->k0 = k0; cp->k1 = k1; cp->boundary = false;
	cell_count += 1;
      }
    return cp;
  }
  void set_corner_vertex(Index k0, Index k1, Index corner, Index v)
  {
    Grid_Cell *c = cell(k0,k1);
    if (c)
      {
	c->vertex[12+corner] = v;
	c->boundary = true;
      }
  }
  void finished_plane()
    {
      cmin += cell_count;
      cell_count = 0;
    }

  Index cell_count;
  Block_Array<Grid_Cell *> cells;

private:
  static const Index out_of_bounds = 0;
  static const Index no_cell = 1;
  Index rsize, cmin;
  Index *cell_table; // Maps cell plane index to cell list index.

  void delete_cells()
  {
    Index cc = cells.size();
    for (Index c = 0 ; c < cc ; ++c)
      delete cells.element(c);
  }
};

// ----------------------------------------------------------------------------
// The grid value for index (i0,i1,i2) where 0 <= ik < size[k] is
//
//	grid[i0*stride[0] + i1*stride[1] + i2*stride[2]]
//
template <class Data_Type>
void CSurface<Data_Type>::compute_surface()
{
  const Index no_vertex = ~(Index)0;
  Index step0 = stride[0], step1 = stride[1], step2 = stride[2];
  Index k0_size = size[0], k1_size = size[1], k2_size = size[2];

  // If grid point value is above threshold check if 6 connected edges
  // cross contour surface and make vertex, add vertex to 4 bordering
  // grid cells, triangulate grid cells between two z grid planes.
  Grid_Cell_List gcp0(k0_size-1, k1_size-1), gcp1(k0_size-1, k1_size-1);
  const Data_Type *gplane = grid;
  for (Index k2 = 0 ; k2 < k2_size ; ++k2, gplane += step2)
    {
      const Data_Type *grow = gplane;
      Grid_Cell_List &gp0 = (k2%2 ? gcp1 : gcp0), &gp1 = (k2%2 ? gcp0 : gcp1);
      for (Index k1 = 0 ; k1 < k1_size ; ++k1, grow += step1)
	{
	  const Data_Type *g = grow;
	  for (Index k0 = 0 ; k0 < k0_size ; ++k0, g += step0)
	    {
	      float v0 = *g - threshold;
	      if (!(v0 < 0))
		{
		  // Check 6 neighbor vertices for edge crossings.
		  Index bv = no_vertex;
		  float v1;
		  // TODO: Removing the k0,k1 bounds checks in the following
		  // six conditionals increased speed 40% on Intel Mac 10.4.10
		  // (benchmark score 175 went up to 196).
		  if (k0 > 0)
		    {
		      if ((v1 = (float)(*(g-step0)-threshold)) < 0)
			{
			  Index v = create_vertex(k0-v0/(v0-v1),k1,k2);
			  gp0.set_edge_vertex(k0-1, k1-1, 6, v);
			  gp0.set_edge_vertex(k0-1, k1, 4, v);
			  gp1.set_edge_vertex(k0-1, k1-1, 2, v);
			  gp1.set_edge_vertex(k0-1, k1, 0, v);
			}
		    }
		  else if (cap_faces)  // boundary vertex for capping box faces.
		    {
		      if (bv == no_vertex)
			bv = create_vertex(k0,k1,k2);
		      gp0.set_corner_vertex(k0, k1-1, 7, bv);
		      gp0.set_corner_vertex(k0, k1, 4, bv);
		      gp1.set_corner_vertex(k0, k1-1, 3, bv);
		      gp1.set_corner_vertex(k0, k1, 0, bv);
		    }
		  if (k0+1 < k0_size)
		    {
		      if ((v1 = (float)(g[step0]-threshold)) < 0)
			{
			  Index v = create_vertex(k0+v0/(v0-v1),k1,k2);
			  gp0.set_edge_vertex(k0, k1-1, 6, v);
			  gp0.set_edge_vertex(k0, k1, 4, v);
			  gp1.set_edge_vertex(k0, k1-1, 2, v);
			  gp1.set_edge_vertex(k0, k1, 0, v);
			}
		    }
		  else if (cap_faces)
		    {
		      if (bv == no_vertex)
			bv = create_vertex(k0,k1,k2);
		      gp0.set_corner_vertex(k0-1, k1-1, 6, bv);
		      gp0.set_corner_vertex(k0-1, k1, 5, bv);
		      gp1.set_corner_vertex(k0-1, k1-1, 2, bv);
		      gp1.set_corner_vertex(k0-1, k1, 1, bv);
		    }
		  if (k1 > 0)
		    {
		      if ((v1 = (float)(*(g-step1)-threshold)) < 0)
			{
			  Index v = create_vertex(k0,k1-v0/(v0-v1),k2);
			  gp0.set_edge_vertex(k0-1, k1-1, 5, v);
			  gp0.set_edge_vertex(k0, k1-1, 7, v);
			  gp1.set_edge_vertex(k0-1, k1-1, 1, v);
			  gp1.set_edge_vertex(k0, k1-1, 3, v);
			}
		    }
		  else if (cap_faces)
		    {
		      if (bv == no_vertex)
			bv = create_vertex(k0,k1,k2);
		      gp0.set_corner_vertex(k0-1, k1, 5, bv);
		      gp0.set_corner_vertex(k0, k1, 4, bv);
		      gp1.set_corner_vertex(k0-1, k1, 1, bv);
		      gp1.set_corner_vertex(k0, k1, 0, bv);
		    }
		  if (k1+1 < k1_size)
		    {
		      if ((v1 = (float)(g[step1]-threshold)) < 0)
			{
			  Index v = create_vertex(k0,k1+v0/(v0-v1),k2);
			  gp0.set_edge_vertex(k0-1, k1, 5, v);
			  gp0.set_edge_vertex(k0, k1, 7, v);
			  gp1.set_edge_vertex(k0-1, k1, 1, v);
			  gp1.set_edge_vertex(k0, k1, 3, v);
			}
		    }
		  else if (cap_faces)
		    {
		      if (bv == no_vertex)
			bv = create_vertex(k0,k1,k2);
		      gp0.set_corner_vertex(k0-1, k1-1, 6, bv);
		      gp0.set_corner_vertex(k0, k1-1, 7, bv);
		      gp1.set_corner_vertex(k0-1, k1-1, 2, bv);
		      gp1.set_corner_vertex(k0, k1-1, 3, bv);
		    }
		  if (k2 > 0)
		    {
		      if ((v1 = (float)(*(g-step2)-threshold)) < 0)
			{
			  Index v = create_vertex(k0,k1,k2-v0/(v0-v1));
			  gp0.set_edge_vertex(k0, k1, 8, v);
			  gp0.set_edge_vertex(k0-1, k1, 9, v);
			  gp0.set_edge_vertex(k0, k1-1, 11, v);
			  gp0.set_edge_vertex(k0-1, k1-1, 10, v);
			}
		    }
		  else if (cap_faces)
		    {
		      if (bv == no_vertex)
			bv = create_vertex(k0,k1,k2);
		      gp1.set_corner_vertex(k0-1, k1-1, 2, bv);
		      gp1.set_corner_vertex(k0-1, k1, 1, bv);
		      gp1.set_corner_vertex(k0, k1-1, 3, bv);
		      gp1.set_corner_vertex(k0, k1, 0, bv);
		    }
		  if (k2+1 < k2_size)
		    {
		      if ((v1 = (float)(g[step2]-threshold)) < 0)
			{
			  Index v = create_vertex(k0,k1,k2+v0/(v0-v1));
			  gp1.set_edge_vertex(k0, k1, 8, v);
			  gp1.set_edge_vertex(k0-1, k1, 9, v);
			  gp1.set_edge_vertex(k0, k1-1, 11, v);
			  gp1.set_edge_vertex(k0-1, k1-1, 10, v);
			}
		    }
		  else if (cap_faces)
		    {
		      if (bv == no_vertex)
			bv = create_vertex(k0,k1,k2);
		      gp0.set_corner_vertex(k0-1, k1-1, 6, bv);
		      gp0.set_corner_vertex(k0-1, k1, 5, bv);
		      gp0.set_corner_vertex(k0, k1-1, 7, bv);
		      gp0.set_corner_vertex(k0, k1, 4, bv);
		    }
		}
	    }
	}
      // Create triangles for cell plane.
      if (k2 > 0)
	{
	  Block_Array<Grid_Cell *> &clist = gp0.cells;
	  Index cc = gp0.cell_count;
	  const Data_Type *g0 = gplane - step2;
	  Index step01 = step0 + step1;
	  for (Index k = 0 ; k < cc ; ++k)
	    {
	      Grid_Cell *c = clist.element(k);
	      const Data_Type *gc = g0 + c->k0*step0 + c->k1*step1, *gc2 = gc + step2;
	      int bits = ((gc[0] < threshold ? 0 : 1) |
			  (gc[step0] < threshold ? 0 : 2) |
			  (gc[step01] < threshold ? 0 : 4) |
			  (gc[step1] < threshold ? 0 : 8) |
			  (gc2[0] < threshold ? 0 : 16) |
			  (gc2[step0] < threshold ? 0 : 32) |
			  (gc2[step01] < threshold ? 0 : 64) |
			  (gc2[step1] < threshold ? 0 : 128));

	      Index *cell_vertices = c->vertex;
	      int *t = triangle_table[bits];
	      for (int e = *t ; e != -1 ; ++t, e = *t)
		add_triangle_corner(cell_vertices[e]);

	      if (c->boundary && cap_faces)
		{
		  // Check 6 faces for being on boundary, assemble 4 bits for
		  // face and call triangle building routine.
		  if (c->k0 == 0)
		    make_cap_triangles(4, bits, cell_vertices);
		  if (c->k0 == k0_size-2)
		    make_cap_triangles(2, bits, cell_vertices);
		  if (c->k1 == 0)
		    make_cap_triangles(1, bits, cell_vertices);
		  if (c->k1 == k1_size-2)
		    make_cap_triangles(3, bits, cell_vertices);
		  if (k2 == 1)
		    make_cap_triangles(0, bits, cell_vertices);
		  if (k2 == k2_size-1)
		    make_cap_triangles(5, bits, cell_vertices);
		}
	    }
	}
      gp0.finished_plane();
    }
}

// ----------------------------------------------------------------------------
// Normals are negative of symmetric difference data gradient.
//
template <class Data_Type>
void CSurface<Data_Type>::normals(float *normals)
{
  Index n3 = 3*vertex_count();
  Index outIndex=3; //added by Quanren Xiong for imodv;
  int a;            // DNM moved declaration here for old intel compiler

  for (Index v = 0 ; v < n3 ; v += 3)
    {  
      float x[3] = {vxyz.element(v), vxyz.element(v+1), vxyz.element(v+2)};
      float g[3];
      for (a = 0 ; a < 3 ; ++a)
	g[a] = (x[a] == 0 ? 1 : (x[a] == size[a]-1 ? -1 : 0));
      if (g[0] == 0 && g[1] == 0 && g[2] == 0)
	{
	  Index i[3] = {(Index)x[0], (Index)x[1], (Index)x[2]};
	  const Data_Type *ga = grid + i[0]*stride[0]+i[1]*stride[1]+i[2]*stride[2];
	  const Data_Type *gb = ga;
	  Index off[3] = {0,0,0};
	  float fb = 0;
	  for (a = 0 ; a < 3 ; ++a)
	    if ((fb = x[a]-i[a]) > 0) { off[a] = 1; gb = ga + stride[a]; break; }
	  float fa = 1-fb;
	  for (a = 0 ; a < 3 ; ++a)
	    {
	      Index s = stride[a], ia = i[a], ib = ia + off[a];
	      g[a] = (fa*(ia == 0 ? 2*(ga[s]-ga[0]) : ga[s]-*(ga-s))
		      + fb*(ib == 0 ? 2*(gb[s]-gb[0]) :
			    ib == size[a]-1 ? 2*(gb[0]-*(gb-s))
			    : gb[s]-*(gb-s)));
	    }
	  float norm = sqrt(g[0]*g[0] + g[1]*g[1] + g[2]*g[2]);
	  if (norm > 0)
	    { g[0] /= norm; g[1] /= norm; g[2] /= norm;}
	}
      //normals[v] = -g[0]; normals[v+1] = -g[1]; normals[v+2] = -g[2];
      //added by Quanren Xiong for imodv;
      normals[outIndex] = -g[0]; normals[outIndex+1] = -g[1];
      normals[outIndex+2] = -g[2]; outIndex+=6;
    }
}

// ----------------------------------------------------------------------------
//
template <class Data_Type>
Contour_Surface *surface(const Data_Type *grid, const Index size[3],
			 const Index stride[3], float threshold, bool cap_faces)
{
  CSurface<Data_Type> *cs = new CSurface<Data_Type>(grid, size, stride,
						    threshold, cap_faces,
						    CONTOUR_ARRAY_BLOCK_SIZE);
  return cs;
}

// ----------------------------------------------------------------------------
// Move surface vertices towards the average of neighboring vertices
// give the surface a smoother appearance.
// The vertex array is xyz points, and the triangle array is triples of
// indices into the vertex array.
//
void smooth_vertex_positions(float *varray, Index nv,
			     const Index *tarray, Index nt,
			     float smoothing_factor, int smoothing_iterations)
{
  float normal[3];
  Index v, t;    // Move declarations here for old Intel compiler
  Index *c = new Index[nv];
  if (!c)    // Added
    return;
  for (v = 0 ; v < nv ; ++v)
    c[v] = 0;

  Index nt3 = 3*nt;
  for (t = 0 ; t < nt3 ; ++t)
    c[tarray[t]/2] += 2;
    //c[tarray[t]] += 2;

  //Index nv3 = 3*nv;

  // Just reuse the normal space as long as they are going to be recomputed
  //float *an = new float[nv3];

  float fv = 1 - smoothing_factor, fa = smoothing_factor;
  for (int iter = 0 ; iter < smoothing_iterations ; ++iter)
  {
    /*for (Index v = 0 ; v < nv3 ; ++v)
      an[v] = 0; */
    // Added: Initialize normal space to 0
    for (v = 0 ; v < nv ; ++v) {
      varray[6 * v + 3] = 0;
      varray[6 * v + 4] = 0;
      varray[6 * v + 5] = 0;
    }

    for (t = 0 ; t < nt3 ; t += 3)
    {
      Index i0 = tarray[t], i1 = tarray[t+1], i2 = tarray[t+2];
      Index v0 = 3*i0, v1 = 3*i1, v2 = 3*i2;
      for (Index a = 0 ; a < 3 ; ++a)
      {
        float va0 = varray[v0+a], va1 = varray[v1+a], va2 = varray[v2+a];
        /*an[v0+a] += va1 + va2;
        an[v1+a] += va0 + va2;
        an[v2+a] += va0 + va1;*/
        /*an[v0/2+a] += va1 + va2;
        an[v1/2+a] += va0 + va2;
        an[v2/2+a] += va0 + va1; */
        varray[v0+a+3] += va1 + va2;
        varray[v1+a+3] += va0 + va2;
        varray[v2+a+3] += va0 + va1;
      }
    }

    for (v = 0 ; v < nv ; ++v)
    {
      Index count = c[v];
      if (count)
      {
        Index v6 = 6*v; //added
        //Index v3 = 3*v;
        for (Index a = 0 ; a < 3 ; ++a)
        {
          //Index va = v3 + a;
          Index vb = v6 + a; //added
          //varray[va] = fv * varray[va] + fa * an[va] / count;
          //varray[vb] = fv * varray[vb] + fa * an[va] / count;
          varray[vb] = fv * varray[vb] + fa * varray[vb+3] / count;
        }
      }
    }
  }

  // Added: Recompute normal after last iteration
  for (v = 0 ; v < nv ; ++v) {
    varray[6*v + 3] = 0.;
    varray[6*v + 4] = 0.;
    varray[6*v + 5] = 0.;
  }
  for (t = 0 ; t < nt3 ; t += 3) {
    Index i0 = tarray[t], i1 = tarray[t+1], i2 = tarray[t+2];
    Index v0 = 3*i0, v1 = 3*i1, v2 = 3*i2;
    imeshNormal((Ipoint *)&normal, (Ipoint *)&varray[v0], 
                (Ipoint *)&varray[v1], (Ipoint *)&varray[v2], NULL);
    for (Index a = 0 ; a < 3 ; ++a) {
      varray[v0 + 3 + a] += normal[a];
      varray[v1 + 3 + a] += normal[a];
      varray[v2 + 3 + a] += normal[a];
    }
  }

  //  delete an;
  delete [] c;
}

