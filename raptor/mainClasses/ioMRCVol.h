//file utilities to read and write MRC files

#ifndef IOMRC_H_INCLUDED
#define IOMRC_H_INCLUDED

#include <string>
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>

using namespace std;
//note: to access position vol(x,y,z)->MRCvol[z*nx*ny+y*nx+x]
//better for numerical stability and reduce one multiplication MRCvol[(z*ny+y)*nx+x]

class ioMRC
{
public:
    ioMRC(void * _MRCvol,int _nx,int _ny,int _nz,int mode);
    ioMRC()
    {
        MRCvol=NULL;
    };
    ~ioMRC()
    {
        clear();
    }

    void clear()
    {
        if (MRCvol!=NULL)
        {
            free(MRCvol);
            MRCvol=NULL;
        }
    }

    bool writeMRCfile(string fileOut,bool createHead=true);
    bool readMRCfile(string fileIn);
    void createHeader();
    bool readMRCheader(string fileIn);
    //read single position in MRC vol
    float readMRCValueFloat(long int pos);
    double readMRCValueDouble(long int pos);
    //Warning: IMOD starts indexing as (1,1,1) while we use (0,0,0)
    //Alsways count this offset if we print out countours for IMOD
    float readMRCValueFloat(int x,int y,int z){
        return readMRCValueFloat(((long int)x)+((long int)MRCheader.nx)*(((long int)y)+((long int)z)*((long int)MRCheader.ny)));
    };
    double readMRCValueDouble(int x,int y,int z){
        return readMRCValueDouble(((long int)x)+((long int)MRCheader.nx)*(((long int)y)+((long int)z)*((long int)MRCheader.ny)));
    };
    //user should make sure that pointer has enough memory allocated
    //first slice is slice=0;
    void readMRCSlice(int numSlice,float *slice);
    void readMRCSlice(int numSlice,double *slice);

    //extract small patch center at x0,y0 of slice=nuMSlice
    //user should make sure that pointer has enough memory
    bool readMRCpatch(int numSlice,int x0,int y0,int sizeX,int sizeY,float *patch);


    //obtains 2D patch using trilinear interpolation
    //patch is obtained as p0+lambda1*e1+lambda2*e2, where p0 is a point in R^3 and e1,e2 are vectors in R^3 defining a plane
    bool get2DPatch(double *p0e123,int patchSizeX,int patchSizeY,float *out);

    //method to get main information from header
    int headerGetNx()
    {
        return MRCheader.nx;
    };
    int headerGetNy()
    {
        return MRCheader.ny;
    };
    int headerGetNz()
    {
        return MRCheader.nz;
    };
    void headerSetNx(int _nx)
    {
        MRCheader.nx=_nx;
    };
    void headerSetNy(int _ny)
    {
         MRCheader.ny=_ny;
    };
    void headerSetNz(int _nz)
    {
        MRCheader.nz=_nz;
    };
    int headerGetMode()
    {
        return MRCheader.mode;
    };
    void headerGetCellDim(float _cellDim[3])
    {
        _cellDim[0]=MRCheader.cellDim[0];
        _cellDim[1]=MRCheader.cellDim[1];
        _cellDim[2]=MRCheader.cellDim[2];
    };
    void headerGetPixelSpacing(float _pixelSp[3])//in angstroms
    {
        _pixelSp[0]=MRCheader.cellDim[0]/MRCheader.mx;
        _pixelSp[1]=MRCheader.cellDim[1]/MRCheader.my;
        _pixelSp[2]=MRCheader.cellDim[2]/MRCheader.mz;
    };
    int getType(void);//numrber of bytes per voxel(mode=0->type=1;mode=2->type=4;and so on...)
    void* getMRCvolPointer(void){return MRCvol;};

    //methods to extract information from the volume
    //gets a 2D patch using linear interpolation. Center of the patch is p with generative vectors e1,e2
    //e1,e2 they ar esuppose to be normalized
    //out has dimensions sizeX*sizeY
    //bool get2DPatch(pointM p,pointM e1,pointM e2,int sizeX,int sizeY,float *out);

private:
    void * MRCvol;
    long int sizeVol;
    //we only keep the useful elements for us. It can be xtended later
    //THIS HEADER IS AS USED BY IMOD!!! (there are other headers)
    //SEE http://bio3d.colorado.edu/imod/doc/mrc_format.txt
    //My machine is short int=2bytes int=4bytes float=4bytes
    struct header
    {
        int nx,ny,nz;
        int mode;
        int nxstart,nystart,nzstart;
        int mx,my,mz;
        float cellDim[3];//cell dimensions (in Angstroms)
        float cellAng[3]; //cell angles in degrees
        int mapc,mapr,maps;
        float dmin,dmax,dmean;//density of the volumne
        short int ispg;
        short int nsymbt;
        int next;
        short int creatid;
        char extra[30];
        short int nint;
        short int nreal;
        char extra2[28];
        short int idtype;
        short int lens;
        short int nd1,nd2;
        short int vd1,vd2;
        float tiltAngles[6];
        float origin[3];//origin of the image
        char cmap[4];
        char stamp[4];//First byte has 17 for big- or 68 for little-endian
        float rms;
        int nlbl;
        char label[800];
    };
    header MRCheader;
};


inline int ioMRC::getType(void)
{
    int type;
    switch (MRCheader.mode)
    {
    case 0:
        type=1;
        break;
    case 1:
        type=2;
        break;
    case 2:
        type=4;
        break;
    case 3:
        type=4;
        break;
    case 4:
        type=8;
        break;
    case 6:
        type=2;
        break;
    default:
        cout<<"Wrong mode read from the header"<<endl;
        exit(-1);
    }
    return type;
}

inline double ioMRC::readMRCValueDouble(long int pos)
{
    switch(MRCheader.mode)
    {
        case 0:
        {
            unsigned char *MRCvol0=(unsigned char*)MRCvol;
            return (double)(MRCvol0[pos]);
            break;
        }
        case 1:
        {
            short int *MRCvol1=(short int*)MRCvol;
            return (double)(MRCvol1[pos]);
            break;
        }
        case 2:
        {
            float *MRCvol2=(float*)MRCvol;
            return (double)(MRCvol2[pos]);
            break;
        }
        case 6:
        {
            unsigned short int *MRCvol6=(unsigned short int*)MRCvol;
            return (double)(MRCvol6[pos]);
            break;
        }
        default:
            cout<<"Error reading MRC volume value. MRC mode "<<MRCheader.mode<<" not recognized"<<endl;
            exit(-1);
        break;

    }
}

inline float ioMRC::readMRCValueFloat(long int pos)
{
    switch(MRCheader.mode)
    {
        case 0:
        {
            unsigned char *MRCvol0=(unsigned char*)MRCvol;
            return (float)(MRCvol0[pos]);
            break;
        }
        case 1:
        {
            short int *MRCvol1=(short int*)MRCvol;
            return (float)(MRCvol1[pos]);
            break;
        }
        case 2:
        {
            float *MRCvol2=(float*)MRCvol;
            return (float)(MRCvol2[pos]);
            break;
        }
        case 6:
        {
            unsigned short int *MRCvol6=(unsigned short int*)MRCvol;
            return (float)(MRCvol6[pos]);
            break;
        }
        default:
            cout<<"Error reading MRC volume value. MRC mode "<<MRCheader.mode<<" not recognized"<<endl;
            exit(-1);
        break;
    }
}

#endif // IOMRC_H_INCLUDED
