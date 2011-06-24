/*
 * ioMRCvol.cpp - class to handle MRC files I/O
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */
#include "ioMRCVol.h"
#include "math.h"
#include <limits.h>

ioMRC::ioMRC(void * _MRCvol,int _nx,int _ny,int _nz,int _mode)
{
    MRCvol=_MRCvol;
    MRCheader.nx=_nx;
    MRCheader.ny=_ny;
    MRCheader.nz=_nz;
    sizeVol=(long int)(((long int)_nx)*((long int)_ny)*((long int)_nz));
    MRCheader.mode=_mode;

    //double check volume is not too big
    if (sizeVol>LONG_MAX)
    {
        cout<<"Volume is too large to be indexed"<<endl;
        exit(-1);
    }
};

bool ioMRC::writeMRCfile(string fileOut,bool createHead)
{
    ofstream out (fileOut.c_str(),ofstream::binary);

    if (out.is_open())
    {
        if (createHead)
            createHeader();
        if (sizeof(header)!=1024)
        {
            cout<<"Size of MRC header="<<sizeof(header)<<". Your machine might have different variable types"<<endl;
            cout<<"It should be:"<<endl;
            cout<<"sizeof(short int)=2 and it is sizeof(short int)="<<sizeof(short int)<<endl;
            cout<<"sizeof(int)=4 and it is sizeof(int)="<<sizeof(int)<<endl;
            cout<<"sizeof(float)=4 and it is sizeof(float)="<<sizeof(float)<<endl;
            return false;
        }
        //write header
        out.write ((char*)&MRCheader, sizeof (header));
        //write MRC vol
        out.write((char*)MRCvol,sizeVol*getType());

        out.close();
        return true;
    }
    else
    {
        cout << "Error opening file"<<endl;
        return false;
    }
}


bool ioMRC::readMRCfile(string fileIn)
{

	//check architecture
	if(sizeof(header)!=1024)
	{
		std::cout<<"Error.Size of header is "<<sizeof(header)<<".It should be 1024."<<std::endl;
		std::cout<<"There is a problem with the architecture of the machine"<<std::endl;
		std::cout<<"Short int="<<sizeof(short int)<<std::endl;
		std::cout<<"Int="<<sizeof(int)<<std::endl;
		std::cout<<"int="<<sizeof(int)<<std::endl;
		std::cout<<"Float="<<sizeof(float)<<std::endl;
		exit(-1);
	}

    ifstream in (fileIn.c_str(),ifstream::binary);

    if (in.is_open())
    {
        in.read ((char*)&MRCheader, sizeof (header));
        if (MRCvol!=NULL)
            free(MRCvol);
        sizeVol=(long int)(((long int)MRCheader.nx)*((long int)MRCheader.ny)*((long int)MRCheader.nz));
        switch (MRCheader.mode)
        {
        case 0:
            MRCvol = (unsigned char*) calloc (sizeVol,getType());
            break;
        case 1:
            MRCvol = (short int*) calloc (sizeVol,getType());
            break;
        case 2:
            MRCvol = (float*) calloc (sizeVol,getType());
            break;
        case 6:
            MRCvol = (unsigned short int*) calloc (sizeVol,getType());
            break;
        default:
            cout<<"Error reading volume.Mode "<<MRCheader.mode<<"for MRC not supported"<<endl;
            exit(-1);
            break;
        }
        if (MRCvol==NULL)
        {
            cout<<"Problem allocating memory for all the volume at once"<<endl;
            exit(-1);
        }
	//move the pointer of the binary file to the start of the volume (some datasets use extra head bytes)
	in.seekg(sizeof(header)+MRCheader.next);

        //write MRC vol
        in.read((char*)MRCvol,sizeVol*getType());
        in.close();
        //double check volume is not too big
        if (sizeVol>LONG_MAX)
        {
            cout<<"Volume is too large to be indexed"<<endl;
            exit(-1);
        }
        return true;
    }
    else
    {
        cout << "Error opening file"<<endl;
        MRCvol=NULL;
        return false;
    }
}


bool ioMRC::readMRCheader(string fileIn)
{
    ifstream in (fileIn.c_str(),ifstream::binary);

    if (in.is_open())
    {
        in.read ((char*)&MRCheader, sizeof (header));
        in.close();
        return true;
    }
    else
    {
        cout << "Error opening file"<<endl;
        MRCvol=NULL;
        return false;
    }
}


//generate parameters for the header from volume
void ioMRC::createHeader()
{
    MRCheader.nxstart=0;
    MRCheader.nystart=0;
    MRCheader.nzstart=0;

    MRCheader.mx=0;
    MRCheader.my=0;
    MRCheader.mz=0;

    MRCheader.cellDim[0]=0.0;
    MRCheader.cellDim[1]=0.0;
    MRCheader.cellDim[2]=0.0;

    MRCheader.cellAng[0]=90;
    MRCheader.cellAng[1]=90;
    MRCheader.cellAng[2]=90;

    MRCheader.mapc=1;
    MRCheader.mapr=2;
    MRCheader.maps=3;

    //compute minimum, maximum and mean
    double minVol=readMRCValueDouble(0);
    double maxVol=minVol;
    double meanVol=0.0;
    double valAux;
    long int idx=0;
    for (int ii=0;ii<MRCheader.nx;ii++)
        for (int jj=0;jj<MRCheader.ny;jj++)
            for (int kk=0;kk<MRCheader.nz;kk++)
            {
                valAux=readMRCValueDouble(idx);
                minVol=min(minVol,valAux);
                maxVol=max(maxVol,valAux);
                meanVol+=valAux;
                idx++;
            }
    meanVol/=(double)idx;
    MRCheader.dmin=minVol;
    MRCheader.dmax=maxVol;
    MRCheader.dmean=meanVol;

    MRCheader.ispg=0;
    MRCheader.nsymbt=0;
    MRCheader.next=0;
    MRCheader.creatid=1000;
    for (unsigned int kk=0;kk<30;kk++)
        MRCheader.extra[kk]='0';

    MRCheader.nint=0;;
    MRCheader.nreal=0;;
    for (unsigned int kk=0;kk<28;kk++)
        MRCheader.extra2[kk]='0';
    MRCheader.idtype=0;;
    MRCheader.lens=0;;
    MRCheader.nd1=0;
    MRCheader.nd2=0;
    MRCheader.vd1=0;
    MRCheader.vd1=0;
    for (unsigned int kk=0;kk<6;kk++)
        MRCheader.tiltAngles[kk]=0.0f;
    MRCheader.origin[0]=0;
    MRCheader.origin[1]=0;
    MRCheader.origin[2]=0;
    MRCheader.cmap[0]='M';
    MRCheader.cmap[1]='A';
    MRCheader.cmap[2]='P';
    MRCheader.cmap[3]=' ';
    MRCheader.stamp[0]=0x44; //68 in hexadecimal
    for (unsigned int kk=1;kk<4;kk++)
        MRCheader.stamp[kk]='0';
    MRCheader.rms=0.0;
    MRCheader.nlbl=0;
    for (unsigned int kk=0;kk<800;kk++)
        MRCheader.label[kk]=' ';
}

void ioMRC::readMRCSlice(int numSlice,float* slice)
{
    if(numSlice>=MRCheader.nz)
    {
        std::cout<<"Error reading slice. Numslice is greater than number of slices"<<std::endl;
        exit(-1);
    }

    long int pos=((long int)numSlice)*((long int)MRCheader.nx)*((long int)MRCheader.ny);

    int count=0;
    switch (MRCheader.mode)
    {
    case 0:
    {
        unsigned char *MRCvol0=(unsigned char*)MRCvol;
        for (int x=0;x<MRCheader.nx; x++)
            for (int y=0;y<MRCheader.ny; y++)
            {
                slice[count]=(float)MRCvol0[pos];
                count++;
                pos++;
            }
        break;
    }
    case 1:
    {
        short int *MRCvol1=(short int*)MRCvol;
        for (int x=0;x<MRCheader.nx; x++)
            for (int y=0;y<MRCheader.ny; y++)
            {
                slice[count]=(float)MRCvol1[pos];
                count++;
                pos++;
            }
        break;
    }
    case 2:
    {
        float *MRCvol2=(float*)MRCvol;
        for (int x=0;x<MRCheader.nx; x++)
            for (int y=0;y<MRCheader.ny; y++)
            {
                slice[count]=(float)MRCvol2[pos];
                count++;
                pos++;
            }
        break;
    }
    case 6:
    {
        unsigned short int *MRCvol6=(unsigned short int*)MRCvol;
        for (int x=0;x<MRCheader.nx; x++)
            for (int y=0;y<MRCheader.ny; y++)
            {
                slice[count]=(float)MRCvol6[pos];
                count++;
                pos++;
            }
        break;
    }
    default:
        cout<<"Error reading slice. MRC mode "<<MRCheader.mode<<" not recognized"<<endl;
        exit(-1);
    break;
    }
}

bool ioMRC::readMRCpatch(int numSlice,int x0,int y0,int sizeX,int sizeY,float *patch)
{
    if(numSlice>=MRCheader.nz)
    {
        std::cout<<"Error reading slice "<<numSlice<<". Numslice is greater than number of slices"<<std::endl;
        exit(-1);
    }

    long int posIni=((long int)numSlice)*((long int)MRCheader.nx)*((long int)MRCheader.ny);


    int count=0;
    int xMin=max(x0-sizeX,0),xMax=min(MRCheader.nx,x0+sizeX);
    int yMin=max(y0-sizeY,0),yMax=min(MRCheader.ny,y0+sizeY);

    if(xMin==0 || xMax==MRCheader.nx || yMin==0 || yMax==MRCheader.ny)
        return false;
    switch (MRCheader.mode)
    {
    case 0:
    {
        unsigned char *MRCvol0=(unsigned char*)MRCvol;
        for (int x=xMin;x<xMax ; x++)
        {
            long int pos=posIni+((long int)yMin*MRCheader.nx)+((long int)x);
            for (int y=yMin;y<yMax; y++)
            {
                patch[count]=(float)MRCvol0[pos];
                count++;
                pos+=((long int)MRCheader.nx);//x,y are transposed when acquiring the patch
            }
        }
        break;
    }
    case 1:
    {
        short int *MRCvol1=(short int*)MRCvol;
        for (int x=xMin;x<xMax ; x++)
        {
            long int pos=posIni+((long int)yMin*MRCheader.nx)+((long int)x);
            for (int y=yMin;y<yMax; y++)
            {
                patch[count]=(float)MRCvol1[pos];
                count++;
                pos+=((long int)MRCheader.nx);//x,y are transposed when acquiring the patch
                //std::cout<<"pos="<<pos<<";count="<<count<<std::endl;
            }
        }
        break;
    }
    case 2:
    {
        float *MRCvol2=(float*)MRCvol;
        for (int x=xMin;x<xMax ; x++)
        {
            long int pos=posIni+((long int)yMin*MRCheader.nx)+((long int)x);
            for (int y=yMin;y<yMax; y++)
            {
                patch[count]=(float)MRCvol2[pos];
                count++;
                pos+=((long int)MRCheader.nx);//x,y are transposed when acquiring the patch
            }
        }
        break;
    }
    case 6:
    {
        unsigned short int *MRCvol6=(unsigned short int*)MRCvol;
        for (int x=xMin;x<xMax ; x++)
        {
            long int pos=posIni+((long int)yMin*MRCheader.nx)+((long int)x);
            for (int y=yMin;y<yMax; y++)
            {
                patch[count]=(float)MRCvol6[pos];
                count++;
                pos+=((long int)MRCheader.nx);//x,y are transposed when acquiring the patch
            }
        }
        break;
    }
    default:
        cout<<"Error reading slice. MRC mode "<<MRCheader.mode<<" not recognized"<<endl;
        exit(-1);
    break;
    }

    return true;
}


void ioMRC::readMRCSlice(int numSlice,double* slice)
{
    if(numSlice>=MRCheader.nz)
    {
        std::cout<<"Error reading slice. Numslice is greater than number of slices"<<std::endl;
        exit(-1);
    }
    long int pos=((long int)numSlice)*((long int)MRCheader.nx)*((long int)MRCheader.ny);

    int count=0;
    switch (MRCheader.mode)
    {
    case 0:
    {
        unsigned char *MRCvol0=(unsigned char*)MRCvol;
        for (int x=0;x<MRCheader.nx; x++)
            for (int y=0;y<MRCheader.ny; y++)
            {
                slice[count]=(double)MRCvol0[pos];
                count++;
                pos++;
            }
        break;
    }
    case 1:
    {
        short int *MRCvol1=(short int*)MRCvol;
        for (int x=0;x<MRCheader.nx; x++)
            for (int y=0;y<MRCheader.ny; y++)
            {
                slice[count]=(double)MRCvol1[pos];
                count++;
                pos++;
            }
        break;
    }
    case 2:
    {
        float *MRCvol2=(float*)MRCvol;
        for (int x=0;x<MRCheader.nx; x++)
            for (int y=0;y<MRCheader.ny; y++)
            {
                slice[count]=(double)MRCvol2[pos];
                count++;
                pos++;
            }
        break;
    }
    case 6:
    {
        unsigned short int *MRCvol6=(unsigned short int*)MRCvol;
        for (int x=0;x<MRCheader.nx; x++)
            for (int y=0;y<MRCheader.ny; y++)
            {
                slice[count]=(double)MRCvol6[pos];
                count++;
                pos++;
            }
        break;
    }
    default:
        cout<<"Error reading slice. MRC mode "<<MRCheader.mode<<" not recognized"<<endl;
        exit(-1);
    break;
    }
}

bool ioMRC::get2DPatch(double *p0e123,int patchSizeX,int patchSizeY,float *out)
{

    float cx=patchSizeX/2.0f;
    float cy=patchSizeY/2.0f;


    float xi,yi,zi;
    //check that we are not going out of bounds of the volume
    xi=ceil(p0e123[0]-cx*p0e123[3]-cy*p0e123[6]);
    yi=ceil(p0e123[1]-cx*p0e123[4]-cy*p0e123[7]);
    zi=ceil(p0e123[2]-cx*p0e123[5]-cy*p0e123[8]);
    if (xi>(MRCheader.nx-1) || yi>(MRCheader.ny-1) || zi>(MRCheader.nz-1))
        return false;
    //ceilf(a)=floor(a)+1 (with float it will never be an integer exact!!
    if ((xi-1.0f)<0.0f || (yi-1.0f)<0.0f || (zi-1.0f)<0.0f)
        return false;

    xi=ceil(p0e123[0]+(patchSizeX-1-cx)*p0e123[3]-cy*p0e123[6]);
    yi=ceil(p0e123[1]+(patchSizeX-1-cx)*p0e123[4]-cy*p0e123[7]);
    zi=ceil(p0e123[2]+(patchSizeX-1-cx)*p0e123[5]-cy*p0e123[8]);
    if (xi>(MRCheader.nx-1) || yi>(MRCheader.ny-1) || zi>(MRCheader.nz-1))
        return false;
    //ceilf(a)=floor(a)+1 (with float it will never be an integer exact!!
    if ((xi-1.0f)<0.0f || (yi-1.0f)<0.0f || (zi-1.0f)<0.0f)
        return false;

    xi=ceil(p0e123[0]+(patchSizeX-1.0f-cx)*p0e123[3]+(patchSizeY-1-cy)*p0e123[6]);
    yi=ceil(p0e123[1]+(patchSizeX-1.0f-cx)*p0e123[4]+(patchSizeY-1-cy)*p0e123[7]);
    zi=ceil(p0e123[2]+(patchSizeX-1.0f-cx)*p0e123[5]+(patchSizeY-1-cy)*p0e123[8]);
    if (xi>(MRCheader.nx-1) || yi>(MRCheader.ny-1) || zi>(MRCheader.nz-1))
        return false;
    //ceilf(a)=floor(a)+1 (with float it will never be an integer exact!!
    if ((xi-1.0f)<0.0f || (yi-1.0f)<0.0f || (zi-1.0f)<0.0f)
        return false;
    xi=ceil(p0e123[0]-cx*p0e123[3]+(patchSizeY-1.0f-cy)*p0e123[6]);
    yi=ceil(p0e123[1]-cx*p0e123[4]+(patchSizeY-1.0f-cy)*p0e123[7]);
    zi=ceil(p0e123[2]-cx*p0e123[5]+(patchSizeY-1.0f-cy)*p0e123[8]);
    if (xi>(MRCheader.nx-1) || yi>(MRCheader.ny-1) || zi>(MRCheader.nz-1))
        return false;
    //ceilf(a)=floor(a)+1 (with float it will never be an integer exact!!
    if ((xi-1.0f)<0.0f || (yi-1.0f)<0.0f || (zi-1.0f)<0.0f)
        return false;

    float xini=p0e123[0]+(-1.0f-cx)*p0e123[3]+(-1.0f-cy)*p0e123[6];
    float yini=p0e123[1]+(-1.0f-cx)*p0e123[4]+(-1.0f-cy)*p0e123[7];
    float zini=p0e123[2]+(-1.0f-cx)*p0e123[5]+(-1.0f-cy)*p0e123[8];
    int countOut=0;

    //cast to different pointer possibilities
    unsigned char *MRCvol0=(unsigned char*)MRCvol;
    short int *MRCvol1=(short int*)MRCvol;
    float *MRCvol2=(float*)MRCvol;
    unsigned short int *MRCvol6=(unsigned short int*)MRCvol;

    for (int ii=0;ii<patchSizeX;ii++)
    {
        xini+=p0e123[3];
        yini+=p0e123[4];
        zini+=p0e123[5];
        xi=xini;
        yi=yini;
        zi=zini;
        for (int jj=0;jj<patchSizeY;jj++)
        {
            xi+=p0e123[6];
            yi+=p0e123[7];
            zi+=p0e123[8];
            //trilinear interpolation
            long int fxi=(long int)floor(xi);
            long int fyi=(long int)floor(yi);
            long int fzi=(long int)floor(zi);

            /*int cxi=(int)ceilf(xi);//ceilf(a)=floor(a)+1 (with float it will never be an integer exact!!
            int cyi=(int)ceilf(yi);
            int czi=(int)ceilf(zi);*/


            float wfx=(xi-fxi);
            float wcx=(1.0f-wfx);
            float wfy=(yi-fyi);
            float wcy=(1.0f-wfy);
            float wfz=(zi-fzi);
            float wcz=(1.0f-wfz);

            long int nx_=((long int)MRCheader.nx);

            long int pos=((long int)fxi)+nx_*(fyi+fzi*((long int)MRCheader.ny));
            long int pos2=pos+nx_*MRCheader.ny;




            switch (MRCheader.mode)
            {
            case 0:
                out[countOut]=((wfx*wfy*wfz)*(float)(MRCvol0[pos2+1+nx_])+
                               (wfx*wfy*wcz)*(float)(MRCvol0[pos+1+nx_])+
                               (wfx*wcy*wfz)*(float)(MRCvol0[pos2+1])+
                               (wfx*wcy*wcz)*(float)(MRCvol0[pos+1])+
                               (wcx*wfy*wfz)*(float)(MRCvol0[pos2+nx_])+
                               (wcx*wfy*wcz)*(float)(MRCvol0[pos+nx_])+
                               (wcx*wcy*wfz)*(float)(MRCvol0[pos2])+
                               (wcx*wcy*wcz)*(float)(MRCvol0[pos]));
                break;
            case 1:
                out[countOut]=((wfx*wfy*wfz)*(float)(MRCvol1[pos2+1+nx_])+
                               (wfx*wfy*wcz)*(float)(MRCvol1[pos+1+nx_])+
                               (wfx*wcy*wfz)*(float)(MRCvol1[pos2+1])+
                               (wfx*wcy*wcz)*(float)(MRCvol1[pos+1])+
                               (wcx*wfy*wfz)*(float)(MRCvol1[pos2+nx_])+
                               (wcx*wfy*wcz)*(float)(MRCvol1[pos+nx_])+
                               (wcx*wcy*wfz)*(float)(MRCvol1[pos2])+
                               (wcx*wcy*wcz)*(float)(MRCvol1[pos]));
                break;
            case 2:
                out[countOut]=((wfx*wfy*wfz)*(float)(MRCvol2[pos2+1+nx_])+
                               (wfx*wfy*wcz)*(float)(MRCvol2[pos+1+nx_])+
                               (wfx*wcy*wfz)*(float)(MRCvol2[pos2+1])+
                               (wfx*wcy*wcz)*(float)(MRCvol2[pos+1])+
                               (wcx*wfy*wfz)*(float)(MRCvol2[pos2+nx_])+
                               (wcx*wfy*wcz)*(float)(MRCvol2[pos+nx_])+
                               (wcx*wcy*wfz)*(float)(MRCvol2[pos2])+
                               (wcx*wcy*wcz)*(float)(MRCvol2[pos]));
                break;
            case 6:
                out[countOut]=((wfx*wfy*wfz)*(float)(MRCvol6[pos2+1+nx_])+
                               (wfx*wfy*wcz)*(float)(MRCvol6[pos+1+nx_])+
                               (wfx*wcy*wfz)*(float)(MRCvol6[pos2+1])+
                               (wfx*wcy*wcz)*(float)(MRCvol6[pos+1])+
                               (wcx*wfy*wfz)*(float)(MRCvol6[pos2+nx_])+
                               (wcx*wfy*wcz)*(float)(MRCvol6[pos+nx_])+
                               (wcx*wcy*wfz)*(float)(MRCvol6[pos2])+
                               (wcx*wcy*wcz)*(float)(MRCvol6[pos]));
                break;
            default:
                std::cout<<"Error at direct.cpp get2DPatch. MRC mode="<<MRCheader.mode<<" is not recognized"<<std::endl;
            }
            countOut++;
        }
    }
    return true;
}
