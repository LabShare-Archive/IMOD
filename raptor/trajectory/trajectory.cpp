/*
 * trajectory.cpp - class to handle trajectories
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */


#include "trajectory.h"
#include "../mainClasses/constants.h"
#include <limits>
#include <fstream>
#include <sstream>
#include <math.h>
#include <string.h>
#include <stdio.h>

trajectory::trajectory()
{
    //famatnote: we need to know when there is no parent
    parent=NULL;
}


trajectory::trajectory(trajectory* parent)
{
    this->parent = parent;
}

trajectory::trajectory(const trajectory &t)
{
    parent=t.parent;
    p.clear();
    p=vector<Point2D*>(t.p);
    child.clear();
    child=vector<trajectory*>(t.child);
}

trajectory::~trajectory()
{
    //p.clear();
    //child.clear();

    //we don't that: otherwise we might destroy points we don't want to destroy
}

vector<trajectory> findTrajectories(vector<frame>* frames, vector< vector<pairCorrespondence> >* correspondences, int numFrames, unsigned int maxMarkersPrevFrame)
{
    const int zerotilt = numFrames / 2 + numFrames % 2 - 1;
    unsigned int minTrajectoryLength = (10 > (int)(numFrames * 0.1)) ? 10 : (int)(numFrames * 0.1);
	if(minTrajectoryLength>(unsigned int)numFrames)
		minTrajectoryLength=numFrames;
	
    vector<trajectory> trajectories;
//  int countCorresp = 0;
    for (unsigned int i = zerotilt; i < frames->size() - 1; i++)
    {
        if (frames->at(i).discard==true)
            continue;
        for (unsigned int j = 0; j < frames->at(i).p.size(); j++)
        {
            if (j < maxMarkersPrevFrame || !frames->at(i).p.at(j)->used)
            {
                trajectory temp = buildTrajectoryForward(frames->at(i).p.at(j), numFrames,minTrajectoryLength);
                if (temp.p.size() > minTrajectoryLength/2)//otherwise we are counting that min trajectory has to be for half the trajectories
                    trajectories.push_back(temp);
            }
        }
    }
    //cout << "start building backward" << endl;
    buildTrajectoryBackward(&trajectories, frames, numFrames, zerotilt, maxMarkersPrevFrame,minTrajectoryLength);
	
	if(trajectories.empty())
	{
		cout<<"ERROR: after pairwise correspondence no trajectory with minimum length of "<<minTrajectoryLength<<" projections could be found"<<endl;
		cout<<"Thus, RAPTOR has to stop before optimization to find microscope alignment"<<endl;
		exit(2);
	}
    return trajectories;
}

trajectory buildTrajectoryForward(Point2D* point, int numFrames, unsigned int minTrajectoryLength)
{
    trajectory ans;
    ans.p.push_back(point);
    Point2D* cur = point;
    unsigned int prevJump = 0;
    unsigned int nextJump = 1;
    while (true)
    {
        //cout << cur->frameID << " " << cur->x << " " << cur->y << " " << prevJump << " " << nextJump << endl;
        if (ans.p.size() != 0)
            cur = ans.p.at(ans.p.size() - 1 - prevJump);
        Point2D* temp = findMarkerInNextFrame(cur, nextJump, numFrames);
        if (temp == NULL)
        {
            nextJump++;
            if (nextJump > maxJumps)
            {
                prevJump++;
                if (prevJump >= ans.p.size() || prevJump > maxJumps )
                    break;
                nextJump = prevJump + 1;
            }
        }
        else
        {
            prevJump = 0;
            nextJump = 1;
            cur->used = true;
            cur = temp;
            ans.p.push_back(cur);
        }
    }
    if (ans.p.size() < minTrajectoryLength/2)
    {
        for (unsigned int i = 0; i < ans.p.size(); i++)
        {
            ans.p.at(i)->used = false;
        }
    }
    return ans;
}

void buildTrajectoryBackward(vector<trajectory>* t, vector<frame>* frames, int numFrames, int zerotilt, unsigned int maxMarkersPrevFrame,unsigned int minTrajectoryLength)
{
    for (unsigned int i = 0; i < t->size(); i++)
    {
        unsigned int nextJump = 1;
        unsigned int prevJump = 0;
        while (true)
        {
            Point2D* cur = t->at(i).p.at(prevJump);
            if (cur->frameID <= 0)
                break;
            Point2D* temp = findMarkerInPrevFrame(cur, nextJump);
            if (temp == NULL)
            {
                if (prevJump == maxJumps - 1)
                    break;
                nextJump++;
                if (nextJump == maxJumps)
                {
                    prevJump++;
                    if (prevJump >= t->at(i).p.size())
                        break;
                    nextJump = prevJump + 1;
                }
            }
            else
            {
                nextJump = 1;
                prevJump = 0;
                temp->used = true;
                t->at(i).p.insert(t->at(i).p.begin(), temp);
                cur = temp;
            }
        }
    }
    for (int i = zerotilt; i > 0; i--)
    {
        if (frames->at(i).discard==true)
            continue;
        for (unsigned int j = 0; j < frames->at(i).p.size(); j++)
        {
            if (j < maxMarkersPrevFrame || !frames->at(i).p.at(j)->used)
            {
                trajectory ans;
                Point2D* cur = frames->at(i).p.at(j);
                unsigned int prevJump = 0;
                unsigned int nextJump = 1;
                while (true)
                {
                    Point2D* temp = findMarkerInPrevFrame(cur, nextJump);
                    if (temp == NULL)
                    {
                        if (prevJump == maxJumps - 1)
                            break;
                        nextJump++;
                        if (nextJump == maxJumps)
                        {
                            prevJump++;
                            if (prevJump >= ans.p.size())
                                break;
                            cur = ans.p.at(prevJump);
                            nextJump = prevJump + 1;
                        }
                    }
                    else
                    {
                        nextJump = 1;
                        prevJump = 0;
                        cur->used = true;
                        ans.p.insert(ans.p.begin(), temp);
                        cur = temp;
                    }
                }
                if (ans.p.size() < minTrajectoryLength/2)
                {
                    for (unsigned int i = 0; i < ans.p.size(); i++)
                    {
                        ans.p.at(i)->used = false;
                    }
                }
                else
                {
                    t->push_back(ans);
                }
            }
        }
    }
}

Point2D* findMarkerInPrevFrame(Point2D* cur, int jump)
{
    Point2D* ans = NULL;
    double max_score = -1E+12;
    int frameID = cur->frameID - jump;
    for (unsigned int i = 0; i < cur->index0.size(); i++)
    {
        pairCorrespondence temp = cur->index0.at(i);
        if (temp.score > max_score && temp.frame2->p.at(temp.point2_index)->used == false && temp.frame2->frameID == frameID)
        {
            max_score = temp.score;
            ans = temp.frame2->p.at(temp.point2_index);
        }
    }
    return ans;
}

Point2D* findMarkerInNextFrame(Point2D* cur, int jump, int numFrames)
{
    Point2D* ans = NULL;
    if (cur->frameID + jump >= numFrames)
        return ans;
    double max_score = -1E+12;
    for (unsigned int i = 0; i < cur->index0.size(); i++)
    {
        pairCorrespondence temp =  cur->index0.at(i);
        if (temp.score > max_score && temp.frame2->p.at(temp.point2_index)->used == false && temp.frame2->frameID == cur->frameID + jump)
        {
            max_score = temp.score;
            ans = temp.frame2->p.at(temp.point2_index);
        }
    }
    return ans;
}

void writeIMODfidModel(vector<trajectory> T, int width, int height, int num_frames, string basename,ostream &out)
{

    out << "imod 1" << endl;
    out << "max " << width << " " << height << " " << num_frames<< endl;
    out << "offsets 0 0 0" << endl;
    out << "angles 0 0 0" << endl;
    out << "scale 1 1 1" << endl;
    out << "mousemode 1" << endl;
    out << "drawmode 1" << endl;
    out << "b&w_level 0,200" << endl;
    out << "resolution 3" << endl;
    out << "threshold 128" << endl;
    out << "pixsize 1" << endl;
    out << "units pixels" << endl;
    out << "symbol circle"<< endl;//so imod displays all the markers at once
    out << "size 7"<<endl;

    out << "object 0 " << T.size() << " 0" << endl;
    out << "name" << endl;
    out << "color 0 1 0 0" << endl;
    out << "open" << endl;
    out << "linewidth 1" << endl;
    out << "surfsize  0" << endl;
    out << "pointsize 0" << endl;
    out << "axis      0" << endl;
    out << "drawmode  1" << endl;

    for (unsigned int i = 0; i < T.size(); i++)
    {
        out << "contour " << i << " 0 " << T.at(i).p.size() << endl;
        for (unsigned int j = 0; j < T.at(i).p.size(); j++)
        {
            out << T.at(i).p.at(j)->x + 1.0f << " " << T.at(i).p.at(j)->y + 1.0f << " " << T.at(i).p.at(j)->frameID << endl;
        }
    }
}

void writeMATLABcontour(string dataset, int numFrames)
{
    ifstream in;
    ofstream out;
    string output_filename = dataset + "_.m";
    out.open(output_filename.c_str());
    out << (dataset + " = [") << endl;
    string input_filename = dataset + ".fid.txt";
    in.open(input_filename.c_str());
    string buffer;
    while (in.eof() == false)
    {
        getline(in, buffer);
        char* token = NULL;
        char *temp = new char [buffer.length()];
        buffer.copy(temp, buffer.length());
        //cout << buffer << endl;
        token = strtok(temp, " ");
        if (token == NULL)
            break;
        if (strcmp(token, "contour") == 0)
        {
            token = strtok(NULL, " ");
            token = strtok(NULL, " ");
            token = strtok(NULL, " ");
            istringstream num_points_(token);
            int num_points;
            num_points_ >> num_points;
            float *temp = new float [numFrames];
            for (int i = 0; i < numFrames; i++)
            {
                temp[i] = 0;
            }
            for (int i = 0; i < num_points; i++)
            {
                getline(in, buffer);
                //cout << buffer << endl;
                char *temp2 = new char[buffer.length()];
                buffer.copy(temp2, buffer.length());
                int x, y, index;
                char* tok = NULL;
                tok = strtok(temp2, " ");
                istringstream xx(tok);
                xx >> x;
                tok = strtok(NULL, " ");
                istringstream yy(tok);
                yy >> y;
                tok = strtok(NULL, " ");
                istringstream ii(tok);
                ii >> index;
                index--;
                temp[index] = x;
                delete [] temp2;
            }
            for (int i = 0; i < numFrames; i++)
                out << temp[i] << ",";
            out << ";" << endl;
            delete [] temp;
        }
        else
          continue;
        delete [] temp;
    }
    in.close();
    out << "];" << endl;
    out.close();
}

void writeIMODtiltalign(double alpha,string path,string basename,ostream &out,vector<frame> *frames,int tiltOption,int rotOption,int MagOption)
{
    out<<"#If you know how to change the options for tiltalign in IMOD just edit this txt file"<<endl;
    out<<"#Execute from command line tiltalign -param "<<basename<<"_tiltalignScript.txt to align images with IMOD"<<endl;
    out<<"#Then execute from command line newstack -input ../prealign/"<<basename<<".preali -output "<<basename<<".ali -offset 0,0 -xform "<<basename<<".xf to align images with IMOD"<<endl<<endl<<endl;
    out<<"ModelFile	"<<path<<basename<<".fid.txt"<<endl;
    out<<"#ImageFile	"<<basename<<".preali"<<endl;
//out<<"OutputModelFile	"<<basename<<".3dmod"<<endl;
//out<<"OutputResidualFile	"<<basename<<".resid"<<endl;
//out<<"OutputFidXYZFile	"<<basename<<".xyz"<<endl;
    out<<"OutputTiltFile	"<<path<<basename<<".tlt"<<endl;
    out<<"OutputTransformFile	"<<path<<basename<<".xf"<<endl;


    out<<"#THOSE ARE THE TWO MAIN PARAMETERS THAT NEED TO BE CHANGED!!!!!!!!!"<<endl;
    out<<"#estimated from our correspondence"<<endl;
    out<<"RotationAngle	"<<alpha<<endl;
//out<<"#estimated from our correspondence using TxBR bundle adjustment"<<endl;

    //list of views to be included in alignment
    out<<"IncludeList ";
    bool flagFirst=true;
    for (unsigned int kk=0;kk<frames->size();kk++)
    {
        if (frames->at(kk).discard==false)
        {
            if (flagFirst)
            {
                out<<kk+1;//IMOD's tiltalign script considers 1 as the first section
                flagFirst=false;
            }
            else out<<","<<kk+1;

        }
    }
    out<<endl;
    //----------------------------------------

    out<<"TiltFile	"<<path<<basename<<".rawtlt"<<endl<<endl;

    out<<"#"<<endl;
    out<<"# ADD a recommended tilt angle change to the existing AngleOffset value: our estimation"<<endl;
    out<<"# should be pretty accurate"<<endl;
    out<<"#"<<endl;
    out<<"AngleOffset	0.0"<<endl;
    out<<"RotOption	"<<rotOption<<endl;
    out<<"RotationFixedView 1"<<endl;
    out<<"RotDefaultGrouping	5"<<endl<<endl;

    out<<"#"<<endl;
    out<<"# TiltOption 0 fixes tilts, 2 solves for all tilt angles; change to 5 to solve"<<endl;
    out<<"# for fewer tilts by grouping views by the amount in TiltDefaultGrouping"<<endl;
    out<<"#"<<endl;
    out<<"TiltOption	"<<tiltOption<<endl;
    out<<"TiltFixedView 1"<<endl;
    out<<"TiltDefaultGrouping	5"<<endl;
    out<<"MagReferenceView	1"<<endl;
    out<<"MagOption	"<<MagOption<<endl;
    out<<"MagDefaultGrouping	4"<<endl<<endl;

    out<<"CompOption 0"<<endl;
    out<<"#"<<endl;
    out<<"# To solve for distortion, change both XStretchOption and SkewOption to 3;"<<endl;
    out<<"# to solve for skew only leave XStretchOption at 0"<<endl;
    out<<"#"<<endl;
    out<<"XStretchOption	0"<<endl;
    out<<"XStretchDefaultGrouping	7"<<endl;
    out<<"SkewOption	0"<<endl;
    out<<"SkewDefaultGrouping	11"<<endl;
    out<<"# "<<endl;
    out<<"# Criterion # of S.Dfprintf above mean residual to report (- for local mean)"<<endl;
    out<<"#"<<endl;
    out<<"ResidualReportCriterion	2.0"<<endl;
    out<<"SurfacesToAnalyze	0"<<endl;
    out<<"MetroFactor	0.25"<<endl;
    out<<"MaximumCycles	5000"<<endl;
    out<<"#"<<endl;
    out<<"# ADD a recommended amount to shift up to the existing AxisZShift value"<<endl;
    out<<"#"<<endl;
    out<<"AxisZShift	0"<<endl;
    out<<"#"<<endl;
    out<<"# Set to 1 to do local alignments"<<endl;
    out<<"#"<<endl;
    out<<"LocalAlignments	0"<<endl;
    out<<"OutputLocalFile	DeinoG9blocal.xf"<<endl;
    out<<"#"<<endl;
    out<<"# Number of local patches to solve for in X and Y"<<endl;
    out<<"#"<<endl;
    out<<"NumberOfLocalPatchesXandY	5,5"<<endl;
    out<<"MinSizeOrOverlapXandY	0.5,0.5"<<endl;
    out<<"#"<<endl;
    out<<"# Minimum fiducials total and on one surface if two surfaces"<<endl;
    out<<"#"<<endl;
    out<<"MinFidsTotalAndEachSurface	8,3"<<endl;
    out<<"FixXYZCoordinates	0"<<endl;
    out<<"LocalOutputOptions	1,0,1"<<endl;
    out<<"LocalRotOption	3"<<endl;
    out<<"LocalRotDefaultGrouping	6"<<endl;
    out<<"LocalTiltOption	5"<<endl;
    out<<"LocalTiltDefaultGrouping	6"<<endl;
    out<<"LocalMagReferenceView	1"<<endl;
    out<<"LocalMagOption	3"<<endl;
    out<<"LocalMagDefaultGrouping	7"<<endl;
    out<<"LocalXStretchOption	0"<<endl;
    out<<"LocalXStretchDefaultGrouping	7"<<endl;
    out<<"LocalSkewOption	0"<<endl;
    out<<"LocalSkewDefaultGrouping	11"<<endl;
}

//famatnote: this is just a patch. I need a better solution to track duplicate points in the tajectories
void freeTrajectoryVector(vector<trajectory> T)
{
    //first pass is to identify repeated points
    for (vector<trajectory>::iterator iter=T.begin();iter!=T.end();++iter)
    {
        //famatnote:if two trajectories are pointing to the same Point2D* this can cause segmentation fault!!!
        for (unsigned int kk=0;kk<iter->p.size();kk++)
        {
            if (fabs(iter->p[kk]->x+10)<1e-3)//it means this pointer is repeated
            {
                iter->p[kk]=NULL;
            }
            else
            {
                iter->p[kk]->x=-10.0;
            }
        }
    }
    //free all the memory
    for (vector<trajectory>::iterator iter=T.begin();iter!=T.end();++iter)
    {
        //famatnote:if two trajectories are pointing to the same Point2D* this can cause segmentation fault!!!
        for (unsigned int kk=0;kk<iter->p.size();kk++)
        {
            if (iter->p[kk]!=NULL)
                delete iter->p[kk];//each iter->p[kk] is a Point2D*
        }
        iter->p.clear();
    }
}

vector<trajectory> fiducialModel2Trajectory(string fidTxtFilename)
{
    ifstream in(fidTxtFilename.c_str());
    if (!in.is_open())
    {
        cout<<"ERROR: opening fiducial model "<<fidTxtFilename<<endl;
        exit(-1);
    }

    vector<trajectory> T;
    int numTrajectories;
    string buffer;

    do
    {
        in >> buffer;
    }
    while (buffer != "object");

    in >> buffer;
    in >> numTrajectories;

    cout << "Number of trajectories: " << numTrajectories << endl;

    do
    {
        in >> buffer;
    }
    while (buffer != "contour");


    for (int i = 0; i < numTrajectories; i++)
    {
        trajectory* newTraj = new trajectory;
        int t1;
        int t2;
        int t3;
        in >> t1;
        in >> t2;
        in >> t3;

        for (int j = 0; j < t3; j++)
        {
            float p1;
            float p2;
            int p3;
            in >> p1;
            in >> p2;
            in >> p3;
            Point2D* newPoint = new Point2D;
            newPoint->x = p1;
            newPoint->y = p2;
            newPoint->frameID = p3;
            //cout << p1 << ", " << p2 << ", " << p3 << endl;
            (newTraj->p).push_back(newPoint);
        }
        T.push_back(*newTraj);
        delete newTraj;
        in >> buffer;
    }

    in.close();

    return T;
}


void writeIMODtiltScript(int W,int H,string pathTilt,string pathAli,string basename,int recoThickness,vector<frame> *frames,string endingRec)
{

//create tilt.com script to run reconstruction
//change some of teh parameters (like RADIAL or thickness if needed)

    ofstream fid((pathAli + "tilt.com").c_str());

    fid<<"# Command file to run Tilt "<<endl;
    fid<<"#"<<endl;
    fid<<"####CreatedVersion#### 3.6.9"<<endl;
    fid<<"# "<<endl;
    fid<<"# RADIAL specifies the frequency at which the Gaussian low pass filter begins"<<endl;
    fid<<"#   followed by the standard deviation of the Gaussian roll-off"<<endl;
    fid<<"#"<<endl;
    fid<<"# LOG takes the logarithm of tilt data after adding the given value"<<endl;
    fid<<"#"<<endl;

    fid<<"$tilt"<<endl;
    fid<<pathAli+basename<<".ali"<<endl;
    fid<<pathAli+basename<<endingRec<<endl;
    fid<<"IMAGEBINNED 1"<<endl;
    fid<<"FULLIMAGE "<<W<<" "<<H<<endl;
    fid<<"LOG 0.0"<<endl;
    fid<<"MODE 2"<<endl;//always mode 2 and then we rescale if necessary using trimvol
    fid<<"OFFSET 0.0"<<endl;
    fid<<"PARALLEL"<<endl;
    fid<<"RADIAL 0.35 0.05"<<endl;
    fid<<"SCALE 1.39 500"<<endl;
    fid<<"SHIFT 0.0 0.0"<<endl;
    fid<<"SUBSETSTART 0 0"<<endl;
    fid<<"THICKNESS "<<recoThickness<<endl;
    //fid<<"TILTFILE "<<pathTilt + basename<<".tlt"<<endl;
    ifstream inTlt((pathTilt + basename+".tlt").c_str());
    if (!inTlt.is_open())
    {
        cout<<"ERROR: opening tilt file "<<pathTilt + basename+".tlt"<<endl;
        cout<<"RAPTOR can not proceed with the reconstruction"<<endl;
        exit(-1);
    }
    float tltAngle;
    fid<<"ANGLE ";
    bool flagFirst=true;
    for (unsigned int kk=0;kk<frames->size();kk++)
    {
        inTlt>>tltAngle;
        if (frames->at(kk).discard==false)
        {
            if (flagFirst)
            {
                fid<<tltAngle;
                flagFirst=false;
            }
            else fid<<","<<tltAngle;
        }
    }
    fid<<endl;
    inTlt.close();

    fid<<"XAXISTILT 0.0"<<endl;
    fid<<"DONE"<<endl;
    fid<<"$if (-e ./savework) ./savework"<<endl;


    fid.close();
}
int * getMarkerTypeFromTrajectory(vector<trajectory> T)
{
    int *mType=new int[T.size()];
    //we average the marker type for all the points in each trajectory to find out each type
    int count=0;
    for (vector<trajectory>::iterator iter=T.begin();iter!=T.end();++iter)
    {
        mType[count]=0;
        for (vector<Point2D*>::iterator iterP=iter->p.begin();iterP!=iter->p.end();++iterP)
        {
            mType[count]+=(*iterP)->markerType;
        }
        mType[count]=(int)floor(0.5+(float)(mType[count])/(float)(iter->p.size()));
        count++;
    }
    return mType;
}
