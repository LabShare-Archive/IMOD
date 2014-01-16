/*
 * main.cpp - main program to run RAPTOR
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */

#include <iostream>
#include "./template/template.h"
#include "./correspondence/correspondence.h"
#include "./trajectory/trajectory.h"
#include "./mainClasses/frame.h"
#include "./mainClasses/paircorrespondence.h"
#include "./mainClasses/ioMRCVol.h"
#include "./mainClasses/stat.h"
#include "./mainClasses/constants.h"
//#include "./opencv/cv.h"
//#include "./opencv/highgui.h"
#include <string>
#include <fstream>
#include <vector>
#include <sstream>
#include <limits>
#include "./optimization/SFMestimationWithBA.h"
#include "./optimization/contour.h"
#include "./optimization/SFMdata.h"
#include <cstdlib>
#include "parse_params.h"
#include "./fillContours/fillContours.h"
#include "sys/stat.h"
#include <time.h>
#ifdef _WIN32
#include "Windows.h"
#endif

//#pragma warning(disable : ) // Ignore warnings for deprecated conversion from string constant to ‘char*’ produced by pip source code
using namespace std;
int *diameter;
int zerotilt;
unsigned int maxMarkersPrevFrame, maxMarkersNextFrame;


int main(int argc, char* argv[])
{
    /* initialize random seed: */
    srand ( time(NULL) );

    int error;
    string cmd;
    int dmax,maxPWtable,minPWtable;
    int numDiffMarkerSize=0;//I need to initialize this to avoid IMOD crashing


    int numOptArgs, numNonOptArgs;
    int numOptions = 17;
    const char* options[] = {"execPath:RaptorExecPath:CH:",
                       "path:InputPath:CH:",
                       "input:InputFile:FN:",
                       "output:OutputPath:CH:",
                       "diameter:Diameter:IA:",
                       "markers:MarkersPerImage:I:",
                       "angles:AnglesInHeader:B:",//IT DOESN'T NEED ANY VALUE. JUST IF YOU ADD IT TO COMMAND LINE IT WILL BE TRUE
                       "bin:Binning:I:",
                       "rec:Reconstruction:I:",
                       "thickness:Thickness:I:",
                       "maxDist:MaxDistanceCandidate:I:",
                       "minNeigh:MinNeighborsMRF:I:",
                       "rollOff:RollOffMRF:I:",
                       "verb:Verbose:I:",
                       "white:WhiteMarkers:B:",
                       "tracking:TrackingOnly:B:",
                       "xray:xRay:B:",
                      };
    PipReadOrParseOptions(argc, argv, options, numOptions, "raptor", 1, 0, 0, &numOptArgs, &numNonOptArgs, NULL);
    char* input_;
    char *path_;
    char* output_;
    char *binPath_;
    int targets;
    int white_,trackingOnly_;
    int verbose;//variable to indicate to indicate amount of output desired
    /*
    verbose:    0->minimial output (just rec, and log file)
                1->Normal mode of operation: some output and align stack is not deleted
                2->Debug mode: lots of information recorded to debug code
    */
    int anglesHeader_,binning,thickness,rec,xRay_;

    if (PipGetString("RaptorExecPath",&binPath_))
        exitError("No binary path specified to execute RAPTOR. Please specify where RAPTOR binary is located\n");
    if (PipGetString("InputPath",&path_))
        exitError("No input path specified exist\n");
    if (PipGetString("OutputPath",&output_))
        exitError("No output path specified exist\n");
    if (PipGetInOutFile("InputFile", 0, &input_))
        exitError("The file does not exist\n");

    int *diameter_=new int[100];//to make sure we never run out of space

    if (PipGetIntegerArray("Diameter", diameter_, &numDiffMarkerSize, 100))
        exitError("No diameter of fiducial markers specified\n");

    diameter=new int[numDiffMarkerSize];
    int *templSize=new int[numDiffMarkerSize];
    for (int kk=0;kk<numDiffMarkerSize;kk++)
    {
        diameter[kk]=diameter_[kk];
        templSize[kk]=2 * diameter[kk] + 1;
    }
    delete []diameter_;

    if (PipGetInteger("MarkersPerImage", &targets))
        targets=-1;
    if (PipGetBoolean("AnglesInHeader",&anglesHeader_))
        anglesHeader_=0;
    if (PipGetInteger("Binning", &binning))
        binning=1;
    if (PipGetInteger("MaxDistanceCandidate", &maxPWtable))
        maxPWtable=-10;
    if (PipGetInteger("MinNeighborsMRF", &minPWtable))
        minPWtable=-10;
    if (PipGetInteger("RollOffMRF", &dmax))
        dmax=-10;
    if (PipGetInteger("Thickness",&thickness))
        thickness=0;
    if (PipGetInteger("Reconstruction",&rec))
        rec=-10000;
    if (PipGetInteger("Verbose",&verbose))
        verbose=1;
    if (PipGetBoolean("WhiteMarkers",&white_))
        white_=0;//indicates if markers are white (instead of black)
    if(PipGetBoolean("xRay",&xRay_))
        xRay_=0;

    if (PipGetBoolean("TrackingOnly",&trackingOnly_))
        trackingOnly_=0;//indicates if we don't want aligned file. Only fiducial markers model

    bool trackingOnly=false;
    if (trackingOnly_==1) trackingOnly=true;

    bool white=false;
    if (white_==1) white=true;

    bool xRay=false;
    if(xRay_==1) xRay=true;

    if (binning<1)
    {
        cout<<"ERROR: binning factor is "<<binning<<".It should be above 1"<<endl;
        exit(-1);
    }



    bool debugMode=true;
    if (verbose<2)
        debugMode=false;
    bool anglesHeader=true;
    if (anglesHeader_==0) anglesHeader=false;

    string inputDir(path_);
    string outputDir(output_);
    string inputFilename(input_);
    string binPATH(binPath_);
    //generate basename
    int posDot=inputFilename.find_last_of(".");
    string basename=inputFilename.substr(0,posDot);

    //make sure all paths have / at the end
#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__) 
    if (inputDir.find_last_of("\\")!=inputDir.size()-1)
        inputDir.append("\\");
    if (outputDir.find_last_of("\\")!=outputDir.size()-1)
        outputDir.append("\\");
    if (binPATH.find_last_of("\\")!=binPATH.size()-1)
        binPATH.append("\\");
#else
	if (inputDir.find_last_of("/")!=inputDir.size()-1)
        inputDir.append("/");
    if (outputDir.find_last_of("/")!=outputDir.size()-1)
        outputDir.append("/");
    if (binPATH.find_last_of("/")!=binPATH.size()-1)
        binPATH.append("/");
#endif

    //create necessary folders
    struct stat St;
    if (stat( outputDir.c_str(), &St ) != 0)//check if folder exists
    {
        cmd=string("mkdir " + outputDir);
        error=system(cmd.c_str());
    }
    if (stat( (outputDir+ "align").c_str(), &St ) != 0)//check if folder exists
    {
        cmd=string("mkdir " + outputDir + "align");
        error=system(cmd.c_str());
    }
    if (stat( (outputDir + "IMOD").c_str(), &St ) != 0)//check if folder exists
    {
        cmd=string("mkdir " + outputDir + "IMOD");
        error=system(cmd.c_str());
    }
    if (stat( (outputDir + "temp").c_str(), &St ) != 0)//check if folder exists
    {
        cmd=string("mkdir " + outputDir + "temp");
        error=system(cmd.c_str());
    }
    if (debugMode)
    {
        if (stat( (outputDir + "debug").c_str(), &St ) != 0)//check if folder exists
        {
            cmd=string("mkdir " + outputDir + "debug");
            error=system(cmd.c_str());
        }
    }

    //setup things so all the outputs are written to a log file
	#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__) 
    ofstream logFile((outputDir + "align\\" + basename + "_RAPTOR.log").c_str());
#else
	 ofstream logFile((outputDir + "align/" + basename + "_RAPTOR.log").c_str());
#endif
	
	if (!logFile.is_open())
    {
        cout<<"ERROR:impossible to open file "<<outputDir + "align/" + basename + "_RAPTOR.log"<< " as log file"<<endl;
        exit(-1);
    }

    streambuf* sbufCout = cout.rdbuf();
    streambuf* sbufCerr = cerr.rdbuf();
    streambuf* sbufClog = clog.rdbuf();
    cout.rdbuf(logFile.rdbuf());
    cerr.rdbuf(logFile.rdbuf());
    clog.rdbuf(logFile.rdbuf());
	#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__) 
    string errorOutputLog(outputDir + "align\\" + basename + "_IMOD.log");
#else
	string errorOutputLog(outputDir + "align/" + basename + "_IMOD.log");
#endif
    //cout.rdbuf(sbuf); //USE this line whenever you want to make cout write to terminal again

    string iniTimeStamp=getDate();
    cout<<"Starting RAPTOR on dataset "<<inputDir<<inputFilename<<" at "<<iniTimeStamp<<endl;
    cout<<"RAPTOR called with the following command:"<<endl;
    for (int ii=0;ii<argc;ii++) cout<<argv[ii]<<" ";
    cout<<endl;
    cout<<"InputPath="<<inputDir<<endl;
    cout<<"Filename="<<inputFilename<<endl;
    cout<<"OutputPath="<<outputDir<<endl;
    cout<<"NumberOfMarkers="<<targets<<endl;
    cout<<"MarkerDiameter (in pixels)=";
    for (int kk=0;kk<numDiffMarkerSize;kk++) cout<<diameter[kk]<<",";
    cout<<endl;
    cout<<"Binning factor="<<binning<<endl;
    cout<<"Angles in header="<<anglesHeader<<endl;
    cout<<"Perform reconstruction after alignment="<<rec<<" (negative number means no reconstruction)"<<endl;
    cout<<"Advanced options:"<<endl;
    cout<<"Maximum distance to consider a candidate="<<maxPWtable<<" (negative number means set to default)"<<endl;
    cout<<"Minimum amount of neighbors per candidate="<<minPWtable<<" (negative number means set to default)"<<endl;
    cout<<"Roll-off in exponential when considering neighbors="<<dmax<<" (negative number means set to default)"<<endl;




    vector<frame> frames;
    ioMRC vol;

    if (!vol.readMRCfile(inputDir+inputFilename))
    {
        cout<<"ERROR:reading mrc file "<<inputDir+inputFilename<<endl;
        cout<<"Most possible source of error: file does not exist and there is not enough memory to load all the stack"<<endl;
        exit(-1);
    }

    int maxSide = (vol.headerGetNx() > vol.headerGetNy()) ? vol.headerGetNx(): vol.headerGetNy();
    size_t found=inputFilename.find_last_of(".");
    string fileExtension(inputFilename.substr(found));
    if (dmax<0)
    {
        if (fileExtension == ".preali" ||  fileExtension == ".ali")
        {
            dmax = maxSide / 8;
        }
        else//we assume files are not prealigned so it is the same as .st
        {
            dmax = maxSide / 4;
        }
    }
    if (maxPWtable<0)
    {
        if (fileExtension == ".preali" ||  fileExtension == ".ali")
        {
            maxPWtable = (maxSide / 10 > 50) ? maxSide / 10 : 50;
        }
        else//we assume files are not prealigned so it is the same as .st
        {
            maxPWtable = (maxSide / 6 > 50) ? maxSide / 6 : 50;
        }
    }

    for (int i = 0; i < vol.headerGetNz(); i++)
    {
        frame temp = frame(&vol, i, vol.headerGetNx(), vol.headerGetNy());
        frames.push_back(temp);
    }

    float **templ;
    templ=new float*[numDiffMarkerSize];
    zerotilt = vol.headerGetNz() / 2 + vol.headerGetNz() % 2 - 1;
    for (int kk=0;kk<numDiffMarkerSize;kk++)
    {
        cout<<"Creating synthetic template number "<<kk<<" at "<<getDate()<<endl;
        // Create the template
        templ[kk] = createSyntheticTemplate(&frames, diameter[kk],(int)(zerotilt + 10.0*((rand()/(double)RAND_MAX)-0.5)), &vol,white,kk);

        if (debugMode)
        {
            stringstream itoa;
            itoa<<kk;
			#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__) 
            ofstream outTemplate((outputDir + "debug\\" + basename + "_syntheticTemplateASCII_" + itoa.str() + ".txt").c_str());
#else
			      ofstream outTemplate((outputDir + "debug/" + basename + "_syntheticTemplateASCII_" + itoa.str() + ".txt").c_str());
#endif
            //debugging purposes to check template
            for (int i = 0; i < templSize[kk]; i++)
            {
                for (int j = 0; j < templSize[kk]; j++)
                {
                    outTemplate << templ[kk][i * templSize[kk] + j] << " ";
                }
                outTemplate<<endl;
            }
            outTemplate.close();
        }

        computeNCC(&frames, templ[kk], &vol,kk);



        if (debugMode)
        {
            stringstream itoa;
            itoa<<kk;
			#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__) 
            ofstream outTemplate((outputDir + "debug\\" + basename + "_templateASCII_" + itoa.str() + ".txt").c_str());
#else
			      ofstream outTemplate((outputDir + "debug/" + basename + "_templateASCII_" + itoa.str() + ".txt").c_str());
#endif
            //debugging purposes to check template
            for (int i = 0; i < templSize[kk]; i++)
            {
                for (int j = 0; j < templSize[kk]; j++)
                {
                    outTemplate << templ[kk][i * templSize[kk] + j] << " ";
                }
                outTemplate<<endl;
            }
            outTemplate.close();
        }
    }
    //--------------------------------------------------------------------------
    //--------------------------------------------------------------------------
    //estimate number of markers per image
    if (targets==-1)//we need to estimate number of markers
    {
        targets=estimateNumberOfMarkers(&frames, templ, &vol,numDiffMarkerSize);
        cout<<"Estimating number of markers automatically. Number of markers= "<<targets<<endl;
    }
    maxMarkersPrevFrame = targets;
    maxMarkersNextFrame = min(targets * 4, maxTargetsNextFrame);
    if (maxMarkersNextFrame<maxMarkersPrevFrame)
        maxMarkersNextFrame=maxMarkersPrevFrame;

    //set parameters for pairwise correspondence
    int maxNCliques = (10 * maxMarkersPrevFrame > 500) ? 10 * maxMarkersPrevFrame : 500;
    if (minPWtable<0) minPWtable = (maxMarkersPrevFrame < 20) ? maxMarkersPrevFrame : 20;
    //==========================================================================


    cout<<"Finding gold beads in different projections at "<<getDate()<<endl;
    // Find peaks in all frames with the template
    findAllPeaks(&frames, templ, &vol,numDiffMarkerSize,xRay);


    cout<<"Computing pairwise correspondences at "<<getDate()<<endl;
    //Compute local correspondences
    vector<vector<pairCorrespondence> > correspondences;
    vector<pairCorrespondence>* prevPair = NULL;
    bool flagNotDiscard=true;
    for (int i = zerotilt; i > 0; i--)
    {
        vector<pairCorrespondence> ith_correspondence;
        for (int j = 1;j <= (int)maxJumps; j++)
        {
            if (i - j >= 0)
            {
                flagNotDiscard=correspondenceRegions(&frames.at(i), &frames.at(i-j), &ith_correspondence, 1, maxMarkersPrevFrame, maxMarkersNextFrame, maxNCliques, dmax, 1, 1, maxPWtable, minPWtable, basename + "_LR", 10, &vol, prevPair, outputDir,binPATH);
            }
            if (flagNotDiscard==false)
                break;
        }
        if (flagNotDiscard==false)//indicate that we can not use these frames
        {
            for (int ll=i-1;ll>=0;ll--) frames[ll].discard=true;
            break;
        }
        correspondences.push_back(ith_correspondence);
		if(maxJumps<=correspondences.size())
			prevPair = &correspondences[correspondences.size()-maxJumps];
		else
			prevPair = NULL;
    }
    //prevPair = &correspondences.front();
    prevPair=NULL;
    for (unsigned int i = zerotilt; i < frames.size() - 1; i++)
    {
        vector<pairCorrespondence> ith_correspondence;
        for (unsigned int j = 1;j <= maxJumps; j++)
        {
            if (i + j < frames.size())
            {
                flagNotDiscard=correspondenceRegions(&frames.at(i), &frames.at(i+j), &ith_correspondence, 1, maxMarkersPrevFrame, maxMarkersNextFrame, maxNCliques, dmax, 1, 1, maxPWtable, minPWtable, basename + "_LR", 10, &vol, prevPair, outputDir,binPATH);
            }
            if (flagNotDiscard==false)
                break;
        }
        if (flagNotDiscard==false)
        {
            for (unsigned int ll=i+1;ll<frames.size();ll++) frames[ll].discard=true;
            break;
        }
        correspondences.push_back(ith_correspondence);
		if(maxJumps<=correspondences.size())
			prevPair = &correspondences[correspondences.size()-maxJumps];
		else
			prevPair = NULL;
    }

    cout<<"Building trajectories using pairwise correspondence at "<<getDate()<<endl;
    // Build the global trajectories with the local correspondences
    vector<trajectory> T = findTrajectories(&frames, &correspondences, vol.headerGetNz(), maxMarkersPrevFrame);

    if (debugMode==true)
    {
		cout<<"Recovered "<<T.size()<<" provisional trajectories before optimization"<<endl;
        ofstream out;
		#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__) 
			out.open((outputDir + "debug\\" + basename + "_trajectoryBeforeOptimization.fid.txt").c_str());
		#else
			out.open((outputDir + "debug/" + basename + "_trajectoryBeforeOptimization.fid.txt").c_str());
		#endif
        writeIMODfidModel(T, vol.headerGetNx(), vol.headerGetNy(), vol.headerGetNz(), basename,out);
        out.close();
    }

    //cout<<"PROGRAM TERMINATED FOR DEBUGGGING"<<endl;
    //exit(-1);
//-------------------------------------------------------------------------------
//-----------------------------------------------------------------------------


//=========================================================================
//comment out this part if code generated a .fid.txt and you just want to test optimization
    /*
            cout<<"!!!!WARNING!!!!: running in test mode to debug optrimzation procedure"<<endl;

            int diameter=10;
            bool anglesHeader=false;
            string inputDir("/media/sda2/datasets/testRaptor/");
            string inputFilename("3Dwt1.st");
            string outputDir("/media/sda2/datasets/testRaptor/3Dwt10output-M30/");

            string fidTxtFilename("/media/sda2/datasets/testRaptor/3Dwt10output-M30/debug/3Dwt1_trajectoryBeforeFillContours_iter0_test.fid.txt");
            string fidTxtReprojFilename("/media/sda2/datasets/testRaptor/3Dwt10output-M30/debug/3Dwt1_trajectoryBeforeFillContoursReproj_iter0_test.fid.txt");
            string templFilename("/media/sda2/datasets/testRaptor/3Dwt10output-M30/debug/3Dwt1_syntheticTemplateASCII.txt");
            //--------the part below does not need to be touched

            bool rec=false;
            int thickness=0;
            int templSize=2 * diameter + 1;
            bool debugMode=true;
            ioMRC vol;
            vol.readMRCfile(inputDir + inputFilename);
            int posDot=inputFilename.find_last_of(".");
            string basename=inputFilename.substr(0,posDot);
            string errorOutputLog(outputDir + "align/" + basename + "_IMOD.log");

            vector<frame> frames;
            for (int i = 0; i < vol.headerGetNz(); i++)
            {
                frame temp = frame(&vol, i, vol.headerGetNx(), vol.headerGetNy());
                frames.push_back(temp);
            }

            // Create the template
            float *templ=new float[templSize*templSize];
            ifstream inTempl(templFilename.c_str());

            int posTempl=0;
            for (int ii=0;ii<templSize;ii++)
            {
                for (int jj=0;jj<templSize;jj++)
                {
                    inTempl>>templ[posTempl];
                    cout<<templ[posTempl]<<" ";
                    posTempl++;
                }
                cout<<endl;
            }

            inTempl.close();
            vector<trajectory> T=fiducialModel2Trajectory(fidTxtFilename);

            streambuf* sbufCout = cout.rdbuf();
            streambuf* sbufCerr = cerr.rdbuf();
            streambuf* sbufClog = clog.rdbuf();
            ofstream logFile("");
    */
//========================================================================


    cout<<endl<<"-------------------------------------"<<endl;


    contour *contour_x = new contour(T, 1, vol.headerGetNz(),&frames);
    contour *contour_y = new contour (T, 2, vol.headerGetNz(),&frames);


    int *mType = getMarkerTypeFromTrajectory(T);
    SFMdata *sfm=new SFMdata(*contour_x,*contour_y,mType);
    delete contour_x;
    delete contour_y;
    delete []mType;


    //once we do conversion from trajectories (T) to contour_x contour_y structure we don't need T anymore
    //free memory correctly
    freeTrajectoryVector(T);
    T.clear();
    //famatimprove: trajectory class allowd for forks/sl=plitting in trajectories while contour_x doesn't allow for it
    //We should make optimization work using trajectory class

    cout<<"Reading tilt angles information at "<<getDate()<<endl;

    //copy rawTilt file from origin if it was not in header

	#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__) 
		string outputDirIMOD(outputDir + "IMOD\\");
#else
	string outputDirIMOD(outputDir + "IMOD/");
#endif

    if (anglesHeader)
    {
        //extract angles from header
        cmd=string("extracttilts -tilts -input " + inputDir + inputFilename + " -output " + outputDirIMOD + basename + ".rawtlt");
        error=system((cmd + " > " + errorOutputLog).c_str());
        if (error!=0)
        {
            cout<<"ERROR: error executing the command "<<cmd<<" in function RAPTOPR::Main"<<endl;
            exit(error);
        }
    }
    else //copy angles from inputDir
    {
#ifdef _WIN32
      BOOL copyOK = CopyFile((inputDir + basename + ".rawtlt").c_str(),
                             (outputDirIMOD + basename + ".rawtlt").c_str(), FALSE);
      error = copyOK ? 0 : 1;
#else
      cmd=string("cp " + inputDir + basename + ".rawtlt " + outputDirIMOD);
      error=system((cmd + " > " + errorOutputLog).c_str());
#endif
      if (error!=0)
        {
            cout<<"ERROR: error executing the command "<<cmd<<" in function RAPTOPR::Main"<<endl;
            exit(error);
        }
    }

    vector<double> tiltAngles;
    ifstream in2;
    in2.open((outputDirIMOD + basename + ".rawtlt").c_str());
    if (in2.is_open()==false)
    {
        cout<<"ERROR: RAPTOR can not find file "<<inputDir + basename + ".rawtlt"<<" containing tilt angles from goniometer"<<endl;
        exit(-1);
    }
    double tiltAngle;
    while (in2.good())
    {
        in2 >> tiltAngle;
        tiltAngles.push_back(tiltAngle);
    }
    in2.close();

    tiltAngles.resize(vol.headerGetNz());//necessary in case C++ reads the last line twice
    //remove tilt angles that correspond to frames which are discarded
    for (int kk=tiltAngles.size()-1;kk>=0;kk--)
    {
        if (frames[kk].discard==true)
            tiltAngles.erase(tiltAngles.begin()+kk);
    }
    //----------------------------------------------------------------

    cout << endl << "Starting optimization loop" << endl;

    int W = vol.headerGetNx();
    int H = vol.headerGetNy();
    double alphaFinal = 0;
    int optionAlpha = 0;
    int iter=0;
    int nnz=sfm->contour_x->calculateNNZ();//computes number of markers overall to avoid doing more iterations if there is no progress.
    int nnzOld=0;
    //while (nnzOld<nnz && iter<2)
    while (iter<2)
    {

        cout<<"Starting iteration "<<iter<<" of optimization procedure at "<<getDate()<<endl;
        SFMdata* sfm_=decideAlphaOption(sfm,&optionAlpha,&frames);//sfm is freed inside decideAlphaOption
        sfm=NULL;
        cout<<"Using alpha option="<<optionAlpha<<endl;

        SFMdata* thesfm = SFMestimationWithBA(sfm_, tiltAngles, W, H, percentile, &alphaFinal, optionAlpha,debugMode);//_sfm is deleted inside the method

        if (optionAlpha==0)
            optionAlpha=2;
        else if (optionAlpha==2)//we change option so next time alpha won't be calculated
            optionAlpha=1;


        SFMdata* newsfm=residAnalysis(thesfm,debugMode);//thesfm is freed inside resid analysis

		#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__) 
		string outputDirDebug(outputDir + "debug\\");
#else
	string outputDirDebug(outputDir + "debug/");
#endif
        if (debugMode==true)
        {
            ofstream out;
            stringstream itoa;
            itoa<<iter;
            out.open((outputDirDebug + basename + "_trajectoryBeforeFillContours_iter"+ itoa.str() +".fid.txt").c_str());
            writeIMODfidModel(newsfm, vol.headerGetNx(), vol.headerGetNy(), vol.headerGetNz(), basename,out,&frames);
            out.close();
            out.open((outputDirDebug + basename + "_trajectoryBeforeFillContoursReproj_iter"+ itoa.str() +".fid.txt").c_str());
            writeIMODfidModelReproj(newsfm, vol.headerGetNx(), vol.headerGetNy(), vol.headerGetNz(), basename,out,&frames);
            out.close();
        }


        fillContours(newsfm,&vol,templ,templSize,debugMode);


        if (debugMode==true)
        {
            ofstream out;
            stringstream itoa;
            itoa<<iter;
            out.open((outputDirDebug + basename + "_trajectoryAfterFillContours_iter"+ itoa.str() +".fid.txt").c_str());
            writeIMODfidModel(newsfm, vol.headerGetNx(), vol.headerGetNy(), vol.headerGetNz(), basename,out,&frames);
            out.close();
            out.open((outputDirDebug + basename + "_trajectoryAfterFillContoursReproj_iter"+ itoa.str() +".fid.txt").c_str());
            writeIMODfidModelReproj(newsfm, vol.headerGetNx(), vol.headerGetNy(), vol.headerGetNz(), basename,out,&frames);
            out.close();
        }

        if (sfm!=NULL)
            delete sfm;
        sfm=joinSimilarContours(newsfm);


        //update all the variables for teh while loop
        nnzOld=nnz;
        nnz=sfm->contour_x->calculateNNZ();
        iter++;
    }

    if (optionAlpha==2) optionAlpha=1;

    //final pass to remove all the outliers without filling contours
    nnzOld=nnz+10;;
    while (nnzOld>nnz)
    {
        cout<<"Starting iteration "<<iter<<" of optimization procedure at "<<getDate()<<endl;
        SFMdata* sfm_=decideAlphaOption(sfm,&optionAlpha,&frames);//sfm is freed inside decideAlphaOption
        sfm=NULL;
        cout<<"Using alpha option="<<optionAlpha<<endl;
        SFMdata* thesfm = SFMestimationWithBA(sfm_, tiltAngles, W, H, percentile, &alphaFinal, optionAlpha,debugMode);//sfm_ is deleted inside

        if (sfm!=NULL)
            delete sfm;
        sfm=residAnalysis(thesfm,debugMode);//thesfm is freed inside resid analysis

        //update all the variables for teh while loop
        nnzOld=nnz;
        nnz=sfm->contour_x->calculateNNZ();
        iter++;
    }

    //--------------------------------------------
    //-----------------------------------
    //-------Write everything to IMOD-----------------
    int tiltOption,magOption,rotOption;//options for tilt alignment
    decideTiltAlignOptions(sfm,&tiltOption,&rotOption,&magOption);

    T=sfm->contour_x->contour2trajectory(*(sfm->contour_x),*(sfm->contour_y),&frames);
    delete sfm;


    ofstream out((outputDirIMOD + basename + ".fid.txt").c_str());
    if (!out.is_open())
    {
        cout<<"ERROR:file "<< outputDirIMOD + basename + ".fid.txt" <<" can no be created to write results"<<endl;
        exit(-1);
    }
    writeIMODfidModel(T,W,H,vol.headerGetNz(),basename,out);
    out.close();

	#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__) 
		string outputDirAlign(outputDir + "align\\");
	#else
		string outputDirAlign(outputDir + "align/");
	#endif

    if (trackingOnly==false)//if trackingOnly==true the program ends here pretty much
    {


        cout<<"------------------------------------------------"<<endl;
        cout<<"Generating aligned stack at "<<getDate()<<endl;

        out.open((outputDirIMOD + basename + "_tiltalignScript.txt").c_str());
        if (!out.is_open())
        {
            cout<<"ERROR:file "<< outputDirIMOD + basename + "_tiltalignScript.txt" <<" can no be created to write results"<<endl;
            exit(-1);
        }
        double alphaIMOD=alphaFinal-90.0;//we need to convert alphaFinal to IMOD coordinates
        if (alphaIMOD<=-90) alphaIMOD+=180.0;//to make sure there is no mirroring of the aligned stack versus the original stack
        writeIMODtiltalign(alphaIMOD,outputDirIMOD,basename,out,&frames,tiltOption,rotOption,magOption);
        out.close();

        //execute tiltalign to compute transformations
        cmd=string("tiltalign -param " + outputDirIMOD +  basename + "_tiltalignScript.txt");
        error=system((cmd + " > " + errorOutputLog).c_str());
        if (error!=0)
        {
            cout<<"ERROR: error executing the command "<<cmd<<" in function RAPTOPR::Main"<<endl;
            exit(error);
        }
        //create aligned stack
        cmd=string("newstack  -input " + inputDir + inputFilename + " -output " + outputDirAlign + basename + ".ali -offset 0,0 -xform " + outputDirIMOD + basename + ".xf -secs ");
        bool flagFirst=true;
        for (unsigned int kk=0;kk<frames.size();kk++)
        {
            if (frames[kk].discard==false)
            {
                stringstream itoa;
                itoa<<kk;
                if (flagFirst)
                {
                    cmd+=itoa.str();
                    flagFirst=false;
                }
                else cmd+=("," + itoa.str());
            }
        }

        error=system((cmd + " > " + errorOutputLog).c_str());
        if (error!=0)
        {
            cout<<"ERROR: error executing the command "<<cmd<<" in function RAPTOR::Main"<<endl;
            exit(error);
        }
        //bin aligned stack if necessary
        if(binning!=1)
        {
            stringstream itoa;
            itoa<<binning;
            cmd=string("newstack -input " + outputDirAlign + basename + ".ali -output " + outputDirAlign + basename + "Bin.ali" + " -bin " + itoa.str());
            error=system((cmd + " > " + errorOutputLog).c_str());
            if (error!=0)
            {
                cout<<"ERROR: generating binned stack from original file"<<endl;
                exit(-1);
            }
            //remove align stack with no binning
            cmd=string("mv " + outputDirAlign + basename + "Bin.ali " + outputDirAlign + basename + ".ali");
            error=system((cmd + " > " + errorOutputLog).c_str());
            if (error!=0)
            {
                cout<<"ERROR: copying binned align stack"<<endl;
                exit(-1);
            }
        }
    }
    //----------------------------------------------

    cout<<"------------------------------------"<<endl;
    cout<<"Releasing memory"<<endl;
    vol.clear();
    //free memory correctly
    freeTrajectoryVector(T);
    T.clear();
    delete []diameter;
    delete []templSize;
    for (int kk=0;kk<numDiffMarkerSize;kk++)
        delete []templ[kk];
    delete []templ;


    //-------------------------------------------------------------------
    string endingRec("_full.rec");
    for (unsigned int kk=0;kk<frames.size();kk++)
    {
        if (frames[kk].discard)
        {
            endingRec=string("_part.rec");
        }
    }

    //----------PERFORM RECONSTRUCTION USING IMOD IF USER HAS REQUESTED SO--------------
    if (rec!=-10000)
    {
        if (rec<0 || rec>2)
        {
            cout<<"ERROR: reconstruction option mode is "<<rec<<".It should be 0,1 or 2 RAPTOR is skipping reconstruction."<<endl;
        }
        else
        {

            if (thickness==0)//in case they have not setup the thickness
                thickness=800;
            cout<<"Starting reconstruction at "<<getDate()<<endl;
            writeIMODtiltScript(W/binning,H/binning,outputDirIMOD,outputDirAlign,basename,thickness,&frames,endingRec);
            cmd=string("submfg " + outputDirAlign + "tilt.com");
            error=system((cmd + " > " + errorOutputLog).c_str());
            if (error!=0)
            {
                cout<<"ERROR: error executing the command "<<cmd<<" in function RAPTOR::Main"<<endl;
                exit(error);
            }
            //rescale output if necessary
            if (rec==0)
            {
                cmd=string("trimvol -c 0,255 " + outputDirAlign + basename + endingRec +" " + outputDirAlign + basename + "_fullTemp.rec");
                error=system((cmd + " > " + errorOutputLog).c_str());
                if (error!=0)
                {
                    cout<<"ERROR: error executing the command "<<cmd<<" in function RAPTOR::Main"<<endl;
                    exit(error);
                }
                cmd=string("rm -f " + outputDirAlign + basename + endingRec);
                error=system(cmd.c_str());
                cmd=string("mv " + outputDirAlign + basename + "_fullTemp.rec " + outputDirAlign + basename + endingRec);
                error=system(cmd.c_str());
            }
            else if (rec==1)
            {
                cmd=string("trimvol -mm 0,32767 " + outputDirAlign + basename + endingRec + " " + outputDirAlign + basename + "_fullTemp.rec");
                error=system((cmd + " > " + errorOutputLog).c_str());
                if (error!=0)
                {
                    cout<<"ERROR: error executing the command "<<cmd<<" in function RAPTOR::Main"<<endl;
                    exit(error);
                }
                cmd=string("rm -f " + outputDirAlign + basename + endingRec);
                error=system(cmd.c_str());
                cmd=string("mv " + outputDirAlign + basename + "_fullTemp.rec " + outputDirAlign + basename + endingRec);
                error=system(cmd.c_str());
            }
        }
    }
    //-------------------------------------------------------------

    for (unsigned int kk=0;kk<frames.size();kk++)
    {
        if (frames[kk].discard)
        {
            cout<<"WARNING: projection "<<kk<<" was discarded during the alignment process. Not enough markers where found."<<endl;
        }
    }

    cout<<"RAPTOR finished succesfully on dataset "<<inputDir<<inputFilename<<" at "<<getDate()<<endl;
    cout<<"You can check aligned stack at "<<outputDir<<"align/"<<endl;
    cout<<"You can check log files at "<<outputDir<<"align/*.log"<<endl;
    cout<<"You can check fiducial marker model at "<<outputDir<<"IMOD/*.fid.txt"<<endl;

    //remove temp folder
	#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__)
		cmd=string("rm -rf " + outputDir + "temp\\");
	#else
		cmd=string("rm -rf " + outputDir + "temp/");
	#endif
	//cmd=string("rm -rf " + outputDir + "temp/" + basename + "_*");
    error=system(cmd.c_str());

	#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__) 
		string outputDirRec(outputDir + "reconstruction\\");
	#else
		string outputDirRec(outputDir + "reconstruction/");
	#endif

    //minimalistic mode
    if (verbose==0)
    {
        cmd=string("mkdir " + outputDirRec);
        error=system(cmd.c_str());
        cmd=string("mv " + outputDirAlign + basename + endingRec + " " + outputDirRec);
        error=system(cmd.c_str());
        //create minimal log file
        ofstream outLog((outputDirRec + basename + "_RAPTOR.log").c_str());
        if (!outLog.is_open())
        {
            cout<<"ERROR: could not open file "<<outputDirRec + basename + "_RAPTOR.log"<<" to write final log file"<<endl;
            exit(-1);
        }

        outLog<<"RAPTOR started at "<<iniTimeStamp<<endl;
        outLog<<"RAPTOR called with the following command:"<<endl;
        for (int ii=0;ii<argc;ii++) outLog<<argv[ii]<<" ";
        outLog<<endl;
        for (unsigned int kk=0;kk<frames.size();kk++)
        {
            if (frames[kk].discard)
            {
                outLog<<"WARNING: projection "<<kk<<" was discarded during the alignment process. Not enough markers where found."<<endl;
            }
        }
        outLog<<"RAPTOR ended succesfully at "<<getDate()<<endl;
        outLog.close();
    }

    cout.rdbuf(sbufCout);
    cerr.rdbuf(sbufCerr);
    clog.rdbuf(sbufClog);
    logFile.close();
    frames.clear();

    //remove folders if minimal output is desired
    if (verbose==0)
    {
        cmd=string("rm -rf " + outputDirAlign);
        error=system(cmd.c_str());
        cmd=string("rm -rf " + outputDirIMOD);
        error=system(cmd.c_str());
        //we don't need to delete debug because if verbatim==0 then we are not in debug mode
        //rename reconstruction as align
        cmd=string("mv " + outputDir + "reconstruction " + outputDir + "align");
        error=system(cmd.c_str());
    }

    return 0;
}
