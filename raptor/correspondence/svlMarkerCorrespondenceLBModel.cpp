/*
 * svlMarkerCorrespondenceLBModel.cpp - main inference engine to run LBP trhough SVL library
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */

/*Fernando Amat Gil April, 20th 2010
Translations of Farshid's markers correspondence from GGL library to SVL library

*/
#include <signal.h>
#include <math.h>
#include "svlMarkerCorrespondenceLBModel.h"
#include <sstream>
#include <iostream>
#include <assert.h>
#include <time.h>


const string svlMarkerCorrespondenceLBModel::usage = "";

// default values, can be changed via methods of gglImageContourLBModel
const double MIN_VAL = 10e-20;
const double OCCLUD_VAL = 10e-7;
const double OVERLAP_VAL = 10e-7;
const double LOW_VAR_THRESH = 10;
const double EDGE_FRACTION = 0.2;
const double ANGLE_FRACTION = 0.2;
const double RAW_DIST_NORM = 100;


// CONSTRUCTOR
svlMarkerCorrespondenceLBModel::svlMarkerCorrespondenceLBModel(gglMatrix const& MarkerLocations1,gglMatrix const& MarkerLocations2, gglMatrix const& MarkerCandidates2,gglMatrix const& SPMatrix, string test_name,int lock_pots_file_exists, istream &  infilename,istream &  infilename_lock_pots, int _debug_init_beliefs ,int _debug_pair_pots ,int _pair_pots_m1 ,int _pair_pots_m2 ,double _dist_diff_intcpt, int _verbose , int _mtime)
{
    // Member Variables
    _MarkerLocations = MarkerLocations1;
    _MarkerLocations2 = MarkerLocations2;
    _MarkerCandidates = MarkerCandidates2;
    _SPMatrix = SPMatrix;
    //_InitBeliefs = SPMatrix;
    _NumMarkers = _MarkerLocations.dim2();
    _NumCandidates = _MarkerCandidates.dim2();
    _test_name=test_name;
    svlMarkerCorrespondenceLBModel::ReadConfig(infilename);
    if(lock_pots_file_exists)
    {
        svlMarkerCorrespondenceLBModel::ReadLockPots(infilename_lock_pots);
    }
    //_dist_thr_min=dist_thr_min;
    //_dist_thr_max=dist_thr_max;
    debug_initial_beliefs = _debug_init_beliefs;
    verbose=_verbose;
    mtime=_mtime;
    debug_pair_pots = _debug_pair_pots;
    pair_pots_m1 = _pair_pots_m1 ;
    pair_pots_m2 = _pair_pots_m2 ;
    //dist_diff_intcpt=_dist_diff_intcpt;
    dist_diff_intcpt=_intcpt_b;
    max_mrkr_pair_dist=_max_mrkr_pair_dist;
    //gcan_potential=1e-6;
    // FARSHID
    // This will be the constructor
    InitValues();
}

//Farshid- complete destructor
svlMarkerCorrespondenceLBModel::~svlMarkerCorrespondenceLBModel()
{
    Clear();
}


void svlMarkerCorrespondenceLBModel::Clear()
{
//added by Farshid

    if ( _lbInfer!=NULL )
        delete _lbInfer;

    //if ( _lbGraph )
   //     delete _lbGraph;
   // _lbGraph = NULL;

    _lbCards.clear();
//  if ( _lbVars!=NULL )
//    delete _lbVars;
//  _lbVars = NULL;
    _modelBuilt = false;
    _inferenceBuilt = false;

}


void svlMarkerCorrespondenceLBModel::InitValues()
//modified by Farshid
{

// loopy
    _lbInfer = NULL;
    _maxProduct = false;//true->we use max-product;false->we use sum product
    _maxMessages = 10000;//for ggl it was 25000
    _doProdDivide = false;
    _smooth = 0.0;
    _treeK = 1;
//_MType = MQT_WEIGHTED;

//  _lbVars = NULL;
    _lbCards.clear();
    //_lbGraph = NULL;

    _combinePotentials = false;
    //_proximityThreshold = 50.0;
    _priorLocations = false;
    _inferenceBuilt=false;

    // potential strengths
    _normalizeSingleton = false;
    _useModelWeights = false;

    // best contour
    _modelBuilt = false;
    _graphBuilt = false;
    _initialized = false;
    _avgLineLength = 0.0;
    _optionsRead = false;
    _revisingFeatures = false;
    //SetMeasureDispatcher(&_lbMD);
}//end Init_Values


gglMatrix svlMarkerCorrespondenceLBModel::GetFinalMarginalBeliefs()
//gglMatrix svlMarkerCorrespondenceLBModel::GetFinalBeliefs()
{

    int j;
    //gglMatrix C(_NumMarkers,_NumCandidates);
    clock_t initTotalTime = clock();
    gglMatrix C(_NumMarkers,_NumCandidates+1);//Garbage can
    BuildModel();

    //output all probs to file for debug
    /*
    string fname("farshid.fastInf");
    ofstream out(fname.c_str());
    this->printVarsAndCardToFile(out);
    this->getGraph().printGraphToFastInfFormat(out);
    this->printModelToFastInfFormat(out);
    out.close();
    */

    assert(_modelBuilt);
    if ( !_inferenceBuilt )
    {
        if(verbose)cerr<<"Constructing loopy inference and propagating messages\n";
        //modified by Farshid using gglCorrInfer as a template
        //_lbInfer = Inference(*this,_maxProduct,_maxMessages,_smooth,_treeK,_doProdDivide);
        //if(verbose)cerr<<"Built Inference...\n";
        //_inferenceBuilt=true;
        clock_t initTime = clock();
        //if(verbose)cerr<<"Calculating Probabilities...\n";
        //bool succ = _lbInfer->calcProbs();
        bool succ;
        _lbInfer=new svlMessagePassingInference(_lbGraph);
        if(_maxProduct)
            succ=_lbInfer->inference(SVL_MP_RBP_MAXPROD,_maxMessages);
        else
            succ=_lbInfer->inference(SVL_MP_RBP_SUMPROD,_maxMessages);

        if(verbose)cerr<<"Finished Calculating Probabilities...\n";
        clock_t finalTime = clock();
        if(mtime)cerr << "Inference took " << (finalTime-initTime)/CLOCKS_PER_SEC << " seconds\n";
        if(mtime)cerr << "TOTAL OPERATION took " << (finalTime-initTotalTime)/CLOCKS_PER_SEC << " seconds\n";
        if ( ! succ )
            cout << "NOT CONVERGED!\n";
        else
            cout << "CONVERGED!\n";
    }
    // Read from probs
    //lbFullAssignment bestAssign;
    int i;

    if(verbose)cout<<"\nThe WINNERS ARE:\n";
    for ( i=0 ; i < _NumMarkers ; i++ )
    {
        double max;
        int winner=0;
        //cout << "CHECKING!\n";
        //cout << "Searching for best assignement to point " << i << endl;
        // get measure from inference object
        varsVec vvec;
        vvec.push_back(i);
        cardVec cvec;
        //modified by Farshid
        cvec.push_back(_lbCards[i]);

        // Get the final belief over variable i
        //lbAssignedMeasure_ptr meas_ptr = _lbInfer->prob(vvec,i);
        svlFactor meas_ptr((*_lbInfer)[i]);

        // find maximal assignment
        //int maxp = 0;
        //double maxv = -HUGE_VAL;
        //lbSmallAssignment assign(vvec);
        //for ( int j=0 ; j < _NumCandidates ; j++ )
        max=-1000000000.;
        for ( int jj=0 ; jj <= _NumCandidates ; jj++ )  //Initialize C to all very small numbers
        {
            C[i][jj]=-1e12;
        }
        for ( int jj=1 ; jj <= cvec[0] ; jj++ )  //Garbage Can
        {
            j=_allowVals[i][jj];
            //assign.setValueForVar(i,jj-1);

            // THIS IS THE PROBABILITY THAT MARKER i
            // IS ASSIGNED TO CANDIDATE j
            //double val = meas_ptr->logValueOfFull(assign);
            double val=meas_ptr[jj-1];
            //cout << "P(M_" << i << " = C_" << j << " = " << val << "\n";
            if(val>max)
            {
                max=val;
                winner=j;
            }
            C[i][j]=log(val);

            // Do what you want with val
        }
        if(verbose)cout<<i<<":"<<winner<<"\n";
        //delete meas_ptr;
    }
    if(verbose)cout<<"\n";
    //cout << "\n....DONE CHECKING!\n";
    return C;
}//end GetFinalMarginalBeliefs


gglMatrix svlMarkerCorrespondenceLBModel::GetInitialBeliefs()
{
    gglMatrix F(_NumMarkers,_NumCandidates);
    F[1][1]=3.0;
    return F;
}//end GetInitialBeliefs

/*********************************
**				**
**    COMPUTE ALLOWED VALUES	**
**				**
**********************************/

void svlMarkerCorrespondenceLBModel::ComputeAllowedValues()
{
    _allowVals.clear();
//Based only on proximity

    AllowValuesByInitialProximity();

}

void svlMarkerCorrespondenceLBModel::AllowValuesByInitialProximity()
{

    unsigned int ncandidates = _MarkerCandidates.dim2();
    unsigned int nmarkers = _MarkerLocations.dim2();
    unsigned int i,j;
    int * cand_assigned_flag;
    cand_assigned_flag=new int[ncandidates];

    //figure out minimum number of candidates
    unsigned int min_cands;
    if(_min_cands<ncandidates)min_cands=_min_cands;
    else min_cands=ncandidates;

    // For Each Point, Allow
    for ( i=0 ; i<nmarkers ; i++ )
    {
        //reset candidates assigned flag
        for ( j=0 ; j<ncandidates ; j++ )cand_assigned_flag[j]=0;
        vector<int> tmp(1,-1);
        double thresh = _proximityThreshold;
        //while(tmp.size() < _min_cands)
        while(tmp.size() < min_cands)
        {
            //cerr << "TMP SIZE="<<tmp.size()<<" THRESH = " << thresh << ": Allowing values for mrkr "<<i<<": \n ";
            for ( j=0 ; j<ncandidates ; j++ )
            {
                if((cand_assigned_flag[j]==0)&&(dist_l2(_MarkerLocations[0][i],_MarkerLocations[1][i],_MarkerCandidates[0][j],_MarkerCandidates[1][j])<thresh))
                {
                    cand_assigned_flag[j]=1;
                    tmp.push_back(j);
                    //cerr << "  " << j << ", ";
                }
            }
            thresh *= 1.5;
            //if(tmp.size() < min_cands)cerr << "TABLE PRUNING:  HAD TO INCREASE PROXIMITY THRESHOLD\n";
        }
//automatically add garbage can as allowed value
        tmp.push_back(ncandidates);
        //cerr << ncandidates ;

        _allowVals.push_back(tmp);
        if(verbose)cerr << "THRESH = " << thresh << ": Allowed " << tmp.size()-1<< " values for marker " << i << endl;
    }

//Print out allowed values
    /*
      for ( i=0 ; i<nmarkers ; i++ )
              cerr<<"\nAllowed "<<_allowVals[i].size()<<" vals for mrkr "<<i<<"\n";
      for ( i=0 ; i<nmarkers ; i++ ){
              cerr<<"\nAllowed "<<_allowVals[i].size()-1<<" vals for mrkr "<<i<<":\n";
              //cerr<<"\nAllowed vals for mrkr "<<i<<":\n";
    	  for ( j=1 ; j<_allowVals[i].size() ; j++ )
                cerr<<_allowVals[i][j]<<", ";
            }
              cerr<<"\n\n";
    */



}



/************************
 ************************
 **		       **
 ** BUILDING THE MODEL **
 **		       **
 ************************
 ************************/
void svlMarkerCorrespondenceLBModel::BuildModel(vector< vector<int> >* forcedAllowedValues,bool replaceExistingValues)
{
    if ( _modelBuilt )
        return;

    //BuildGraph();

    if(verbose)cerr << "Constructing lbModel object\n";

    // STEP -1: Compute allowed values
    ComputeAllowedValues();

    // STEP 0:
    BuildCards();
    //SetCards(_lbCards);

    //build cluster graph
    if(_graphBuilt) return;
    _lbGraph=svlClusterGraph(_NumMarkers,_lbCards);
    _graphBuilt=true;


    // STEP 1:
    clock_t initTime = clock();
    BuildSingletonPotentials();
    clock_t finalTime = clock();
    if(mtime)cerr << "Singleton Potentials took " << (finalTime-initTime)/CLOCKS_PER_SEC << " seconds\n";

    // STEP 2: build pairwise potentials
    initTime = clock();
    BuildPairwiseCliques();
    BuildPairwisePotentials();
    finalTime = clock();
    if(mtime)cerr << "Pairwise Potentials took " << (finalTime-initTime)/CLOCKS_PER_SEC << " seconds\n";


    //STEP 3: connect cliques
    _lbGraph.betheApprox();

    _modelBuilt = true;
}

void svlMarkerCorrespondenceLBModel::BuildCards()
{


    //-------------------------------------
    // create cards (occluded + cardinality
    //	     of plausible edges points)
    //Farshid - setting cardinality of variables in cliques
    //This is the size of each variable.  For an NxN image, each marker
    //location can have one of N^2 values, and each edge is between
    //two markers, so can have one of N^4 values
    //variables are candidate marker locations
    //-------------------------------------
//    int ncandidates = _MarkerCandidates.dim2();
    int nmarkers = _MarkerLocations.dim2();


    //if ( _lbCards != NULL )
     //   delete _lbCards;
    //_lbCards = new lbCardsList(*_lbVars);
    _lbCards.clear();
    _lbCards.reserve(nmarkers)    ;
    //for ( int i = 0; i < ncandidates; i++ ) {
    //Farshid - changed 3/6/06 - check with Geremy!
    for ( int i = 0; i < nmarkers; i++ )
    {
        //famatdebug:make sure this step +- 1 is right
        //_lbCards->setCardForVar(i,ncandidates);
        //_lbCards->setCardForVar(i,ncandidates+1);//Garbage Can
        _lbCards.push_back(_allowVals[i].size()-1);//Garbage Can
    }
}

/*
int svlMarkerCorrespondenceLBModel::UpdateMeasure(varsVec vvec,int cind,lbFeatureTableMeasure_Sptr meas)
{
    measIndex mind;
    if ( !isMeasureAssignedForClique(cind) )
    {
        //    cout << "Adding new measure " << mind << " to clique " << cind << endl;
        ostringstream mname;
        mname << "meas" << cind;
        mind = addMeasure(meas,mname.str());
        setMeasureForClique(cind,mind);
        // meas->print(cout);
    }
    else
    {
        // cout << "Replacing measure " << mind << " for clique " << cind << endl;
        mind = getMeasureIndexForClique(cind);
        string mname = getMeasureName(mind);
        lbFeatureTableMeasure_Sptr newMeas(meas->multiply((lbFeatureTableMeasure&)getMeasure(mind)));
        replaceMeasure(newMeas,mind,mname);
        // meas->print(cout);
    }
    return mind;
}
*/
/*
svlMessagePassingInference* svlMarkerCorrespondenceLBModel::Inference(svlMarkerCorrespondenceLBModel& model,
        bool maxProduct,
        int maxMessages,
        double smoothParam,
        int treeK,
        bool doProdDivide)
{

    // create inference object
    addVerbose(V_PROPAGATION);
    lbBeliefPropagation* inf = new lbBeliefPropagation(model,md);
    inf->createInferenceMonitor();
    inf->getInferenceMonitor()->setStatGapMessage(1000);
    lbOptions::addVerboseOption(1,string("propagation"));

    lbMeasure::setMaxProduct(maxProduct);
    inf->setMaxMessages(maxMessages);
    inf->setSmoothing(smoothParam);
    inf->setUpdateSize(treeK);
    inf->setQueueType(type);
    inf->setCompareType(C_MAX);
    inf->setInitType(MIT_UNIFORM);

    return inf;
}
*/
/******************************
 * OCCLUSION PRIOR POTENTIALS *
 ******************************/

void svlMarkerCorrespondenceLBModel::BuildSingletonPotentials()
{

    int nmarkers = _MarkerLocations.dim2();
    int ncandidates = _MarkerCandidates.dim2();
    double max_dot_i_j_k_l,norm_i_j_k_l;
    double dist_mrkr_cand;
    double factor,potentialValue;
    int ind=0;
    ofstream out;

    string fname;
    string init_beliefs_var_name;

    fname=_test_name+"_init_beliefs_scr.m";
    init_beliefs_var_name=_test_name+"_init_beliefs";
    //out.open("init_beliefs.m");
    out.open(fname.c_str());


    // FARSHID
    //For each marker, candidate pair..
    //For each marker....
    if(verbose)cout << "\n\n\nCreating Singleton Potential Matrix Initial Belief SP:\n " ;
    out << init_beliefs_var_name.c_str()<<"=[ \n ";
    for ( int i=0 ; i < nmarkers ; i++ )
    {
        //cerr<<"CALC_SING_POTS: i="<<i<<":\n";

        //  cout << "SINGLETON POTENTIAL FOR MARKER " << i << "\n";
        //-----------------------------------------------
        // create singleton potentials
        //-----------------------------------------------

        cardVec cvec;
        cvec.push_back(_lbCards[i]);
        varsVec vvec;
        vvec.push_back(0);
        //lbWeightedTableMeasure_Sptr SingleMeas(new lbWeightedTableMeasure(cvec));
        //SingleMeas->setWeight(1.0);
        svlFactor SingleMeas;//factor to store table for i-th marker
        SingleMeas.addVariable(i,_lbCards[i]);

        //... And For each candidate,

        //for( int j = 0; j < ncandidates+1  ; j++) //Garbage Can
        for( int jj = 1; jj <= cvec[0]  ; jj++) //Garbage Can
        {
            //lbSmallAssignment assign(vvec);

            //assign.setValueForVar(0,jj-1);
            int j=_allowVals[i][jj];
            //cerr<<"  jj="<<jj<<", j="<<j<<", ";

            //calculate singleton potential:
            //sum_over_k (max_l(dot(u(i->j),u(k->l)))), i!=k, j!=l



            //FIRST:Check if Potential is LOCKED DOWN:

            if(_lock_pot[i]==1)
            {

                if(_locked_pot_val[i]==j)
                {
                    potentialValue =1;
                }
                else
                {
                    potentialValue =1e-10;

                }
                //SingleMeas->setFeatureValueOfFull(assign,vvec, potentialValue);
            }


            //OTHERWISE calculate potential as usual:
            else
            {

                //Initialize potential...
                potentialValue =1e-10;// VALUE OF ASSIGNING MARKER i TO CANDIDATE j


                if(j<ncandidates) //evaluate singleton potentials other than garbage can
                    //if(jj<(cvec[0]))
                {
//singleton potentials options:
//_sing_po_choice
//1:uniform
//2:dot product based
//3:read in from file directly with no qualification
//4:uniform subject to distance constraint
//5:read in from file directly multiplied by Gaussian distance rolloff distribution

                    if(_sing_po_choice==1)
                    {
                        potentialValue=1; //Farshid 4-17-06- using uniform potentials
                    }//end if _sing_po_choice==1

                    if(_sing_po_choice==2)
                    {
                        //For each possible remaining marker candidate pair:
                        //For each remaining marker...
                        for( int k = 0; k <nmarkers ; k++)
                        {
                            if(k!=i)
                            {


///*FARSHID 0428-make uniform singleton potentials for now
                                max_dot_i_j_k_l=0.0;
                                //.... And For each remaining candidate...
                                for( int l = 0; l < ncandidates ; l++)
                                {
                                    if(l!=j)
                                    {
                                        //calculate dot products, take max
                                        norm_i_j_k_l=norm_dot(_MarkerLocations[0][i],_MarkerLocations[1][i],_MarkerCandidates[0][j],_MarkerCandidates[1][j],_MarkerLocations[0][k],_MarkerLocations[1][k],_MarkerCandidates[0][l],_MarkerCandidates[1][l],_dist_thr_max[i],_dist_thr_min[i],_dist_thr_max[k],_dist_thr_min[k]);
                                        //cout << "NORM_" << i <<j<< "_" << k << l<<" = " << norm_i_j_k_l << "\n";
                                        if(norm_i_j_k_l>max_dot_i_j_k_l)
                                        {
                                            max_dot_i_j_k_l=norm_i_j_k_l;
                                        }
                                    }//end if(l!=j)
                                }//for(int l=0....

                                //update potential accumulation for current marker-candidate pair
                                potentialValue+=max_dot_i_j_k_l;

                            }//end if(k!=i)


                        }//for(int k=0....
                    }//end if _sing_po_choice==2

                    if((_sing_po_choice==3)||(_sing_po_choice==5)) 	   //Farshid 5-7-06read singleton potentials directly from file
                    {
                        potentialValue=_SPMatrix[i][j];
                        //  cout << "Assigned sp for marker " << i << " candidate " << j << endl;
                        if(_sing_po_choice==5) 	   //multiply by e^-(dist/dist_thr)^2
                        {
                            dist_mrkr_cand= dist_l2(_MarkerLocations2[0][i],_MarkerLocations2[1][i],_MarkerCandidates[0][j],_MarkerCandidates[1][j]);

                            factor=exp(-pow(dist_mrkr_cand/_dist_thr_max[i],2));
                            potentialValue=potentialValue*factor;
                            //cout << "DEBUG POT:dist= "<< dist_mrkr_cand <<",  factor e(-(dist/dist_max)^2)= "<<factor<<"\n";
                        }// end if(_sing_po_choice==5)

                    } //end if(_sing_po_choice==3/5)

                    if(_sing_po_choice==4)
                    {
                        if(dist_l2(_MarkerLocations[0][i],_MarkerLocations[1][i],_MarkerCandidates[0][j],_MarkerCandidates[1][j])<_dist_thr_max[i])
                        {
                            potentialValue=1;
                        }
                        else
                        {
                            potentialValue=1e-10;
                        }
                    }//end if _sing_po_choice==4


                } // end if(j<ncandidates)

                else 	//this is the garbage can
                {
                    potentialValue=gcan_potential;

                }

            } //potential not locked down
            //put lower limit on potentials
            if(potentialValue <1e-10)
            {
                potentialValue =1e-10;
            }
            //cout << potentialValue << "  " ;
            out << potentialValue << "  " ;
            //if(j==(cvec[0]-1))
            if(j==(ncandidates))
            {
                //cout << ";\n" ;
                out << ";\n" ;
            }
            else
            {
                //cout << ", " ;
                out << ", " ;
            }

            //SingleMeas->setFeatureValueOfFull(assign,vvec, potentialValue);
            SingleMeas[jj-1]=potentialValue;
        }//for(int j=0....
        //cerr<<"\n";

        ind++;
        //measIndex mind = UpdateMeasure(vvec, i, SingleMeas);
        svlClique cl;
        cl.insert(i);
        _lbGraph.addClique(cl,SingleMeas);
    } // over markers
    if(verbose)cerr << "\n\n\nCompleted Calculation of  " << ind << " " << " Singleton potentials\n";

    out << "];\n " ;
    out.close();
}




//calculate absolute value of normalized dot product of 2 vectors
double svlMarkerCorrespondenceLBModel::norm_dot(double x1, double y1, double x2, double y2,double x3, double y3, double x4, double y4,double _dist_thr_max_a,double _dist_thr_min_a,double _dist_thr_max_b,double _dist_thr_min_b)
{
    double norm_a,norm_b, result ,uveca_x,uveca_y,uvecb_x,uvecb_y;
    double veca_x,veca_y,vecb_x,vecb_y;
    double dist_a,dist_b;
//calculate unit vectors
    norm_a=pow(pow((x2-x1),2)+pow((y2-y1),2),0.5);
    norm_b=pow(pow((x4-x3),2)+pow((y4-y3),2),0.5);

    veca_x=(x2-x1);
    veca_y=(y2-y1);
    vecb_x=(x4-x3);
    vecb_y=(y4-y3);

    uveca_x=veca_x/norm_a;
    uveca_y=veca_y/norm_a;
    uvecb_x=vecb_x/norm_b;
    uvecb_y=vecb_y/norm_b;

//dotproduct of a/|a| and a
    dist_a= uveca_x*veca_x +uveca_y*veca_y;

//dotproduct of b/|b| and b
    dist_b= uvecb_x*vecb_x +uvecb_y*vecb_y;

//calculate dot prouct of unit vectors
    //if(norm_a<_dist_thr_max  && norm_b<_dist_thr_max){
    if(dist_a<_dist_thr_max_a  && dist_a>_dist_thr_min_a  && dist_b>_dist_thr_min_b  &&
            dist_b<_dist_thr_max_b)
    {
//Farshid 5/21/2006 -added power for dot product
        //result= abs(uveca_x*uvecb_x +uveca_y*uvecb_y);
        result= pow(fabs(uveca_x*uvecb_x +uveca_y*uvecb_y),10);
    }
    else
    {
        result=0;
    }

    return result;

}

//calculate Euclidean distance between 2 points
double svlMarkerCorrespondenceLBModel::dist_l2(double x1, double y1, double x2, double y2)
{

    double result;

    result=pow(pow((x2-x1),2)+pow((y2-y1),2),0.5);
    return result;

}

//calculate L1 distance between 2 points
double svlMarkerCorrespondenceLBModel::dist_l1(double x1, double y1, double x2, double y2)
{

    double result;

    result=fabs(x2-x1)+fabs(y2-y1);
    return result;

}

/***********************
 * PAIRWISE POTENTIALS *
 ***********************/

void svlMarkerCorrespondenceLBModel::BuildPairwiseCliques()
{
    int Alice, Bob,AliceClique,BobClique,cind=1;
    int nmarkers = _MarkerLocations.dim2();
//int ncandidates = _MarkerCandidates.dim2();
//int num_pairs =ncandidates*(ncandidates-1)/2;  // this is (n choose 2)
    int p_clique_cnt=0;
    double *min_range_per_mrkr; //minimum range around marker which must
//be included if min_pw_cliques are to exist

//intermediate variables for calculation of min_range_per_mrkr
    int *min_dist_idx;
    double *min_dist;
    int    *flag;
//    double min;
  //  int min_idx;
    list<double> dist_list;

    min_dist_idx = new int[_min_pw_cliques];
    min_dist=new double[_min_pw_cliques];
    min_range_per_mrkr=new double[nmarkers];
    flag=new int[nmarkers];

    for(int k=0; k<_min_pw_cliques; k++)
    {
        min_dist[k]=10000.;
    }

//maintain stats for marker distances
    _PairDistances=gglVectorVector(nmarkers,gglVector(nmarkers));
//for each marker
    for(int i=0; i<nmarkers; i++)
    {
//initialize min range for marker i
        min_range_per_mrkr[i]=1000000;
        //for each other marker
        if(!dist_list.empty())cerr<<"BIG FAT ERROR! DIST LIST NOT EMPTY!\n";
        for(int j=i; j<nmarkers; j++)
        {
            //calculate distance between marker i and marker j, push onto list
            _PairDistances[i][j]=dist_l2(_MarkerLocations[0][i],_MarkerLocations[1][i],_MarkerLocations[0][j],_MarkerLocations[1][j]);
            if(i!=j)dist_list.push_back(_PairDistances[i][j]);
            //if(i==1)cerr << "DIST FROM MARKER "<<i<<  " to MARKER "<<j<<" = "<< _PairDistances[i][j]<< "\n";
        }
        for(int j=0; j<i; j++)
        {
            _PairDistances[i][j]=_PairDistances[j][i];
            if(i!=j)dist_list.push_back(_PairDistances[i][j]);
            //if(i==1)cerr << "DIST FROM MARKER "<<i<<  " to MARKER "<<j<<" = "<< _PairDistances[i][j]<< "\n";
        }
        //sort list of distances
        dist_list.sort();
//pop first min_pw_cliques-1 entries
        for(int k=0; k<_min_pw_cliques; k++)
        {
            dist_list.pop_front();
        }
//get min_pw_cliques'th lowest distance
        list<double>::iterator p = dist_list.begin();
        min_range_per_mrkr[i]=*p;
        //cerr << "MIN RANGE FOR MARKER "<<i<<  "= "<< min_range_per_mrkr[i]<< "\n";
        //clear list for next marker
        dist_list.clear();
    }

//update minimum distance array for marker i

//sort bottom few distances per marker
    /*
      for(int i=0;i<nmarkers;i++){
        for(int j=0;j<nmarkers;j++){
          flag[j]=0;
        }
        for(int k=1;k<_min_pw_cliques;k++){
          min=10000.;
          for(int j=0;j<nmarkers;j++){
            if((_PairDistances[i][j]<min)&&(flag[j]==0)){
              min=_PairDistances[i][j];
              min_idx=j;
            }//if
          }//for j=0
          min_dist[k]=min;
          min_dist_idx[k]=min_idx;
          flag[min_idx]=1;
        }//for k=0
        min_range_per_mrkr[i]=min_dist[_min_pw_cliques-1];
         cerr << "MIN RANGE FOR MARKER "<<i<<  "= "<< min_range_per_mrkr[i]<< "\n";
      }//for i=0
    */
    // FARSHID
    // For each Marker Pair

    _PairToClique=gglIntVectorVector(nmarkers,gglIntVector(nmarkers));

    for(int i=0; i<nmarkers; i++)
    {
        for(int j=i; j<nmarkers; j++)
        {
            double mrkr_pair_dist=_PairDistances[i][j];
            //dist_l2(_MarkerLocations[0][i],_MarkerLocations[1][i],_MarkerLocations[0][j],_MarkerLocations[1][j]);
//Can add conditions on whether or not to add the pairwise clique
            Alice=i;
            Bob=j;	//just for fun
//check for markers being closer than the max, or close enough to guarantee
//a minimum number of cliques
            if((j!=i)&((mrkr_pair_dist<max_mrkr_pair_dist)|(mrkr_pair_dist<=min_range_per_mrkr[i])))
            {
                AliceClique=i;
                BobClique=j;	//just for fun

                varsVec vvec;
                vvec.push_back(Alice);
                vvec.push_back(Bob);
                //cind = _lbGraph->addClique(vvec);
                _PairToClique[Alice][Bob] = cind;
                //_lbGraph->addCliqueNeighbor(cind,AliceClique);
                //_lbGraph->addCliqueNeighbor(cind,BobClique);
                //cerr << "Clique " << cind << " created for " << i << "," << j << endl;
                //cerr << "(marker pair dist= " << mrkr_pair_dist << " )\n";
                p_clique_cnt++;

            }
            else
            {
                _PairToClique[Alice][Bob] = -1;
            }
        }
    }
    //if(verbose)cerr << "Created " << p_clique_cnt << " PAIRWISE cliques\n";
    cerr << "Created " << p_clique_cnt << " PAIRWISE cliques\n";


}//end BuildPairwiseCliques



void svlMarkerCorrespondenceLBModel::BuildPairwisePotentials()
{

    //---------------------------------------
    // create pairwise potentials of distance
    // --------------------------------------
    int ind = 0;

//various normalized dot products:
//norm_i1j1_i2j2 is the dot product of the two vectors in this pair
//norm_i_j_k_l_1 is the dot product of the vector k->l with the first
//vector in this pair
//norm_i_j_k_l_2 is the dot product of the vector k->l with the second
//vector in this pair
//double norm_i_j_k_l_2;
    double norm_i1j1_i2j2=0.0, norm_i_j_k_l_1=0.0,norm_i_j_k_l=0.0;
//double ****norm_prods;
    double ***norm_prods_i1=NULL;

//for precalculating distances between markers and candidates (pair_po_choice=8)
    double **exp_mrkr_cand_dists=NULL;


//DEBUG potentials
    double **marg_sing_pots=NULL;

//double **cand_pair_dists; //array to hold precalculated pairwise distances between candidates
    double cand_pair_dist; //pairwise distance between candidates
    double mrkr_pair_dist;//distance between a pair of markers

    double mrkr_cand_dist_1;
//    double mrkr_cand_dist_2;
    double x1;
    double x2;
    double y1;
    double y2;
    double dist_vec_norm;
    double mrkr_dist_i1_i2;
    double rolloff_fac;
    double dist_diff ;
    double potentialValue;
//    double cand_dist1,cand_dist2;

//    int flag=0;
    int num_cand_pairs=0;
//gglPWPotsList plist; //potential list
//PairwiseAssignment PWValue;



//for each pair of markers:
//first marker....
    int nmarkers = _MarkerLocations.dim2();
    int ncandidates = _MarkerCandidates.dim2();
    int num_entries=0;


//_pair_po_choice
//1-use dot product of pairs of matches and that of all possible remaining matches
//2-use dot product of pairs only
//3-use dot product of pairs only and distance based constraint
//4-use only distance based constraint
//5-uniform
//6-use dot product of pairs only and new distance based constraint
//7-use only new distance based constraint
//8- 7+exponential rolloff

    if(_pair_po_choice==1) //when using all dot products, precalculate them
    {
//precalculate norm products
        norm_prods_i1=new double**[ncandidates];
        for ( int i=0 ; i < ncandidates ; i++ )
        {
            norm_prods_i1[i]=new double*[nmarkers];
            for ( int j=0 ; j < nmarkers ; j++ )
            {
                norm_prods_i1[i][j]=new double[ncandidates];
                //for ( int k=0 ; k < nmarkers ; k++ ) {
                //norm_prods[i][j][k]=new double[ncandidates];
                //}
            }
        }
    }

    if(_pair_po_choice==8) //when using rolloffs, precalculate them
    {
//precalculate exp(-dist^2)
        exp_mrkr_cand_dists=new double*[nmarkers];
        for ( int i=0 ; i < nmarkers ; i++ )
        {
            exp_mrkr_cand_dists[i]=new double[ncandidates];
        }
    }


//DEBUG potentials

    if(debug_initial_beliefs)
    {
        marg_sing_pots =new double*[nmarkers];
        for ( int i=0 ; i < nmarkers ; i++ )
        {
            marg_sing_pots [i]=new double[ncandidates];
        }
    }


//precalculate exp(-dist^2) if using pair_po_choice=8

    if(_pair_po_choice==8)
    {
        for ( int i=0 ; i < nmarkers ; i++ )
        {
            for ( int j=0 ; j < ncandidates ; j++ )
            {
                //evaluate distance of marker i and candidate j

                mrkr_cand_dist_1=dist_l2(_MarkerLocations2[0][i],_MarkerLocations2[1][i],_MarkerCandidates[0][j],_MarkerCandidates[1][j]);
                exp_mrkr_cand_dists[i][j]= exp(-pow(mrkr_cand_dist_1/_dist_thr_max[i],2));
            }
        }
    }



//DEBUG potentials-initialize marginals
    if(debug_initial_beliefs)
    {
        for ( int i1=0 ; i1 < nmarkers ; i1++ )
            for(int j1=0; j1<ncandidates; j1++)marg_sing_pots[i1][j1]=0;
    }

//For every marker...
    for ( int i1=0 ; i1 < nmarkers ; i1++ )
    {
        //-----------------------------------------------
        // create pairwise potentials
        //-----------------------------------------------

//precalculate normalized dot products

        //when using all dot products, precalculate them
        if(_pair_po_choice==1)
        {
            for ( int i2=0 ; i2 < nmarkers ; i2++ )
            {
                for ( int k2=0 ; k2 < ncandidates ; k2++ )
                {
                    for ( int l2=0 ; l2 < ncandidates ; l2++ )
                    {
                        norm_prods_i1[k2][i2][l2] =norm_dot(_MarkerLocations[0][i1],_MarkerLocations[1][i1],_MarkerCandidates[0][k2],_MarkerCandidates[1][k2],_MarkerLocations[0][i2],_MarkerLocations[1][i2],_MarkerCandidates[0][l2],_MarkerCandidates[1][l2],_dist_thr_max[i1],_dist_thr_min[i1],_dist_thr_max[i2],_dist_thr_min[i2]);
                    }
                }

            }
        }


//....and for each other marker...
        for ( int i2=i1+1 ; i2 < nmarkers ; i2++ )
        {

            if(_PairToClique[i1][i2]!=-1)   //If clique exists
            {
                // Create Potential
                varsVec vvec;
                cardVec cvec;
                vvec.push_back(0);
                vvec.push_back(1);
                cvec.push_back(_lbCards[i1]);
                cvec.push_back(_lbCards[i2]);
                //cerr <<"CARD FOR I1= "<<_lbCards->getCardForVar(i1)<<"\n";
                //cerr <<"CARD FOR I2= "<<_lbCards->getCardForVar(i2)<<"\n";
                //cerr << "DOOKY \n";

                //lbWeightedTableMeasure_Sptr PairwiseMeas(new lbWeightedTableMeasure(cvec));
                //PairwiseMeas->setWeight(1.0);
                svlFactor PairwiseMeas;
                PairwiseMeas.addVariable(i1,_lbCards[i1]);
                PairwiseMeas.addVariable(i2,_lbCards[i2]);
                svlClique cl;
                cl.insert(i1);
                cl.insert(i2);

                //cerr << "Marker "<<i1<<" and Marker"<<i2<<" have sizes: "<<cvec[0]<<",  "<<cvec[1]<<" \n";
                //calculate number of entries
                num_entries=cvec[0]*cvec[1];


                if(debug_pair_pots && i1==pair_pots_m1 && i2==pair_pots_m2)cout << "PAIRWISE POT for MARKERS "  << i1 << " and  "<<i2<<"\n";
                //... And For each pair of candidates,
                //for( int j1 = 0; j1 < ncandidates  ; j1++)
                //for( int j2 = 0; j2 < ncandidates  ; j2++)
                //for( int j1 = 0; j1 <= ncandidates  ; j1++)  //Garbage Can
                for( int jj1 = 1; jj1 <= cvec[0]  ; jj1++)  //Garbage Can
                {
                    int j1=_allowVals[i1][jj1];
                    //cand_dist1=dist_l1(_MarkerLocations[0][i1],_MarkerLocations[1][i1],_MarkerLocations[0][j1],_MarkerLocations[1][j1]);
                    //for( int j2 = 0; j2 <= ncandidates  ; j2++)
                    for( int jj2 = 1; jj2 <= cvec[1]  ; jj2++)
                    {
                        int j2=_allowVals[i2][jj2];
                        //	cand_dist2=dist_l1(_MarkerLocations[0][i2],_MarkerLocations[1][i2],_MarkerLocations[0][j2],_MarkerLocations[1][j2]);


                        //lbSmallAssignment assign(vvec);
                        //assign.setValueForVar(0,jj1-1);
                        //assign.setValueForVar(1,jj2-1);



                        //FIRST:Check if Potential(s) is (are) NOT LOCKED DOWN:

                        if(((_lock_pot[i1]==1)&&(_lock_pot[i2]==0)&&(_locked_pot_val[i1]==j1)) ||
                                ((_lock_pot[i1]==0)&&(_lock_pot[i2]==1)&&(_locked_pot_val[i2]==j2)) ||
                                ((_lock_pot[i1]==0)&&(_lock_pot[i2]==0)))
                        {




                            //calculate pairwise potential:

                            potentialValue=1 ;// VALUE OF ASSIGJING MARKER i1 TO CANDIDATE j1
                            if(j1<ncandidates && j2<ncandidates)  //real entries
                            {
                                if((_pair_po_choice==2)||(_pair_po_choice==3)||(_pair_po_choice==6)) //one dot product only
                                {
                                    //norm_i1j1_i2j2 =norm_prods[i1][j1][i2][j2];
                                    norm_i1j1_i2j2 = norm_dot(_MarkerLocations[0][i1],_MarkerLocations[1][i1],_MarkerCandidates[0][j1],_MarkerCandidates[1][j1],_MarkerLocations[0][i2],_MarkerLocations[1][i2],_MarkerCandidates[0][j2],_MarkerCandidates[1][j2],_dist_thr_max[i1],_dist_thr_min[i1],_dist_thr_max[i2],_dist_thr_min[i2]);
                                }
                                //Initialize potential...
                                if(_pair_po_choice==1) //using sum of products, initialize sum to 0
                                {
                                    potentialValue =0;
                                }
                                else if(_pair_po_choice==2)//one dot product only
                                    potentialValue = norm_i1j1_i2j2 ;
                                // and MARKER i2 TO CANDIDATE j2

                                if(j2!=j1)  //By definition, i1!=i2 because pair ordering
                                {
                                    //doesn't matter for the markers
                                    //but it does for the candidates
                                    //e.g. 1->2 and 2->3 is not the same as
                                    //1->3 and 2->2

//using original distance constraint

                                    if( (_pair_po_choice==3) | (_pair_po_choice==4) )  //use distance constraint
                                    {
                                        //evaluate pairwise distance of markers i1 and i2
                                        mrkr_pair_dist=dist_l2(_MarkerLocations[0][i1],_MarkerLocations[1][i1],_MarkerLocations[0][i2],_MarkerLocations[1][i2]);
                                        //evaluate pairwise distance of candidates j1 and j2
                                        cand_pair_dist=dist_l2(_MarkerCandidates[0][j1],_MarkerCandidates[1][j1],_MarkerCandidates[0][j2],_MarkerCandidates[1][j2]);
                                        dist_diff = (fabs(mrkr_pair_dist-cand_pair_dist )+1e-9);
                                        if(dist_diff<=dist_diff_intcpt)
                                        {
                                            potentialValue = 1.-dist_diff/dist_diff_intcpt;
                                        }
                                        else
                                        {
                                            potentialValue = 1e-10;
                                        }

                                    }

//using new distance constraint

                                    if( (_pair_po_choice==6) | (_pair_po_choice==7) | (_pair_po_choice==8))  //use new distance constraint
                                    {
                                        //evaluate norm of of vector (A->B) - (1->2) of markers i1 and i2
                                        x1=_MarkerLocations2[0][i1]-_MarkerCandidates[0][j1];
                                        x2=_MarkerLocations2[0][i2]-_MarkerCandidates[0][j2];
                                        y1=_MarkerLocations2[1][i1]-_MarkerCandidates[1][j1];
                                        y2=_MarkerLocations2[1][i2]-_MarkerCandidates[1][j2];

                                        dist_vec_norm=dist_l2(x1,y1,x2,y2);
                                        mrkr_dist_i1_i2=dist_l2(_MarkerLocations2[0][i1],_MarkerLocations2[1][i1],_MarkerLocations2[0][i2],_MarkerLocations2[1][i2]);
                                        rolloff_fac=dist_diff_intcpt;
                                        //rolloff_fac=mrkr_dist_i1_i2/dist_diff_intcpt;
                                        potentialValue = exp(-pow(dist_vec_norm/rolloff_fac,2));
                                        //potentialValue = exp(-pow(dist_vec_norm/dist_diff_intcpt,2));
                                        if(_pair_po_choice==8)   //multiply by exponential rolloffs...
                                        {

                                            //evaluate distance of marker i1 and candidate j1
                                            /*
                                                                       mrkr_cand_dist_1=dist_l2(_MarkerLocations2[0][i1],_MarkerLocations2[1][i1],_MarkerCandidates[0][j1],_MarkerCandidates[1][j1]);
                                                                       //evaluate distance of marker i2 and candidate j2
                                                                       mrkr_cand_dist_2=dist_l2(_MarkerLocations2[0][i2],_MarkerLocations2[1][i2],_MarkerCandidates[0][j2],_MarkerCandidates[1][j2]);
                                                                       potentialValue = potentialValue* exp(-pow(mrkr_cand_dist_1/_dist_thr_max[i1],2));
                                                                       potentialValue = potentialValue* exp(-pow(mrkr_cand_dist_2/_dist_thr_max[i2],2));
                                            */
                                            if(j1>=ncandidates)cerr<<"ERROR:J1= "<<j1<<"\n";
                                            if(j2>=ncandidates)cerr<<"ERROR:J2= "<<j2<<"\n";
                                            potentialValue=potentialValue*exp_mrkr_cand_dists[i1][j1]*exp_mrkr_cand_dists[i2][j2];
                                        }

                                        //if((mrkr_cand_dist_1<=_dist_thr_max[i1]) &&(mrkr_cand_dist_2<=_dist_thr_max[i2]) ){
                                        //}
                                        //else {
                                        //potentialValue = 1e-10;
                                        //}

                                    }



                                    //  FARSHID 0428- simplifying pairwise potentials
                                    if(_pair_po_choice==1)
                                    {
                                        //norm_i1j1_i2j2*  sum_over_k (max_l(dot(u(i1->j1),u(k->l)))) i1,i2!=k,j1,j2!=l
                                        //first, calculate norm dot product of i1->j1, i2->j2
                                        //For each possible remaining marker candidate pair:
                                        //For each remaining marker...
                                        for( int k = 0; k <nmarkers ; k++)
                                        {
                                            if(k!=i1 && k!=i2)
                                            {
                                                double max_dot_i_j_k_l=0.0;
                                                //.... And For each remaining candidate...
                                                for( int l = 0; l < ncandidates ; l++)
                                                {
                                                    if(l!=j1 && l!=j2)
                                                    {
                                                        //calculate dot products, take max
                                                        //norm_i_j_k_l_1=norm_prods[i1][j1][k][l];
                                                        norm_i_j_k_l_1=norm_prods_i1[j1][k][l];
                                                        //norm_i_j_k_l_2=norm_prods[i2][j2][k][l];
                                                        //norm_i_j_k_l = norm_i1j1_i2j2* (norm_i_j_k_l_1+norm_i_j_k_l_2)/2;
                                                        norm_i_j_k_l = norm_i1j1_i2j2* norm_i_j_k_l_1;
                                                        if(norm_i_j_k_l>max_dot_i_j_k_l)  max_dot_i_j_k_l = norm_i_j_k_l;
                                                    }//end if(l!=j1 && l!=j2)
                                                }//for(int l=0....

                                                //update potential accumulation for current marker-candidate pair
                                                potentialValue+=max_dot_i_j_k_l;
                                            }//end if(k!=i1&&k!=i2)
                                            else    // k==i1 || k == i2
                                            {
                                                potentialValue = 1e-10;
                                            }

                                        }//for(int k=0....
                                    }//end if(_pair_po_choice==1)

                                }//if(j1!=j2)
                                else    // j1 == j2
                                {
                                    potentialValue = 1e-10;
                                }

//Multiply final result by dot product of <i1,j1> and <i2,j2>

                                //potentialValue *= norm_prods[i1][j1][i2][j2];
                                if((_pair_po_choice==3)||(_pair_po_choice==6))
                                    potentialValue *= norm_i1j1_i2j2 ;

                                if(_pair_po_choice==5) //uniform
                                {
                                    potentialValue = 1 ;
                                }

                            }//if(j1<ncandidates && j2<ncandidates)

                            else    //Garbage Can
                            {
                                potentialValue = gcan_potential ;
                            }

                            //cout << " "  << potentialValue << "\n";
                            if(debug_pair_pots && i1==pair_pots_m1 && i2==pair_pots_m2)cout << " "  << potentialValue << ", ";

                            //put lower limit on potentials
                            if(potentialValue <1e-10)
                            {
                                potentialValue =1e-10;
                            }
//Added scale factor for pairwise potentials (nominally 1)
                            potentialValue *=_ppot_scale_factor;
                            //cerr << "DOOKY "<<i1<<" "<<jj1<<", "<<i2<<" "<<jj2<<" "<<potentialValue<<"\n";
                            //PairwiseMeas->setFeatureValueOfFull(assign,vvec, potentialValue);
                            PairwiseMeas[jj1-1+cvec[0]*(jj2-1)]=potentialValue;


//DEBUG potentials
                            if(debug_initial_beliefs)
                            {
                                marg_sing_pots[i1][j1]+=potentialValue;
                                marg_sing_pots[i2][j2]+=potentialValue;
                            }

                        }	//if pots NOT LOCKED DOWN

                        //OTHERWISE LOCK values down
                        else
                        {

                            if((_lock_pot[i1]==1)&&(_locked_pot_val[i1]==j1)&&
                                    (_lock_pot[i2]==1)&&(_locked_pot_val[i2]==j2))
                            {
                                potentialValue =1;
                            }
                            else
                            {
                                potentialValue =1e-10;

                            }
                            potentialValue *=_ppot_scale_factor;
                            //PairwiseMeas->setFeatureValueOfFull(assign,vvec, potentialValue);
                            PairwiseMeas[jj1-1+cvec[0]*(jj2-1)]=potentialValue;
                        } //else LOCK VALUES DOWN
                        //potentialValue *=_ppot_scale_factor;

                        //put lower limit on potentials
                        if(potentialValue <1e-10)
                        {
                            potentialValue =1e-10;
                        }
//UPDATE potentials
                        /*
                                PWValue.valA=j1;
                                PWValue.valB=j2;
                                PWValue.pot_value=potentialValue;
                                plist.push_back(PWValue);
                        */

//update number of candidate pairs recorded
                        num_cand_pairs +=1;


                        //PairwiseMeas->setFeatureValueOfFull(assign,vvec, potentialValue);


                    }//for(int j2=0....
                    if(debug_pair_pots && i1==pair_pots_m1 && i2==pair_pots_m2)cout << ";\n";
                    //cout << ";\n";

                }//for(int j1=0....

                if(num_cand_pairs==0)
                {
                    cerr << "WARNING: mrkr "<<i1<<" - mrkr "<<i2<<": NO CANDIDATES\n";
                }
//sort list
                /*
                	 plist.sort();

                //take top _PWTableSz entries (pruned table)
                        for(int k=0;k<_PWTableSz  && k<num_cand_pairs ;k++){
                           PWValue= plist.back();
                           plist.pop_back();
                           int j1=PWValue.valA;
                           int j2=PWValue.valB;
                	   potentialValue= PWValue.pot_value;
                           lbSmallAssignment assign(vvec);
                           assign.setValueForVar(0,j1);
                           assign.setValueForVar(1,j2);
                           PairwiseMeas->setFeatureValueOfFull(assign,vvec, potentialValue);
                //DEBUG
                           if(!flag && k!=0){
                             cerr << "mrkr "<<i1<<" - mrkr "<<i2<<": "<< k<<"th potential value = "<<potentialValue<<", j1= "<<j1<<", j2= "<<j2<<"\n";
                           }
                           if(k==0){
                             cerr << "mrkr "<<i1<<" - mrkr "<<i2<<": highest potential value = "<<potentialValue<<", j1= "<<j1<<", j2= "<<j2<<"\n";
                             cerr << "	num_cand_pairs= "<< num_cand_pairs<<"\n";
                           }
                        }//for(int k =0; k<_PWTableSz...


                         flag=1;
                //clear list for use next round
                	 plist.clear();
                         num_cand_pairs =0;
                */

                //measIndex mind = UpdateMeasure(vvec,_PairToClique[i1][i2],PairwiseMeas);
                _lbGraph.addClique(cl,PairwiseMeas);
                ind++;
            }//if(_PairToClique[i1][i2]!=-1)  //If clique exists
        } // for(int i2=0...    over markers
        if(verbose)cerr << num_entries<<" Pairwise Potentials for marker  " << i1  << " ...\n";
    } // for(int i1=0...    over markers

//DEBUG potentials -output the marginalized pairwise potentials
    if(debug_initial_beliefs)
    {
        cout << "MARGINALIZED PAIRWISE POTENTIALS " ;
        for(int i1=0; i1<nmarkers; i1++)
        {
            for(int j1=0; j1<nmarkers; j1++)
            {
                cout << marg_sing_pots[i1][j1] << " " ;
            }
            cout << ";\n " ;
        }
    }

    if(_pair_po_choice==1) //when using all dot products, delete memory for precalculation (done now)
    {
        delete(norm_prods_i1);
    }
    if(_pair_po_choice==8) //when using exponential rolloff, delete memory for precalculation (done now)
    {
        delete(exp_mrkr_cand_dists);
    }

    if(verbose)cerr << "Completed Calculation of  " << ind << " " << " Pairwise potentials\n";
}

