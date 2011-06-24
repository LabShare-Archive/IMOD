/*
 * mainCorrespondMainTest.cpp - main program to run MRF inference to correspond markers between frames
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */


/*Fernando Amat Gil April 23rd 2010
Modification of Farshid's main for markers correspondence
*/

#include "svlMarkerCorrespondenceLBModel.h"
#include <iostream>
#include <fstream>
#include <string.h>


/** Farshid & Fernando Program **/

const string usage = "\n\
Usage: markersCorrespond <PARAMS>\n\
\n";

gglMatrix gglFarshidReadMatrix(int dim1, int dim2, istream & in,int verbose=0);
void PrintMatrix(gglMatrix const& mx, ostream &out, bool delimiters = true);

int main(int argc, char *argv[])
{
    // INPUTS:
    //  - M = Image1 marker coordinates
    //  - K = Image2 marker candidate coordinates
    //  - SP = Initial Singleton Beliefs for marker i to candidate j



    // OUTPUTS:
    //  - B = M X K matrix of marginal beliefs
    int debug_init_beliefs=0;
    int debug_pair_pots=0;
    int pair_pots_m1=1;
    int pair_pots_m2=1;
    double dist_diff_intcpt=25;
    int  verbose=0;
    int  mtime=1;
    int  lock_pots_file_exists=0;
//string usage = "Usage: markersCorrespond <config file> <markers_file> <cand_file> <singleton_potential file> <C matrix output file>\n";
string usage = "Usage: markersCorrespond <test name> [-debug_init_beliefs pair_pots_m1  pair_pots_m2] \n NOTE: files <test name>.cfg, <test name>.lock_pots, <test name>.mrkr, <test name>.mrkr_trans, <test name>.cand, and <test name>.sp are required to exist\n";


    int min_args=1;
    int max_args=5;
    if((argc <= min_args)||(argc>max_args))
    {
        cerr << usage;
        exit(0);
    }
    else
    if(verbose){
      cout<<"Num args = "<<argc<<"\n";
    }
/*
    string ConfigFilename(argv[1]); argv[1][0] = '\0';
    string MarkerFilename(argv[2]); argv[2][0] = '\0';
    string CandidateFilename(argv[3]); argv[3][0] = '\0';
    string SPMatrixFilename(argv[4]); argv[4][0] = '\0';
    string CMatrixFilename(argv[5]); argv[5][0] = '\0';
*/
    string TestName(argv[min_args]); argv[min_args][0] = '\0';
    if(argc >(min_args+1)){
      if(!strcmp("-debug_pair_pots", argv[min_args+1])) {
         debug_pair_pots=1;
         pair_pots_m1=atoi(argv[min_args+2]);
         pair_pots_m2=atoi(argv[min_args+3]);
        }
      }
    //if(argc >6)if(!strcmp("-debug_init_beliefs", argv[6])) debug_init_beliefs=1;

    string ConfigFilename=TestName+".cfg";
    string LockPotsFilename=TestName+".lock_pots";
    string MarkerFilename=TestName+".mrkr";
    string MarkerFilename2=TestName+".mrkr_trans";
    string CandidateFilename=TestName+".cand";
    string SPMatrixFilename=TestName+".sp";
    string CMatrixFilename=TestName+"_final_beliefs_scr.m";

    gglMatrix C;
    gglMatrix InitBeliefs;
    int rows=2;
    int cols=5; //this is ignored by ReadMatrix
//    int num_markers;
    //double dist_thr,dist_var_thr;

    ifstream infile1(MarkerFilename.c_str());
    if(!infile1){
      cerr<<"Marker File: "<<MarkerFilename.c_str()<<" could NOT be opened..exiting now...\n";
      exit(0);
    }
    //gglMatrix M = ReadMatrix(rows, cols, infile1);
    gglMatrix M = gglFarshidReadMatrix(rows, cols, infile1);
    if(verbose) cout<< "Read in M"<<endl;
    infile1.close();

    ifstream infile5(MarkerFilename2.c_str());
    if(!infile5){
      cerr<<"Marker File: "<<MarkerFilename2.c_str()<<" could NOT be opened..exiting now...\n";
      exit(0);
    }
    //gglMatrix M_trans = ReadMatrix(rows, cols, infile5);
    gglMatrix M_trans = gglFarshidReadMatrix(rows, cols, infile5);
    if(verbose) cout<< "Read in M (translated)"<<endl;
    infile5.close();

    ifstream infile2(CandidateFilename.c_str());
    if(!infile2){
      cerr<<"Candidate File: "<<CandidateFilename.c_str()<<" could NOT be opened..exiting now...\n";
      exit(0);
    }
    //gglMatrix K = ReadMatrix(rows, cols, infile2);
    gglMatrix K = gglFarshidReadMatrix(rows, cols, infile2);
    if(verbose) cout<< "Read in K"<<endl;
    infile2.close();


    rows=M.dim2();
    if(verbose) cout<< "Read in num rows "<<rows<<endl;
    ifstream infile3(SPMatrixFilename.c_str());
    if(!infile3){
      cerr<<"SP Matrix File: "<<SPMatrixFilename.c_str()<<" could NOT be opened..exiting now...\n";
      exit(0);
    }
    //gglMatrix SP = ReadMatrix(rows, cols, infile3);
    gglMatrix SP = gglFarshidReadMatrix(rows, cols, infile3);
    if(verbose) cout<< "Read in SP, num rows is "<<rows<<endl;
    infile3.close();

    ifstream infile4(ConfigFilename.c_str());
    if(!infile4){
      cerr<<"Config File: "<<ConfigFilename.c_str()<<" could NOT be opened..exiting now...\n";
      exit(0);
    }

    ifstream infile6(LockPotsFilename.c_str());
    if(infile6){
      if(verbose)cerr<<"LockPots File: "<<LockPotsFilename.c_str()<<" was found...\n";
      lock_pots_file_exists=1;
    }

    // Create Inference Object

    svlMarkerCorrespondenceLBModel Model(M, M_trans, K,SP,  TestName, lock_pots_file_exists, infile4,infile6, debug_pair_pots,pair_pots_m1,pair_pots_m2,debug_init_beliefs, dist_diff_intcpt, verbose, mtime);
    infile4.close();


    // Run Inference
    C = Model.GetFinalMarginalBeliefs();
    //C = Model.GetFinalBeliefs();

    //InitBeliefs= Model.GetInitialBeliefs();
    //int dummy= Model.GetDummy();
    // Write it
    if(verbose){
      cout<< "MATRIX M"<<endl;
      //PrintMatrix(M);
      cout<< endl;

      cout<< "MATRIX K"<<endl;
      //PrintMatrix(K);
      cout<< endl;
    }

    //cout<< "INIT BELIEFS "<<endl;
    //PrintMatrix(InitBeliefs);

    //cout<< "MATRIX C"<<endl;
    //PrintMatrix(C);


    string CMatrixName;
    CMatrixName=TestName+"_final_beliefs";
    ofstream c_outfile(CMatrixFilename.c_str());
    c_outfile<<CMatrixName<<"=["<<endl;
    PrintMatrix(C,c_outfile);
    c_outfile<<"];"<<endl;
    c_outfile.close();


	return 0;
}


gglMatrix gglFarshidReadMatrix(int dim1, int dim2, istream & in,int verbose)
{
    int nmarkers;
    in >> nmarkers ;
    if(verbose) cerr << "You have just read in nmarkers = "<<nmarkers<<"\n";
    gglMatrix M(dim1,nmarkers);
    for(int i = 0; i < dim1; i++)
        for(int j = 0; j < nmarkers; j++)
            in >> M[i][j];

    if(verbose)
    {
        cerr << "You have just read in the matrix:\n";
        PrintMatrix(M,cerr);
    }

    return M;
}

void PrintMatrix(gglMatrix const& mx, ostream &out, bool delimiters)
{
    for(int i = 0; i < mx.dim1(); i++)
    {
	for(int j = 0; j < mx.dim2(); j++)
	{
	    if(j>0) {
		if(delimiters)
		    out << ",";
		out << " ";
	    }
	    out << mx[i][j];
	}

	if(delimiters) out << ";";
	out << endl;
    }
}
