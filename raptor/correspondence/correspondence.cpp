/*
 * correspondence.cpp - correspondence between pairs of images
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */

#include "correspondence.h"
#include <math.h>
#include <string>
#include "../template/template.h"
#include "../mainClasses/constants.h"
#include <fstream>
#include <sstream>
#include <cstdlib>
#include "../opencv/cxcore.h"
#include "../opencv/cv.h"
#include <stdio.h>

using namespace std;

bool correspondenceRegions(frame* frame1, frame* frame2, vector<pairCorrespondence>* correspondences, int Ntemplate, unsigned int M, unsigned int K, int maxNCliques, int dmax, int Q1, int W1, int maxPWTable, int minPWTable, string idxStr, int diameter, ioMRC* vol, vector<pairCorrespondence>* prevPair,string outputDir,string binPATH)
{
    //float* ans; //unused variable
    double diffThreshold = 1.0;
    // threshold used for maker detection
    // the higher this value, the less markers we get
    //float peakThreshold = 0.3; //unused variable
    // minimum number of pairwise cliques per marker
    unsigned int minCliques = 8;

    unsigned int frame1_size = frame1->p.size() < M ? frame1->p.size() : M;
    unsigned int frame2_size = frame2->p.size() < K ? frame2->p.size() : K;

    K=frame2_size;//to avoid segmentation fault

    // write files for Farshid's software
	#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__)
		string cfg_filename = outputDir + "temp\\" + idxStr + "FFcorrespondence.cfg";
	#else
		string cfg_filename = outputDir + "temp/" + idxStr + "FFcorrespondence.cfg";
	#endif
    ofstream out;
    out.open(cfg_filename.c_str());
    if(!out.is_open())
    {
        cout<<"ERROR:file "<<cfg_filename<<" could not be opened at correspondenceRegions(...)"<<endl;
        exit(-1);
    }
    out << M << endl;
    out << " 3" << endl;
    out << " 8" << endl;
    out << " 7" << endl;
    int nCliques = maxNCliques + 10;
    vector<Point2D*> referencePeaks;
    if (prevPair == NULL)
    {
        for (unsigned int i = 0; i < min(frame1_size,M); i++)
        {
            referencePeaks.push_back(frame1->p.at(i));
        }
    }
    else
    {
        unsigned int j = 0;
        for (unsigned int i = 0; i < frame1->p.size(); i++)
        {
            if (frame1->p.at(i)->used_pairCorrespondence)
            {
                referencePeaks.push_back(frame1->p.at(i));
                j++;
                if (j > M)
                    break;
            }
        }
        if (j < M)
        {
            for (unsigned int i = 0; i < frame1_size; i++)
            {
                if (!frame1->p.at(i)->used_pairCorrespondence)
                {
                    referencePeaks.push_back(frame1->p.at(i));
                    j++;
                }
                if (j > M)
                    break;
            }
        }
    }

    M=referencePeaks.size();//update final number of targets (sometimes we might not have M);

    double d = sqrt((double)(frame1->width * frame1->width) + (double)(frame1->height * frame1->height)) + 50.0;
    while (nCliques > maxNCliques)
    {
        d = d - 50;
        nCliques = 0;
        for (unsigned int i = 0; i < M-1; i++)
        {
            double x1 = referencePeaks[i]->x;
            double y1 = referencePeaks[i]->y;
            for (unsigned int j = i + 1; j < M; j++)
            {
                double x2 = referencePeaks[j]->x;
                double y2 = referencePeaks[j]->y;
                double temp = sqrt((x1-x2) * (x1-x2) + (y1-y2) * (y1-y2));
                if (temp < d)
                    nCliques++;
            }
        }
    }
    out << " " << d << endl;
    out << " " << 0.0001 << endl;
    out << " " << 5 << endl;
    if (M <= minCliques)
        out << " " << M - 1 << endl;
    else
        out << " " << minCliques << endl;
    out << " " << maxPWTable << endl;
    out << " " << minPWTable << endl;
    for (unsigned int i = 0; i < M; i++)
    {
        out << (float)-dmax << endl;
        out << (float)dmax << endl;
    }
    out.close();

	#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__)
    string mrkr_filename = outputDir + "temp\\" + idxStr + "FFcorrespondence.mrkr";
#else
	string mrkr_filename = outputDir + "temp/" + idxStr + "FFcorrespondence.mrkr";
#endif
    out.open(mrkr_filename.c_str());
    if(!out.is_open())
    {
        cout<<"ERROR:file "<<mrkr_filename<<" could not be opened at correspondenceRegions(...)"<<endl;
        exit(-1);
    }
    out << M << endl;
    for (unsigned int i = 0; i < M; i++)
        out << referencePeaks[i]->x << endl;
    for (unsigned int i = 0; i < M; i++)
        out << referencePeaks[i]->y << endl;
    out.close();

	#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__)
    string mrkr_trans_filename = outputDir + "temp\\" + idxStr + "FFcorrespondence.mrkr_trans";
#else
	string mrkr_trans_filename = outputDir + "temp/" + idxStr + "FFcorrespondence.mrkr_trans";
#endif
    out.open(mrkr_trans_filename.c_str());
    if(!out.is_open())
    {
        cout<<"ERROR:file "<<mrkr_trans_filename<<" could not be opened at correspondenceRegions(...)"<<endl;
        exit(-1);
    }
    out << M << endl;
    for (unsigned int i = 0; i < M; i++)
        out << referencePeaks[i]->x << endl;
    for (unsigned int i = 0; i < M; i++)
        out << referencePeaks[i]->y << endl;
    out.close();

	#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__)
    string cand_filename = outputDir + "temp\\" + idxStr + "FFcorrespondence.cand";
#else
	string cand_filename = outputDir + "temp/" + idxStr + "FFcorrespondence.cand";
#endif
    out.open(cand_filename.c_str());
    if(!out.is_open())
    {
        cout<<"ERROR:file "<<cand_filename<<" could not be opened at correspondenceRegions(...)"<<endl;
        exit(-1);
    }
    out << K << endl;
    for (unsigned int i = 0; i < K; i++)
        out << frame2->p.at(i)->x << endl;
    for (unsigned int i = 0; i < K; i++)
        out << frame2->p.at(i)->y << endl;
    out.close();


    float *S = new float[M * K];
    singlePotentials(S, frame1, frame2, 15, dmax, 2, 1, vol, M, K, referencePeaks);
    //cout << "Computing single potentials complete" << endl;

	#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__)
    string sp_filename = outputDir + "temp\\" + idxStr + "FFcorrespondence.sp";
#else
	string sp_filename = outputDir + "temp/" + idxStr + "FFcorrespondence.sp";
#endif
    out.open(sp_filename.c_str());
    if(!out.is_open())
    {
        cout<<"ERROR:file "<<sp_filename<<" could not be opened at correspondenceRegions(...)"<<endl;
        exit(-1);
    }
    out << K << endl;
    int posS=0;
    for (unsigned int i = 0; i < M; i++)
    {
        for(unsigned int j=0;j<K;j++)
        {
            out << S[posS] << " ";
            posS++;
        }
        out<<endl;
    }
    out << endl;
    out.close();
    /*
    int error=system((binPATH + "markersCorrespond_17 " + outputDir + "temp/" + idxStr + "FFcorrespondence > " + outputDir + "temp/" + idxStr + "_temp.txt").c_str());
    if(error!=0)
    {
        cout<<"ERROR: called to binary markersCorrespond_17 failed at projections "<<frame1->frameID<<"->"<<frame2->frameID<<endl;
        exit(-1);
    }*/
    //to avoid output from markersCorrespond_17 showing on terminal

    //using ggl version
    //FILE *pipe=popen((binPATH + "markersCorrespond_17 " + outputDir + "temp/" + idxStr + "FFcorrespondence > " + outputDir + "temp/" + idxStr + "_temp.txt 2>&1").c_str(),"r");
    //using svl version

#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__)
    FILE *pipe=_popen((binPATH + "MarkersCorrespond " + outputDir + "temp\\" + idxStr + "FFcorrespondence > " + outputDir + "temp\\" + idxStr + "_temp.txt 2>&1").c_str(),"r");
	_pclose(pipe);
#else
	FILE *pipe=popen((binPATH + "MarkersCorrespond " + outputDir + "temp/" + idxStr + "FFcorrespondence > " + outputDir + "temp/" + idxStr + "_temp.txt 2>&1").c_str(),"r");
	pclose(pipe);
#endif
    

#if  defined(_WIN32) || defined(WIN32) || defined(__WIN32__)
    string belief_filename = outputDir + "temp\\" + idxStr + "FFcorrespondence_final_beliefs_scr.m";
#else
	string belief_filename = outputDir + "temp/" + idxStr + "FFcorrespondence_final_beliefs_scr.m";
#endif
    ifstream in;
    in.open(belief_filename.c_str());

    if(!in.is_open())
    {
        cout<<"ERROR:file "<<belief_filename<<" could not be opened at correspondenceRegions(...) to read inference results"<<endl;
        exit(-1);
    }

    double *C = new double [M * K];
    string buffer;
    getline(in, buffer);
    //cout << buffer << endl;
    for (unsigned int i = 0; i < M; i++)
    {
        getline(in, buffer);
        char* token = NULL;
        char *temp = new char [buffer.length()];
        buffer.copy(temp, buffer.length());
        for (unsigned int j = 0; j < K; j++)//we don't read the garbage potential at position K+1
        {
            if (j == 0)
                token = strtok(temp, " ");
            else
                token = strtok(NULL, " ");
            istringstream ss(token);
            ss >> C[j + i * K];
        }
        delete [] temp;
    }
    in.close();
    double *max = new double [M];
    double *second_max = new double [M];
    int *max_index = new int [M];

    for (unsigned int j = 0; j < M; j++)
    {
        max[j] = -1E+37;
        second_max[j] = -1E+37;
        max_index[j] = -1;
    }
    for (unsigned int i = 0; i < M; i++)
    {
        for (unsigned int j = 0; j < K; j++)
        {
            if (C[j + i * K] > max[i])
            {
                second_max[i] = max[i];
                max[i] = C[j + i * K];
                max_index[i] = j;
            }
        }
    }
    //cout << "ok until here" << endl;
    int num = 0;
    for (unsigned int i = 0; i < M; i++)
    {
        if (referencePeaks[i]->pairwise_score == initialPairwiseScore)
            referencePeaks[i]->pairwise_score = max[i];
        if (max[i] - second_max[i] > diffThreshold && max[i]>-2.3026)//log(0.1)=-2.3026   //|| fabs(max[i] - frame1->p.at(i)->pairwise_score) < 1.0) {
        {
            //if (frame1->frameID < frame2->frameID) {
            int index=0;
            for (unsigned int t = 0; t < frame1->p.size(); t++)
            {
                if (referencePeaks[i] == frame1->p.at(t))
                    index = t;
            }
            pairCorrespondence temp(index, max_index[i], frame1, frame2, C[i * K + max_index[i]]);
            referencePeaks[i]->index0.push_back(temp);
            referencePeaks[i]->used_pairCorrespondence = true;
            frame2->p.at(max_index[i])->pairwise_score = max[i];
            frame2->p.at(max_index[i])->used_pairCorrespondence = true;
            frame2->p.at(max_index[i])->index1.push_back(temp);
            correspondences->push_back(temp);
            num++;
            /*}
            else {
            pairCorrespondence temp(max_index[i], i, frame2, frame1, C[i * K + max_index[i]]);
            frame2->p.at(max_index[i])->index1.push_back(temp);
            frame1->p.at(i)->pairwise_score = max[i];
            frame1->p.at(i)->index0.push_back(temp);
            correspondences->push_back(temp);
            }*/
        }
    }

    delete [] S;
    delete [] C;
    delete [] max;
    delete [] second_max;
    delete [] max_index;

    if(num<4 && abs(frame1->frameID-frame2->frameID)<2)
    {
        cout<<"WARNING: pairwise correspondence between projections"<<frame1->frameID<<"->"<<frame2->frameID<<" (first projection is 0) contains only "<<num<<" correspondences"<<endl;
        //cout<<"It is not safe to continue. We have too few detected markers to fit parameters"<<endl;
        //cout<<"RAPTOR is stopping computation at "<<getDate()<<endl;
        //exit(-1);
        cout<<"All the projections from now on are going to be ignored. Alignment will be produced with less projections."<<endl;
        return false;
    }
    else
        cout<<"Found "<<num<<" pairwise correspondences out of "<<M<<" targeted markers between projections"<<frame1->frameID<<"->"<<frame2->frameID<<" (first projection is 0)"<<endl;

    //clear pointers:NO because pointers are embedded with frames
    //for(unsigned int kk=0;kk<M;kk++) delete referencePeaks[kk];
    //referencePeaks.clear();
    return true;
}

void singlePotentials(float* S, frame* frame1, frame* frame2, int win, int dmax, int mode, int option, ioMRC* vol, unsigned int maxMarkersPrevFrame_, unsigned int maxMarkersNextFrame_, vector<Point2D*> referencePeaks)
{
    float threshold = 0.01;
    float* image1 = new float[vol->headerGetNx() * vol->headerGetNy()];
    vol->readMRCSlice(frame1->frameID, image1);
    float* image2 = new float[vol->headerGetNx() * vol->headerGetNy()];
    vol->readMRCSlice(frame2->frameID, image2);
    float auxMV;
    float var_frame1;
    meanAndVariance(image1, frame1->width * frame1->height,&auxMV,&var_frame1);
    float var_frame2;
    meanAndVariance(image2, frame2->width * frame2->height,&auxMV,&var_frame2);
    //cout << "variances are " << var_frame1 << " " << var_frame2 << endl;
    if (maxMarkersPrevFrame_ * maxMarkersNextFrame_ == 0)
        return;

    int S_width = maxMarkersNextFrame_;
    int S_height = maxMarkersPrevFrame_;

    int side = win * 2 + 1;
    int _2_side=side*side;
    float *patches_frame1 = new float [maxMarkersPrevFrame_ * _2_side];
    float *patches_frame2 = new float [maxMarkersNextFrame_* _2_side];
    float *vars_frame1 = new float [maxMarkersPrevFrame_];
    float *vars_frame2 = new float [maxMarkersNextFrame_];
    float *norms_frame1 = new float [maxMarkersPrevFrame_];
    float *norms_frame2 = new float [maxMarkersNextFrame_];
    float mean_patch;

    int aux = win;
    int img_x, img_y;
    for (unsigned int j = 0; j < maxMarkersPrevFrame_; j++)
    {
        //patches_frame1[j] = new float[_2_side];
        while ( (referencePeaks[j]->x - aux < 1.5) ||
                (referencePeaks[j]->x + aux > frame1->width - 1) ||
                (referencePeaks[j]->y + aux > frame1->height - 1) ||
                (referencePeaks[j]->y - aux < 1.5)
              )
            aux--;
        if (aux != win)
            for (int i = 0; i < S_width; i++)
                S[i + j * S_width] = threshold;
        else
        {
            img_x = (int)(referencePeaks[j]->x - win);
            img_y = (int)(referencePeaks[j]->y - win);
            float* img = image1;
            for (int x = 0; x < side; x++)
                for (int y = 0; y < side; y++)
                {
                    patches_frame1[j * _2_side + x + y * side] = img[img_x + x + (img_y + y) * frame1->width];
                }
            //vars_frame1[j] = variance(patches_frame1[j], _2_side);
            //float mean_patch = average(patches_frame1[j], _2_side);
            meanAndVariance(&patches_frame1[j * _2_side],_2_side,&mean_patch,&vars_frame1[j]);
            //float norm = 0;
            for (int i = 0; i < _2_side; i++)
            {
                patches_frame1[j * _2_side + i] -= mean_patch;
                //norm += patches_frame1[j][i] * patches_frame1[j][i];// calculate the norm of patches_frame1[j]
            }
            norms_frame1[j] = sqrt(vars_frame1[j]*((_2_side)-1));
        }
        aux = win;
    }
    //cout << "single potentials ok until here" << endl;
    for (unsigned int i = 0; i < maxMarkersNextFrame_; i++)
    {
        //patches_frame2[i] = new float[_2_side];
        while ( (frame2->p.at(i)->x - aux < 1.5) ||
                (frame2->p.at(i)->x + aux > frame2->width - 1) ||
                (frame2->p.at(i)->y - aux < 1.5) ||
                (frame2->p.at(i)->y + aux > frame2->height -1)
              )
            aux--;
        if (aux != win)
            for (int j = 0; j < S_height; j++)
                S[i + j * S_width] = threshold;
        else
        {
            img_x = (int)(frame2->p.at(i)->x - win);
            img_y = (int)(frame2->p.at(i)->y - win);
            float* img = image2;
            for (int x = 0; x < side; x++)
                for (int y = 0; y < side; y++)
                {
                    patches_frame2[i * _2_side + x + y * side] = img[img_x + x + (img_y + y) * frame2->width];
                }
            //vars_frame2[i] = variance(patches_frame2[i], _2_side);
            //float mean_patch = average(patches_frame2[i], _2_side);
            meanAndVariance(&patches_frame2[i * _2_side],_2_side,&mean_patch,&vars_frame2[i]);
            //float norm = 0;
            for (int j = 0; j < _2_side; j++)
            {
                patches_frame2[i * _2_side + j] -= mean_patch;
                //norm += patches_frame2[i][j] * patches_frame2[i][j];    // calculate the norm of patches_frame1[j]
            }
            norms_frame2[i] = sqrt(vars_frame2[i]*((_2_side)-1));
        }
        aux = win;
    }
    //cout << "is null? " << (patches_frame1[8] ==NULL)<<endl;
    for (int j = 0; j < S_height; j++)
    {
        if (average(&S[j * S_width], S_width) != threshold)
        {
            for (int i = 0; i < S_width; i++)
            {
                if (S[i + j * S_width] != threshold)
                {
                    float vx = referencePeaks[j]->x - frame2->p.at(i)->x;
                    float vy = referencePeaks[j]->y - frame2->p.at(i)->y;
                    double d = 0.0;
                    if (option == 1 || option == 3)
                        d = 2.0 * dmax;
                    else
                        d = dmax;
                    double v_norm = sqrt((vx * vx) + (vy * vy));
                    if (v_norm > d) // norm
                        S[i + j * S_width] = 1E-10;
                    else if (referencePeaks[j]->markerType!=frame2->p[i]->markerType)//both points have to come from the same marker
                        S[i + j * S_width] = 1E-10;
                    else
                    {
                        if (mode == 1)
                        {
                            //S[i + j * S_width]; //TODO mutual information
                            cout << "Error: trying to calculate singleton potentials using Mutual Information. Not implemented yet." << endl;
                            exit(-1);
                        }
                        else if (mode == 2)
                        {
                            float m=0.0f;
                            for (int p1 = 0; p1 < _2_side; p1++)
                                m += patches_frame1[j * _2_side + p1] * patches_frame2[i * _2_side + p1];
                            m /= norms_frame1[j] * norms_frame2[i];
                            S[i + j * S_width] = (m > 1E-10) ? m : 1E-10;
                            //cout << S[i + j * S_width] << " " << i << " "<< j << endl;
                        }
                    }
                }
            }
        }
    }
    //cout << "single potentials ok until here 2" << endl;
    // SECOND PASS TO ADD TEXTURE INFORMATION
    float a;
    if (max(S, S_width * S_height) > threshold)
        a = threshold;
    else
    {
        float sum = 0;
        float num_bigger = 0;
        for (int i = 0; i < S_width * S_height; i++)
        {
            if (S[i] > threshold)
            {
                sum += S[i];
                num_bigger++;
            }
        }
        a = sum / num_bigger;
    }
    //cout << "ok?" << endl;
    for (unsigned int j = 0;j < maxMarkersPrevFrame_; j++)
    {
        if (average(S + j * maxMarkersNextFrame_, maxMarkersNextFrame_) != threshold)
        {
            for (unsigned int i = 0; i < maxMarkersNextFrame_; i++)
            {
                if (S[i + j * S_width] != threshold)
                {
                    float vx = referencePeaks[j]->x - frame2->p.at(i)->x;
                    float vy = referencePeaks[j]->y - frame2->p.at(i)->y;
                    double d = 0;
                    if (option == 1 || option == 3)
                        d = 2.0 * dmax;
                    else
                        d = dmax;
                    double v_norm = sqrt(vx * vx + vy * vy);
                    if (v_norm > d)  // norm
                    {
                        S[i + j * S_width] = 1E-10;
                    }
                    else
                    {
                        // texture information
                        float t = 0.5 * (vars_frame1[j] / var_frame1 + vars_frame2[i] / var_frame2);
                        S[i + j * S_width] += (1 - t) * a;
                        if (option == 1)
                        {
                            S[i + j * S_width] *= (float)exp(-pow(v_norm / dmax, 2));
                        }
                        else if (option == 3);
                        //TODO implement
                        //estimated homography?
                    }
                }
            }
        }
    }
    //cout << "single potentials ok until here 3" << endl;
    float max = -1e+10;
    for (unsigned int i = 0;i < maxMarkersPrevFrame_ * maxMarkersNextFrame_; i++)
        if (S[i] > max)
            max = S[i];
    for (unsigned int i = 0;i < maxMarkersPrevFrame_ * maxMarkersNextFrame_; i++)
        S[i] = S[i] / max;
    delete [] image1;
    delete [] image2;
    delete [] patches_frame1;
    delete [] patches_frame2;
    delete [] vars_frame1;
    delete [] vars_frame2;
    delete [] norms_frame1;
    delete [] norms_frame2;
}
