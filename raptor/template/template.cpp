#include "template.h"
#include <stdio.h>
#include <cmath>
#include <string.h>
#include <stdlib.h>
#include "../mainClasses/frame.h"
#include "../mainClasses/constants.h"
#include "../mainClasses/paircorrespondence.h"
#include <limits>
#include <fstream>
#include <algorithm>
#include <sstream>

using namespace std;
float min_potential_peak;
unsigned int min_index;
float* createSyntheticTemplate(vector<frame>* frames, int diameter, int frameNumber, ioMRC* vol,bool white,int mType)
{
    //the slice to compute NCC with
    float* slice = new float[vol->headerGetNx() * vol->headerGetNy()];
    vol->readMRCSlice(frameNumber, slice);
    int n = 2 * diameter + 1; // width and height of the template
    float *templ = new float[n * n]; // the template for computing NCC
    double m = (n + 1) / 2;
    float background,marker;
    if (white)
    {
        background=0.0f;
        marker=1.0f;
    }
    else
    {
        background=1.0f;
        marker=0.0f;
    }
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            double norm = sqrt(pow(fabs((double)i + 1 - m), 2.0) + pow(fabs((double)j + 1 - m), 2.0));
            if (norm <= (double)(diameter / 2))
                templ[i * n + j] = marker;
            else
                templ[i * n + j] = background;
        }
    }

    int image_width = frames->at(frameNumber).width;
    int image_height = frames->at(frameNumber).height;
    int template_side = 2 * diameter + 1;
    int result_width = image_width - template_side + 1;
    int result_height = image_height - template_side + 1;
    float* result = new float[result_width * result_height];


    IplImage* imageIpl;
    IplImage* templIpl;
    IplImage* resultIpl;
    imageIpl = cvCreateImage(cvSize(image_width, image_height), IPL_DEPTH_32F, 1);
    templIpl = cvCreateImage(cvSize(template_side, template_side), IPL_DEPTH_32F, 1);
    resultIpl = cvCreateImage(cvSize(result_width, result_height), IPL_DEPTH_32F, 1);
    vol->readMRCSlice(frameNumber, (float*)imageIpl->imageData);
    memcpy((float*)templIpl->imageData, templ, template_side * template_side * sizeof(float));

    cvMatchTemplate(imageIpl, templIpl, resultIpl, CV_TM_CCOEFF_NORMED);

    memcpy(result, (float*)resultIpl->imageData, result_width * result_height * sizeof(float));

    cvReleaseImage(&imageIpl);
    cvReleaseImage(&templIpl);
    cvReleaseImage(&resultIpl);
    free(imageIpl);
    free(templIpl);
    free(resultIpl);
    //cropBorders(slice, result, image_width, image_height, template_side);

    //suppress negative values and find the maximal value
    float max = -10.0f;
    for (int i = 0; i < result_width * result_height; i++)
    {
        if (result[i] > max)
            max = result[i];
    }
    //normalize cross correlation values to avoid threshold depending on patch size
    for (int i = 0; i < result_width * result_height; i++)
        result[i] /= max;

    //select the 5 highest peaks
    vector<Point2D*> peaks = findPeaks(result, result_width, result_height, 0.5, 5, 3, template_side, frameNumber,mType);
    //peaks.at(2)->y++;
    if (peaks.size()<3)
    {
        cout<<"ERROR: unable to create marker template from synthetic template. Try to change teh specified diameter"<<endl;
        exit(-1);
    }

    //reset template to zero
    for (int i = 0; i < template_side*template_side; i++) templ[i]=0.0f;
    for (int k = 0; k <= 2; k++)
    {
        for (int i = 0; i < template_side; i++)
            for (int j = 0; j < template_side; j++)
            {
                int x = (int)(peaks.at(k)->x + i);
                int y = (int)(peaks.at(k)->y + j);
                if (x < image_width && y < image_height)
                    templ[i + j * template_side] += slice[x + y * image_width];
            }
    }
    for (int i = 0; i < template_side*template_side; i++) templ[i]/=3.0f;//perform average

    delete [] result;
    delete [] slice;

    for (unsigned int kk=0;kk<peaks.size();kk++) delete peaks[kk];
    peaks.clear();
    /*
    	for (int i = 0; i < 21; i++) {
    		for (int j = 0; j < 21; j++) {
    			cout << templ[i + j * 21] << " ";
    		}
    		cout << endl;
    	}
    */
    return templ;
}
//refines template using MRC volume. The starting point is a synthetic template
void computeNCC(vector<frame>* frames, float* templ, ioMRC* vol,int mType)
{
    extern int *diameter;
    extern int zerotilt;
    float threshold = 0.3;
    int l = zerotilt + 1 - (frames->size() % 2);
    //for (unsigned int k = 0; k < frames->size(); k++)
    int avgFactor=3;
    for (int k = 0; k < min((int)frames->size(),20); k++)//to refine the template we don;t need all the projections
    {
        //double temp = pow(-1.0, (double)k);
        //l += k * (int)temp;
        //better implementation
        if ((k%2)==1)
            l-=k;
        else
            l+=k;
        //---------------------
        //cout << "Refining template using slice " << l << endl;
        float* image = new float[vol->headerGetNx() * vol->headerGetNy()];
        vol->readMRCSlice(l, image);
        IplImage* imageIpl;
        IplImage* templIpl;
        int template_side = diameter[mType] * 2 + 1;
        int image_width = frames->at(k).width;
        int image_height = frames->at(k).height;
        int result_width = frames->at(k).width - template_side + 1;
        int result_height = frames->at(k).height - template_side + 1;
        float* result = new float[result_width * result_height];
        IplImage* resultIpl;
        imageIpl = cvCreateImage(cvSize(image_width, image_height), IPL_DEPTH_32F, 1);
        templIpl = cvCreateImage(cvSize(template_side, template_side), IPL_DEPTH_32F, 1);
        resultIpl = cvCreateImage(cvSize(result_width, result_height), IPL_DEPTH_32F, 1);
        memcpy((float*)imageIpl->imageData, image, image_width * image_height * sizeof(float));
        memcpy((float*)templIpl->imageData, templ, template_side * template_side * sizeof(float));

        cvMatchTemplate(imageIpl, templIpl, resultIpl, CV_TM_CCOEFF_NORMED);

        memcpy(result, (float*)resultIpl->imageData, result_width * result_height * sizeof(float));

        cvReleaseImage(&imageIpl);
        cvReleaseImage(&templIpl);
        cvReleaseImage(&resultIpl);
        //cropBorders(image, result, image_width, image_height, template_side);
        //suppress negative values and find the maximal value
        float max = -10.0f;
        for (int i = 0; i < result_width * result_height; i++)
        {
            if (result[i] > max)
            {
                max = result[i];
            }
        }
        //cout << image[0] << " " <<templ[0] << " max is " << max << endl;
        /*
        	for (int i = 2047; i >= 0; i--)
        	for (int j = 2047; j >= 0; j--)
        	if (image[i + j * 2048] == 25){
        	cout << "i j is " << i << "  " << j << endl;
        	}
        */
        //cout << "result is " << result[0] << endl;
        //cout << "result is " << result[2027 + 2027 * 2028] << endl;

        //cout << "k is " << k << endl;
        for (int i = 0; i < result_width * result_height; i++)
            result[i] /= max;
        if ((int)(k + 1) < vol->headerGetNz())
        {
            for (int i = 0; i < template_side; i++)
                for (int j = 0; j < template_side; j++)
                    templ[i + j * template_side] *= 5 * k + 3;
            vector<Point2D*> peaks = findPeaks(result, result_width, result_height, threshold, 5, 0, template_side, k,mType);
            //cout << "peak search complete!" << endl;
            for (unsigned int p = 0; p < peaks.size(); p++)
            {
                int x=-1, y=-1;
                peaks.at(p)->x += diameter[mType];
                peaks.at(p)->y += diameter[mType];
                for (int i = 0; i < template_side; i++)
                    for (int j = 0; j < template_side; j++)
                    {
                        x = (int)(peaks.at(p)->x + i - diameter[mType]);
                        y = (int)(peaks.at(p)->y + j - diameter[mType]);
                        templ[i + j * template_side] += image[x + y * image_width];
                    }
                //cout << "x and y are " << x << " " << y << " " << peaks.at(p)->score << endl;
            }
            avgFactor+=(int)peaks.size();
            for (int i = 0; i < template_side; i++)
                for (int j = 0; j < template_side; j++)
                    templ[i + j * template_side] /= (float)avgFactor;
            //free memory
            for (unsigned int kk=0;kk<peaks.size();kk++) delete peaks[kk];
            peaks.clear();
        }
        delete [] result;
        delete [] image;
        //frames->at(k).fv = result;
    }
    //cout << "Computing NCC complete!" << endl;
}

// C++ implementation of diff() in MATLAB
float* diffVector(float* vector, int length)
{
    float* result = new float[length - 1];
    for (int i = 0; i < length - 1; i++)
    {
        result[i] = vector[i + 1] - vector[i];
    }
    return result;
}

// Find the peaks in the given frame
vector<Point2D*> findPeaks(float* fv, int width, int height, float threshold, unsigned int maxMarkers, unsigned int minMarkers, int template_side, int frameID,int mType)
{

    vector<Point2D*> peaks;

    //find the peaks
    min_potential_peak = numeric_limits<float>::min();
    min_index = -1;
    vector<simplePoint2D> potential_peaks;
    for (int i = 0; i < width; i++)
        for (int j = 0; j < height; j++)
        {
            if (fv[i + j * width] > min_potential_peak)
            {
                simplePoint2D p(i, j, fv[i+ j * width]);
                processSimplePoint2D(&potential_peaks, p, maxMarkers,mType);
            }
        }
    sort(potential_peaks.begin(), potential_peaks.end(), simplepointcmp);//peak[i].score>=peak[i+1].score
    int points = (potential_peaks.size() < maxMarkers) ? potential_peaks.size() : maxMarkers;
    for (int i = 0; i < points; i++)
    {
        simplePoint2D p = potential_peaks.at(i);
        if (p.score<threshold)
            break;
        Point2D* point = new Point2D(p.x, p.y, frameID);
        point->score = p.score;
        peaks.push_back(point);
    }

    bool interp=true;
    if (peaks.size()<minMarkers && threshold>0.2)
    {
        threshold-=0.1;
        for (unsigned int kk=0;kk<peaks.size();kk++)
            delete peaks[kk];
        peaks.clear();
        peaks=findPeaks(fv, width, height, threshold, maxMarkers, minMarkers, template_side,frameID,mType);//nested function
        interp=false;//if we enter here we don't need interpolation because of recursion
    }

    //interpolation to achieve subpixel accuracy
    int radius=2;
    if (interp)
    {
        int ii,jj;
        double w,wTotal,posx,posy;
        for (std::vector<Point2D*>::iterator iter=peaks.begin();iter!=peaks.end();++iter)
        {
            ii=(int)(*iter)->x;
            jj=(int)(*iter)->y;
            if (ii<radius || jj<radius || ii>width-radius-1 || jj>height-radius-1)
                continue;//avoid segmentation fault
            //we use center mass to find the center
            wTotal=0.0f;
            posx=0.0;
            posy=0.0;
            for (int dx=-radius;dx<=radius;dx++)
            {
                for (int dy=-radius;dy<=radius;dy++)
                {
                    w=fv[(ii+dx)+(jj+dy)*width];
                    wTotal+=w;
                    posx+=((double)(dx))*w;
                    posy+=((double)(dy))*w;
                }
            }
            posx/=wTotal;
            posy/=wTotal;
            //update location
            ((*iter)->x)-=posx;
            ((*iter)->y)-=posy;
        }
    }

    return peaks;
}

int nnz(float* matrix, int size)
{
    int ans = 0;
    for (int i = 0; i < size; i++)
    {
        if (matrix[i] != 0)
            ans++;
    }
    return ans;
}

/*
void cropBorders(float* image, float* fv, int image_width, int image_height, int template_side) {
  int fv_width = image_width - template_side + 1;
  int fv_height = image_height - template_side + 1;
	cout << fv_width << "   " << fv_height << endl;
  //MATLAB: (row, col) (y, x)
  //the middle column
  int image_width_half;
  if (fmod((double)image_width, 1.0) < 0.5)
    image_width_half = (int)floor(((double)image_width) / 2.0);
  else
    image_width_half = (int)ceil(((double)image_width) / 2.0);
  float diffV[image_height];
  for (int i = 0; i < image_height; i++)
    diffV[i] = image[i * image_width + image_width_half];
  float* auxD = diffVector(diffV, image_height);
  int l = image_height - 1;
  for (int i = 0; i < image_height; i++)
    if (diffV[i] != 0) {
      l = i;
      break;
    }
  cout << "template_side is " << template_side << endl;
  int max = (l+template_side < fv_height) ? l+template_side : fv_height;
  cout << "max is " << max << endl;
  for (int i = 0; i < max; i++)
    for (int j = 0; j < fv_width; j++)
      fv[i * fv_width + j] = 0;
  for (int i = image_height - 1; i >= 0; i--)
    {
      if (diffV[i] != 0) {
	l = i;
	break;
      }
    }
  l = image_width - 1 - l;
  for (int i = fv_height - template_side - l - 1; i < fv_height; i++)
    for (int j = 0; j < fv_width ; j++)
      fv[i * fv_width + j] = 0;
  delete [] auxD;

 // the middle row
  int image_height_half;
  if (fmod((double)image_height, 1.0) < 0.5)
    image_height_half = (int)floor(((double)image_height) / 2.0);
  else
    image_height_half = (int)ceil(((double)image_height) / 2.0);

  for (int i = 0; i < image_width; i++)
    diffV[i] = image[i + image_width * image_height_half];
  auxD = diffVector(diffV, image_width);
  l = 0;
  for (int i = 0; i < image_width; i++)
    if (diffV[i] != 0) {
      l = i;
      break;
    }
  for (int i = 0; i < fv_height; i++)
    for (int j = 0; j < template_side + l; j++)
      fv[i * fv_height + j] = 0;
  l = image_height - 1;
  for (int i = fv_width - 1; i >= 0; i--)
    if (diffV[i] != 0) {
      l = i;
      break;
    }
  l = fv_width - l - 1;
  for (int i = 0; i < fv_height; i++)
     for (int j = fv_width - template_side - l; j < fv_width; j++)
       fv[i * fv_height + j] = 0;
}
*/

bool simplepointcmp(simplePoint2D a, simplePoint2D b)
{
    return a.score > b.score;
}

bool pointcmp(Point2D* a, Point2D* b)
{
    return a->score > b->score;
}

void mldivide(double* ans, double* a_, int a_row, int a_col, double* b_, int b_row, int b_col)
{
}


int estimateNumberOfMarkers(vector<frame>* frames, float** templ, ioMRC* vol,int numDiffMarkerSize)
{
    int targets=-1;
    double thrAbsDist=0.05;//to decide where the kink is
    double minNCCval=0.8;//any value above that is considered a marker

    extern int *diameter;
    int maxMarkers=120;//we plot a profile with a hundred markers
    unsigned int zerotilt=frames->size()/2;
    float peakThreshold = 0.1;
    for (int tt=0;tt<numDiffMarkerSize;tt++)
    {
        CvMat *xx=cvCreateMat(maxMarkers,1,CV_64FC1);
        CvMat *xxW=cvCreateMat(maxMarkers,1,CV_64FC1);
        for (int ii=0;ii<maxMarkers;ii++)
        {
            cvmSet(xx,ii,0,0.0);
            cvmSet(xxW,ii,0,0.0);
        }
        int template_side = diameter[tt] * 2 + 1;
        for (unsigned int i = zerotilt-4; i < zerotilt+4; i++)//we use 9 frames to average the peak profile
        {
            IplImage* imageIpl;
            IplImage* templIpl;
            int image_width = frames->at(i).width;
            int image_height = frames->at(i).height;
            int result_width = image_width - template_side + 1;
            int result_height = image_height - template_side + 1;

            float* result = new float[result_width * result_height];
            IplImage* resultIpl;
            imageIpl = cvCreateImage(cvSize(image_width, image_height), IPL_DEPTH_32F, 1);
            templIpl = cvCreateImage(cvSize(template_side, template_side), IPL_DEPTH_32F, 1);
            resultIpl = cvCreateImage(cvSize(result_width, result_height), IPL_DEPTH_32F, 1);
            vol->readMRCSlice(i, (float*)imageIpl->imageData);
            memcpy((float*)templIpl->imageData, templ[tt], template_side * template_side * sizeof(float));

            cvMatchTemplate(imageIpl, templIpl, resultIpl, CV_TM_CCOEFF_NORMED);

            memcpy(result, (float*)resultIpl->imageData, result_width * result_height * sizeof(float));

            cvReleaseImage(&imageIpl);
            cvReleaseImage(&templIpl);
            cvReleaseImage(&resultIpl);

            //suppress negative values and find the maximal value
            float max = -10.0f;
            for (int kk = 0; kk < result_width * result_height; kk++)
            {
                if (result[kk] > max)
                    max = result[kk];
            }
            //normalize cross correlation values to avoid threshold depending on patch size
            for (int kk = 0; kk < result_width * result_height; kk++)
                result[kk] /= max;


            vector<Point2D*> peaks=findPeaks(result, result_width, result_height, peakThreshold, maxMarkers, 6, template_side, i,tt);

            delete [] result;

            for (unsigned int kk=0;kk<peaks.size();kk++)
            {
                xxW->data.db[kk]+=1.0;
                xx->data.db[kk]+=peaks[kk]->score;
            }
        }

        //estimate number of marker using least squares
        for (int kk=0;kk<maxMarkers;kk++)
        {
            if (xxW->data.db[kk]>0.5)
                xx->data.db[kk]/=xxW->data.db[kk];
        }

        //fit least squares to positions 9-19
        CvMat *A=cvCreateMat(11,2,CV_64FC1);
        CvMat *b=cvCreateMat(11,1,CV_64FC1);
        CvMat *pp=cvCreateMat(2,1,CV_64FC1);
        for (int kk=0;kk<11;kk++)
        {
            A->data.db[kk+kk]=(double)(kk+9);
            A->data.db[kk+kk+1]=1.0;
            b->data.db[kk]=xx->data.db[kk+9];
        }
        cvSolve(A,b,pp,CV_SVD);
        int targetsAux;
        for (int kk=20;kk<maxMarkers;kk++)
        {
            targetsAux=kk;
            if (fabs(kk*pp->data.db[0]+pp->data.db[1]-xx->data.db[kk])>thrAbsDist || xx->data.db[kk]<0.6)
            {
                if (xx->data.db[kk]<minNCCval)
                {
                    break;
                }
            }
        }
        targets=max(targets,targetsAux);//we choose the max out of all the possible diameter sizes
        cvReleaseMat(&A);
        cvReleaseMat(&b);
        cvReleaseMat(&pp);
        cvReleaseMat(&xx);
        cvReleaseMat(&xxW);
    }

    return min(targets,maxTargetsPrevFrame);
}

void findAllPeaks(vector<frame>* frames, float** templ, ioMRC* vol,int numDiffMarkerSize,bool xRay)
{
    extern int *diameter;
    extern unsigned int maxMarkersNextFrame;
    for (int tt=0;tt<numDiffMarkerSize;tt++)
    {
        int template_side = diameter[tt] * 2 + 1;
        float peakThreshold = 0.3;
        for (unsigned int i = 0; i < frames->size(); i++)
        {
            IplImage* imageIpl;
            IplImage* templIpl;
            int image_width = frames->at(i).width;
            int image_height = frames->at(i).height;
            int result_width = image_width - template_side + 1;
            int result_height = image_height - template_side + 1;

            float* result = new float[result_width * result_height];
            IplImage* resultIpl;
            imageIpl = cvCreateImage(cvSize(image_width, image_height), IPL_DEPTH_32F, 1);
            templIpl = cvCreateImage(cvSize(template_side, template_side), IPL_DEPTH_32F, 1);
            resultIpl = cvCreateImage(cvSize(result_width, result_height), IPL_DEPTH_32F, 1);
            vol->readMRCSlice(i, (float*)imageIpl->imageData);
            memcpy((float*)templIpl->imageData, templ[tt], template_side * template_side * sizeof(float));

            //remove lines from image to avoid missguiding template matching
            //This is especially useful for soft X-ray where two parallel straight lines from cylinder will ruin the template matching
            float *mask=new float[image_width * image_height];
            for(int rr=0;rr<image_width * image_height;rr++)
                mask[rr]=1.0f;
            if (xRay)
                removeLinesXray(imageIpl,mask);

            cvMatchTemplate(imageIpl, templIpl, resultIpl, CV_TM_CCOEFF_NORMED);

            memcpy(result, (float*)resultIpl->imageData, result_width * result_height * sizeof(float));

            //apply mask
            //WARNING: IMAGE SIZE IS DIFFERENT THAN RESULT
            int posR=0,posM;
            int cc=(template_side-1)/2;
            for(int aa=0;aa<result_height;aa++)
            {
                posM=(aa+cc)*image_width+cc;
                for(int bb=0;bb<result_width;bb++)
                {
                    result[posR]*=mask[posM];
                    posR++;
                    posM++;
                }
            }

            delete []mask;
            cvReleaseImage(&imageIpl);
            cvReleaseImage(&templIpl);
            cvReleaseImage(&resultIpl);

            //suppress negative values and find the maximal value
            float max = -10.0f;
            for (int kk = 0; kk < result_width * result_height; kk++)
            {
                if (result[kk] > max)
                    max = result[kk];
            }
            //normalize cross correlation values to avoid threshold depending on patch size
            for (int kk = 0; kk < result_width * result_height; kk++)
                result[kk] /= max;


            vector<Point2D*> peaks=findPeaks(result, result_width, result_height, peakThreshold, maxMarkersNextFrame, 6, template_side, i,tt);

            delete [] result;

            for (unsigned int j = 0; j < peaks.size(); j++)
            {
                peaks[j]->x += diameter[tt];
                peaks[j]->y += diameter[tt];
                peaks[j]->markerType = tt;
                if (tt!=0)
                {
                    if (isPeakInFrame(&(frames->at(i)),peaks[j]))
                    {
                        delete peaks[j];//delete the pointer to avoid memory leaking
                        continue;//avoid repeating teh same peak
                    }
                }
                frames->at(i).p.push_back(peaks[j]);
            }
            //we need to sort them out based on score
            if (tt!=0) sort(frames->at(i).p.begin(),frames->at(i).p.end(),pointcmp);
        }
    }
}

bool isPeakInFrame(frame *frames,Point2D *p)
{
    int markerType_=p->markerType;
    for (vector<Point2D*>::iterator iter=frames->p.begin();iter!=frames->p.end();++iter)
    {
        if (withinDiameter((*iter)->x,(*iter)->y,p->x,p->y,markerType_))
            return true;
    }
    return false;

}


bool withinDiameter(float x1, float y1, float x2, float y2,int diamNum)
{
    extern int *diameter;
    float dist_sq = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2);
    if ((int)(diameter[diamNum] * diameter[diamNum]) >  dist_sq)
        return true;
    else return false;
}


bool processSimplePoint2D(vector<simplePoint2D>* potential_peaks, simplePoint2D p, unsigned int maxMarkers,int mType)
{
    for (unsigned int i = 0; i < potential_peaks->size(); i++)
    {
        simplePoint2D target = potential_peaks->at(i);
        if (withinDiameter(p.x, p.y, target.x, target.y,mType))
        {
            if (p.score > target.score)
            {
                potential_peaks->at(i).x = p.x;
                potential_peaks->at(i).y = p.y;
                potential_peaks->at(i).score = p.score;
                if (i == min_index)
                {
                    findMin(potential_peaks);
                }
                return true;
            }
            else
                return false;
        }
    }
    if (potential_peaks->size() >= maxMarkers)
    {
        potential_peaks->at(min_index) = p;
    }
    else
    {
        potential_peaks->push_back(p);
    }
    findMin(potential_peaks);
    return true;
}

void findMin(vector<simplePoint2D>* potential_peaks)
{
    min_index = 0;
    min_potential_peak = potential_peaks->at(0).score;
    for (unsigned int i = 0; i < potential_peaks->size(); i++)
    {
        simplePoint2D p = potential_peaks->at(i);
        if (p.score < min_potential_peak)
        {
            min_potential_peak = p.score;
            min_index = i;
        }
    }
}

//October 5th 2009
//remove lines using hough transform
void removeLinesXray(IplImage *image,float *mask)
{
    int width=image->width;
    int safetyPixels=width/100;//to remove further teh cylinder borders

    float mean;
    //for each x-line find the two droping areas
    float *lineProfile=(float*)image->imageData;
    int pos;
    for(int y=0;y<image->height;y++)
    {
        pos=y*width;

        //compute mean in the center
        mean=0;
        int count=0;
        for(int ii=pos+width/4;ii<pos+3*width/4;ii++)
        {
            mean+=lineProfile[ii];
            count++;
        }
        mean/=(float)count;

        //find white areas in the extremes
        while(lineProfile[pos]>mean)
        {
            mask[pos]=0.0f;
            pos++;
        }
        for(int ii=pos;ii<pos+safetyPixels;ii++)
            mask[ii]=0.0f;
        //start at the other end
        pos=(y+1)*width-1;
        while(lineProfile[pos]>mean)
        {
            mask[pos]=0.0f;
            pos--;
        }
        for(int ii=pos;ii>pos-safetyPixels;ii--)
            mask[ii]=0.0f;
    }

}
