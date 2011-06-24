#ifndef CONSTANTS_H
#define CONSTANTS_H
#include <limits>
#include <string>
#include <time.h>

using namespace std;

static const double PI = 3.14159265358979;

static const unsigned int maxJumps = 2;

//
static const double initialPairwiseScore = 999;


//optimization procedure
static const float percentile = 0.7;//percentile to display Mean Squared Residual error without outliers during optimization procedure

static const double minWeight = 1e-4;//to avoid numerical error when trajectories have missing markers. We set it to very low score.
static const double delta = 5; //Huber penalty constant (step between quadratic to linear error)

static  const double tol = 0.1;//tolerance for the

static const double sparseMatrixZeroVal=numeric_limits<double>::min();//belowe this value we drop elements in sparse matrices to speed up and avoid erros

static const double peakThresholdFillContours=0.45;

static const int maxTargetsNextFrame=180;//maximum number of candidates
static const int maxTargetsPrevFrame=80;//maximum number of targets if we use automatic number of marker estimation

string getDate(void); //simple rutine to get date and time
#endif
