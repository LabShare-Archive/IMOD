/*
 * linearfitting.h - Header for LinearFitting class
 *
 *  $Id$
 *
 */
#ifndef LINEARFITTING_H
#define LINEARFITTING_H

class LinearFitting
{
   public:
     LinearFitting(int nRaw);
     ~LinearFitting();
     int computeFitting(double *result, double *model, int nModel, int index1,
                        int index2, double &xAtMin);
     int getDim() {return nDim;}
     void setRaw(double *rawData);
   private:
    int nDim;
    double *raw;
};

#endif
