#ifndef POINT2D_H
#define POINT2D_H

#include <iostream>
#include <vector>
#include "paircorrespondence.h"
#include "./constants.h"

using namespace std;

class Point2D
{
public:
    float x, y;
    int markerType;//needed to identify to which marker size belongs to when there are different markers sizes in the same sample
    int frameID;
    double score;
    double pairwise_score;
    bool used;
    bool used_pairCorrespondence;
    vector<pairCorrespondence> index0;
    vector<pairCorrespondence> index1;
    Point2D()
    {
        used = false;
        pairwise_score = initialPairwiseScore;
        used_pairCorrespondence = false;
        //index0.clear();
        //index1.clear();
        //cout<<"I am in point2D constructor default"<<endl;
    }
    Point2D(int a, int b, int ID)
    {
        x = (float)a;
        y = (float)b;
        frameID= ID;
        used = false;
        pairwise_score = -1E+12;
        used_pairCorrespondence = false;
    }
    Point2D(int a, int b, int ID, int markerType_)
    {
        x = (float)a;
        y = (float)b;
        markerType=markerType_;
        frameID= ID;
        used = false;
        pairwise_score = -1E+12;
        used_pairCorrespondence = false;
    }
    //famatnote: whenever you have vectors or pointers you need a copy constructor
    Point2D & operator=(const Point2D &p)
    {
        if (this!=&p)
        {
            x=p.x;
            y=p.y;
            markerType=p.markerType;
            frameID=p.frameID;
            used=p.used;
            pairwise_score=p.pairwise_score;
            score=p.score;
            used_pairCorrespondence=p.used_pairCorrespondence;
            index0.clear();
            for (vector<pairCorrespondence>::const_iterator iter=p.index0.begin();iter!=p.index0.end();++iter)
                index0.push_back(*iter);
            index1.clear();
            for (vector<pairCorrespondence>::const_iterator iter=p.index1.begin();iter!=p.index1.end();++iter)
                index1.push_back(*iter);
        }
        return *this;

    }
    Point2D(const Point2D &p)//copy constructor
    {
        //cout<<"I am in point2D constructor copy"<<endl;

        x=p.x;
        y=p.y;
        markerType=p.markerType;
        frameID=p.frameID;
        used=p.used;
        pairwise_score=p.pairwise_score;
        score=p.score;
        used_pairCorrespondence=p.used_pairCorrespondence;
        index0.clear();
        for (vector<pairCorrespondence>::const_iterator iter=p.index0.begin();iter!=p.index0.end();++iter)
            index0.push_back(*iter);
        index1.clear();
        for (vector<pairCorrespondence>::const_iterator iter=p.index1.begin();iter!=p.index1.end();++iter)
            index1.push_back(*iter);
    }
    //famatnote: whenever we have pointers or vectors we need a destructor
    ~Point2D()
    {
        //cout<<"I am in Point2D destructor and index0 size is="<<index0.size()<<endl;

        //if(!index0.empty())
            index0.clear();
        //if(!index1.empty())
            index1.clear();

    }

    /* operator+ and operator- allow you to use the class
     * like a 2-d vector like this:
     *  Point2D p1(4, 3), p2(1, 7);
     *  Point2D p3 = p2 + p1;  // calls p2.operator+(p1);
     *//*
Point2D operator+(const Point2D &p) const {
return Point2D(x + p.x, y + p.y);
}

Point2D operator-(const Point2D &p) const {
return Point2D(x - p.x, y - p.y);
}

Point2D operator*(int m) const {
return Point2D(x * m, y * m);
}

Point2D operator/(int m) const {
return Point2D(x / m, y / m);
}
*/
    /* These functions are useful for reading data in from
     * the problem's input stream, and writing it out for
     * output or (more likely) debugging purposes. This
     * allows:
     *  cin >> p1 >> p2;
     *  cout << p1 << ", " << p2;
     * Note that these are declared "friend" functions and
     * are defined outside of the class (since they are not
     * really class member functions). They are declared
     * "friend" just in case we decide to make the data
     * members (x & y) private or protected.
     */

    friend istream &operator>>(istream &is, Point2D &p);
    friend ostream &operator<<(ostream &os,
                               const Point2D &p);
};

#endif
