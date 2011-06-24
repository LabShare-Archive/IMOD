#include "point2d.h"


// elsewhere, in the .cpp file (*not* in a .h file, unless we're
// using templates), we define the input/output functions:
istream &operator>>(istream &is, Point2D &p) {
  is >> p.x >> p.y; // depends on the format of the input
  
  // don't forget this -- enables chaining of input
  return is;
}

ostream &operator<<(ostream &os, const Point2D &p) {
  // this will output it as "(x, y)"; but you can do it however
  // you want...
  os << "(" << p.x << ", " << p.y << ")";
  
  // don't forget this -- enables chaining of output
  return os;
}
