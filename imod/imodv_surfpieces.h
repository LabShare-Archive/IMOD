class Surface_Piece {
  public:
  float area;
  std::vector<int> * vList;
  std::vector<int> * tList;
  Surface_Piece( std::vector<int> * , std::vector<int> * );
  //~Surface_Piece();
};

class Surface_Pieces
{
public:
  std::vector<Surface_Piece > pieces;
  Surface_Pieces(const struct Mod_Point *vertex_xyz, const b3dInt32 *tarray, int
      tc, b3dInt32 *sortTArray);
  ~Surface_Pieces();
};

