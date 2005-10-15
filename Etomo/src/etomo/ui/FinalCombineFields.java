package etomo.ui;

/**
* <p>Description: </p>
*
* <p>Copyright: Copyright 2004</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* Univeristy of Colorado</p>
*
* @author $$Author$$
*
* @version $$Revision$$
*
* <p> $$Log$
* <p> $Revision 1.3  2005/10/13 22:33:32  sueh
* <p> $bug# 532 Added parallelProcess and noVolcombine
* <p> $
* <p> $Revision 1.2  2004/05/11 21:46:29  sueh
* <p> $bug# 302 making FinalCombineFields interface local to the
* <p> $ui package
* <p> $
* <p> $Revision 1.1  2004/05/11 20:48:38  sueh
* <p> $bug #302 interface to final combine screen fields.  Used by
* <p> $setup panel and final panel.
* <p> $$ </p>
*/
interface FinalCombineFields {
  public static final String rcsid = "$$Id$$";
  
  public void setUsePatchRegionModel(boolean usePatchRegionModel);
  public boolean isUsePatchRegionModel();
  public void setXMin(String xMin);
  public String getXMin();
  public void setXMax(String xMax);
  public String getXMax();
  public void setYMin(String yMin);
  public String getYMin();
  public void setYMax(String yMax);
  public String getYMax();
  public void setZMin(String zMin);
  public String getZMin();
  public void setZMax(String zMax);
  public String getZMax();
  public void setParallel(boolean parallel);
  public boolean isParallel();
  public void setNoVolcombine(boolean noVolcombine);
  public boolean isNoVolcombine();
}
