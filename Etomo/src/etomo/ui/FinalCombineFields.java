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
* <p> $$Log$$ </p>
*/
public interface FinalCombineFields {
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
}
