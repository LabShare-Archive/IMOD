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
public interface InitialCombineFields {
  public static final String rcsid = "$$Id$$";
  
  public abstract void setUseMatchingModels(boolean useMatchingModels);
  public boolean isUseMatchingModels();
  public void setBinBy2(boolean binBy2);
  public boolean isBinBy2();
  public void setFiducialMatchListA(String fiducialMatchListA);
  public String getFiducialMatchListA();
  public void setFiducialMatchListB(String fiducialMatchListB);
  public String getFiducialMatchListB();

}
