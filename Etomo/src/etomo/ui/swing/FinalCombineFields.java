package etomo.ui.swing;

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
* <p> $Revision 1.1  2010/11/13 16:07:35  sueh
* <p> $bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p> $
* <p> $Revision 1.6  2006/03/16 01:54:54  sueh
* <p> $bug# 828 Added isEnabled().
* <p> $
* <p> $Revision 1.5  2005/11/21 20:46:30  sueh
* <p> $bug# 772 Disabling the parallel process checkbox when the cpu.adoc is
* <p> $missing.  Copy parallel process checkbox's enabled setting from the
* <p> $Setup to the Final tab.
* <p> $
* <p> $Revision 1.4  2005/10/15 00:33:20  sueh
* <p> $bug# 532 Standardized is and set parallel processing checkbox functions
* <p> $to setParallel() and isParallel().
* <p> $
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

  public void setParallelEnabled(boolean parallelEnabled);

  public boolean isParallelEnabled();

  public void setNoVolcombine(boolean noVolcombine);

  public boolean isNoVolcombine();

  public boolean isEnabled();
}
