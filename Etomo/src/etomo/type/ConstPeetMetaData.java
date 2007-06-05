package etomo.type;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.6  2007/05/01 22:26:30  sueh
* <p> bug# 964 Added yaxisType and yaxisContour.
* <p>
* <p> Revision 1.5  2007/04/11 22:16:08  sueh
* <p> bug# 964 Added getEdgeShift.
* <p>
* <p> Revision 1.4  2007/04/09 20:58:25  sueh
* <p> bug# 964 Added support for reference.
* <p>
* <p> Revision 1.3  2007/03/26 23:34:40  sueh
* <p> bug# 964 Added getAxisType.
* <p>
* <p> Revision 1.2  2007/03/20 23:03:17  sueh
* <p> bug# 964 Added getInitMotlFile, getTiltRangeStart, and getTiltRangeEnd.
* <p>
* <p> Revision 1.1  2007/02/21 04:19:17  sueh
* <p> bug# 964 Const interface for PeetMetaData.
* <p> </p>
*/
public interface ConstPeetMetaData {
  public static  final String  rcsid =  "$Id$";
  
  public String getName();
  public String getInitMotlFile(int key);
  public String getTiltRangeStart(int key);
  public String getTiltRangeEnd(int key);
  public AxisType getAxisType();
  public String getReferenceFile();
  public ConstEtomoNumber getReferenceParticle();
  public ConstEtomoNumber getReferenceVolume();
  public String getEdgeShift();
  public ConstEtomoNumber getYaxisContourModelNumber();
  public String getYaxisContourObjectNumber();
  public String getYaxisContourContourNumber();
  public boolean isFlgWedgeWeight();
}
