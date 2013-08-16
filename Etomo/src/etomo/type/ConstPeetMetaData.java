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
* <p> Revision 1.14  2009/11/23 23:26:12  sueh
* <p> bug# 1292 Removing yaxisContour.
* <p>
* <p> Revision 1.13  2009/09/01 02:43:00  sueh
* <p> bug# 1222 Made functions public.
* <p>
* <p> Revision 1.12  2009/01/13 19:36:27  sueh
* <p> bug# 1170 Added isUseNWeightGroup.
* <p>
* <p> Revision 1.11  2008/09/10 20:56:22  sueh
* <p> bug# 1135 Added EtomoBoolean2 tiltRange to MetaData so that the state
* <p> of the check box on the screen can be saved.
* <p>
* <p> Revision 1.10  2008/08/22 17:50:54  sueh
* <p> bug# 1136 Added getNWeightGroup.
* <p>
* <p> Revision 1.9  2008/04/02 02:00:10  sueh
* <p> bug# 1095 Added mask fields.
* <p>
* <p> Revision 1.8  2007/07/25 22:58:30  sueh
* <p> bug# 1027 Change start and end angles to min and max angles.
* <p>
* <p> Revision 1.7  2007/06/05 21:28:23  sueh
* <p> bug# 1010 Added flgWedgeWeight.
* <p>
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
  public static final String rcsid = "$Id$";

  public String getName();

  public String getInitMotlFile(int key);

  public String getTiltRangeMultiAxesFile(int key);

  public String getTiltRangeMin(int key);

  public String getTiltRangeMax(int key);

  public AxisType getAxisType();

  public String getReferenceFile();

  public ConstEtomoNumber getReferenceParticle();

  public ConstEtomoNumber getReferenceVolume();

  public int getReferenceMultiparticleLevel();

  public ConstEtomoNumber getEdgeShift();

  public boolean isFlgWedgeWeight();

  public ConstEtomoNumber getMaskModelPtsZRotation();

  public String getMaskModelPtsYRotation();

  public String getMaskTypeVolume();

  public ConstEtomoNumber getNWeightGroup();

  public boolean isTiltRange();

  public boolean isManualCylinderOrientation();

  public boolean isTiltRangeMultiAxes();
}
