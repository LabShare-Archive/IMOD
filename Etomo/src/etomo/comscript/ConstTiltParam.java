/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.18  2010/03/05 03:56:53  sueh
 * <p> bug# 1319 Changed the return value of getLogShift to String.
 * <p>
 * <p> Revision 3.17  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 3.16  2009/09/21 17:44:42  sueh
 * <p> bug# 1267 Using this class in the tilt monitors.
 * <p>
 * <p> Revision 3.15  2007/12/13 01:04:29  sueh
 * <p> bug# 1056 Added adjustOrigin.  Merged ConstTiltParam with TiltParam and made
 * <p> ConstTiltParam an interface.
 * <p>
 * <p> Revision 3.14  2007/11/06 19:08:14  sueh
 * <p> bug# 1047 Added getFloatValue.
 * <p>
 * <p> Revision 3.13  2007/05/11 15:26:31  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 3.12  2007/03/07 21:00:26  sueh
 * <p> bug# 981 Reduced visibility of protected fields to package private.
 * <p>
 * <p> Revision 3.11  2007/02/05 21:41:49  sueh
 * <p> bug# 962  Put EtomoNumber type info into an inner class.
 * <p>
 * <p> Revision 3.10  2006/09/19 21:58:39  sueh
 * <p> bug# 920 Added Storables, an object for storing variables that don't going into the
 * <p> the .com file.  Storables can be added to MetaData or another storable type
 * <p> without creating an instance of TiltParam.  Moved fiducialess to Storables.
 * <p>
 * <p> Revision 3.9  2006/09/13 23:12:40  sueh
 * <p> bug# 920 Changed xOffset and zOffset to xShift and zShift.  Changed zShift
 * <p> and tiltAngleOffset to EtomoNumber.
 * <p>
 * <p> Revision 3.8  2006/05/11 19:41:58  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 3.7  2005/07/29 00:44:46  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.6  2005/06/10 22:49:57  sueh
 * <p> bug# 583, bug# 682 Moved binning calculation to ApplicationManager.
 * <p> Upgraded tilt.com to have all unbinned parameters and a binning value.
 * <p> Added member variables:  imageBinned, loadedFromFile.  Added
 * <p> function:  isOldVersion.
 * <p>
 * <p> Revision 3.5  2005/01/12 18:33:43  sueh
 * <p> bug# 505 Added excludeList2.
 * <p>
 * <p> Revision 3.4  2005/01/08 01:37:28  sueh
 * <p> bug# 578  Added useZFactors, which is set by the user, and
 * <p> zFactorFileName, which is generated or comes from the comscript.
 * <p>
 * <p> Revision 3.3  2004/07/20 23:05:29  sueh
 * <p> bug# 502 adding fiducialess, which is not retrieved from tilt
 * <p>
 * <p> Revision 3.2  2004/03/24 18:11:55  rickg
 * <p> xAxisTilt default value corrected
 * <p>
 * <p> Revision 3.1  2004/03/24 02:55:44  rickg
 * <p> Bug# 395 Implemented ability to create binned tomogram
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.6  2003/10/22 21:30:28  rickg
 * <p> Bug# 287 Default value handling for SLICE OFFSET and SHIFT
 * <p>
 * <p> Revision 2.5  2003/08/21 22:17:27  rickg
 * <p> Added density scaling getters
 * <p>
 * <p> Revision 2.4  2003/06/25 22:15:50  rickg
 * <p> Manage all tilt parameters
 * <p>
 * <p> Revision 2.3  2003/06/10 22:55:36  rickg
 * <p> Modeled all of the parameters from the man page
 * <p>
 * <p> Revision 2.2  2003/05/23 21:26:32  rickg
 * <p> Implemented radial filter parameters
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
package etomo.comscript;

import etomo.type.ConstEtomoNumber;

public interface ConstTiltParam extends CommandDetails {
  public static final String rcsid = "$Id$";

  public String getInputFile();

  public boolean hasMode();

  public String getExcludeList2();

  public ConstEtomoNumber getImageBinned();

  public int getMode();

  public String getOutputFile();

  public boolean isFiducialess();

  public int getFullImageX();

  public int getIdxSliceStart();

  public int getIdxSliceStop();

  public String getLogShift();

  public double getRadialBandwidth();

  public double getRadialFalloff();

  public double getScaleCoeff();

  public double getScaleFLevel();

  public int getThickness();

  public ConstEtomoNumber getTiltAngleOffset();

  public int getWidth();

  public double getXAxisTilt();

  public double getXShift();

  public ConstEtomoNumber getZShift();

  public boolean hasLogOffset();

  public boolean hasRadialWeightingFunction();

  public boolean hasScale();

  public boolean hasSlice();

  public boolean hasThickness();

  public boolean hasTiltAngleOffset();

  public boolean hasWidth();

  public boolean hasXAxisTilt();

  public boolean hasXShift();

  public boolean hasZShift();

  public boolean isUseGpu();

  public boolean hasLocalAlignFile();

  public boolean hasZFactorFileName();
}