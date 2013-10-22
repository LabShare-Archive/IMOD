package etomo.comscript;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.24  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.23  2008/12/15 22:58:47  sueh
 * <p> bug# 1161 Made axisID package private so that NewstParam could use it.
 * <p>
 * <p> Revision 3.22  2007/12/13 01:03:27  sueh
 * <p> bug# 1056 Added adjustOrigin.
 * <p>
 * <p> Revision 3.21  2007/11/06 19:06:59  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 3.20  2007/09/11 21:16:21  sueh
 * <p> bug# 1035 Added getSizeToOutputInX and Y and isSizeToOutputInXandYSet.
 * <p>
 * <p> Revision 3.19  2007/05/11 15:06:47  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 3.18  2007/02/05 21:39:17  sueh
 * <p> bug# 962 Put mode info into an inner class.
 * <p>
 * <p> Revision 3.17  2006/05/22 22:35:44  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 3.16  2006/05/11 19:39:07  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 3.15  2006/04/06 18:49:04  sueh
 * <p> bug# 808 Implementing ProcessDetails.  Added Fields to pass requests to
 * <p> the generic gets.
 * <p>
 * <p> Revision 3.14  2006/03/22 21:28:21  sueh
 * <p> bug# 803 Added DATA_MODE_OPTION and
 * <p> FLOAT_DENSITIES_OPTION.
 * <p>
 * <p> Revision 3.13  2006/03/22 18:37:02  sueh
 * <p> bug# 803 Added statics FLOAT_DENSITIES_MEAN and
 * <p> FLOAT_DENSITIES_DEFAULT, which are used with floatDensities.
 * <p>
 * <p> Revision 3.12  2006/03/22 17:52:34  sueh
 * <p> bug# 803 Added statics DATA_MODE_BYTE and
 * <p> DATA_MODE_DEFAULT, which used with modeToOutput.
 * <p>
 * <p> Revision 3.11  2006/01/20 20:45:43  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 3.10  2005/11/19 01:51:43  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 3.9  2005/09/02 18:55:55  sueh
 * <p> bug# 721 Adding magGradientFile.
 * <p>
 * <p> Revision 3.8  2005/08/25 01:47:18  sueh
 * <p> bug# 716 preventing index error in getOutputFile()
 * <p>
 * <p> Revision 3.7  2005/06/10 22:45:08  sueh
 * <p> Added GET_BINNING
 * <p>
 * <p> Revision 3.6  2005/04/25 20:38:26  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.5  2005/01/08 01:32:14  sueh
 * <p> bug# 578 Create 2 modes - whole tomo sample and full aligned stack.
 * <p> Implement Command.  Add fiducialessAlignment variable and make it
 * <p> available through the Command interface.
 * <p>
 * <p> Revision 3.4  2004/06/25 18:04:33  sueh
 * <p> bug# 484 returning default when binByFactor is not set.
 * <p>
 * <p> Revision 3.3  2004/02/18 00:50:32  rickg
 * <p> Check buffer length when deleting trailing comma in getOffsetsInXandY
 * <p>
 * <p> Revision 3.2  2004/02/14 00:16:12  rickg
 * <p> Updated for PIP based newstack, fixed return values where
 * <p> internal objects were returned.
 * <p>
 * <p> Revision 3.1  2004/02/13 01:04:08  rickg
 * <p> Updated for PIP based newstack
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.2  2003/10/02 18:57:47  sueh
 * <p> bug236 added testing:
 * <p> NewstParamTest
 * <p> ComScriptTest
 * <p>
 * <p> Removed marks
 * <p>
 * <p> Revision 2.1  2003/09/29 23:34:57  sueh
 * <p> bug236 Added UseLinearInterpolation to
 * <p> TomogramGenerationDialog.
 * <p>
 * <p> UseLinearInterpolation:
 * <p> check box
 * <p> Advanced
 * <p> newst -linear
 * <p>
 * <p> Files:
 * <p> ComScriptManager.java
 * <p> ConstNewstParam.java
 * <p> NewstParam.java
 * <p> TomogramGenerationDialog.java
 * <p> ApplicationManager.java
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

public interface ConstNewstParam extends CommandDetails {
  public static final String rcsid = "$Id$";

  /**
   * @return Returns the binByFactor.
   */
  public int getBinByFactor();

  /**
   * @return Returns the floatDensities.
   */
  public int getFloatDensities();

  /**
   * Backward compatibility with pre PIP structure, just return the first input
   * file
   * @return Returns the inputFile.
   */
  public String getInputFile();

  /**
   * @return Returns the linearInterpolation.
   */
  public boolean isLinearInterpolation();

  /**
   * @return Returns the modeToOutput.
   */
  public int getModeToOutput();

  /**
   * Backward compatibility with pre PIP structure, just return the first ouput
   * file
   * @return Returns the inputFile.
   */
  public String getOutputFile();

  public int getSizeToOutputInX();

  public int getSizeToOutputInY();

  public boolean isSizeToOutputInXandYSet();

  public boolean fillValueEquals(int value);

  public String getOffsetInX();

  public String getOffsetInY();

  public boolean isAntialiasFilterNull();

  public String getAntialiasFilter();
}