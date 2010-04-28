/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright(c) 2002, 2003, 2004</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$
 * <p> $Revision 1.9  2005/11/03 00:49:47  sueh
 * <p> $bug# 740 Added functions getEndingZ, getStartingZ, isEndingZSet, and
 * <p> $isStartingZSet.
 * <p> $
 * <p> $Revision 1.8  2005/01/26 04:26:37  sueh
 * <p> $bug# 83 Added getOutputFile().
 * <p> $
 * <p> $Revision 1.7  2004/06/14 23:25:55  rickg
 * <p> $Bug #383  ParamUtilities interface change.
 * <p> $$
 * <p> Revision 1.6  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p> 
 * <p> Revision 1.5  2004/04/16 01:48:50  sueh
 * <p> bug# 409 added startingAndEndingZ (Starting and Ending Views)
 * <p> 
 * <p> Revision 1.4  2004/04/12 17:13:46  sueh
 * <p> bug# 409  Change HighFrequencyRadiusSigma to LowPassRadiusSigma.  reset()
 * <p> should unset values, not set to comscript defaults.
 * <p> 
 * <p> Revision 1.3  2004/03/29 20:47:47  sueh
 * <p> bug# 409 user cannot change output file
 * <p> 
 * <p> Revision 1.2  2004/03/25 00:43:23  sueh
 * <p> bug# 409, bug# 418 remove default, add InverseRolloffRadiusSigma, use
 * <p> ParamUtilities
 * <p> 
 * <p> Revision 1.1  2004/03/24 18:15:18  sueh
 * <p> bug# 409 MTF Filter const params
 * </p>
 */
package etomo.comscript;

public interface ConstMTFFilterParam extends Command {
  public static final String rcsid = "$$Id$$";

  public String getMtfFile();

  public String getMaximumInverseString();

  public String getLowPassRadiusSigmaString();

  public String getStartingAndEndingZString();

  public boolean isStartingZSet();

  public boolean isEndingZSet();

  public int getStartingZ();

  public int getEndingZ();

  public String getInverseRolloffRadiusSigmaString();

  public String getOutputFile();
}
