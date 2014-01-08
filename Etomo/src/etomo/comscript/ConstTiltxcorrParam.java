package etomo.comscript;

/**
 * <p>Description: A read only model of the parameter interface for the
 *  tiltxcorr program</p>
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
 * <p> Revision 3.23  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 3.22  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 3.21  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.20  2009/03/24 20:26:49  sueh
 * <p> bug# 1201 Added angleOffset.
 * <p>
 * <p> Revision 3.19  2007/11/06 19:09:34  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 3.18  2007/02/05 21:42:27  sueh
 * <p> bug# 962 Changed getCommandMode to return CommandMode.
 * <p>
 * <p> Revision 3.17  2006/06/05 16:05:24  sueh
 * <p> bug# 766 In ProcessName:  Changed getCommand() and getCommandArray() to
 * <p> getComscript... because the fuctions are specialized for comscripts.
 * <p>
 * <p> Revision 3.16  2006/05/22 22:36:55  sueh
 * <p> bug# 577 Changed getCommand() to conform with the Command interface.
 * <p>
 * <p> Revision 3.15  2006/05/11 19:42:11  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 3.14  2006/04/06 18:56:26  sueh
 * <p> bug# 808 Implementing ProcessDetails.
 * <p>
 * <p> Revision 3.13  2006/01/20 20:46:33  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 3.12  2005/11/19 01:52:21  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 3.11  2005/11/14 21:19:20  sueh
 * <p> removed extra ;'s.
 * <p>
 * <p> Revision 3.10  2005/08/24 22:33:18  sueh
 * <p> bug# 715 Implemented Command to allow param to be checked in
 * <p> postProcess() and errorProcess().
 * <p>
 * <p> Revision 3.9  2005/03/04 00:09:35  sueh
 * <p> bug# 533 Added the label for the tiltxcorr command in the xcorr.com file.
 * <p>
 * <p> Revision 3.8  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.7  2004/05/03 18:00:51  sueh
 * <p> bug# 418 standardizing get functions
 * <p> And param testing proof of concept
 * <p>
 * <p> Revision 3.6  2004/03/13 02:24:41  sueh
 * <p> bug# 373 getting an empty string for a default FortranInputString
 * <p>
 * <p> Revision 3.5  2004/03/12 20:57:55  sueh
 * <p> bug# 373 Changed reset().
 * <p>
 * <p> Revision 3.4  2004/03/12 19:59:48  sueh
 * <p> bug# 412 added absoluteCosineStretch, cumulativeCorreslation, noCosineStretch,
 * <p> testOutput, xMinAndMax, yMinAndMax, reset()
 * <p>
 * <p> Revision 3.3  2004/01/30 02:11:38  sueh
 * <p> bug# 373 formatting
 * <p>
 * <p> Revision 3.2  2004/01/30 02:08:59  sueh
 * <p> bug# 373 initialized values
 * <p>
 * <p> Revision 3.1  2004/01/30 01:27:09  sueh
 * <p> bug# 373 Changed fields and functions to match autodoc
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/09/09 17:18:47  rickg
 * <p> Changed view list to view range and made it a 2 element integer
 * <p> FortranInputString
 * <p> padPercent, taperPercent, and trim are now integer FortranInputString
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

public interface ConstTiltxcorrParam extends Command {
  public static final String rcsid = "$Id$";

  public String getAngleOffset();

  public String getBordersInXandY();

  public boolean getExcludeCentralPeak();

  public String getFilterRadius2String();

  public String getFilterSigma1String();

  public String getFilterSigma2String();

  public String getPadsInXandYString();

  public String getStartingEndingViews();

  public String getTaperPercentString();

  public String getTestOutput();

  public String getXMaxString();

  public String getXMinString();

  public String getYMaxString();

  public String getYMinString();

  public boolean isAbsoluteCosineStretch();

  public boolean isCumulativeCorrelation();

  public boolean isNoCosineStretch();

  public String getSizeOfPatchesXandY();

  public String getOverlapOfPatchesXandY();

  public boolean isOverlapOfPatchesXandYSet();

  public boolean isSearchMagChanges();

  public boolean isNumberOfPatchesXandYSet();

  public String getNumberOfPatchesXandY();

  public int getIterateCorrelations();

  public String getShiftLimitsXandY();

  public boolean isBoundaryModelSet();

  public boolean isBordersInXandYSet();

  public boolean isFilterRadius2Set();

  public boolean isFilterSigma1Set();

  public boolean isFilterSigma2Set();

  public String getSkipViews();

  public String getViewsWithMagChanges();
}
