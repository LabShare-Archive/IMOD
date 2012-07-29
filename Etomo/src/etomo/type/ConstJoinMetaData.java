package etomo.type;

import java.util.ArrayList;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.17  2009/09/20 21:29:04  sueh
 * <p> bug# 1268 Reformatted.
 * <p>
 * <p> Revision 1.16  2008/08/18 22:37:20  sueh
 * <p> bug# 1130 Added isLocalFits.
 * <p>
 * <p> Revision 1.15  2007/12/10 22:34:49  sueh
 * <p> bug# 1041 Made class an interface so inheritance can come from BaseMetaData.
 * <p>
 * <p> Revision 1.14  2007/07/30 22:39:30  sueh
 * <p> bug# 963 Added DatasetFiles.JOIN_DATA_FILE_EXT.
 * <p>
 * <p> Revision 1.13  2007/03/01 01:24:24  sueh
 * <p> bug# 964 Saving immutable Number elements instead of EtomoNumber elements
 * <p> in IntKeyList.
 * <p>
 * <p> Revision 1.12  2007/02/08 02:02:39  sueh
 * <p> bug# 962 Added rejoinTrialBinning and rejoinUseEveryNSlices.
 * <p>
 * <p> Revision 1.11  2007/02/05 23:24:16  sueh
 * <p> bug# 962 Added Model and Rejoin fields.
 * <p>
 * <p> Revision 1.10  2005/12/14 01:28:27  sueh
 * <p> bug# 782 Updated toString().
 * <p>
 * <p> Revision 1.9  2005/11/02 23:59:35  sueh
 * <p> bug# 738 Added midas limit.
 * <p>
 * <p> Revision 1.8  2005/07/29 19:46:49  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 1.7  2005/05/10 02:24:23  sueh
 * <p> bug# 658 Added ScriptParameter.useDefaultAsDisplayValue() to set
 * <p> displayValue equal to defaultValue.  When default is used, these are
 * <p> usually the same.
 * <p>
 * <p> Revision 1.6  2005/01/25 21:58:38  sueh
 * <p> Converting EtomoNumbers parameters to ScriptParameters.
 * <p>
 * <p> Revision 1.5  2004/12/16 02:27:49  sueh
 * <p> bug# 564 Remove recommendedValue.  Use resetValue instead.
 * <p>
 * <p> Revision 1.4  2004/12/14 21:43:50  sueh
 * <p> bug# 572:  Removing state object from meta data and managing it with a
 * <p> manager class.  All state variables saved after a process is run belong in
 * <p> the state object.
 * <p>
 * <p> Revision 1.3  2004/12/04 01:00:17  sueh
 * <p> bug# 569 Fixed the check to see if working directory is empty in isValid()
 * <p>
 * <p> Revision 1.2  2004/11/19 23:33:42  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.22  2004/11/19 03:04:15  sueh
 * <p> bug# 520 Setting displayDefault to default to true for the shift variables and
 * <p> most of the spinners.
 * <p>
 * <p> Revision 1.1.2.21  2004/11/19 00:09:15  sueh
 * <p> bug# 520 Fixed null pointer bug in store
 * <p>
 * <p> Revision 1.1.2.20  2004/11/17 02:22:20  sueh
 * <p> bug# 520 Created a isValid() function that takes workingDirName, so the
 * <p> working dir name in join dialog can be tested before it is placed in
 * <p> propertyUserDir and used to create paramFile.
 * <p>
 * <p> Revision 1.1.2.19  2004/11/16 02:26:22  sueh
 * <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
 * <p> EtomoLong with EtomoNumber.
 * <p>
 * <p> Revision 1.1.2.18  2004/11/15 22:20:26  sueh
 * <p> bug# 520 Moved all of file validation to Utilities so that is called be called
 * <p> from other places.
 * <p>
 * <p> Revision 1.1.2.17  2004/11/13 02:38:18  sueh
 * <p> bug# 520 Added sampleProduced state boolean.
 * <p>
 * <p> Revision 1.1.2.16  2004/11/12 22:58:00  sueh
 * <p> bug# 520 Added finishjoinTrial values:  binning, size, and shift.
 * <p>
 * <p> Revision 1.1.2.15  2004/11/11 01:37:13  sueh
 * <p> bug# 520 Added useEveryNSlices and trialBinning.
 * <p>
 * <p> Revision 1.1.2.14  2004/11/08 22:22:09  sueh
 * <p> bug# 520 Remove default from shift in X and Y.
 * <p>
 * <p> Revision 1.1.2.13  2004/10/29 22:12:55  sueh
 * <p> bug# 520  Added removeSectionTableData() to remove section table data
 * <p> rows from the meta data file before adding them.  This gets rid of deleted
 * <p> rows.
 * <p>
 * <p> Revision 1.1.2.12  2004/10/29 01:19:29  sueh
 * <p> bug# 520 Removing workingDir.  Calling isValid with workingDir.  Moving
 * <p> file validations to Utilities.
 * <p>
 * <p> Revision 1.1.2.11  2004/10/25 23:09:16  sueh
 * <p> bug# 520 Added get functions.
 * <p>
 * <p> Revision 1.1.2.10  2004/10/22 21:02:12  sueh
 * <p> bug# 520 Simplifying by passing EtomoSimpleType instead of String and
 * <p> int in get functions.
 * <p>
 * <p> Revision 1.1.2.9  2004/10/22 03:22:52  sueh
 * <p> bug# 520 Reducing the number of ConstJoinMetaData functions by
 * <p> passing EtomoInteger, EtomoFloat, etc and using their get() and
 * <p> getString() functions.
 * <p>
 * <p> Revision 1.1.2.8  2004/10/21 02:50:06  sueh
 * <p> bug# 520 Added get functions.
 * <p>
 * <p> Revision 1.1.2.7  2004/10/18 18:01:46  sueh
 * <p> bug# 520 Added fields from JoinDialog.  Converted densityRefSection to
 * <p> an EtomoInteger.  Added validation checks for rootName and workingDir.
 * <p>
 * <p> Revision 1.1.2.6  2004/10/15 00:17:15  sueh
 * <p> bug# 520 Added toString().  Fixed createPrepend().
 * <p>
 * <p> Revision 1.1.2.5  2004/10/14 02:28:12  sueh
 * <p> bug# 520 Added getWorkingDir().
 * <p>
 * <p> Revision 1.1.2.4  2004/10/11 02:07:17  sueh
 * <p> bug# 520 Fixed a bug in ConstMetaData where the open edf file menu
 * <p> item wasn't working because it was validating the propertyUserDir of the
 * <p> current manager, not the parent of the edf file being opened.  Now able
 * <p> to pass in the edf file to get the parent from to use in validation.
 * <p>
 * <p> Revision 1.1.2.3  2004/10/06 01:54:45  sueh
 * <p> bug# 520 Removed Use density reference checkbox.  Created
 * <p> isValidForMakeSamples() which validates for the situation when Make
 * <p> Samples is pressed.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/01 19:45:26  sueh
 * <p> bug# 520 Define a new join string that will go in the menu.  Set a file
 * <p> extension value.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/29 19:17:41  sueh
 * <p> bug# 520 The const part of the JoinMetaData class.  Implements
 * <p> storable with abstract load functions.  Contains member variables and
 * <p> get functions.
 * <p> </p>
 */
public interface ConstJoinMetaData {
  public static final String rcsid = "$Id$";

  public ConstEtomoNumber getAlignmentRefSection();

  public String getBoundariesToAnalyze();

  public int getCoordinate(ConstEtomoNumber coordinate, JoinState state)
      throws NullRequiredNumberException;

  public String getDatasetName();

  public ConstEtomoNumber getDensityRefSection();

  public boolean isUseAlignmentRefSection();

  public ConstEtomoNumber getShiftInX();

  public ConstEtomoNumber getSizeInX();

  public ConstEtomoNumber getShiftInY();

  public ConstEtomoNumber getSizeInY();

  public boolean isLocalFits();

  public ConstEtomoNumber getUseEveryNSlices();

  public ConstEtomoNumber getRejoinUseEveryNSlices();

  public ConstEtomoNumber getTrialBinning();

  public Transform getModelTransform();

  public ConstEtomoNumber getMidasLimit();

  public String getObjectsToInclude();

  public ConstEtomoNumber getGap();

  public ConstEtomoNumber getGapStart();

  public ConstEtomoNumber getGapEnd();

  public ConstEtomoNumber getGapInc();

  public ConstEtomoNumber getPointsToFitMin();

  public ConstEtomoNumber getPointsToFitMax();

  public ConstEtomoNumber getRejoinTrialBinning();

  public ConstEtomoNumber getBoundaryRowEnd(int key);

  public boolean isBoundaryRowEndListEmpty();

  public ArrayList getSectionTableData();

  public IntKeyList.Walker getBoundaryRowEndListWalker();

  public IntKeyList.Walker getBoundaryRowStartListWalker();

  public ScriptParameter getSizeInXParameter();

  public ScriptParameter getSizeInYParameter();

  public ScriptParameter getShiftInXParameter();

  public ScriptParameter getShiftInYParameter();

  public ScriptParameter getRejoinTrialBinningParameter();

  public ScriptParameter getTrialBinningParameter();

  public AutoAlignmentMetaData getAutoAlignmentMetaData();

  public String getName();

  public ScriptParameter getDensityRefSectionParameter();
}