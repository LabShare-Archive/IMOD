package etomo.type;

import java.util.ArrayList;
import java.util.Properties;

import etomo.EtomoDirector;

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
* <p> Revision 1.1.2.12  2004/11/16 02:28:23  sueh
* <p> bug# 520 Replacing EtomoSimpleType, EtomoInteger, EtomoDouble,
* <p> EtomoFloat, and EtomoLong with EtomoNumber.
* <p>
* <p> Revision 1.1.2.11  2004/11/15 22:24:04  sueh
* <p> bug# 520 Added setSampleProduced().
* <p>
* <p> Revision 1.1.2.10  2004/11/13 02:38:28  sueh
* <p> bug# 520 Added sampleProduced state boolean.
* <p>
* <p> Revision 1.1.2.9  2004/11/12 22:59:25  sueh
* <p> bug# 520 Added finishjoinTrial values:  binning, size, and shift.
* <p>
* <p> Revision 1.1.2.8  2004/11/11 01:37:30  sueh
* <p> bug# 520 Added useEveryNSlices and trialBinning.
* <p>
* <p> Revision 1.1.2.7  2004/10/29 01:19:48  sueh
* <p> bug# 520 Removing workingDir.
* <p>
* <p> Revision 1.1.2.6  2004/10/22 21:06:25  sueh
* <p> bug# 520 Changed offsetInX, Y to shiftInX, Y.
* <p>
* <p> Revision 1.1.2.5  2004/10/18 18:05:52  sueh
* <p> bug# 520 Added fields from JoinDialog.  Converted densityRefSection to
* <p> an EtomoInteger.
* <p>
* <p> Revision 1.1.2.4  2004/10/15 00:30:02  sueh
* <p> bug# 520 Fixed load().  Added the rowNumber to the
* <p> SectionTableRowData constructor because rowNumber is used to store
* <p> values.
* <p>
* <p> Revision 1.1.2.3  2004/10/08 16:23:40  sueh
* <p> bug# 520 Make sure  sectionTableData exists before it is used.
* <p>
* <p> Revision 1.1.2.2  2004/10/06 02:14:13  sueh
* <p> bug# 520 Removed Use density reference checkbox.  Changed string
* <p> default to "", since their default when coming from store() is "".  Added
* <p> variables to the load() function.
* <p>
* <p> Revision 1.1.2.1  2004/09/29 19:28:03  sueh
* <p> bug# 520 Meta data for serial sections.  Non-const class implements load
* <p> and set functions.
* <p> </p>
*/
public class JoinMetaData extends ConstJoinMetaData {
  public static final String rcsid = "$Id$";

  public JoinMetaData() {
    reset();
  }

  private void reset() {
    revisionNumber = "";
    sectionTableData = null;
    densityRefSection.reset();
    rootName = "";
    sigmaLowFrequency.reset();
    cutoffHighFrequency.reset();
    sigmaHighFrequency.reset();
    fullLinearTransformation = defaultFullLinearTransformation;
    rotationTranslationMagnification = false;
    rotationTranslation = false;
    useAlignmentRefSection = false;
    alignmentRefSection.reset();
    sizeInX.reset();
    sizeInY.reset();
    shiftInX.reset();
    shiftInY.reset();
    trialBinning.reset();
    finishjoinTrialBinning.reset();
    finishjoinTrialSizeInX.reset();
    finishjoinTrialSizeInY.reset();
    finishjoinTrialShiftInX.reset();
    finishjoinTrialShiftInY.reset();
    sampleProduced = defaultSampleProduced;
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    reset();
    prepend = createPrepend(prepend);
    String group = prepend + ".";

    revisionNumber = props.getProperty(group + revisionNumberString, latestRevisionNumber);
    rootName = props.getProperty(group + rootNameString, "");
    densityRefSection.load(props, prepend);
    sigmaLowFrequency.load(props, prepend);
    cutoffHighFrequency.load(props, prepend);
    sigmaHighFrequency.load(props, prepend);
    fullLinearTransformation = Boolean.valueOf(props.getProperty(group
        + fullLinearTransformationString, Boolean.toString(defaultFullLinearTransformation))).booleanValue();
    rotationTranslationMagnification = Boolean.valueOf(props.getProperty(group
        + rotationTranslationMagnificationString, "false")).booleanValue();
    rotationTranslation = Boolean.valueOf(props.getProperty(group
        + rotationTranslationString, "false")).booleanValue();
    useAlignmentRefSection = Boolean.valueOf(props.getProperty(group
        + useAlignmentRefSectionString, "false")).booleanValue();
    alignmentRefSection.load(props, prepend);
    sizeInX.load(props, prepend);
    sizeInY.load(props, prepend);
    shiftInX.load(props, prepend);
    shiftInY.load(props, prepend);
    useEveryNSlices.load(props, prepend);
    trialBinning.load(props, prepend);
    finishjoinTrialBinning.load(props, prepend);
    finishjoinTrialSizeInX.load(props, prepend);
    finishjoinTrialSizeInY.load(props, prepend);
    finishjoinTrialShiftInX.load(props, prepend);
    finishjoinTrialShiftInY.load(props, prepend);
    sampleProduced = Boolean.valueOf(props.getProperty(group
        + sampleProducedString, Boolean.toString(defaultSampleProduced))).booleanValue();
    
    int sectionTableRowsSize = Integer.parseInt(props.getProperty(group
        + sectionTableDataSizeString, "-1"));
    if (sectionTableRowsSize < 1) {
      return;
    }
    sectionTableData = new ArrayList(sectionTableRowsSize);
    for (int i = 0; i < sectionTableRowsSize; i++) {
      SectionTableRowData row = new SectionTableRowData(i + 1);
      row.load(props, prepend);
      int rowIndex = row.getRowIndex();
      if (rowIndex < 0) {
        EtomoDirector.getInstance().getMainFrame().openMessageDialog(
            "Invalid row index: " + rowIndex, "Corrupted .ejf file");
      }
      sectionTableData.add(row.getRowIndex(), row);
    }
  }

  public void setDensityRefSection(Object densityRefSection) {
    this.densityRefSection.set((Integer) densityRefSection);
  }
  
  public void setFinishjoinTrialBinning(int trialBinning) {
    finishjoinTrialBinning.set(trialBinning);
  }
  
  public void setFinishjoinTrialShiftInX(int trialShiftInX) {
    finishjoinTrialShiftInX.set(trialShiftInX);
  }
  
  public void setFinishjoinTrialShiftInY(int trialShiftInY) {
    finishjoinTrialShiftInY.set(trialShiftInY);
  }
  
  public void setFinishjoinTrialSizeInX(int trialSizeInX) {
    finishjoinTrialSizeInX.set(trialSizeInX);
  }
  
  public void setFinishjoinTrialSizeInY(int trialSizeInY) {
    finishjoinTrialSizeInY.set(trialSizeInY);
  }
  
  public void setUseEveryNSlices(Object useEveryNSlices) {
    this.useEveryNSlices.set((Integer) useEveryNSlices);
  }
  
  public void setTrialBinning(Object trialBinning) {
    this.trialBinning.set((Integer) trialBinning);
  }

  public void setRootName(String rootName) {
    this.rootName = rootName;
  }

  public void resetSectionTableData() {
    sectionTableData = null;
  }

  public void setSectionTableData(ConstSectionTableRowData row) {
    if (sectionTableData == null) {
      sectionTableData = new ArrayList();
    }
    sectionTableData.add(row);
  }
  
  public ConstEtomoNumber setSigmaLowFrequency(String sigmaLowFrequency) {
    return this.sigmaLowFrequency.set(sigmaLowFrequency);
  }
  public void setCutoffHighFrequency(String cutoffHighFrequency) {
    this.cutoffHighFrequency.set(cutoffHighFrequency);
  }
  public void setSigmaHighFrequency(String sigmaHighFrequency) {
    this.sigmaHighFrequency.set(sigmaHighFrequency);
  }
  public void setFullLinearTransformation(boolean fullLinearTransformation) {
    this.fullLinearTransformation = fullLinearTransformation;
  }
  public void setRotationTranslationMagnification(boolean rotationTranslationMagnification) {
    this.rotationTranslationMagnification = rotationTranslationMagnification;
  }
  
  public void setSampleProduced(boolean sampleProduced) {
    this.sampleProduced = sampleProduced;
  }
  
  public void setRotationTranslation(boolean rotationTranslation) {
    this.rotationTranslation = rotationTranslation;
  }
  public void setUseAlignmentRefSection(boolean useAlignmentRefSection) {
    this.useAlignmentRefSection = useAlignmentRefSection;
  }
  public void setAlignmentRefSection(Object alignmentRefSection) {
    this.alignmentRefSection.set((Integer) alignmentRefSection);
  }
  public ConstEtomoNumber setSizeInX(String sizeInX) {
    return this.sizeInX.set(sizeInX);
  }
  public void setSizeInY(String sizeInY) {
    this.sizeInY.set(sizeInY);
  }
  public void setShiftInX(String shiftInX) {
    this.shiftInX.set(shiftInX);
  }
  public void setShiftInY(String shiftInY) {
    this.shiftInY.set(shiftInY);
  }
}