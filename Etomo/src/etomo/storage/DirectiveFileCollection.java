package etomo.storage;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import etomo.BaseManager;
import etomo.logic.DatasetTool;
import etomo.logic.UserEnv;
import etomo.storage.DirectiveFile.AttributeName;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAttributeIterator;
import etomo.type.AxisID;
import etomo.type.DirectiveFileType;
import etomo.type.EtomoNumber;
import etomo.type.TiltAngleSpec;
import etomo.type.TiltAngleType;
import etomo.ui.SetupReconInterface;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public class DirectiveFileCollection implements SetupReconInterface {
  public static final String rcsid = "$Id:$";

  private final DirectiveFile[] directiveFileArray = new DirectiveFile[] { null, null,
      null, null };

  private final BaseManager manager;
  private final AxisID axisID;

  private boolean debug = false;

  public DirectiveFileCollection(final BaseManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
  }

  public void setDebug(final boolean input) {
    debug = input;
  }

  /**
   * Returns true if an attribute called name is in any of the directive files.
   * @param parentName
   * @param name
   * @return
   */
  private boolean containsAttribute(final DirectiveFile.AttributeName parentName,
      final String name) {
    for (int i = 0; i < directiveFileArray.length; i++) {
      if (directiveFileArray[i] != null
          && directiveFileArray[i].containsAttribute(parentName, name)) {
        return true;
      }
    }
    for (int i = 0; i < directiveFileArray.length; i++) {
      if (directiveFileArray[i] != null
          && directiveFileArray[i].containsExtraValue(parentName, name)) {
        return true;
      }
    }
    return false;
  }

  private String getValue(final DirectiveFile.AttributeName parentName, final String name) {
    String value = null;
    for (int i = 0; i < directiveFileArray.length; i++) {
      if (directiveFileArray[i] != null
          && directiveFileArray[i].containsAttribute(parentName, name)) {
        value = directiveFileArray[i].getValue(parentName, name);
      }
    }
    if (value == null) {
      for (int i = 0; i < directiveFileArray.length; i++) {
        if (directiveFileArray[i] != null
            && directiveFileArray[i].containsExtraValue(parentName, name)) {
          value = directiveFileArray[i].getExtraValue(parentName, name);
        }
      }
    }
    return value;
  }

  /**
   * Returns true if getValue does not return null.  If getValue does return null then
   * either the attribute wasn't there, or it was there and was overridden (the last time
   * it appeared in the collection there was no value).
   * @param parentName
   * @param name
   * @return
   */
  private boolean isValueSet(final DirectiveFile.AttributeName parentName,
      final String name) {
    return getValue(parentName, name) != null;
  }

  public boolean containsBinning() {
    return containsAttribute(AttributeName.COPY_ARG, DirectiveFile.BINNING_NAME);
  }

  public boolean containsDatasetDirectory() {
    return containsAttribute(DirectiveFile.AttributeName.SETUP_SET,
        DirectiveFile.DATASET_DIRECTORY_NAME);
  }

  public boolean containsDistort() {
    return containsAttribute(AttributeName.COPY_ARG, DirectiveFile.DISTORT_NAME);
  }

  public boolean containsFocus(final AxisID axisID) {
    return containsAttribute(AttributeName.COPY_ARG,
        DirectiveFile.convertAttributeName(axisID, DirectiveFile.FOCUS_NAME));
  }

  public boolean containsGold() {
    return containsAttribute(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.GOLD_NAME);
  }

  public boolean containsGradient() {
    return containsAttribute(AttributeName.COPY_ARG, DirectiveFile.GRADIENT_NAME);
  }

  public boolean containsPixel() {
    return containsAttribute(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.PIXEL_NAME);
  }

  public boolean containsTwodir(final AxisID axisID) {
    return containsAttribute(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.convertAttributeName(axisID, DirectiveFile.TWODIR_NAME));
  }

  public boolean containsRotation() {
    return containsAttribute(AttributeName.COPY_ARG, DirectiveFile.ROTATION_NAME);
  }

  public boolean containsTiltAngleSpec(final AxisID axisID) {
    return containsAttribute(AttributeName.COPY_ARG,
        DirectiveFile.convertAttributeName(axisID, DirectiveFile.FIRST_INC_NAME))
        || containsAttribute(AttributeName.COPY_ARG,
            DirectiveFile.convertAttributeName(axisID, DirectiveFile.USE_RAW_TLT_NAME))
        || containsAttribute(AttributeName.COPY_ARG,
            DirectiveFile.convertAttributeName(axisID, DirectiveFile.EXTRACT_NAME));
  }

  public String getBackupDirectory() {
    return null;
  }

  public String getBinning() {
    return getValue(DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.BINNING_NAME);
  }

  /**
   * Returns an entry set containing the names/value pairs in all of the directive files
   * (one entry per name).  This function will not return null.  A name/value pair with a
   * blank values cause the name/value pair to be removed from the entry set.  The
   * name/value pair will be re-added afterwards if a pair with a non-blank value is
   * encountered.
   * @return
   */
  public CopyArgEntrySet getCopyArgEntrySet() {
    return CopyArgEntrySet.getInstance(directiveFileArray);
  }

  public String getDataset() {
    return getName();
  }

  public String getDatasetDirectory() {
    return getValue(DirectiveFile.AttributeName.SETUP_SET,
        DirectiveFile.DATASET_DIRECTORY_NAME);
  }

  public DirectiveFile getDirectiveFile(final DirectiveFileType type) {
    return directiveFileArray[type.getIndex()];
  }

  public DirectiveFileCollection getDirectiveFileCollection() {
    return this;
  }

  public String getDistortionFile() {
    return getValue(DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.DISTORT_NAME);
  }

  /**
   * @param doValidation has no effect
   */
  public String getExcludeList(final AxisID axisID, final boolean doValidation) {
    return getValue(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.convertAttributeName(axisID, DirectiveFile.SKIP_NAME));
  }

  /**
   * @param doValidation has no effect
   */
  public String getTwodir(final AxisID axisID, final boolean doValidation) {
    return getValue(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.convertAttributeName(axisID, DirectiveFile.TWODIR_NAME));
  }

  public boolean isTwodir(final AxisID axisIDn) {
    return isValueSet(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.convertAttributeName(axisID, DirectiveFile.TWODIR_NAME));
  }

  /**
   * @param doValidation has no effect
   */
  public String getFiducialDiameter(final boolean doValidation) {
    return getValue(DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.GOLD_NAME);
  }

  /**
   * @param doValidation has no effect
   */
  public String getImageRotation(final AxisID axisID, final boolean doValidation) {
    return getValue(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.convertAttributeName(axisID, DirectiveFile.ROTATION_NAME));
  }

  /**
   * Returns binning or defaultRetValue if binning is invalid or missing.
   * @param defaultRetValue
   * @return
   */
  public int getIntBinning(final int defaultRetValue) {
    EtomoNumber binning = new EtomoNumber();
    binning.set(getBinning());
    if (binning.isValid() && !binning.isNull()) {
      return binning.getInt();
    }
    return defaultRetValue;
  }

  public String getMagGradientFile() {
    return getValue(DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.GRADIENT_NAME);
  }

  public String getName() {
    return getValue(DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.NAME_NAME);
  }

  public String getPixelSize(final boolean doValidation) {
    return getValue(DirectiveFile.AttributeName.COPY_ARG, DirectiveFile.PIXEL_NAME);
  }

  public boolean getTiltAngleFields(final AxisID axisID,
      final TiltAngleSpec tiltAngleSpec, final boolean doValidation) {
    tiltAngleSpec.reset();
    for (int i = 0; i < directiveFileArray.length; i++) {
      if (directiveFileArray[i] != null && containsTiltAngleSpec(axisID)) {
        directiveFileArray[i].getTiltAngleFields(axisID, tiltAngleSpec, doValidation);
      }
    }
    return true;
  }

  public boolean isAdjustedFocusSelected(final AxisID axisID) {
    return DirectiveFile.toBoolean(getValue(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.convertAttributeName(axisID, DirectiveFile.FOCUS_NAME)));
  }

  public boolean isDual() {
    return DirectiveFile.toBoolean(getValue(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.DUAL_NAME));
  }

  public boolean containsDual() {
    return containsAttribute(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.DUAL_NAME);
  }

  public boolean isDualAxisSelected() {
    return isDual();
  }

  public boolean isMontage() {
    return DirectiveFile.toBoolean(getValue(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.MONTAGE_NAME));
  }

  public boolean containsMontage() {
    return containsAttribute(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.MONTAGE_NAME);
  }

  public boolean isGpuProcessingSelected(final String propertyUserDir) {
    return UserEnv.isGpuProcessing(manager, axisID, propertyUserDir);
  }

  public boolean isParallelProcessSelected(final String propertyUserDir) {
    return UserEnv.isParallelProcessing(manager, axisID, propertyUserDir);
  }

  public boolean isScanHeader() {
    return DirectiveFile.toBoolean(getValue(DirectiveFile.AttributeName.SETUP_SET,
        DirectiveFile.SCAN_HEADER_NAME));
  }

  public boolean isSingleAxisSelected() {
    return !isDual();
  }

  public boolean isSingleViewSelected() {
    return !DirectiveFile.toBoolean(getValue(DirectiveFile.AttributeName.COPY_ARG,
        DirectiveFile.MONTAGE_NAME));
  }

  public void setBatchDirectiveFile(final DirectiveFile baseDirectiveFile) {
    directiveFileArray[DirectiveFileType.BATCH.getIndex()] = baseDirectiveFile;
    if (baseDirectiveFile != null) {
      setDirectiveFile(baseDirectiveFile.getScopeTemplate(), DirectiveFileType.SCOPE);
      setDirectiveFile(baseDirectiveFile.getSystemTemplate(), DirectiveFileType.SYSTEM);
      setDirectiveFile(baseDirectiveFile.getUserTemplate(), DirectiveFileType.USER);
    }
  }

  public void setBinning(final int input) {
    if (directiveFileArray[DirectiveFileType.BATCH.getIndex()] != null) {
      directiveFileArray[DirectiveFileType.BATCH.getIndex()].setBinning(input);
    }
  }

  public void setTwodir(final AxisID axisID, final double input) {
    if (directiveFileArray[DirectiveFileType.BATCH.getIndex()] != null) {
      directiveFileArray[DirectiveFileType.BATCH.getIndex()].setTwodir(axisID, input);
    }
  }

  public void setImageRotation(final String input) {
    if (directiveFileArray[DirectiveFileType.BATCH.getIndex()] != null) {
      directiveFileArray[DirectiveFileType.BATCH.getIndex()].setImageRotation(input);
    }
  }

  public void setPixelSize(final double input) {
    if (directiveFileArray[DirectiveFileType.BATCH.getIndex()] != null) {
      directiveFileArray[DirectiveFileType.BATCH.getIndex()].setPixelSize(input);
    }
  }

  public void setDirectiveFile(final String absPath, final DirectiveFileType type) {
    if (absPath == null) {
      directiveFileArray[type.getIndex()] = null;
    }
    else {
      directiveFileArray[type.getIndex()] = DirectiveFile.getInstance(manager, axisID,
          new File(absPath));
    }
  }

  public void setDirectiveFile(final File file, final DirectiveFileType type) {
    if (file == null) {
      directiveFileArray[type.getIndex()] = null;
    }
    else {
      directiveFileArray[type.getIndex()] = DirectiveFile.getInstance(manager, axisID,
          file);
    }
  }

  public boolean validateTiltAngle(final AxisID axisID, final String errorTitle) {
    TiltAngleSpec tiltAngleSpec = new TiltAngleSpec();
    getTiltAngleFields(axisID, tiltAngleSpec, false);
    return DatasetTool.validateTiltAngle(manager, AxisID.ONLY, errorTitle, axisID,
        tiltAngleSpec.getType() == TiltAngleType.RANGE,
        String.valueOf(tiltAngleSpec.getRangeMin()),
        String.valueOf(tiltAngleSpec.getRangeStep()));
  }

  public static final class CopyArgEntrySet {
    private final Map<String, String> pairMap = new HashMap<String, String>();

    /**
     * Don't call constructor directly.
     */
    private CopyArgEntrySet() {
    }

    /**
     * This function should not return null.
     * @return initialized instance
     */
    private static CopyArgEntrySet getInstance(final DirectiveFile[] directiveFileArray) {
      CopyArgEntrySet instance = new CopyArgEntrySet();
      instance.init(directiveFileArray);
      return instance;
    }

    /**
     * Load all of the directive file copyarg values into pairMap.  Pairs with the same
     * name as a previously saved pair overrides the previous pair.  A pair with a blank
     * value is not saved and causes the pair with the same name in the map to be removed.
     * After loading all of copyarg values, load the scan header output from the directive
     * file if scan header is in the map and is set to "1".  Only load pairs with names
     * that are not already in the map, because the directive files all override scan
     * header.
     * @param directiveFileArray
     */
    private void init(final DirectiveFile[] directiveFileArray) {
      for (int i = 0; i < directiveFileArray.length; i++) {
        if (directiveFileArray[i] != null) {
          ReadOnlyAttributeIterator iterator = directiveFileArray[i].getCopyArgIterator();
          if (iterator == null) {
            continue;
          }
          while (iterator.hasNext()) {
            ReadOnlyAttribute attribute = iterator.next();
            String name = attribute.getName();
            String value = attribute.getValue();
            pairMap.remove(name);
            // A blank value means remove the previously added pair, otherwise override
            // the previous added pair with the new value.
            if (value != null) {
              pairMap.put(name, value);
            }
          }
        }
      }
      // If scan header (only found in the batch directive file and is not in copyarg) is
      // true, then get values from scanning the header which aren't already in the map.
      if (directiveFileArray[DirectiveFileType.BATCH.getIndex()] != null
          && directiveFileArray[DirectiveFileType.BATCH.getIndex()].isScanHeader()) {
        Iterator<Entry<String, String>> iterator = directiveFileArray[DirectiveFileType.BATCH
            .getIndex()].getCopyArgExtraValuesIterator();
        while (iterator.hasNext()) {
          Entry<String, String> entry = iterator.next();
          String name = entry.getKey();
          if (!pairMap.containsKey(name)) {
            pairMap.put(name, entry.getValue());
          }
        }
      }
    }

    public Iterator<Entry<String, String>> iterator() {
      return pairMap.entrySet().iterator();
    }
  }
}
