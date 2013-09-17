package etomo.type;

import etomo.storage.Storable;
import etomo.util.CircularBuffer;

import java.awt.Point;
import java.io.File;
import java.util.*;

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
 * <p> Revision 3.15  2011/02/22 05:54:56  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 3.14  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.13  2010/06/02 21:44:48  sueh
 * <p> bug# 1380 Added lastLocationX and Y.
 * <p>
 * <p> Revision 3.12  2010/03/09 01:41:54  sueh
 * <p> bug# 1323 Doing backwards compatibility for parameters which had been saved with a key that started with a ".".
 * <p>
 * <p> Revision 3.11  2010/01/11 23:58:30  sueh
 * <p> bug# 1299 Added gpuProcessing.
 * <p>
 * <p> Revision 3.10  2009/03/05 23:25:40  sueh
 * <p> bug 1194 Added logProperties.
 * <p>
 * <p> Revision 3.9  2008/12/01 22:23:14  sueh
 * <p> bug# 1131 Added montage.
 * <p>
 * <p> Revision 3.8  2008/10/06 22:37:17  sueh
 * <p> bug# 1113 Added parallelTableSize, joinTableSize, and peetTableSize.
 * <p>
 * <p> Revision 3.7  2008/09/10 21:33:48  sueh
 * <p> Attempting to increase the size of the remembered file list.
 * <p>
 * <p> Revision 3.6  2008/07/19 00:51:01  sueh
 * <p> bug# 1125 Added parallelProcessing and cpus.
 * <p>
 * <p> Revision 3.5  2007/08/08 14:53:00  sueh
 * <p> bug# 834 Added singleAxis, noParallelProcessing, tiltAnglesRawltlFile, and
 * <p> swapYAndZ.
 * <p>
 * <p> Revision 3.4  2006/07/31 21:42:35  sueh
 * <p> bug# 438 Added compactDisplay
 * <p>
 * <p> Revision 3.3  2005/05/13 18:49:08  sueh
 * <p> bug# 615 Temporarily setting autofit on all the time.
 * <p>
 * <p> Revision 3.2  2004/11/19 23:40:52  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.1.2.2  2004/10/13 22:58:30  sueh
 * <p> bug# 520 fixed paramString()
 * <p>
 * <p> Revision 3.1.2.1  2004/10/11 02:08:06  sueh
 * <p> bug# 520 Added toString() function for debugging purposes.
 * <p>
 * <p> Revision 3.1  2004/07/22 23:58:59  sueh
 * <p> bug# 513 saving autofit property
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/01/28 00:16:24  rickg
 * <p> Main window now remembers its size
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3  2002/12/11 21:27:33  rickg
 * <p> Implemented font for user config
 * <p>
 * <p> Revision 1.2  2002/12/11 00:36:17  rickg
 * <p> Added revision number
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public final class UserConfiguration implements Storable {
  public static final String rcsid = "$Id$";

  private static final String COMPACT_DISPLAY_KEY = "CompactDisplay";
  private static final String DEFAULTS_KEY = "Defaults";
  private static final String CURRENT_REVISION_NUMBER = "1.3";
  private static final String MONTAGE_KEY = DEFAULTS_KEY + ".Montage";
  private static final String PARALLEL_PROCESSING_KEY = "ParallelProcessing";
  private static final String SINGLE_AXIS_KEY = DEFAULTS_KEY + ".SingleAxis";
  private static final String NO_PARALLEL_PROCESSING_KEY = DEFAULTS_KEY
      + ".NoParallelProcessing";
  private static final String GPU_PROCESSING_DEFAULT_KEY = DEFAULTS_KEY
      + ".GpuProcessingDefault";
  private static final String SWAP_Y_AND_Z_KEY = DEFAULTS_KEY + ".SwapYAndZ";
  private static final String TILT_ANGLES_RAWTLT_FILE_KEY = DEFAULTS_KEY
      + ".TiltAnglesRawtltFile";
  
  private final EtomoVersion revisionNumber = EtomoVersion.getInstance("RevisionNumber",
      CURRENT_REVISION_NUMBER);
  private final EtomoBoolean2 gpuProcessing = new EtomoBoolean2("GpuProcessing");
  private final EtomoNumber cpus = new EtomoNumber("Cpus");
  private final EtomoNumber parallelTableSize = new EtomoNumber("ParallelTableSize");
  private final EtomoNumber joinTableSize = new EtomoNumber("JoinTableSize");
  private final EtomoNumber peetTableSize = new EtomoNumber("PeetTableSize");
  private final EtomoNumber mainLastLocationX = new EtomoNumber("Main.LastLocationX");
  private final EtomoNumber mainLastLocationY = new EtomoNumber("Main.LastLocationY");
  private final EtomoNumber subLastLocationX = new EtomoNumber("Sub.LastLocationX");
  private final EtomoNumber subLastLocationY = new EtomoNumber("Sub.LastLocationY");
  private final EtomoBoolean2 setFEIPixelSize = new EtomoBoolean2(DEFAULTS_KEY
      + ".SetFEIPixelSize");
  private final StringProperty userTemplateDirAbsPath = new StringProperty(DEFAULTS_KEY
      + ".UserTemplateDir", true);
  private final StringProperty scopeTemplateAbsPath = new StringProperty(DEFAULTS_KEY
      + ".ScopeTemplate", true);
  private final StringProperty systemTemplateAbsPath = new StringProperty(DEFAULTS_KEY
      + ".SystemTemplate", true);
  private final StringProperty userTemplateAbsPath = new StringProperty(DEFAULTS_KEY
      + ".UserTemplate", true);

  private boolean nativeLookAndFeel = false;
  private boolean advancedDialogs = false;
  private boolean compactDisplay = false;
  private int toolTipsInitialDelay = 2000;
  private int toolTipsDismissDelay = 20000;
  private int nMRUFiles = 10;
  private CircularBuffer MRUFileList;
  private String fontFamily = "Dialog";
  private int fontSize = 12;
  private int mainWindowWidth = 800;
  private int mainWindowHeight = 600;
  private boolean autoFit = false;
  private EtomoBoolean2 montage = null;
  private EtomoBoolean2 parallelProcessing = null;
  private EtomoBoolean2 singleAxis = null;
  private EtomoBoolean2 noParallelProcessing = null;
  private EtomoBoolean2 gpuProcessingDefault = null;
  private EtomoBoolean2 swapYAndZ = null;
  private EtomoBoolean2 tiltAnglesRawtltFile = null;


  public UserConfiguration() {
    MRUFileList = new CircularBuffer(nMRUFiles);
    for (int i = 0; i < nMRUFiles; i++) {
      MRUFileList.put("");
    }
    parallelTableSize.setDisplayValue(15);
    joinTableSize.setDisplayValue(10);
    peetTableSize.setDisplayValue(10);
    setFEIPixelSize.setDisplayValue(true);
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "\n,revisionNumber=" + revisionNumber + ",\nnativeLookAndFeel="
        + nativeLookAndFeel + ",\nadvancedDialogs=" + advancedDialogs
        + ",\ntoolTipsInitialDelay=" + toolTipsInitialDelay + ",\ntoolTipsDismissDelay="
        + toolTipsDismissDelay + ",\nnMRUFiles=" + nMRUFiles + ",\nMRUFileList="
        + MRUFileList.toString() + ",\nfontFamily=" + fontFamily + ",\nfontSize="
        + fontSize + ",\nmainWindowWidth=" + mainWindowWidth + ",\nmainWindowHeight="
        + mainWindowHeight + ",\nautoFit=" + autoFit;
  }

  /**
   *  Insert the objects attributes into the properties object.
   */
  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    String origPrepend = prepend;
    prepend = getPrepend(prepend);
    String group = getGroup(prepend);

    revisionNumber.set(CURRENT_REVISION_NUMBER);
    revisionNumber.store(props, origPrepend);
    // props.setProperty(group + "RevisionNumber", revisionNumber);
    props.setProperty(group + "NativeLookAndFeel", String.valueOf(nativeLookAndFeel));
    props.setProperty(group + "AdvancedDialogs", String.valueOf(advancedDialogs));
    props.setProperty(group + COMPACT_DISPLAY_KEY, String.valueOf(compactDisplay));
    props.setProperty(group + "ToolTipsInitialDelay",
        String.valueOf(toolTipsInitialDelay));
    props.setProperty(group + "ToolTipsDismissDelay",
        String.valueOf(toolTipsDismissDelay));
    props.setProperty(group + "FontFamily", String.valueOf(fontFamily));
    props.setProperty(group + "FontSize", String.valueOf(fontSize));
    EtomoBoolean2.store(singleAxis, props, prepend, SINGLE_AXIS_KEY);
    EtomoBoolean2.store(montage, props, prepend, MONTAGE_KEY);
    EtomoBoolean2.store(noParallelProcessing, props, prepend, NO_PARALLEL_PROCESSING_KEY);
    EtomoBoolean2.store(gpuProcessingDefault, props, prepend, GPU_PROCESSING_DEFAULT_KEY);
    EtomoBoolean2
        .store(tiltAnglesRawtltFile, props, prepend, TILT_ANGLES_RAWTLT_FILE_KEY);
    EtomoBoolean2.store(swapYAndZ, props, prepend, SWAP_Y_AND_Z_KEY);
    EtomoBoolean2.store(parallelProcessing, props, prepend, PARALLEL_PROCESSING_KEY);
    gpuProcessing.store(props, prepend);
    cpus.store(props, prepend);
    parallelTableSize.store(props, prepend);
    joinTableSize.store(props, prepend);
    peetTableSize.store(props, prepend);
    mainLastLocationX.store(props, prepend);
    mainLastLocationY.store(props, prepend);
    subLastLocationX.store(props, prepend);
    subLastLocationY.store(props, prepend);
    setFEIPixelSize.store(props, prepend);
    userTemplateDirAbsPath.store(props, prepend);
    scopeTemplateAbsPath.store(props, prepend);
    systemTemplateAbsPath.store(props, prepend);
    userTemplateAbsPath.store(props, prepend);

    props.setProperty(group + "MainWindowWidth", String.valueOf(mainWindowWidth));
    props.setProperty(group + "MainWindowHeight", String.valueOf(mainWindowHeight));

    props.setProperty(group + "NMRUFiles", String.valueOf(nMRUFiles));
    for (int i = 0; i < nMRUFiles; i++) {
      props.setProperty(group + "EtomoDataFile" + String.valueOf(i),
          (String) MRUFileList.get());
    }

    props.setProperty(group + "AutoFit", String.valueOf(autoFit));
  }

  public void load(Properties props) {
    load(props, "");
  }

  /**
   * modify prepend to include "Setup"
   * @param prepend
   * @return
   */
  private String getPrepend(String prepend) {
    if (prepend == null || prepend.matches("\\s*")) {
      return "";
    }
    return prepend + ".Setup";
  }

  /**
   * Run on the output of getPrepend
   * @param modifiedPrepend
   * @return
   */
  private String getGroup(String modifiedPrepend) {
    if (modifiedPrepend == "") {
      return "";
    }
    return modifiedPrepend + ".";
  }

  public void load(Properties props, String prepend) {
    String origPrepend = prepend;
    prepend = getPrepend(prepend);
    String group = getGroup(prepend);
    //
    // Get the user configuration data from the Properties object
    //
    revisionNumber.load(props, origPrepend);
    parallelProcessing = EtomoBoolean2.load(parallelProcessing, PARALLEL_PROCESSING_KEY,
        props, prepend);
    montage = EtomoBoolean2.load(montage, MONTAGE_KEY, props, prepend);
    singleAxis = EtomoBoolean2.load(singleAxis, SINGLE_AXIS_KEY, props, prepend);
    noParallelProcessing = EtomoBoolean2.load(noParallelProcessing,
        NO_PARALLEL_PROCESSING_KEY, props, prepend);
    gpuProcessingDefault = EtomoBoolean2.load(gpuProcessingDefault,
        GPU_PROCESSING_DEFAULT_KEY, props, prepend);
    parallelTableSize.load(props, prepend);
    swapYAndZ = EtomoBoolean2.load(swapYAndZ, SWAP_Y_AND_Z_KEY, props, prepend);
    cpus.load(props, prepend);
    tiltAnglesRawtltFile = EtomoBoolean2.load(tiltAnglesRawtltFile,
        TILT_ANGLES_RAWTLT_FILE_KEY, props, prepend);
    joinTableSize.load(props, prepend);
    peetTableSize.load(props, prepend);
    // Backward compatibility
    if (revisionNumber.le(EtomoVersion.getDefaultInstance("1.2"))) {
      // Fields where mistakenly saved with a "." in front of them - use backward
      // compatibility to fix.
      if (parallelProcessing == null) {

        parallelProcessing = EtomoBoolean2.load(parallelProcessing,
            PARALLEL_PROCESSING_KEY, props, ".");
      }
      if (montage == null) {
        montage = EtomoBoolean2.load(montage, MONTAGE_KEY, props, ".");
      }
      if (singleAxis == null) {
        singleAxis = EtomoBoolean2.load(singleAxis, SINGLE_AXIS_KEY, props, ".");
      }
      if (noParallelProcessing == null) {
        noParallelProcessing = EtomoBoolean2.load(noParallelProcessing,
            NO_PARALLEL_PROCESSING_KEY, props, ".");
      }
      if (parallelTableSize.isNull()) {
        parallelTableSize.load(props, ".");
      }
      if (swapYAndZ == null) {
        swapYAndZ = EtomoBoolean2.load(swapYAndZ, SWAP_Y_AND_Z_KEY, props, ".");
      }
      if (cpus.isNull()) {
        cpus.load(props, ".");
      }
      if (tiltAnglesRawtltFile == null) {
        tiltAnglesRawtltFile = EtomoBoolean2.load(tiltAnglesRawtltFile,
            TILT_ANGLES_RAWTLT_FILE_KEY, props, ".");
      }
      if (joinTableSize.isNull()) {
        joinTableSize.load(props, ".");
      }
      if (peetTableSize.isNull()) {
        peetTableSize.load(props, ".");
      }
    }
    // revisionNumber = props.getProperty(group + "RevisionNumber", "1.0");
    nativeLookAndFeel = Boolean.valueOf(
        props.getProperty(group + "NativeLookAndFeel", "false")).booleanValue();
    advancedDialogs = Boolean.valueOf(
        props.getProperty(group + "AdvancedDialogs", "false")).booleanValue();
    compactDisplay = Boolean.valueOf(
        props.getProperty(group + COMPACT_DISPLAY_KEY, "false")).booleanValue();
    toolTipsInitialDelay = Integer.parseInt(props.getProperty(group
        + "ToolTipsInitialDelay", "1000"));
    toolTipsDismissDelay = Integer.parseInt(props.getProperty(group
        + "ToolTipsDismissDelay", "30000"));
    fontFamily = props.getProperty(group + "FontFamily", "Dialog");
    fontSize = Integer.parseInt(props.getProperty(group + "FontSize", "12"));

    mainWindowWidth = Integer.parseInt(props
        .getProperty(group + "MainWindowWidth", "800"));
    mainWindowHeight = Integer.parseInt(props.getProperty(group + "MainWindowHeight",
        "600"));

    nMRUFiles = Integer.parseInt(props.getProperty(group + "NMRUFiles", "10"));
    MRUFileList = new CircularBuffer(nMRUFiles);
    for (int i = nMRUFiles - 1; i >= 0; i--) {
      MRUFileList.put(props.getProperty("EtomoDataFile" + String.valueOf(i), ""));
    }
    autoFit = Boolean.valueOf(props.getProperty(group + "AutoFit", "false"))
        .booleanValue();
    // TEMP bug# 614
    autoFit = true;
    gpuProcessing.load(props, prepend);
    mainLastLocationX.load(props, prepend);
    if (mainLastLocationX.isNull()) {
      mainLastLocationX.loadFromOtherKey(props, prepend, "LastLocationX");
    }
    mainLastLocationY.load(props, prepend);
    if (mainLastLocationY.isNull()) {
      mainLastLocationY.loadFromOtherKey(props, prepend, "LastLocationY");
    }
    subLastLocationX.load(props, prepend);
    subLastLocationY.load(props, prepend);
    setFEIPixelSize.load(props, prepend);
    userTemplateDirAbsPath.load(props, prepend);
    scopeTemplateAbsPath.load(props, prepend);
    systemTemplateAbsPath.load(props, prepend);
    userTemplateAbsPath.load(props, prepend);
  }

  /**
   * Put a etomo data file onto the MRU list if it is not already there
   */
  public void putDataFile(String filename) {
    if (MRUFileList.search(filename) == -1) {
      MRUFileList.put(filename);
    }
  }

  /**
   * Get the next etomo data file from the MRU list
   */
  public String getDataFile() {
    return (String) MRUFileList.get();
  }

  /**
   * Get the advanced dialog state
   */
  public boolean getAdvancedDialogs() {
    return advancedDialogs;
  }

  public boolean getCompactDisplay() {
    return compactDisplay;
  }

  /**
   * Set the advanced dialog state
   */
  public void setAdvancedDialogs(boolean state) {
    advancedDialogs = state;
  }

  public void setCompactDisplay(boolean state) {
    compactDisplay = state;
  }

  /**
   * Get the native look and feel state
   */
  public boolean getNativeLookAndFeel() {
    return nativeLookAndFeel;
  }

  /**
   * Set the native look and feel state
   */
  public void setNativeLookAndFeel(boolean state) {
    nativeLookAndFeel = state;
  }

  /**
   * Get the ToolTips initial delay
   */
  public int getToolTipsInitialDelay() {
    return toolTipsInitialDelay;
  }

  /**
   * Set the ToolTips initial delay
   */
  public void setToolTipsInitialDelay(int milliSeconds) {
    toolTipsInitialDelay = milliSeconds;
  }

  /**
   * Get the ToolTips dismiss delay
   */
  public int getToolTipsDismissDelay() {
    return toolTipsDismissDelay;
  }

  /**
   * Set the ToolTips dismiss delay
   */
  public void setToolTipsDismissDelay(int milliSeconds) {
    toolTipsDismissDelay = milliSeconds;
  }

  /**
   * Get the number of MRU files stored
   */
  public int getNMRUFIles() {
    return nMRUFiles;
  }

  /**
   * Set the number of MRU files stored
   */
  public void setNMRUFIles(int nFiles) {
    nMRUFiles = nFiles;

  }

  /**
   * Get the MRU file list at a string array
   */
  public String[] getMRUFileList() {
    String[] list = new String[MRUFileList.size()];

    for (int i = 0; i < MRUFileList.size(); i++) {
      list[i] = (String) MRUFileList.get();
    }
    return list;
  }

  /**
   * Returns the fontFamily.
   * @return String
   */
  public String getFontFamily() {
    return fontFamily;
  }

  /**
   * Returns the fontSize.
   * @return int
   */
  public int getFontSize() {
    return fontSize;
  }

  /**
   * Sets the fontFamily.
   * @param fontFamily The fontFamily to set
   */
  public void setFontFamily(String fontFamily) {
    this.fontFamily = fontFamily;
  }

  /**
   * Sets the fontSize.
   * @param fontSize The fontSize to set
   */
  public void setFontSize(int fontSize) {
    this.fontSize = fontSize;
  }

  /**
   * Returns the mainWindowHeight.
   * @return int
   */
  public int getMainWindowHeight() {
    return mainWindowHeight;
  }

  /**
   * Returns the mainWindowWidth.
   * @return int
   */
  public int getMainWindowWidth() {
    return mainWindowWidth;
  }

  /**
   * Sets the mainWindowHeight.
   * @param mainWindowHeight The mainWindowHeight to set
   */
  public void setMainWindowHeight(int mainWindowHeight) {
    this.mainWindowHeight = mainWindowHeight;
  }

  /**
   * Sets the mainWindowWidth.
   * @param mainWindowWidth The mainWindowWidth to set
   */
  public void setMainWindowWidth(int mainWindowWidth) {
    this.mainWindowWidth = mainWindowWidth;
  }

  /**
   * 
   * @return
   */
  public boolean isAutoFit() {
    return autoFit;
  }

  /**
   * 
   * @param autoFit
   */
  public void setAutoFit(boolean autoFit) {
    this.autoFit = autoFit;
  }

  public ConstEtomoNumber getCpus() {
    return cpus;
  }

  public ConstEtomoNumber getParallelTableSize() {
    return parallelTableSize;
  }

  public ConstEtomoNumber getJoinTableSize() {
    return joinTableSize;
  }

  public ConstEtomoNumber getPeetTableSize() {
    return peetTableSize;
  }

  public boolean isLastLocationSet(final FrameType frameType) {
    if (frameType == FrameType.Main) {
      return !mainLastLocationX.isNull() && !mainLastLocationY.isNull();
    }
    if (frameType == FrameType.Sub) {
      return !subLastLocationX.isNull() && !subLastLocationY.isNull();
    }
    return false;
  }

  public int getLastLocationX(final FrameType frameType) {
    if (frameType == FrameType.Main) {
      return mainLastLocationX.getInt();
    }
    if (frameType == FrameType.Sub) {
      return subLastLocationX.getInt();
    }
    return EtomoNumber.INTEGER_NULL_VALUE;
  }

  public int getLastLocationY(final FrameType frameType) {
    if (frameType == FrameType.Main) {
      return mainLastLocationY.getInt();
    }
    if (frameType == FrameType.Sub) {
      return subLastLocationY.getInt();
    }
    return EtomoNumber.INTEGER_NULL_VALUE;
  }

  public void setLastLocation(final FrameType frameType, Point point) {
    if (point == null) {
      if (frameType == FrameType.Main) {
        subLastLocationX.reset();
        subLastLocationY.reset();
      }
      else if (frameType == FrameType.Sub) {
        mainLastLocationX.reset();
        mainLastLocationY.reset();
      }
    }
    else {
      if (frameType == FrameType.Main) {
        mainLastLocationX.set(point.x);
        mainLastLocationY.set(point.y);
      }
      else if (frameType == FrameType.Sub) {
        subLastLocationX.set(point.x);
        subLastLocationY.set(point.y);
      }
    }
  }

  public boolean isParallelProcessing() {
    if (parallelProcessing == null) {
      return false;
    }
    return parallelProcessing.is();
  }

  public boolean isGpuProcessing() {
    return gpuProcessing.is();
  }

  public boolean getNoParallelProcessing() {
    if (noParallelProcessing == null) {
      return false;
    }
    return noParallelProcessing.is();
  }

  public boolean getGpuProcessingDefault() {
    if (gpuProcessingDefault == null) {
      return false;
    }
    return gpuProcessingDefault.is();
  }

  public boolean getSingleAxis() {
    if (singleAxis == null) {
      return false;
    }
    return singleAxis.is();
  }

  public boolean getMontage() {
    if (montage == null) {
      return false;
    }
    return montage.is();
  }

  public boolean getSwapYAndZ() {
    if (swapYAndZ == null) {
      return false;
    }
    return swapYAndZ.is();
  }

  public boolean isSetFEIPixelSize() {
    return setFEIPixelSize.is();
  }

  public boolean isUserTemplateDirSet() {
    return !userTemplateDirAbsPath.isEmpty();
  }

  public String getUserTemplateDir() {
    return userTemplateDirAbsPath.toString();
  }

  public String getScopeTemplate() {
    return scopeTemplateAbsPath.toString();
  }

  public boolean equalsScopeTemplate(final File input) {
    String absPath = null;
    if (input != null) {
      absPath = input.getAbsolutePath();
    }
    return !scopeTemplateAbsPath.equals(absPath);
  }

  public boolean equalsSystemTemplate(final File input) {
    String absPath = null;
    if (input != null) {
      absPath = input.getAbsolutePath();
    }
    return !systemTemplateAbsPath.equals(absPath);
  }

  public boolean equalsUserTemplate(final File input) {
    String absPath = null;
    if (input != null) {
      absPath = input.getAbsolutePath();
    }
    return !userTemplateAbsPath.equals(absPath);
  }

  public boolean isScopeTemplateSet() {
    return !scopeTemplateAbsPath.isEmpty();
  }

  public boolean isSystemTemplateSet() {
    return !systemTemplateAbsPath.isEmpty();
  }

  public boolean isUserTemplateSet() {
    return !userTemplateAbsPath.isEmpty();
  }

  public String getSystemTemplate() {
    return systemTemplateAbsPath.toString();
  }

  public String getUserTemplate() {
    return userTemplateAbsPath.toString();
  }

  public boolean getTiltAnglesRawtltFile() {
    if (tiltAnglesRawtltFile == null) {
      return false;
    }
    return tiltAnglesRawtltFile.is();
  }

  public void setParallelProcessing(boolean input) {
    if (parallelProcessing == null) {
      parallelProcessing = new EtomoBoolean2(PARALLEL_PROCESSING_KEY);
    }
    parallelProcessing.set(input);
  }

  public void setGpuProcessing(boolean input) {
    gpuProcessing.set(input);
  }

  public void setCpus(String input) {
    cpus.set(input);
  }

  public void setParallelTableSize(String input) {
    parallelTableSize.set(input);
  }

  public void setJoinTableSize(String input) {
    joinTableSize.set(input);
  }

  public void setPeetTableSize(String input) {
    peetTableSize.set(input);
  }

  public void setNoParallelProcessing(boolean input) {
    if (noParallelProcessing == null) {
      noParallelProcessing = new EtomoBoolean2(NO_PARALLEL_PROCESSING_KEY);
    }
    noParallelProcessing.set(input);
  }

  public void setGpuProcessingDefault(boolean input) {
    if (gpuProcessingDefault == null) {
      gpuProcessingDefault = new EtomoBoolean2(GPU_PROCESSING_DEFAULT_KEY);
    }
    gpuProcessingDefault.set(input);
  }

  public void setSingleAxis(boolean input) {
    if (singleAxis == null) {
      singleAxis = new EtomoBoolean2(SINGLE_AXIS_KEY);
    }
    singleAxis.set(input);
  }

  public void setMontage(boolean input) {
    if (montage == null) {
      montage = new EtomoBoolean2(MONTAGE_KEY);
    }
    montage.set(input);
  }

  public void setSwapYAndZ(boolean input) {
    if (swapYAndZ == null) {
      swapYAndZ = new EtomoBoolean2(SWAP_Y_AND_Z_KEY);
    }
    swapYAndZ.set(input);
  }

  public void setSetFEIPixelSize(boolean input) {
    setFEIPixelSize.set(input);
  }

  public void setUserTemplateDir(final File input) {
    if (input != null) {
      userTemplateDirAbsPath.set(input.getAbsolutePath());
    }
    else {
      userTemplateDirAbsPath.reset();
    }
  }

  public void setUserTemplate(final File input) {
    if (input != null) {
      userTemplateAbsPath.set(input.getAbsolutePath());
    }
    else {
      userTemplateAbsPath.reset();
    }
  }

  public void setSystemTemplate(final File input) {
    if (input != null) {
      systemTemplateAbsPath.set(input.getAbsolutePath());
    }
    else {
      systemTemplateAbsPath.reset();
    }
  }

  public void setScopeTemplate(final File input) {
    if (input != null) {
      scopeTemplateAbsPath.set(input.getAbsolutePath());
    }
    else {
      scopeTemplateAbsPath.reset();
    }
  }

  public void setTiltAnglesRawtltFile(boolean input) {
    if (tiltAnglesRawtltFile == null) {
      tiltAnglesRawtltFile = new EtomoBoolean2(TILT_ANGLES_RAWTLT_FILE_KEY);
    }
    tiltAnglesRawtltFile.set(input);
  }
}
