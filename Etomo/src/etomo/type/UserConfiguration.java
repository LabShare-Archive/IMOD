package etomo.type;

import etomo.storage.Storable;
import etomo.util.CircularBuffer;
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

public class UserConfiguration implements Storable {
  public static final String rcsid =
    "$Id$";

  private String revisionNumber = "1.2";
  private boolean nativeLookAndFeel = false;
  private boolean advancedDialogs = false;
  private int toolTipsInitialDelay = 2000;
  private int toolTipsDismissDelay = 20000;
  private int nMRUFiles = 4;
  private CircularBuffer MRUFileList;
  private String fontFamily = "Dialog";
  private int fontSize = 12;
  private int mainWindowWidth = 800;
  private int mainWindowHeight = 600;
  private boolean autoFit = false;

  public UserConfiguration() {
    MRUFileList = new CircularBuffer(4);
    for (int i = 0; i < nMRUFiles; i++) {
      MRUFileList.put("");
    }
  }
  
  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "\n,revisionNumber=" + revisionNumber + ",\nnativeLookAndFeel="
        + nativeLookAndFeel + ",\nadvancedDialogs=" + advancedDialogs
        + ",\ntoolTipsInitialDelay=" + toolTipsInitialDelay
        + ",\ntoolTipsDismissDelay=" + toolTipsDismissDelay + ",\nnMRUFiles="
        + nMRUFiles + ",\nMRUFileList=" + MRUFileList.toString()
        + ",\nfontFamily=" + fontFamily + ",\nfontSize=" + fontSize
        + ",\nmainWindowWidth=" + mainWindowWidth + ",\nmainWindowHeight="
        + mainWindowHeight + ",\nautoFit=" + autoFit;
  } 


  /**
   *  Insert the objects attributes into the properties object.
   */
  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = "";
    }
    else {
      group = prepend + ".Setup.";
    }
    props.setProperty(group + "RevisionNumber", revisionNumber);
    props.setProperty(
      group + "NativeLookAndFeel",
      String.valueOf(nativeLookAndFeel));
    props.setProperty(
      group + "AdvancedDialogs",
      String.valueOf(advancedDialogs));
    props.setProperty(
      group + "ToolTipsInitialDelay",
      String.valueOf(toolTipsInitialDelay));
    props.setProperty(
      group + "ToolTipsDismissDelay",
      String.valueOf(toolTipsDismissDelay));
    props.setProperty(group + "FontFamily", String.valueOf(fontFamily));
    props.setProperty(group + "FontSize", String.valueOf(fontSize));

    props.setProperty(
      group + "MainWindowWidth",
      String.valueOf(mainWindowWidth));
    props.setProperty(
      group + "MainWindowHeight",
      String.valueOf(mainWindowHeight));

    props.setProperty(group + "NMRUFiles", String.valueOf(nMRUFiles));
    for (int i = 0; i < nMRUFiles; i++) {
      props.setProperty(
        group + "EtomoDataFile" + String.valueOf(i),
        (String) MRUFileList.get());
    }
    
    props.setProperty(group + "AutoFit", String.valueOf(autoFit));
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    //
    //  Construct the parameter group from the prepend string "User"
    //  if a prepend string is not empty
    //
    String group;
    if (prepend == "") {
      group = "";
    }
    else {
      group = prepend + ".User.";
    }

    //
    //  Get the user configuration data from the Properties object
    //
    revisionNumber = props.getProperty(group + "RevisionNumber", "1.0");
    nativeLookAndFeel =
      Boolean
        .valueOf(props.getProperty(group + "NativeLookAndFeel", "false"))
        .booleanValue();
    advancedDialogs =
      Boolean
        .valueOf(props.getProperty(group + "AdvancedDialogs", "false"))
        .booleanValue();
    toolTipsInitialDelay =
      Integer.parseInt(
        props.getProperty(group + "ToolTipsInitialDelay", "1000"));
    toolTipsDismissDelay =
      Integer.parseInt(
        props.getProperty(group + "ToolTipsDismissDelay", "30000"));
    fontFamily = props.getProperty(group + "FontFamily", "Dialog");
    fontSize = Integer.parseInt(props.getProperty(group + "FontSize", "12"));

    mainWindowWidth =
      Integer.parseInt(props.getProperty(group + "MainWindowWidth", "800"));
    mainWindowHeight =
      Integer.parseInt(props.getProperty(group + "MainWindowHeight", "600"));

    nMRUFiles = Integer.parseInt(props.getProperty(group + "NMRUFiles", "4"));
    MRUFileList = new CircularBuffer(nMRUFiles);
    for (int i = nMRUFiles - 1; i >= 0; i--) {
      MRUFileList.put(
        props.getProperty("EtomoDataFile" + String.valueOf(i), ""));
    }
    autoFit =
      Boolean
        .valueOf(props.getProperty(group + "AutoFit", "false"))
        .booleanValue();

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

  /**
   * Set the advanced dialog state
   */
  public void setAdvancedDialogs(boolean state) {
    advancedDialogs = state;
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

}
