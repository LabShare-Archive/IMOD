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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class UserConfiguration implements Storable {
  public static final String rcsid =
    "$Id$";

  private String revisionNumber = "1.0";
  private boolean nativeLookAndFeel = false;
  private boolean advancedDialogs = false;
  private int toolTipsInitialDelay = 500;
  private int toolTipsDismissDelay = 120000;
  private int nMRUFiles = 4;
  private CircularBuffer MRUFileList;

  public UserConfiguration() {
    MRUFileList = new CircularBuffer(4);
    for (int i = 0; i < nMRUFiles; i++) {
      MRUFileList.put("");
    }
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
    props.setProperty(group + "NMRUFiles", String.valueOf(nMRUFiles));
    for (int i = 0; i < nMRUFiles; i++) {
      props.setProperty(
        group + "EtomoDataFile" + String.valueOf(i),
        (String) MRUFileList.get());
    }

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
    nMRUFiles = Integer.parseInt(props.getProperty(group + "NMRUFiles", "4"));
    MRUFileList = new CircularBuffer(nMRUFiles);
    for (int i = nMRUFiles - 1; i >= 0; i--) {
      MRUFileList.put(
        props.getProperty("EtomoDataFile" + String.valueOf(i), ""));
    }
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
}
