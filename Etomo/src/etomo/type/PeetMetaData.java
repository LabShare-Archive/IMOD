package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.util.DatasetFiles;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.5  2007/03/01 01:26:47  sueh
 * <p> bug# 964 removed unnecesary protected modifier
 * <p>
 * <p> Revision 1.4  2007/02/21 22:29:31  sueh
 * <p> bug# 964 Fixing validation.
 * <p>
 * <p> Revision 1.3  2007/02/21 04:21:02  sueh
 * <p> bug# 964 Added validations.
 * <p>
 * <p> Revision 1.2  2007/02/20 20:35:50  sueh
 * <p> bug# 964 Added setName, to set the root name.
 * <p>
 * <p> Revision 1.1  2007/02/19 21:59:45  sueh
 * <p> bug# 964 Meta data for the PEET interface.
 * <p> </p>
 */
public class PeetMetaData extends BaseMetaData implements ConstPeetMetaData {
  public static final String rcsid = "$Id$";

  public static final String NEW_TITLE = "PEET";

  private static final String ROOT_NAME_KEY = "RootName";
  private static final String TILT_RANGE_KEY = "TiltRange";
  private static final String GROUP_KEY = "Peet";
  private String rootName = "";
  private IntKeyList initMotlFile = IntKeyList.getStringInstance("InitMotlFile");
  private IntKeyList tiltRangeStart = IntKeyList.getStringInstance("TILT_RANGE_KEY"+".Start");
  private IntKeyList tiltRangeEnd = IntKeyList.getStringInstance("TILT_RANGE_KEY"+".End");

  public PeetMetaData() {
    fileExtension = DatasetFiles.PEET_DATA_FILE_EXT;
  }

  public String getMetaDataFileName() {
    if (rootName == null) {
      return null;
    }
    return DatasetFiles.getParallelDataFileName(rootName);
  }

  public String getName() {
    if (rootName.equals("")) {
      return NEW_TITLE;
    }
    return rootName;
  }

  public void setName(String name) {
    rootName = name;
  }

  public boolean isValid() {
    return validate() == null;
  }

  /**
   * returns null if valid
   * @return error message if invalid
   */
  public String validate() {
    if (rootName.equals("") || rootName.matches("\\s*")) {
      return "Missing root name.";
    }
    if (rootName.indexOf(File.pathSeparatorChar) != -1
        || rootName.indexOf(File.separatorChar) != -1) {
      return "Invalid root name, " + rootName + ".";
    }
    return null;
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    reset();
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    rootName = props.getProperty(group + ROOT_NAME_KEY,"");
    initMotlFile.load(props,prepend);
    tiltRangeStart.load(props,prepend);
    tiltRangeEnd.load(props,prepend);
  }

  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    props.setProperty(group + ROOT_NAME_KEY, rootName);
    initMotlFile.store(props,prepend);
    tiltRangeStart.store(props,prepend);
    tiltRangeEnd.store(props,prepend);
  }
  
  public String getInitMotlFile(int key) {
    return initMotlFile.getString(key);
  }
  
  public void setInitMotlFile(String initMotlFile,int key) {
    this.initMotlFile.put(key,initMotlFile);
  }
  
  public void setTiltRangeStart(String tiltRangeStart,int key) {
    this.tiltRangeStart.put(key,tiltRangeStart);
  }
  
  public void setTiltRangeEnd(String tiltRangeEnd,int key) {
    this.tiltRangeEnd.put(key,tiltRangeEnd);
  }
  
  public String getTiltRangeStart(int key) {
    return tiltRangeStart.getString(key);
  }
  
  public String getTiltRangeEnd(int key) {
    return tiltRangeEnd.getString(key);
  }

  private static String createPrepend(String prepend) {
    if (prepend == "") {
      return GROUP_KEY;
    }
    return prepend + "." + GROUP_KEY;
  }

  private void reset() {
    initMotlFile.reset();
    tiltRangeStart.reset();
    tiltRangeEnd.reset();
  }
}
