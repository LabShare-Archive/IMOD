package etomo.type;

import java.util.Properties;

import etomo.util.DatasetFiles;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class ParallelMetaData extends BaseMetaData {
  public static final String rcsid = "$Id$";

  private static final String REVISION_KEY = "Revision";
  private static final String CURRENT_REVISION = "1.0";
  private static final String NEW_TITLE = "Parallel Processing";
  private static final String GROUP_KEY = "Parallel";
  private static final String ROOT_NAME_KEY = "RootName";

  private String revision = null;
  private String rootName = null;

  public ParallelMetaData() {
    super();
    axisType = AxisType.SINGLE_AXIS;
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]\n";
  }

  protected String paramString() {
    return "revision=" + revision + ",rootName=" + rootName;
  }

  private void reset() {
    revision = null;
    rootName = null;
  }

  public static String getNewTitle() {
    return NEW_TITLE;
  }

  public String getName() {
    if (rootName == null) {
      return NEW_TITLE;
    }
    return rootName;
  }

  public String getMetaDataFileName() {
    if (rootName == null) {
      return null;
    }
    return DatasetFiles.getParallelDataFileName(rootName);
  }

  public boolean isValid() {
    return validate() == null;
  }

  /**
   * returns null if valid
   * @return error message if invalid
   */
  public String validate() {
    if (rootName == null) {
      return "Missing root name.";
    }
    return null;
  }

  public void setRootName(String rootName) {
    this.rootName = rootName;
  }

  public String getRootName() {
    return rootName;
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    reset();
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    revision = props.getProperty(group + REVISION_KEY, CURRENT_REVISION);
    rootName = props.getProperty(group + ROOT_NAME_KEY);
  }

  public void store(Properties props, String prepend) {
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    props.setProperty(group + REVISION_KEY, CURRENT_REVISION);
    props.setProperty(group + ROOT_NAME_KEY, rootName);
  }

  protected static String createPrepend(String prepend) {
    if (prepend == "") {
      return GROUP_KEY;
    }
    return prepend + "." + GROUP_KEY;
  }
}
/**
 * <p> $Log$ </p>
 */