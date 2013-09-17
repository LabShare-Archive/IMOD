package etomo.type;

import java.util.Properties;

import etomo.ui.LogProperties;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.1  2009/10/27 20:39:47  sueh
 * <p> bug# 1275 Meta data for FrontPageManager.  MetaData is a required class
 * <p> for a manager.
 * <p> </p>
 */
public final class FrontPageMetaData extends BaseMetaData {

  private static final String NAME = "Front Page";
  private static final String FRONT_PAGE_GROUP_KEY = "FrontPage";

  /**
   * For testing
   */
  private String name = null;

  public FrontPageMetaData(final LogProperties logProperties) {
    super(logProperties);
    axisType = AxisType.SINGLE_AXIS;
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]\n";
  }

  String paramString() {
    return FRONT_PAGE_GROUP_KEY;
  }

  /**
   * for testing
   * @param axisType
   */
  public void setAxisType(final AxisType axisType) {
    this.axisType = axisType;
  }

  /**
   * For testing
   * @param name
   */
  public void setName(String name) {
    this.name = name;
  }

  public String getName() {
    // for testing
    if (name != null) {
      return name;
    }
    return NAME;
  }

  public String getDatasetName() {
    // for testing
    if (name != null) {
      return name;
    }
    return NAME;
  }

  public String getMetaDataFileName() {
    // for testing
    if (name != null) {
      return name;
    }
    return NAME;
  }

  public boolean isValid() {
    return true;
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    super.load(props, prepend);
    // reset
    // load
    prepend = createPrepend(prepend);
    String group = prepend + ".";
  }

  public void store(Properties props, String prepend) {
    super.store(props, prepend);
    prepend = createPrepend(prepend);
    String group = prepend + ".";
  }

  String getGroupKey() {
    return FRONT_PAGE_GROUP_KEY;
  }
}
