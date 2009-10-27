package etomo.type;

import java.util.Properties;

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
 * <p> $Log$ </p>
 */
public final class FrontPageMetaData extends BaseMetaData {

  private static final String NAME = "Front Page";
  private static final String FRONT_PAGE_GROUP_KEY = "FrontPage";

  public FrontPageMetaData() {
    super();
    axisType = AxisType.SINGLE_AXIS;
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]\n";
  }

  String paramString() {
    return FRONT_PAGE_GROUP_KEY;
  }

  public String getName() {
      return NAME;
  }

  public String getDatasetName() {
    return NAME;
  }

  public String getMetaDataFileName() {
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
    //reset
    //load
    prepend = createPrepend(prepend);
    String group = prepend + ".";
  }

  public void store(Properties props, String prepend) {
    super.store(props, prepend);
    prepend = createPrepend(prepend);
    String group = prepend + ".";
  }

  String createPrepend(String prepend) {
    String groupKey;
    groupKey = FRONT_PAGE_GROUP_KEY;
    if (prepend == "") {
      return groupKey;
    }
    return prepend + "." + groupKey;
  }
}
