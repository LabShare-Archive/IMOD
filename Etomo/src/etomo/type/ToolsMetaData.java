package etomo.type;

import java.io.File;

import etomo.ui.LogProperties;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2010</p>
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
public final class ToolsMetaData extends BaseMetaData {
  public static final String rcsid = "$Id$";

  private final DialogType dialogType;
  private final ToolType toolType;

  private String rootName = null;

  public ToolsMetaData(DialogType dialogType, ToolType toolType,
      final LogProperties logProperties) {
    super(logProperties);
    this.dialogType = dialogType;
    this.toolType = toolType;
    axisType = AxisType.SINGLE_AXIS;
  }

  String getGroupKey() {
    return null;
  }

  String createPrepend(String prepend) {
    return null;
  }

  public String getDatasetName() {
    return rootName;
  }

  public void setRootName(File file) {
    rootName = file.getName();
  }

  public String getMetaDataFileName() {
    // Tools projects are not saved
    return null;
  }

  public String getName() {
    if (rootName == null) {
      return toolType.toString();
    }
    return rootName;
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
}
