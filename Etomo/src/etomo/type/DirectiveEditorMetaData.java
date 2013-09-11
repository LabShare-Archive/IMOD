package etomo.type;

import java.io.File;

import etomo.ui.LogProperties;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
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
public final class DirectiveEditorMetaData extends BaseMetaData {
  public static final String rcsid = "$Id:$";
  private final DirectiveFileType type;

  private String rootName = null;

  public DirectiveEditorMetaData(final DirectiveFileType type,
      final LogProperties logProperties) {
    super(logProperties);
    this.type = type;
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
    // Directive editor projects are not saved
    return null;
  }

  public String getName() {
    if (rootName == null) {
      if (type == null) {
        return "Directive File Editor";
      }
      return type.getLabel() + " Editor";
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
