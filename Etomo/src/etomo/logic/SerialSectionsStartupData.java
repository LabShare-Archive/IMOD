package etomo.logic;

import java.io.File;

import etomo.type.DataFileType;
import etomo.type.EnumeratedType;
import etomo.type.ViewType;

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
public final class SerialSectionsStartupData {
  public static final String rcsid = "$Id:$";

  private File stack = null;
  private String stackLabel = null;
  private ViewType viewType = null;
  private String viewTypeLabel = null;
  private File distortionFile = null;
  private Number binning = null;

  public void setStack(final File input, final String label) {
    stack = input;
    stackLabel = label;
  }

  public void setViewType(final EnumeratedType input, final String label) {
    viewType = ViewType.getInstance(input);
    viewTypeLabel = label;
  }

  public void setDistortionFile(final File input) {
    distortionFile = input;
  }

  public void setBinning(final Number input) {
    binning = input;
  }

  /**
   * Validation
   * @return an error message or null if valid
   */
  public String validate() {
    if (stack == null) {
      return "Missing required entry: " + stackLabel == null ? "stack" : stackLabel + ".";
    }
    if (viewType == null) {
      return "Missing required entry: " + viewTypeLabel == null ? "view type"
          : viewTypeLabel + ".";
    }
    return null;
  }

  /**
   * The root name is the file name of the stack member variable, minus the extension.
   * @return
   */
  public String getRootName() {
    if (stack == null) {
      return null;
    }
    String name = stack.getName();
    int index = name.lastIndexOf('.');
    if (index == -1) {
      return name;
    }
    return name.substring(0, index);
  }

  /**
   * Builds and returns the param file
   * @return
   */
  public File getParamFile() {
    if (stack == null) {
      return null;
    }
    String rootName = getRootName();
    if (rootName == null) {
      return null;
    }
    return new File(stack.getParentFile(), rootName
        + DataFileType.SERIAL_SECTIONS.extension);
  }

  public ViewType getViewType() {
    return viewType;
  }
}
