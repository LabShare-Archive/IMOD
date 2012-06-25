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

  private final String stackLabel;
  private final String viewTypeLabel;

  private File stack = null;
  private ViewType viewType = null;
  private File distortionField = null;
  private Number imagesAreBinned = null;

  public SerialSectionsStartupData(final String stackLabel, final String viewTypeLabel) {
    if (stackLabel == null || stackLabel.matches("\\s*")) {
      this.stackLabel = "stack";
    }
    else {
      this.stackLabel = stackLabel;
    }
    if (viewTypeLabel == null || viewTypeLabel.matches("\\s*")) {
      this.viewTypeLabel = "view type";
    }
    else {
      this.viewTypeLabel = viewTypeLabel;
    }
  }

  public void setStack(final File input) {
    stack = input;
  }

  public void setViewType(final EnumeratedType input) {
    viewType = ViewType.getInstance(input);
  }

  public void setDistortionFile(final File input) {
    distortionField = input;
  }

  public void setBinning(final Number input) {
    imagesAreBinned = input;
  }

  /**
   * Validation
   * @return an error message or null if valid
   */
  public String validate() {
    if (stack == null) {
      return "Missing required entry: " + stackLabel + ".";
    }
    if (!stack.exists()) {
      return stackLabel + "doesn't exist.";
    }
    if (!stack.canRead()) {
      return stackLabel + "is not readable.";
    }
    if (viewType == null) {
      return "Missing required entry: " + viewTypeLabel + ".";
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

  public File getStack() {
    return stack;
  }

  public File getDistortionField() {
    return distortionField;
  }

  public Number getImagesAreBinned() {
    return imagesAreBinned;
  }
}
