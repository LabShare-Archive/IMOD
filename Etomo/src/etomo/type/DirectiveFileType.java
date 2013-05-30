package etomo.type;

import java.io.File;

import etomo.BaseManager;

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
public final class DirectiveFileType {
  public static final String rcsid = "$Id:$";

  public static DirectiveFileType SCOPE = new DirectiveFileType(0, "Scope",
      "Scope Template", FileType.LOCAL_SCOPE_TEMPLATE);
  public static DirectiveFileType SYSTEM = new DirectiveFileType(1, "System",
      "System Template", FileType.LOCAL_SYSTEM_TEMPLATE);
  public static DirectiveFileType USER = new DirectiveFileType(2, "User",
      "User Template", FileType.LOCAL_USER_TEMPLATE);
  public static DirectiveFileType BATCH = new DirectiveFileType(3, "Batch",
      "Batch Directive File", FileType.LOCAL_BATCH_DIRECTIVE_FILE);

  public static int NUM = 4;

  private final int index;
  private final String string;
  private final String label;
  private final FileType fileType;

  private DirectiveFileType(final int index, final String string, final String label,
      final FileType fileType) {
    this.index = index;
    this.string = string;
    this.label = label;
    this.fileType = fileType;
  }

  public static DirectiveFileType getInstance(final String label) {
    if (label == null) {
      return null;
    }
    if (label.equals(SCOPE.label)) {
      return SCOPE;
    }
    if (label.equals(SYSTEM.label)) {
      return SYSTEM;
    }
    if (label.equals(USER.label)) {
      return USER;
    }
    if (label.equals(BATCH.label)) {
      return BATCH;
    }
    return null;
  }

  public static DirectiveFileType getInstance(final int input) {
    if (input == SCOPE.index) {
      return SCOPE;
    }
    if (input == SYSTEM.index) {
      return SYSTEM;
    }
    if (input == USER.index) {
      return USER;
    }
    if (input == BATCH.index) {
      return BATCH;
    }
    return null;
  }

  public File getLocalFile(final BaseManager manager, final AxisID axisID) {
    return fileType.getFile(manager, axisID);
  }

  public String toString() {
    return string;
  }

  public String getLabel() {
    return label;
  }

  public static String toString(final int index) {
    if (index == SCOPE.index) {
      return SCOPE.string;
    }
    if (index == SYSTEM.index) {
      return SYSTEM.string;
    }
    if (index == USER.index) {
      return USER.string;
    }
    if (index == BATCH.index) {
      return BATCH.string;
    }
    return null;
  }

  public static String getLabel(final int index) {
    if (index == SCOPE.index) {
      return SCOPE.label;
    }
    if (index == SYSTEM.index) {
      return SYSTEM.label;
    }
    if (index == USER.index) {
      return USER.label;
    }
    if (index == BATCH.index) {
      return BATCH.label;
    }
    return null;
  }

  public int getIndex() {
    return index;
  }
}
