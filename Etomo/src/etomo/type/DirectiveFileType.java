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

  public static DirectiveFileType SCOPE = new DirectiveFileType(0);
  public static DirectiveFileType SYSTEM = new DirectiveFileType(1);
  public static DirectiveFileType USER = new DirectiveFileType(2);
  public static DirectiveFileType BATCH = new DirectiveFileType(3);

  public final int index;

  private DirectiveFileType(final int index) {
    this.index = index;
  }

  public File getLocalFile(final BaseManager manager, final AxisID axisID) {
    if (this == SCOPE) {
      return FileType.LOCAL_SCOPE_TEMPLATE.getFile(manager, axisID);
    }
    if (this == SYSTEM) {
      return FileType.LOCAL_SYSTEM_TEMPLATE.getFile(manager, axisID);
    }
    if (this == USER) {
      return FileType.LOCAL_USER_TEMPLATE.getFile(manager, axisID);
    }
    if (this == BATCH) {
      return FileType.LOCAL_BATCH_DIRECTIVE_FILE.getFile(manager, axisID);
    }
    return null;
  }
}
