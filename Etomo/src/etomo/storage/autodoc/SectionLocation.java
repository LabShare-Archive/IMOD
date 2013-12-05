package etomo.storage.autodoc;

/**
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright 2002 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $$Revision$$
 *
 * <p> $$Log$
 * <p> $Revision 1.2  2006/05/01 21:17:51  sueh
 * <p> $bug# 854
 * <p> $
 * <p> $Revision 1.1  2006/01/12 17:03:35  sueh
 * <p> $bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p> $
 * <p> $Revision 1.1  2003/12/31 01:31:14  sueh
 * <p> $bug# 372 simple iterator for sections, based on section type
 * <p> $$ </p>
 */

public final class SectionLocation {
  public static final String rcsid = "$$Id$$";
  String type = null;
  int index = -1;

  SectionLocation(String type, int index) {
    this.type = type;
    this.index = index;
  }

  SectionLocation(int index) {
    type = null;
    this.index = index;
  }

  String getType() {
    return type;
  }

  int getIndex() {
    return index;
  }

  void setIndex(int index) {
    this.index = index;
  }

  public String toString() {
    return "[type=" + type + ",index=" + index + "]";
  }
}
