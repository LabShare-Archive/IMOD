package etomo.uitest;

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
final class SectionType {
  public static final String rcsid = "$Id$";

  static final SectionType DATASET = new SectionType("dataset");
  static final SectionType IF = new SectionType("if");
  static final SectionType INTERFACE = new SectionType("interface");
  static final SectionType TEST = new SectionType("Test");


  private final String string;

  private SectionType(String string) {
    this.string = string;
  }
  
  static SectionType getInstance(String string) {
    if (string==null) {
      return null;
    }
    if (string.equals(DATASET.toString())) {
      return DATASET;
    }
    if (string.equals(IF.toString())) {
      return IF;
    }
    if (string.equals(INTERFACE.toString())) {
      return INTERFACE;
    }
    if (string.equals(TEST.toString())) {
      return TEST;
    }
    return null;
  }


  public String toString() {
    return string;
  }
}
