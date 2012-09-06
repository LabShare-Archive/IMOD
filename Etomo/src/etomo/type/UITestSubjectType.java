package etomo.type;

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
 * <p> Revision 1.4  2010/03/03 05:00:05  sueh
 * <p> bug# 1311 Added DEBUG.
 * <p>
 * <p> Revision 1.3  2009/09/22 21:03:47  sueh
 * <p> bug# 1259 Removed COM.
 * <p>
 * <p> Revision 1.2  2009/09/11 22:41:44  sueh
 * <p> bug# 1259 comparing com files by sorting them first.
 * <p>
 * <p> Revision 1.1  2009/01/20 19:42:50  sueh
 * <p> bug# 1102 Describes the subject of a uitest command.
 * <p> </p>
 */
public final class UITestSubjectType {
  public static final String rcsid = "$Id$";

  public static final UITestSubjectType ADOC = new UITestSubjectType("adoc");
  public static final UITestSubjectType DATASET = new UITestSubjectType("dataset");
  public static final UITestSubjectType DEBUG = new UITestSubjectType("debug");
  public static final UITestSubjectType DIALOG = new UITestSubjectType("dialog");
  public static final UITestSubjectType DIR = new UITestSubjectType("dir");
  public static final UITestSubjectType FILE = new UITestSubjectType("file");
  public static final UITestSubjectType FILE_CHOOSER = new UITestSubjectType(
      "file-chooser");
  public static final UITestSubjectType FRAME = new UITestSubjectType("frame");
  public static final UITestSubjectType FUNCTION = new UITestSubjectType("function");
  public static final UITestSubjectType INDEX = new UITestSubjectType("index");
  public static final UITestSubjectType INTERFACE = new UITestSubjectType("interface");
  public static final UITestSubjectType PARAM = new UITestSubjectType("param");
  public static final UITestSubjectType POPUP = new UITestSubjectType("popup");
  public static final UITestSubjectType PROCESS = new UITestSubjectType("process");
  public static final UITestSubjectType SECTION = new UITestSubjectType("section");
  public static final UITestSubjectType TEST = new UITestSubjectType("test");
  public static final UITestSubjectType TESTDIR = new UITestSubjectType("testdir");
  public static final UITestSubjectType VAR = new UITestSubjectType("var");

  private final String string;

  private UITestSubjectType(String string) {
    this.string = string;
  }

  public static UITestSubjectType getInstance(String string) {
    if (string == null) {
      return null;
    }
    if (string.equals(ADOC.toString())) {
      return ADOC;
    }
    if (string.equals(DATASET.toString())) {
      return DATASET;
    }
    if (string.equals(DEBUG.toString())) {
      return DEBUG;
    }
    if (string.equals(DIALOG.toString())) {
      return DIALOG;
    }
    if (string.equals(DIR.toString())) {
      return DIR;
    }
    if (string.equals(FILE.toString())) {
      return FILE;
    }
    if (string.equals(FILE_CHOOSER.toString())) {
      return FILE_CHOOSER;
    }
    if (string.equals(FRAME.toString())) {
      return FRAME;
    }
    if (string.equals(FUNCTION.toString())) {
      return FUNCTION;
    }
    if (string.equals(INDEX.toString())) {
      return INDEX;
    }
    if (string.equals(INTERFACE.toString())) {
      return INTERFACE;
    }
    if (string.equals(PARAM.toString())) {
      return PARAM;
    }
    if (string.equals(POPUP.toString())) {
      return POPUP;
    }
    if (string.equals(PROCESS.toString())) {
      return PROCESS;
    }
    if (string.equals(SECTION.toString())) {
      return SECTION;
    }
    if (string.equals(TEST.toString())) {
      return TEST;
    }
    if (string.equals(TESTDIR.toString())) {
      return TESTDIR;
    }
    if (string.equals(VAR.toString())) {
      return VAR;
    }
    return null;
  }

  public String toString() {
    return string;
  }
}
