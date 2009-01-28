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
 * <p> Revision 1.1  2009/01/20 19:41:53  sueh
 * <p> bug# 1102 Was UITestAction.
 * <p> </p>
 */

public final class UITestActionType {
  public static final String rcsid = "$Id$";

  //Cannot use the same strings as etomo.type.UITestFieldType.
  public  static final UITestActionType ASSERT = new UITestActionType("assert");
  public  static final UITestActionType END = new UITestActionType("end");
  public  static final UITestActionType COPY = new UITestActionType("copy");
  public static final UITestActionType GOTO = new UITestActionType("goto");
  public static final UITestActionType IF = new UITestActionType("if");
  public static final UITestActionType IFNOT = new UITestActionType("ifnot");
  public static final UITestActionType OPEN = new UITestActionType("open");
  public static final UITestActionType RETURN = new UITestActionType("return");
  public static final UITestActionType RUN = new UITestActionType("run");
  public static final UITestActionType SET = new UITestActionType("set");
  public static final UITestActionType SKIPTO = new UITestActionType("skipto");
  public static final UITestActionType SLEEP = new UITestActionType("sleep");
  public static final UITestActionType WAIT = new UITestActionType("wait");

  private final String string;

  private UITestActionType(String string) {
    this.string = string;
  }

  public static UITestActionType getInstance(String string) {
    if (string == null) {
      return null;
    }
    if (ASSERT.equals(string)) {
      return ASSERT;
    }
    if (END.equals(string)) {
      return END;
    }
    if (COPY.equals(string)) {
      return COPY;
    }
    if (GOTO.equals(string)) {
      return GOTO;
    }
    if (IF.equals(string)) {
      return IF;
    }
    if (IFNOT.equals(string)) {
      return IFNOT;
    }
    if (OPEN.equals(string)) {
      return OPEN;
    }
    if (RETURN.equals(string)) {
      return RETURN;
    }
    if (RUN.equals(string)) {
      return RUN;
    }
    if (SET.equals(string)) {
      return SET;
    }
    if (SKIPTO.equals(string)) {
      return SKIPTO;
    }
    if (SLEEP.equals(string)) {
      return SLEEP;
    }
    if (WAIT.equals(string)) {
      return WAIT;
    }
    return null;
  }

  public boolean equals(String string) {
    return this.string.equals(string);
  }

  public String toString() {
    return string;
  }
}
