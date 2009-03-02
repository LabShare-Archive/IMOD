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
 * <p> $Log$
 * <p> Revision 1.1  2009/01/20 20:50:54  sueh
 * <p> bug# 1102 List of action modifiers of commands.
 * <p> </p>
 */
final class UITestModifierType {
  public static final String rcsid = "$Id$";

  //Cannot be the same as AxisID ("a" or "b")
  static final UITestModifierType ALWAYS = new UITestModifierType("always");
  static final UITestModifierType ENABLED = new UITestModifierType("enabled");
  static final UITestModifierType EQUALS = new UITestModifierType("equals");
  static final UITestModifierType EXISTS = new UITestModifierType("exists");
  static final UITestModifierType DISABLED = new UITestModifierType("disabled");
  static final UITestModifierType KEEP = new UITestModifierType("keep");
  static final UITestModifierType NOT = new UITestModifierType("not");
  static final UITestModifierType NOT_EQUALS = new UITestModifierType("not-equals");
  static final UITestModifierType NOT_EXISTS = new UITestModifierType(
      "not-exists");
  static final UITestModifierType SINGLE = new UITestModifierType("single");
  static final UITestModifierType GE = new UITestModifierType("ge");//greater then or equal to
  static final UITestModifierType LE = new UITestModifierType("le");//less then or equal to

  private final String string;

  private UITestModifierType(String string) {
    this.string = string;
  }

  static UITestModifierType getInstance(String string) {
    if (string == null) {
      return null;
    }
    if (ALWAYS.equals(string)) {
      return ALWAYS;
    }
    if (ENABLED.equals(string)) {
      return ENABLED;
    }
    if (EQUALS.equals(string)) {
      return EQUALS;
    }
    if (EXISTS.equals(string)) {
      return EXISTS;
    }
    if (DISABLED.equals(string)) {
      return DISABLED;
    }
    if (KEEP.equals(string)) {
      return KEEP;
    }
    if (NOT.equals(string)) {
      return NOT;
    }
    if (NOT_EQUALS.equals(string)) {
      return NOT_EQUALS;
    }
    if (NOT_EXISTS.equals(string)) {
      return NOT_EXISTS;
    }
    if (SINGLE.equals(string)) {
      return SINGLE;
    }
    if (GE.equals(string)) {
      return GE;
    }
    if (LE.equals(string)) {
      return LE;
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
