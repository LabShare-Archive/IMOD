package etomo.storage;

import etomo.ui.FieldType;

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
public final class DirectiveValueType {
  public static final String rcsid = "$Id:$";

  public static final DirectiveValueType BOOLEAN = new DirectiveValueType("Bool", 3);
  public static final DirectiveValueType FLOATING_POINT = new DirectiveValueType("Float",
      5);
  public static final DirectiveValueType FLOATING_POINT_PAIR = new DirectiveValueType(
      "2 Float", 10);
  public static final DirectiveValueType INTEGER = new DirectiveValueType("Int", 5);
  public static final DirectiveValueType INTEGER_PAIR = new DirectiveValueType("2 Int",
      10);
  public static final DirectiveValueType LIST = new DirectiveValueType("List", 15);
  public static final DirectiveValueType STRING = new DirectiveValueType("String", 15);

  private final String tag;
  private final int columns;

  private DirectiveValueType(final String tag, final int columns) {
    this.tag = tag;
    this.columns = columns;
  }

  static DirectiveValueType getInstance(final String input) {
    if (input == null) {
      return null;
    }
    if (input.equals(BOOLEAN.tag)) {
      return BOOLEAN;
    }
    if (input.equals(FLOATING_POINT.tag)) {
      return FLOATING_POINT;
    }
    if (input.equals(FLOATING_POINT_PAIR.tag)) {
      return FLOATING_POINT_PAIR;
    }
    if (input.equals(INTEGER.tag)) {
      return INTEGER;
    }
    if (input.equals(INTEGER_PAIR.tag)) {
      return INTEGER_PAIR;
    }
    if (input.equals(LIST.tag)) {
      return LIST;
    }
    if (input.equals(STRING.tag)) {
      return STRING;
    }
    return null;
  }

  public int getColumns() {
    return columns;
  }

  public FieldType toFieldType() {
    if (this == BOOLEAN) {
      return null;
    }
    if (this == FLOATING_POINT) {
      return FieldType.FLOATING_POINT;
    }
    if (this == FLOATING_POINT_PAIR) {
      return FieldType.FLOATING_POINT_PAIR;
    }
    if (this == INTEGER) {
      return FieldType.INTEGER;
    }
    if (this == INTEGER_PAIR) {
      return FieldType.INTEGER_PAIR;
    }
    if (this == LIST) {
      return FieldType.INTEGER_LIST;
    }
    if (this == STRING) {
      return FieldType.STRING;
    }
    return null;
  }

  public String toString() {
    return tag;
  }
}