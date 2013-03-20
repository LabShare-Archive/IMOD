package etomo.storage;
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
 final class DirectiveValueType {
  public static  final String  rcsid =  "$Id:$";
  
  static final DirectiveValueType BOOLEAN = new DirectiveValueType("Bool");
  static final DirectiveValueType FLOATING_POINT = new DirectiveValueType("Float");
  static final DirectiveValueType FLOATING_POINT_PAIR = new DirectiveValueType("2 Float");
  static final DirectiveValueType INTEGER = new DirectiveValueType("Int");
  static final DirectiveValueType INTEGER_PAIR = new DirectiveValueType("2 Int");
  static final DirectiveValueType LIST = new DirectiveValueType("List");
  static final DirectiveValueType STRING = new DirectiveValueType("String");

  private final String tag;

  private DirectiveValueType(final String tag) {
    this.tag = tag;
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
    if (input.equals(BOOLEAN.tag)) {
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
}