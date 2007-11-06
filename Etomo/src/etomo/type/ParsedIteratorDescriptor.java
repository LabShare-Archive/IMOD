package etomo.type;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
final class ParsedIteratorDescriptor extends ParsedDescriptor {
  public static final String rcsid = "$Id$";

  static final Character DIVIDER_SYMBOL = new Character('-');

  ParsedIteratorDescriptor() {
    super(EtomoNumber.Type.INTEGER);
  }

  boolean isCompact() {
    return false;
  }

  Character getDividerSymbol() {
    return DIVIDER_SYMBOL;
  }
  
  boolean isIteratorDescriptor() {
    return true;
  }
  
  int getIncrement(ParsedNumber first,ParsedNumber last) {
    if (first.gt(last)) {
      return -1;
    }
    return 1;
  }
}
