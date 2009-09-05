package etomo.type;

import java.util.ArrayList;
import java.util.Collection;

/**
 * <p>Description: An immutable single, or pair of, integer()s.  Values are
 * returned as Strings.
 * 
 * When both integers are not null IteratorElement represents a range.  The step
 * is implied.  It is 1 when the first number less then the second number, and
 * -1 when the first number is greater then the second number.  A range whose
 * numbers equal represents a single number.</p>
 * 
 * <p>Copyright: Copyright 2009</p>
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
final class IteratorElement {
  public static final String rcsid = "$Id$";

  private final EtomoNumber first = new EtomoNumber();
  private final EtomoNumber second;

  public IteratorElement(String first) {
    this.first.set(first);
    second = null;
  }

  public IteratorElement(String first, String second) {
    this.first.set(first);
    this.second = new EtomoNumber();
    this.second.set(second);
  }

  /**
   * Return convert the one or two numbers to strings and return.
   */
  public String toString() {
    if (!first.isNull()) {
      if (second != null && !second.isNull()) {
        return first.toString() + " - " + second.toString();
      }
      else {
        return first.toString();
      }
    }
    else if (second != null && !second.isNull()) {
      return second.toString();
    }
    else {
      return "";
    }
  }

  boolean isRange() {
    return !first.isNull() && second != null && !second.isNull();
  }

  /**
   * Returns a String version of the first number or, if it is null,
   * the second number.  If both numbers are null, returns null.
   * @return
   */
  String getNumber() {
    if (!first.isNull()) {
      return first.toString();
    }
    else if (second != null && !second.isNull()) {
      return second.toString();
    }
    else {
      return null;
    }
  }

  /**
   * Returns String values of a range from first to second.
   * @return
   */
  Collection getRange() {
    Collection range = new ArrayList();
    if (!isRange() || first.equals(second)) {
      String number = getNumber();
      if (number != null) {
        range.add(getNumber());
      }
    }
    else {
      range.add(first.toString());
      if (first.lt(second)) {
        for (int i = first.getInt() + 1; i < second.getInt(); i++) {
          range.add(String.valueOf(i));
        }
      }
      else {
        for (int i = first.getInt() - 1; i > second.getInt(); i--) {
          range.add(String.valueOf(i));
        }
      }
      range.add(second.toString());
    }
    return range;
  }

}
