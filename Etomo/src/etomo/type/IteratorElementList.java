package etomo.type;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import etomo.BaseManager;
import etomo.ui.swing.AnisotropicDiffusionDialog;

/**
 * <p>Description: List of positive whole numbers including descriptions of lists:
 * Depends on IteratorParser.
 * 
 * Example:
 * This list:
 * 1,3 - 10, 20, 50-45
 * translates to:
 * 1,3,4,5,6,7,8,9,10,20,50,49,48,47,46,45
 * 
 * Future upgrades:
 * Add decimals: Add a mode to PrimativeTokenizer.
 * Negative numbers:  Handle in IteratorParser.
 *</p>
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
 * <p> $Log$
 * <p> Revision 1.1  2009/09/05 00:10:21  sueh
 * <p> bug# 1256 Parsed by IteratorParser.  Contains IteratorElements.
 * <p> </p>
 */
public final class IteratorElementList {
  public static final String rcsid = "$Id$";

  /**
   * List of elements.  List may be cleared and reused.  Do not distribute.
   */
  private final List list = new ArrayList();

  private final BaseManager manager;
  private final AxisID axisID;
  private final String label;

  private IteratorParser parser = null;

  public IteratorElementList(BaseManager manager, AxisID axisID, String label) {
    this.manager = manager;
    this.axisID = axisID;
    this.label = label;
  }

  /**
   * Sets a new list by parsing input.
   * @param input
   */
  public void setList(String input) {
    list.clear();
    if (input == null || input.matches("\\s*")) {
      return;
    }
    if (parser == null) {
      parser = new IteratorParser(manager, axisID,
          AnisotropicDiffusionDialog.ITERATION_LIST_LABEL);
    }
    parser.parse(input, this);
  }

  /**
   * Adds the elements in input.list to list.  This elements are
   * IteratorElements, which are immutable.
   * @param input
   */
  public void setList(IteratorElementList input) {
    list.clear();
    if (input==null) {
      return;
    }
    Iterator iterator = input.list.iterator();
    while (iterator.hasNext()) {
      list.add(iterator.next());
    }
  }

  /**
   * Adds an element to the list.
   * @param element
   */
  public void add(IteratorElement element) {
    list.add(element);
  }

  /**
   * If the parser hasn't been used - always valid.
   * If the parser has been used return parser.isValid.
   * @return
   */
  public boolean isValid() {
    if (parser == null) {
      return true;
    }
    return parser.isValid();
  }

  public String toString() {
    StringBuffer buffer = new StringBuffer();
    Iterator iterator = list.iterator();
    if (iterator.hasNext()) {
      buffer.append(((IteratorElement) iterator.next()).toString());
    }
    while (iterator.hasNext()) {
      buffer.append(",");
      buffer.append(((IteratorElement) iterator.next()).toString());
    }
    return buffer.toString();
  }

  /**
   * Returns a list of strings containing all the integers specified by the
   * numbers and ranges in list.
   * @return
   */
  public List getExpandedList() {
    List expandedList = new ArrayList();
    Iterator iterator = list.iterator();
    while (iterator.hasNext()) {
      IteratorElement element = (IteratorElement) iterator.next();
      if (!element.isRange()) {
        expandedList.add(element.getNumber());
      }
      else {
        expandedList.addAll(element.getRange());
      }
    }
    return expandedList;
  }

  public void store(Properties props, String prepend) {
    if (props == null) {
      return;
    }
    String propertiesKey = makePropertyKey(prepend);
    if (prepend == null || prepend.matches("\\s*")) {
      propertiesKey = label;
    }
    else {
      propertiesKey = prepend + "." + label;
    }
    if (list.isEmpty()) {
      props.remove(propertiesKey);
    }
    else {
      props.setProperty(propertiesKey, toString());
    }
  }

  public void load(Properties props, String prepend) {
    if (props == null) {
      list.clear();
    }
    else {
      setList(props.getProperty(makePropertyKey(prepend)));
    }
  }

  private String makePropertyKey(String prepend) {
    if (prepend == null || prepend.matches("\\s*")) {
      return label;
    }
    return prepend + "." + label;
  }
}
