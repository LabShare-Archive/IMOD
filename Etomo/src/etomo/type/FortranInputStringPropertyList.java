package etomo.type;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import etomo.comscript.FortranInputString;
import etomo.comscript.FortranInputSyntaxException;

/**
 * <p>Description: A class for saving a property that exists in multiple panels
 * and dialogs to different keys in the property file.  Expects a unique id with
 * each value.  All values are expected to have the same format, but can have
 * different values.</p>
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
 * <p> Revision 1.1  2009/09/01 03:00:36  sueh
 * <p> bug# 1222 Class to hold multiple properties.
 * <p> </p>
 */
final class FortranInputStringPropertyList {
  public static final String rcsid = "$Id$";

  //Contains Object uniqueContainerID, FortranInputString.
  private final Map elementMap = new HashMap();

  private final int nParams;

  private boolean[] isIntArray = null;

  /**
   * Create a list of FortranInputStrings all constructed with nParams.
   * @param nParams
   */
  FortranInputStringPropertyList(final int nParams) {
    this.nParams = nParams;
  }

  /**
   * Creates an element with a uniqueContainerID and a property key.  The
   * uniqueContainerID must be unique in this instance.  The key must be unique
   * in the property file that it will be saved to.
   * @param uniqueContainerID
   * @param key
   */
  void addProperty(final Object uniqueContainerID, final String key) {
    if (elementMap.containsKey(uniqueContainerID)) {
      throw new IllegalArgumentException("Property already exists.  uniqueContainerID="
          + uniqueContainerID + ",key=" + key);
    }
    FortranInputString element = new FortranInputString(nParams);
    if (isIntArray != null) {
      element.setIntegerType(isIntArray);
    }
    element.setPropertiesKey(key);
    elementMap.put(uniqueContainerID, element);
  }

  /**
   * Loads properties into all the elements in elementMap.
   * @param props
   * @param prepend
   */
  void load(final Properties props, final String prepend) {
    Collection collection = elementMap.values();
    Iterator iterator = collection.iterator();
    while (iterator.hasNext()) {
      ((FortranInputString) iterator.next()).load(props, prepend);
    }
  }

  /**
   * Stores properties from all the elements in elementMap.
   * @param props
   * @param prepend
   */
  void store(final Properties props, final String prepend) {
    Collection collection = elementMap.values();
    Iterator iterator = collection.iterator();
    while (iterator.hasNext()) {
      ((FortranInputString) iterator.next()).store(props, prepend);
    }
  }

  FortranInputString get(final Object uniqueContainerID) {
    if (!elementMap.containsKey(uniqueContainerID)) {
      throw new IllegalArgumentException("Property doesn't exist.  uniqueContainerID="
          + uniqueContainerID);
    }
    return (FortranInputString) elementMap.get(uniqueContainerID);
  }

  /**
   * Get a element from elementMap and run its validateAndSet function.
   * @param id
   * @param newValues
   * @throws FortranInputSyntaxException
   */
  void validateAndSet(final Object uniqueContainerID, final String newValues)
      throws FortranInputSyntaxException {
    if (!elementMap.containsKey(uniqueContainerID)) {
      throw new IllegalArgumentException("Property doesn't exist.  uniqueContainerID="
          + uniqueContainerID + ", newValues=" + newValues);
    }
    ((FortranInputString) elementMap.get(uniqueContainerID)).validateAndSet(newValues);
  }

  /**
   * Call setIntegerType on all elements in elementMap.  Save isIntArray and
   * call setIntegerType on on newly created elements.
   * @param isIntArray
   */
  void setIntegerType(final boolean[] isIntArray) {
    this.isIntArray = isIntArray;
    Collection collection = elementMap.values();
    Iterator iterator = collection.iterator();
    while (iterator.hasNext()) {
      ((FortranInputString) iterator.next()).setIntegerType(isIntArray);
    }
  }

  /**
   * Call setDefault on all elements in elementMap.
   */
  void setDefault() {
    Collection collection = elementMap.values();
    Iterator iterator = collection.iterator();
    while (iterator.hasNext()) {
      ((FortranInputString) iterator.next()).setDefault();
    }
  }
}
