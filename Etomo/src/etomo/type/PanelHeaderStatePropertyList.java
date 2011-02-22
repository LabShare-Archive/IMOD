package etomo.type;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

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
 * <p> Revision 1.1  2009/09/01 03:09:28  sueh
 * <p> bug# 1222 Class to hold multiple properties.
 * <p> </p>
 */
final class PanelHeaderStatePropertyList {
  public static final String rcsid = "$Id$";

  //Contains Object uniqueContainerID, Element.
  private final Map elementMap = new HashMap();

  void addProperty(final Object uniqueContainerID, final String key) {
    if (elementMap.containsKey(uniqueContainerID)) {
      throw new IllegalArgumentException("Property already exists.  uniqueContainerID="
          + uniqueContainerID + ",key=" + key);
    }
    Element element = new Element(new PanelHeaderState(key));
    elementMap.put(uniqueContainerID, element);
  }

  void store(final Properties props, final String prepend) {
    Collection collection = elementMap.values();
    Iterator iterator = collection.iterator();
    while (iterator.hasNext()) {
      (((Element) iterator.next())).panelHeaderState.store(props, prepend);
    }
  }

  /**
   * When the key changes, record the version before the key changed and the
   * previous key here.
   * @param uniqueContainerID
   * @param oldVersion
   * @param oldKey
   */
  void setBackwardCompatibility(final Object uniqueContainerID,
      final EtomoVersion backwardCompatibleVersion, final String backwardCompatibleKey) {
    if (!elementMap.containsKey(uniqueContainerID)) {
      throw new IllegalArgumentException("Property doesn't exist.  uniqueContainerID="
          + uniqueContainerID + ", backwardCompatibleVersion="
          + backwardCompatibleVersion);
    }
    Element element = (Element) elementMap.get(uniqueContainerID);
    element.addToBackwardCompatibilityList(backwardCompatibleVersion,
        backwardCompatibleKey);
  }

  void load(final Properties props, final String prepend) {
    Collection collection = elementMap.values();
    Iterator iterator = collection.iterator();
    while (iterator.hasNext()) {
      (((Element) iterator.next())).panelHeaderState.load(props, prepend);
    }
  }

  /**
   * Load properties into elements.  If an element has one or more backwards
   * compatibility versions attached, check whether savedVersion is less then or
   * equal to any of the backward compatibility versions and load the most
   * appropriate one.
   * @param savedVersion
   * @param props
   * @param prepend
   */
  void load(EtomoVersion savedVersion, final Properties props, final String prepend) {
    Collection collection = elementMap.values();
    Iterator iterator = collection.iterator();
    while (iterator.hasNext()) {
      Element element = (Element) iterator.next();
      List backwardCompatibilityList = element.getBackwardCompatibilityList();
      //Check for backward compatility versions.
      EtomoVersion smallestBackwardCompatibleVersion = null;
      if (backwardCompatibilityList != null) {
        for (int i = 0; i < backwardCompatibilityList.size(); i++) {
          Element.BackwardCompabilityPair backwardCompabilityPair = (Element.BackwardCompabilityPair) backwardCompatibilityList
              .get(i);
          EtomoVersion backwardCompatibleVersion = backwardCompabilityPair.backwardCompatibleVersion;
          String backwardCompatibleKey = backwardCompabilityPair.backwardCompatibleKey;
          if (savedVersion.le(backwardCompatibleVersion)) {
            //Keep loading the backward compatibility version that is currently
            //closest to savedVersion.  Only the smallest one that is greater
            //then or equal to the saved version is valid.
            if (smallestBackwardCompatibleVersion == null
                || backwardCompatibleVersion.lt(smallestBackwardCompatibleVersion)) {
              smallestBackwardCompatibleVersion = backwardCompatibleVersion;
              element.panelHeaderState.load(props, prepend, backwardCompatibleKey);
            }
          }
        }
      }
      //If there are no backward compatibility versions or none of them where
      //greater or equal to savedVersion, load in the usual way.
      if (smallestBackwardCompatibleVersion == null) {
        element.panelHeaderState.load(props, prepend);
      }
    }
  }

  void set(final Object uniqueContainerID, final PanelHeaderState input) {
    if (!elementMap.containsKey(uniqueContainerID)) {
      throw new IllegalArgumentException("Property doesn't exist.  uniqueContainerID="
          + uniqueContainerID + ", input=" + input);
    }
    (((Element) elementMap.get(uniqueContainerID))).panelHeaderState.set(input);
  }

  PanelHeaderState get(final Object uniqueContainerID) {
    if (!elementMap.containsKey(uniqueContainerID)) {
      throw new IllegalArgumentException("Property doesn't exist.  uniqueContainerID="
          + uniqueContainerID);
    }
    return ((Element) elementMap.get(uniqueContainerID)).panelHeaderState;
  }

  /**
   * Contains the EtomoNumber property and a list of backward compatibility
   * information.
   * @author sueh
   */
  private static final class Element {
    private final PanelHeaderState panelHeaderState;

    //Contains EtomoVersion backwardCompatibleVersion, backwardCompatibleKey oldKey.
    private List backwardCompatibilityList = null;

    private Element(final PanelHeaderState panelHeaderState) {
      this.panelHeaderState = panelHeaderState;
    }

    private void addToBackwardCompatibilityList(EtomoVersion backwardCompatibleVersion,
        String backwardCompatibleKey) {
      if (backwardCompatibilityList == null) {
        backwardCompatibilityList = new ArrayList();
      }
      backwardCompatibilityList.add(new BackwardCompabilityPair(
          backwardCompatibleVersion, backwardCompatibleKey));
    }

    private boolean equals(Element element) {
      if (element == null) {
        return false;
      }
      return panelHeaderState.equals(element.panelHeaderState);
    }

    private List getBackwardCompatibilityList() {
      return backwardCompatibilityList;
    }

    /**
     * Contains a version and 
     * @author sueh
     *
     */
    private static final class BackwardCompabilityPair {
      private final EtomoVersion backwardCompatibleVersion;
      private final String backwardCompatibleKey;

      private BackwardCompabilityPair(EtomoVersion backwardCompatibleVersion,
          String backwardCompatibleKey) {
        this.backwardCompatibleVersion = backwardCompatibleVersion;
        this.backwardCompatibleKey = backwardCompatibleKey;
      }
    }
  }
}
