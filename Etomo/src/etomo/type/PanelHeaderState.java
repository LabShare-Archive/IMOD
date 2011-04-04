package etomo.type;

import java.util.Properties;

import etomo.storage.Storable;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class PanelHeaderState implements Storable, ConstPanelHeaderState {
  public static final String rcsid = "$Id$";

  static final String KEY = "Header";

  private static final String OPEN_CLOSE_NAME = "OpenClose";
  private static final String ADVANCED_BASIC_NAME = "AdvancedBasic";
  private static final String MORE_LESS_NAME = "MoreLess";

  private final String group;

  private String openCloseState = null;
  private String advancedBasicState = null;
  private String moreLessState = null;
  private boolean debug = false;

  public PanelHeaderState(String group) {
    this.group = group;
  }

  void set(PanelHeaderState input) {
    openCloseState = input.openCloseState;
    advancedBasicState = input.advancedBasicState;
    moreLessState = input.moreLessState;
  }

  public String toString() {
    return "[group=" + group + ",openCloseState=" + openCloseState
        + ",advancedBasicState=" + advancedBasicState + ",moreLessState=" + moreLessState
        + "]";
  }

  public void setDebug(final boolean input) {
    debug = input;
  }

  private String getGroup(String prepend, String key) {
    if (prepend == null || prepend.matches("\\s*")) {
      return key + '.';
    }
    return prepend + '.' + key + '.';
  }

  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    String group = getGroup(prepend, this.group);
    if (openCloseState != null) {
      props.setProperty(group + OPEN_CLOSE_NAME, openCloseState);
    }
    if (advancedBasicState != null) {
      props.setProperty(group + ADVANCED_BASIC_NAME, advancedBasicState);
    }
    if (moreLessState != null) {
      props.setProperty(group + MORE_LESS_NAME, moreLessState);
    }
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    String group = getGroup(prepend, this.group);
    openCloseState = props.getProperty(group + OPEN_CLOSE_NAME);
    advancedBasicState = props.getProperty(group + ADVANCED_BASIC_NAME);
    moreLessState = props.getProperty(group + MORE_LESS_NAME);
  }

  /**
   * Load with key instead of this.group.
   * @param props
   * @param prepend
   * @param key
   */
  public void load(Properties props, String prepend, String key) {
    String group = getGroup(prepend, key);
    openCloseState = props.getProperty(group + OPEN_CLOSE_NAME);
    advancedBasicState = props.getProperty(group + ADVANCED_BASIC_NAME);
    moreLessState = props.getProperty(group + MORE_LESS_NAME);
  }

  private void reset() {
    openCloseState = null;
    advancedBasicState = null;
    moreLessState = null;
  }

  public boolean isNull() {
    return openCloseState == null && advancedBasicState == null && moreLessState == null;
  }

  public final void setAdvancedBasicState(String advancedBasicState) {
    this.advancedBasicState = advancedBasicState;
  }

  public final void setMoreLessState(String moreLessState) {
    this.moreLessState = moreLessState;
  }

  public final void setOpenCloseState(String openCloseState) {
    this.openCloseState = openCloseState;
  }

  public final String getAdvancedBasicState() {
    return advancedBasicState;
  }

  public final String getMoreLessState() {
    return moreLessState;
  }

  public final String getOpenCloseState() {
    return openCloseState;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.5  2011/02/22 05:46:50  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.4  2009/09/01 03:09:14  sueh
 * <p> bug# 1222 Added isNull.
 * <p>
 * <p> Revision 1.3  2008/10/16 20:59:20  sueh
 * <p> bug# 1141 Added set(PanelHeaderState)
 * <p>
 * <p> Revision 1.2  2007/02/21 04:20:08  sueh
 * <p> bug# 964 Added KEY for storing/loading.
 * <p>
 * <p> Revision 1.1  2005/09/27 23:21:50  sueh
 * <p> bug# 532 A class used by PanelHeader to save its state.
 * <p> </p>
 */
