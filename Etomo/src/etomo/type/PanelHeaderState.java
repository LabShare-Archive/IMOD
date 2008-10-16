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
public class PanelHeaderState implements Storable,ConstPanelHeaderState {
  public static  final String  rcsid =  "$Id$";
  
  static final String KEY = "Header";
  
  private static final String OPEN_CLOSE_NAME = "OpenClose";
  private static final String ADVANCED_BASIC_NAME = "AdvancedBasic";
  private static final String MORE_LESS_NAME = "MoreLess";
  
  private final String group;
  
  private String openCloseState = null;
  private String advancedBasicState = null;
  private String moreLessState = null;
  
  public PanelHeaderState(String group) {
    this.group = group;
  }
  
  void set(PanelHeaderState input) {
    openCloseState=input.openCloseState;
    advancedBasicState=input.advancedBasicState;
    moreLessState=input.moreLessState;
  }
  
  public String toString() {
    return "[group=" + group + ",openCloseState=" + openCloseState
        + ",advancedBasicState=" + advancedBasicState + ",moreLessState="
        + moreLessState + "]";
  }
  
  private String getGroup(String prepend) {
    if (prepend.equals("")) {
      return group + '.';
    }
    return prepend + '.' + group + '.';
  }
  
  public void store(Properties props) {
    store(props, "");
  }
  
  public void store(Properties props, String prepend) {
    String group = getGroup(prepend);
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
    String group = getGroup(prepend);
    openCloseState = props.getProperty(group + OPEN_CLOSE_NAME);
    advancedBasicState = props.getProperty(group + ADVANCED_BASIC_NAME);
    moreLessState = props.getProperty(group + MORE_LESS_NAME);
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
* <p> Revision 1.2  2007/02/21 04:20:08  sueh
* <p> bug# 964 Added KEY for storing/loading.
* <p>
* <p> Revision 1.1  2005/09/27 23:21:50  sueh
* <p> bug# 532 A class used by PanelHeader to save its state.
* <p> </p>
*/