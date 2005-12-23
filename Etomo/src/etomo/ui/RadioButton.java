package etomo.ui;

import javax.swing.JRadioButton;

import etomo.EtomoDirector;
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
final class RadioButton extends JRadioButton {
  public static  final String  rcsid =  "$Id$";
  
  public RadioButton(String text) {
    super(text);
    String name = UIUtilities.convertLabelToName(text);
    setName(name);
    if (EtomoDirector.getInstance().isPrintNames()) {
      System.out.println(name + ".rb " + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }
  
/*
  public RadioButton() {
    super();
  }
  
  public RadioButton(String text, boolean selected) {
    super(text, selected);
    setName(UIUtilities.convertLabelToName(text));
  }
  
  public RadioButton(Action a) {
    super(a);
  }
  
  public RadioButton(Icon icon) {
    super(icon);
  }
  
  public RadioButton(Icon icon, boolean selected) {
    super(icon, selected);
  }
  
  public RadioButton(String text, Icon icon) {
    super(text, icon);
    setName(UIUtilities.convertLabelToName(text));
  }
  
  public RadioButton(String text, Icon icon, boolean selected) {
    super(text, icon, selected);
    setName(UIUtilities.convertLabelToName(text));
  }
  
  public final void setText(String text) {
    super.setText(text);
    setName(UIUtilities.convertLabelToName(text));
  }*/
}
/**
* <p> $Log$ </p>
*/
