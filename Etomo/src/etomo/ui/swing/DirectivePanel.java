package etomo.ui.swing;

import java.awt.Component;

import javax.swing.JPanel;

import etomo.storage.Directive;

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
final class DirectivePanel {
  public static final String rcsid = "$Id:$";

  private final JPanel pnlRoot = new JPanel();

  private final Directive directive;

  private DirectivePanel(final Directive directive) {
    this.directive = directive;
  }

  static DirectivePanel getInstance(final Directive directive) {
    DirectivePanel instance = new DirectivePanel(directive);
    instance.createPanel();
    return instance;
  }

  private void createPanel() {
  }

  Component getComponent() {
    return pnlRoot;
  }
}
