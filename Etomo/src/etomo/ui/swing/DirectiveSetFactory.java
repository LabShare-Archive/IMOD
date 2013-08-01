package etomo.ui.swing;

import etomo.BaseManager;
import etomo.logic.DirectiveTool;
import etomo.storage.Directive;
import etomo.type.AxisType;

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
final class DirectiveSetFactory {
  public static final String rcsid = "$Id:$";

  private DirectiveSetFactory() {
  }

  static DirectiveSetInterface createDirectiveSet(final BaseManager manager,
      final Directive directive, final DirectiveTool tool, final AxisType sourceAxisType) {
    return DirectivePanel.getSoloInstance(manager, directive, tool, sourceAxisType);
    // Currently not using any directive sets (which allow editing the A and B values
    // separately.
    // return DirectiveSetPanel.getInstance(manager, directive, tool, sourceAxisType);
  }
}
