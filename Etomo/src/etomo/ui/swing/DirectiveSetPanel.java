package etomo.ui.swing;

import java.awt.Component;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.logic.DirectiveTool;
import etomo.storage.Directive;
import etomo.type.AxisID;
import etomo.type.AxisType;

/**
* <p>Description: Directive set panel handles the three (A, B, and both axes) directives
* that can potientially be created from a single storage.Directive instance of type Param
* or Runtime.</p>
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
class DirectiveSetPanel implements Expandable, DirectiveSetInterface {
  public static final String rcsid = "$Id:$";

  private final JPanel pnlRoot = new JPanel();
  private final JPanel pnlBody = new JPanel();
  private final JPanel pnlAandB = new JPanel();
  private final ExpandButton expandButton = ExpandButton.getInstance(this,
      ExpandButton.Type.MORE);

  private final DirectiveTool tool;
  private final Directive directive;
  private final DirectivePanel pnlDirective;
  private final DirectivePanel pnlDirectiveA;
  private final DirectivePanel pnlDirectiveB;
  private final BaseManager manager;

  private int debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();

  private DirectiveSetPanel(final BaseManager manager, final Directive directive,
      final AxisType sourceAxisType, final DirectiveTool tool) {
    this.manager = manager;
    this.directive = directive;
    this.tool = tool;
    pnlDirectiveA = DirectivePanel.getSetInstance(directive, AxisID.FIRST, tool,
        sourceAxisType);
    pnlDirectiveB = DirectivePanel.getSetInstance(directive, AxisID.SECOND, tool,
        sourceAxisType);
    pnlDirective = DirectivePanel.getAnyInstance(directive, null, tool, pnlDirectiveA,
        pnlDirectiveB, sourceAxisType);
  }

  static DirectiveSetPanel getInstance(final BaseManager manager,
      final Directive directive, final DirectiveTool tool, final AxisType sourceAxisType) {
    DirectiveSetPanel instance = new DirectiveSetPanel(manager, directive,
        sourceAxisType, tool);
    instance.createPanel(directive, sourceAxisType);
    return instance;
  }

  private void createPanel(final Directive directive, final AxisType sourceAxisType) {
    // init
    // Call the Any directive's init function and it will take care of initialization for
    // all three directives.
    pnlDirective.init();
    expandButton.setExpanded(!pnlDirective.isEnabled());
    // root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.X_AXIS));
    pnlRoot.add(expandButton.getComponent());
    pnlRoot.add(pnlBody);
    // body panel
    pnlBody.setLayout(new BoxLayout(pnlBody, BoxLayout.Y_AXIS));
    pnlBody.add(pnlDirective.getComponent());
    pnlBody.add(pnlAandB);
    // A and B panel
    pnlAandB.setLayout(new BoxLayout(pnlAandB, BoxLayout.Y_AXIS));
    pnlAandB.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlAandB.add(pnlDirectiveA.getComponent());
    pnlAandB.add(pnlDirectiveB.getComponent());
    pnlAandB.add(Box.createRigidArea(FixedDim.x0_y3));
  }

  public Component getComponent() {
    return pnlRoot;
  }

  public boolean msgControlChanged(final boolean includeChange, final boolean expandChange) {
    pnlDirective.msgControlChanged(includeChange, expandChange);
    // decide whether this panel is visible
    boolean visible = tool.isDirectiveVisible(directive, pnlDirective.isIncluded()
        || pnlDirectiveA.isIncluded() || pnlDirectiveB.isIncluded(),
        isDifferentFromCheckpoint(false));
    pnlRoot.setVisible(visible);
    if (expandChange) {
      boolean expand = !pnlDirective.isEnabled();
      if (expand != expandButton.isExpanded()) {
        expandButton.setExpanded(expand);
      }
    }
    return visible;
  }

  public boolean isDifferentFromCheckpoint(final boolean checkInclude) {
    return pnlDirective.isDifferentFromCheckpoint(checkInclude)
        || pnlDirectiveA.isDifferentFromCheckpoint(checkInclude)
        || pnlDirectiveB.isDifferentFromCheckpoint(checkInclude);
  }

  public void expand(final ExpandButton button) {
    boolean expand = button.isExpanded();
    pnlDirective.expand(expand);
    pnlDirective.setVisible(!expand);
    pnlAandB.setVisible(expand);
    UIHarness.INSTANCE.pack(manager);
  }

  public void expand(final GlobalExpandButton button) {
  }

  public boolean isIncluded() {
    return pnlDirective.isIncluded() || pnlDirectiveA.isIncluded()
        || pnlDirectiveB.isIncluded();
  }
}