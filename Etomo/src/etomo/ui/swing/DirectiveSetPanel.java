package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

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
  private final AxisType sourceAxisType;
  private final DirectivePanel pnlDirective;
  private final DirectivePanel pnlDirectiveA;
  private final DirectivePanel pnlDirectiveB;
  private final BaseManager manager;

  private int debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();

  private DirectiveSetPanel(final BaseManager manager, final Directive directive,
      final AxisType sourceAxisType, final DirectiveTool tool) {
    this.manager = manager;
    this.sourceAxisType = sourceAxisType;
    this.directive = directive;
    this.tool = tool;
    pnlDirective = DirectivePanel.getSetInstance(directive, null, tool);
    pnlDirectiveA = DirectivePanel.getSetInstance(directive, AxisID.FIRST, tool);
    pnlDirectiveB = DirectivePanel.getSetInstance(directive, AxisID.SECOND, tool);
  }

  static DirectiveSetPanel getInstance(final BaseManager manager,
      final Directive directive, final AxisType sourceAxisType, final DirectiveTool tool) {
    DirectiveSetPanel instance = new DirectiveSetPanel(manager, directive,
        sourceAxisType, tool);
    instance.createPanel(directive, sourceAxisType);
    instance.addListeners();
    return instance;
  }

  private void createPanel(final Directive directive, final AxisType sourceAxisType) {
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
    pnlDirective.setSiblings(pnlDirectiveA, pnlDirectiveB);
    if (sourceAxisType != AxisType.DUAL_AXIS) {
      updateDisplay(false, null);
    }
    else {
      updateDisplay(false, AxisID.FIRST);
    }
  }

  public Component getComponent() {
    return pnlRoot;
  }

  private void addListeners() {
    pnlDirective.addActionListener(new DirectiveSetListener(this, null));
    pnlDirectiveA.addActionListener(new DirectiveSetListener(this, AxisID.FIRST));
    pnlDirectiveB.addActionListener(new DirectiveSetListener(this, AxisID.SECOND));
  }

  /**
   * Call updateDisplay for the Any directive panel (which takes care of updateDisplay for
   * the A and B directive panels.  Toggles root panel visibility if necessary.
   * @param action - true if cause by the user changing include or value - triggered listeners in this class
   * @param changedAxisID
   */
  private void updateDisplay(final boolean action, final AxisID changedAxisID) {
    // handle expand button
    boolean expanded = expandButton.isExpanded();
    pnlDirective.setVisible(!expanded);
    pnlAandB.setVisible(expanded);
    // tell directives to update display
    pnlDirective.updateDisplay(action, changedAxisID);
    // decide whether this panel is visible
    boolean visible = pnlRoot.isVisible();
    if (visible != tool.isDirectiveVisible(
        directive,
        pnlDirective.isInclude() || pnlDirectiveA.isInclude()
            || pnlDirectiveB.isInclude(),
        pnlDirective.isDifferentFromCheckpoint()
            || pnlDirectiveA.isDifferentFromCheckpoint()
            || pnlDirectiveB.isDifferentFromCheckpoint())) {
      pnlRoot.setVisible(!visible);
    }
  }

  /**
   * Calls updateDisplay with the assumption that the cause isn't the user changing a
   * directive.  Changes will move from A and B to Any.
   */
  public void updateDisplay() {
    updateDisplay(false, AxisID.FIRST);
  }

  public void expand(final ExpandButton button) {
    updateDisplay(true, AxisID.FIRST);
    UIHarness.INSTANCE.pack(manager);
  }

  public void expand(final GlobalExpandButton button) {
  }

  public boolean isInclude() {
    return pnlDirective.isInclude() || pnlDirectiveA.isInclude()
        || pnlDirectiveB.isInclude();
  }

  public boolean isVisible() {
    return pnlRoot.isVisible();
  }

  private void action(final AxisID axisID) {
    updateDisplay(true, axisID);
  }

  private static final class DirectiveSetListener implements ActionListener {
    private final DirectiveSetPanel panel;
    private final AxisID axisID;

    private DirectiveSetListener(final DirectiveSetPanel panel, final AxisID axisID) {
      this.panel = panel;
      this.axisID = axisID;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.action(axisID);
    }
  }
}