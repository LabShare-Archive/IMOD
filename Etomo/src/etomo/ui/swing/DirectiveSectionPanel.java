package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.logic.DirectiveTool;
import etomo.storage.Directive;
import etomo.storage.DirectiveDescrSection;
import etomo.storage.DirectiveMap;
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
final class DirectiveSectionPanel {
  public static final String rcsid = "$Id:$";

  private final JPanel pnlRoot = new JPanel();
  private final JPanel pnlBody = new JPanel();
  private final List<DirectiveSetInterface> directiveSetArray = new ArrayList<DirectiveSetInterface>();
  private final JPanel pnlDirectives = new JPanel();

  private final CheckBox cbShow;
  private final BaseManager manager;
  private final AxisType sourceAxisType;
  private final DirectiveTool tool;
  private final DirectiveDescrSection descrSection;
  private final DirectiveMap directiveMap;

  private DirectiveSectionPanel(BaseManager manager, final AxisType sourceAxisType,
      final DirectiveTool tool, final DirectiveDescrSection descrSection,
      final DirectiveMap directiveMap) {
    this.manager = manager;
    this.sourceAxisType = sourceAxisType;
    this.tool = tool;
    this.descrSection = descrSection;
    this.directiveMap = directiveMap;
    cbShow = new CheckBox(descrSection.toString());
  }

  static DirectiveSectionPanel getInstance(final BaseManager manager,
      final DirectiveDescrSection descrSection, final DirectiveMap directiveMap,
      final AxisType sourceAxisType, final DirectiveTool tool) {
    DirectiveSectionPanel instance = new DirectiveSectionPanel(manager, sourceAxisType,
        tool, descrSection, directiveMap);
    instance.createPanel();
    instance.addListeners();
    instance.setTooltips();
    return instance;
  }

  private void createPanel() {
    // root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(new EtchedBorder(descrSection.toString()).getBorder());
    pnlRoot.add(pnlBody);
    // body panel
    pnlBody.setLayout(new BoxLayout(pnlBody, BoxLayout.Y_AXIS));
    pnlBody.add(Box.createRigidArea(FixedDim.x0_y3));
    JPanel pnl = new JPanel();
    pnl.setLayout(new BoxLayout(pnl, BoxLayout.X_AXIS));
    pnl.add(new JLabel("Include:"));
    pnl.add(Box.createHorizontalGlue());
    pnlBody.add(pnl);
    pnlBody.add(pnlDirectives);
    // directives panel
    pnlDirectives.setLayout(new BoxLayout(pnlDirectives, BoxLayout.Y_AXIS));
    Iterator<String> nameIterator = descrSection.nameIterator();
    while (nameIterator.hasNext()) {
      Directive directive = directiveMap.get(nameIterator.next());
      if (directive == null) {
        continue;
      }
      // directive set panels
      DirectiveSetInterface directiveSet = DirectiveSetFactory.createDirectiveSet(
          manager, directive, tool, sourceAxisType);
      directiveSetArray.add(directiveSet);
      pnlDirectives.add(directiveSet.getComponent());
    }
    msgControlChanged(false, true, true);
  }

  void msgControlChanged(final boolean includeChange, final boolean showChange,
      final boolean expandChange) {
    Iterator<DirectiveSetInterface> iterator = directiveSetArray.iterator();
    boolean visibleDirectives = false;
    boolean include = false;
    while (iterator.hasNext()) {
      DirectiveSetInterface directiveSet = iterator.next();
      if (directiveSet.msgControlChanged(includeChange, expandChange)
          && !visibleDirectives) {
        visibleDirectives = true;
      }
      if (showChange && !include && directiveSet.isIncluded()) {
        include = true;
      }
    }
    if (include) {
      cbShow.setSelected(true);
    }
    cbShow.setEnabled(visibleDirectives);
    pnlRoot.setVisible(cbShow.isSelected() && cbShow.isEnabled());
  }

  boolean isDifferentFromCheckpoint(final boolean checkInclude) {
    Iterator<DirectiveSetInterface> iterator = directiveSetArray.iterator();
    while (iterator.hasNext()) {
      DirectiveSetInterface directiveSet = iterator.next();
      if (iterator.next().isDifferentFromCheckpoint(checkInclude)) {
        return true;
      }
    }
    return false;
  }

  void close() {
    cbShow.setSelected(false);
    pnlRoot.setVisible(false);
  }

  Component getComponent() {
    return pnlRoot;
  }

  Component getShowCheckBox() {
    return cbShow;
  }

  void addListeners() {
    cbShow.addActionListener(new DirectiveSectionListener(this));
  }

  private void action() {
    pnlRoot.setVisible(cbShow.isEnabled() && cbShow.isSelected());
    UIHarness.INSTANCE.pack(manager);
  }

  private void setTooltips() {
    cbShow.setToolTipText("Show directives.");
  }

  private static final class DirectiveSectionListener implements ActionListener {
    private final DirectiveSectionPanel panel;

    private DirectiveSectionListener(final DirectiveSectionPanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.action();
    }
  }
}
