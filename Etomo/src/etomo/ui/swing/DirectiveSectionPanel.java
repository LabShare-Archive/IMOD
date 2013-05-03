package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
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
  private final DirectiveEditorDialog container;

  private int currentSize = 0;

  private DirectiveSectionPanel(BaseManager manager, final AxisType sourceAxisType,
      final DirectiveTool tool, final DirectiveDescrSection descrSection,
      final DirectiveMap directiveMap, final DirectiveEditorDialog container) {
    this.manager = manager;
    this.sourceAxisType = sourceAxisType;
    this.tool = tool;
    this.descrSection = descrSection;
    this.directiveMap = directiveMap;
    this.container = container;
    cbShow = new CheckBox(descrSection.toString());
  }

  static DirectiveSectionPanel getInstance(final BaseManager manager,
      final DirectiveDescrSection descrSection, final DirectiveMap directiveMap,
      final AxisType sourceAxisType, final DirectiveTool tool,
      final DirectiveEditorDialog container) {
    DirectiveSectionPanel instance = new DirectiveSectionPanel(manager, sourceAxisType,
        tool, descrSection, directiveMap, container);
    instance.createPanel();
    instance.addListeners();
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
          manager, directive, sourceAxisType, tool);
      directiveSetArray.add(directiveSet);
      pnlDirectives.add(directiveSet.getComponent());
    }
    updateDisplay();
  }

  void updateDisplay() {
    currentSize = 0;
    Iterator<DirectiveSetInterface> iterator = directiveSetArray.iterator();
    boolean visibleDirectives = false;
    while (iterator.hasNext()) {
      DirectiveSetInterface directiveSet = iterator.next();
      directiveSet.updateDisplay();
      if (directiveSet.isVisible()) {
        visibleDirectives = true;
        currentSize++;
      }
    }
    cbShow.setEnabled(visibleDirectives);
    pnlRoot.setVisible(cbShow.isEnabled() && cbShow.isSelected());
  }

  int getCurrentSize() {
    return currentSize;
  }

  Component getComponent() {
    return pnlRoot;
  }

  Component getShowCheckBox() {
    return cbShow;
  }

  boolean isShow() {
    return cbShow.isSelected();
  }

  void addListeners() {
    cbShow.addActionListener(new DirectiveSectionListener(this));
  }

  private void action() {
    container.showSection();
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
