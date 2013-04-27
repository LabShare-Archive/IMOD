package etomo.ui.swing;

import java.awt.Component;
import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.logic.DirectiveTool;
import etomo.storage.Directive;
import etomo.storage.DirectiveDescrSection;
import etomo.storage.DirectiveMap;
import etomo.type.AxisType;
import etomo.type.DialogType;

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
final class DirectiveSectionPanel implements Expandable {
  public static final String rcsid = "$Id:$";

  private final JPanel pnlRoot = new JPanel();
  private final JPanel pnlBody = new JPanel();
  private final List<DirectiveSetPanel> directivePanelArray = new ArrayList<DirectiveSetPanel>();
  private final JPanel pnlDirectives = new JPanel();

  private final BaseManager manager;
  private final PanelHeader header;
  private final AxisType sourceAxisType;
  private final DirectiveTool tool;
  private final DirectiveDescrSection descrSection;
  private final DirectiveMap directiveMap;

  private GridLayout gridlayout = null;

  private DirectiveSectionPanel(BaseManager manager, final AxisType sourceAxisType,
      final DirectiveTool tool, final DirectiveDescrSection descrSection,
      final DirectiveMap directiveMap) {
    this.manager = manager;
    this.sourceAxisType = sourceAxisType;
    this.tool = tool;
    this.descrSection = descrSection;
    this.directiveMap = directiveMap;
    header = PanelHeader.getInstance(descrSection.toString(), this,
        DialogType.DIRECTIVE_EDITOR);
  }

  static DirectiveSectionPanel getInstance(final BaseManager manager,
      final DirectiveDescrSection descrSection, final DirectiveMap directiveMap,
      final AxisType sourceAxisType, final DirectiveTool tool) {
    DirectiveSectionPanel instance = new DirectiveSectionPanel(manager, sourceAxisType,
        tool, descrSection, directiveMap);
    instance.createPanel();
    return instance;
  }

  private void createPanel() {
    // construct
    gridlayout = new GridLayout(1, 2, 20, 5);
    // root panel
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(BorderFactory.createEtchedBorder());
    pnlRoot.add(header.getContainer());
    pnlRoot.add(pnlBody);
    // body panel
    pnlBody.setLayout(new BoxLayout(pnlBody, BoxLayout.Y_AXIS));
    pnlBody.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlBody.add(pnlDirectives);
    // directives panel
    Iterator<String> nameIterator = descrSection.nameIterator();
    boolean leaveOpen = false;
    int count = 0;
    pnlDirectives.setLayout(gridlayout);
    while (nameIterator.hasNext()) {
      Directive directive = directiveMap.get(nameIterator.next());
      if (directive == null) {
        continue;
      }
      // directive set panels
      DirectiveSetPanel directiveSetPanel = DirectiveSetPanel.getInstance(directive,
          sourceAxisType, tool);
      directivePanelArray.add(directiveSetPanel);
      boolean visible = false;
      if (directiveSetPanel.updateVisible()) {
        visible = true;
        count++;
      }
      if (visible) {
        pnlDirectives.add(directiveSetPanel.getComponent());
        if (directiveSetPanel.isInclude()) {
          leaveOpen = true;
        }
      }
    }
    ExpandButton btnOpenClose = header.getOpenCloseButton();
    btnOpenClose.setEnabled(count > 0);
    if (count > 0) {
      int rows = count / 2;
      if (rows * 2 < count) {
        rows++;
      }
      gridlayout.setRows(rows);
    }
    else {
      btnOpenClose.setExpanded(false);
    }
    if (!leaveOpen) {
      header.setOpen(false);
    }
  }

  void updateDisplay(final boolean updateInclude) {
    pnlDirectives.removeAll();
    Iterator<DirectiveSetPanel> iterator = directivePanelArray.iterator();
    boolean leaveOpen = false;
    int count = 0;
    while (iterator.hasNext()) {
      DirectiveSetPanel directiveSetPanel = iterator.next();
      // directive set panels
      if (updateInclude) {
        directiveSetPanel.updateIncluded();
      }
      if (directiveSetPanel.updateVisible()) {
        count++;
        pnlDirectives.add(directiveSetPanel.getComponent());
        if (directiveSetPanel.isInclude()) {
          leaveOpen = true;
        }
      }
    }
    ExpandButton btnOpenClose = header.getOpenCloseButton();
    btnOpenClose.setEnabled(count > 0);
    if (count > 0) {
      int rows = count / 2;
      if (rows * 2 < count) {
        rows++;
      }
      gridlayout.setRows(rows);
    }
    else {
      btnOpenClose.setExpanded(false);
    }
  }

  Component getComponent() {
    return pnlRoot;
  }

  public void expand(final ExpandButton button) {
    if (button == null) {
      return;
    }
    if (header.equalsOpenClose(button)) {
      pnlBody.setVisible(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(manager);
  }

  public final void expand(final GlobalExpandButton button) {
  }
}
