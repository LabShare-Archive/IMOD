package etomo.ui.swing;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.storage.Directive;
import etomo.storage.DirectiveEditorSection;
import etomo.storage.DirectiveMap;
import etomo.storage.DirectiveType;
import etomo.type.AxisID;
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
public final class DirectiveEditor implements Expandable {
  public static final String rcsid = "$Id:$";

  private final JFrame frame = new JFrame();
  private final JPanel pnlRoot = new JPanel();

  private final AxisID axisID;
  private final BaseManager manager;
  private final List<DirectiveEditorSection> sectionArray;
  private final DirectiveMap setupMap;
  private final DirectiveMap paramMap;

  private PanelHeader[] headerArray = null;
  private JPanel[] pnlArray = null;
  private JPanel[] pnlBodyArray = null;

  private DirectiveEditor(final BaseManager manager, final AxisID axisID,
      final List<DirectiveEditorSection> sectionArray, final DirectiveMap setupMap,
      final DirectiveMap paramMap) {
    this.manager = manager;
    this.axisID = axisID;
    this.sectionArray = sectionArray;
    this.setupMap = setupMap;
    this.paramMap = paramMap;
  }

  public static DirectiveEditor getInstance(final BaseManager manager,
      final AxisID axisID, final List<DirectiveEditorSection> sectionArray,
      final DirectiveMap setupMap, final DirectiveMap paramMap) {
    DirectiveEditor instance = new DirectiveEditor(manager, axisID, sectionArray,
        setupMap, paramMap);
    instance.createPanel();
    return instance;
  }

  private void createPanel() {
    frame.getRootPane().add(pnlRoot);
    int size = sectionArray.size();
    headerArray = new PanelHeader[size];
    pnlArray = new JPanel[size];
    pnlBodyArray = new JPanel[size];
    int sIndex = 0;
    Iterator<DirectiveEditorSection> iterator = sectionArray.iterator();
    List<DirectivePanel> directivePanelArray = new ArrayList<DirectivePanel>();
    while (iterator.hasNext()) {
      pnlArray[sIndex] = new JPanel();
      pnlBodyArray[sIndex] = new JPanel();
      DirectiveEditorSection section = iterator.next();
      // root panel
      pnlRoot.add(pnlArray[sIndex]);
      // main section panels
      pnlArray[sIndex].setLayout(new BoxLayout(pnlArray[sIndex], BoxLayout.Y_AXIS));
      pnlArray[sIndex].setBorder(BorderFactory.createEtchedBorder());
      headerArray[sIndex] = PanelHeader.getInstance(section.toString(), this,
          DialogType.DIRECTIVE_EDITOR);
      headerArray[sIndex].setHeaderId(sIndex);
      pnlArray[sIndex].add(headerArray[sIndex].getContainer());
      pnlArray[sIndex].add(pnlBodyArray[sIndex]);
      // body panels
      pnlBodyArray[sIndex]
          .setLayout(new BoxLayout(pnlBodyArray[sIndex], BoxLayout.Y_AXIS));
      // Pick a directive map for this section
      DirectiveType directiveType = section.getDirectiveType();
      DirectiveMap map = directiveType == DirectiveType.SETUP_SET ? setupMap : paramMap;
      Iterator<String> nameIterator = section.nameIterator();
      while (nameIterator.hasNext()) {
        Directive directive = map.get(nameIterator.next());
        if (directive == null) {
          continue;
        }
        //directive panels
        DirectivePanel directivePanel = DirectivePanel.getInstance(directive);
        directivePanelArray.add(directivePanel);
        pnlBodyArray[sIndex].add(directivePanel.getComponent());
      }
      sIndex++;
    }
  }

  public final void expand(final ExpandButton button) {
    if (button == null || headerArray == null) {
      return;
    }
    int index = button.getHeaderId();
    if (index == -1 || headerArray.length < 1 || headerArray.length <= index
        || headerArray[index] == null || !headerArray[index].equalsOpenClose(button)) {
      return;
    }
    pnlBodyArray[index].setVisible(button.isExpanded());
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  public final void expand(final GlobalExpandButton button) {
  }
}
